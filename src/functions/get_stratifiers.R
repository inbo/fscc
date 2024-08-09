
#' Get harmonised plot-specific information
#'
#' This function generates a table with mostly harmonised metadata per plot,
#' such as the WRB Reference Soil Group, the European Forest Type Category
#' and the humus form; for plots included in the solid soil surveys.
#'
#' @param level A character string specifying the ICP Forests monitoring level
#'              for which the table should be generated. Possible values are
#'              "LI" or "LII".
#'
#' @return A dataframe with the metadata per plot
#' @export
#'
#' @examples
#' get_stratifiers(level = "LII")


get_stratifiers <- function(level) {

  assertthat::assert_that(level %in% c("LI", "LII"))

  if (level == "LI") {
    code_survey <- "s1"
  }

  if (level == "LII") {
    code_survey <- "so"
  }

  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("sf"),
            require("geodata"))

  source("./src/functions/as_sf.R")
  source("./src/functions/get_env.R")
  source("./src/functions/overlay_tif.R")

  ## Import general source data ----

  d_forest_type <-
    read.csv2("./data/additional_data/d_forest_type.csv") %>%
    select(code, short_descr) %>%
    mutate(code = as.character(code))

  d_soil_group <-
    read.csv2("./data/raw_data/so/adds/dictionaries/d_soil_group.csv") %>%
    select(code, description)

  d_soil_adjective <-
    read.csv2("./data/raw_data/so/adds/dictionaries/d_soil_adjective.csv")

  d_humus <-
    read.csv2("./data/raw_data/so/adds/dictionaries/d_humus.csv") %>%
    filter(is.na(valid_to_survey_year)) %>%
    arrange(code) %>%
    select(code, description)

  d_tree_spec <-
    read.csv2("./data/raw_data/si/adds/dictionaries/d_tree_spec.csv") %>%
    select(code, description)

  d_parent_material <-
    read.csv2("./data/raw_data/so/adds/dictionaries/d_parent_material.csv") %>%
    # For now: filter for parent material codes from the same type like the
    # European Soil Database. The types from before 2005 need to be harmonised
    # to this new version.
    filter(is.na(valid_to_survey_year)) %>%
    mutate(description = str_to_title(description)) %>%
    filter(code != 0) %>%
    arrange(code) %>%
    select(code, description)

  # Import the shapefile with biogeographical regions

  biogeo_sf <-
    read_sf(paste0("./data/additional_data/shapefiles/",
                   "BiogeoRegions2016.shp")) %>%
    # Reduce the file size
    st_simplify(dTolerance = 1000) %>%
    # Remove category "outside"
    filter(.data$short_name != "outside") %>%
    # Rename category "Black Sea"
    mutate(code = if_else(.data$short_name == "blackSea",
                          "Black Sea", .data$code)) %>%
    select(code, geometry)





  ## Make overlays with shapefiles ----


  # Get climate data(WorldClim version 2.1; 1970-2000)

  path_worldclim <- "./data/additional_data/shapefiles/worldclim/wc2.1_30s_"

  clim_sf <-
    get_env(paste0("coordinates_", code_survey)) %>%
    as_sf

  # Add average temperatures (for twelve months of the year)
  # in °C

  for (i in seq(1, 12)) {

    path_worldclim_i <- paste0(path_worldclim,
                               "tavg/wc2.1_30s_tavg_",
                               sprintf("%02d", i),
                               ".tif")

    clim_sf <- overlay_tif(sf1 = clim_sf,
                           path_tif = path_worldclim_i)

  }

  # Select the columns that start with "wc2.1_30s_tavg_"

  tavg_cols <- grep("^wc2.1_30s_tavg_", names(clim_sf), value = TRUE)

  clim_sf <- clim_sf %>%
    st_drop_geometry() %>%
    # Calculate row-wise average over the twelve months using dplyr
    mutate(tavg = round(rowMeans(select(., all_of(tavg_cols)),
                                 na.rm = TRUE), 1)) %>%
    # Remove the original columns
    select(-all_of(tavg_cols)) %>%
    as_sf


  # Add cumulative precipitation (for twelve months of the year)
  # in mm

  for (i in seq(1, 12)) {

    path_worldclim_i <- paste0(path_worldclim,
                               "prec/wc2.1_30s_prec_",
                               sprintf("%02d", i),
                               ".tif")

    clim_sf <- overlay_tif(sf1 = clim_sf,
                           path_tif = path_worldclim_i)

  }

  # Select the columns that start with "wc2.1_30s_prec_"

  prec_cols <- grep("^wc2.1_30s_prec_", names(clim_sf), value = TRUE)

  clim_sf <- clim_sf %>%
    st_drop_geometry() %>%
    # Calculate row-wise average over the twelve months using dplyr
    mutate(prec = round(rowSums(select(., all_of(prec_cols)),
                                na.rm = TRUE), 0)) %>%
    # Remove the original columns
    select(-all_of(prec_cols)) %>%
    as_sf


  # Elevation data from worlclim
  # in meter

  clim_sf <- overlay_tif(sf1 = clim_sf,
                         path_tif = paste0(path_worldclim,
                                           "elev/wc2.1_30s_elev.tif")) %>%
    st_drop_geometry() %>%
    rename(elev = "wc2.1_30s_elev") %>%
    as_sf


  # Add parent material data

  par_mat_dom1_dictionary <-
    read.csv(paste0("./data/additional_data/shapefiles/",
                    "European Soil Database/SoilDB_rasters/",
                    "par_mat_dom1_dictionary.csv"),
             sep = ";") %>%
    mutate(par_mat_dom1 = str_to_sentence(par_mat_dom1))

  clim_sf <- overlay_tif(sf1 = clim_sf,
                      path_tif = paste0("./data/additional_data/shapefiles/",
                                        "European Soil Database/",
                                        "SoilDB_rasters/parmado1.tif")) %>%
    st_drop_geometry() %>%
    left_join(par_mat_dom1_dictionary %>%
                rename(parmado1 = PAR.MAT.DOM1) %>%
                rename(parent_material_esd = par_mat_dom1) %>%
                select(parmado1, parent_material_esd),
              by = "parmado1") %>%
    select(-parmado1)





  # 1. "so" ----

  if (level == "LII") {




  ### Import source data ----

  # Retrieve data for base saturation

  if (!"sum_base_cations" %in% names(get_env("so_som"))) {
    df_bs <- read.csv("./data/layer1_data/so/so_som.csv",
                      sep = ";")
  } else {
    df_bs <- get_env("so_som")
  }

  df_bs <- df_bs %>%
    mutate(base_saturation = case_when(
      (!is.na(base_saturation)) ~ 0.01 * base_saturation,
      (!is.na(sum_base_cations) & !is.na(sum_acid_cations)) ~
        sum_base_cations / (sum_base_cations + sum_acid_cations),
      .default = NA))



  # Import data from Copernicus World-DEM30 (Europe incl. Cyprus and Canaries)
  # (compiled within ESB WBM analyses)

  dir <- "./data/additional_data/ICP-LII-plots_slope_aspec_COPDEM30.csv"

  assertthat::assert_that(file.exists(dir))

  # Define the bin edges
  # Assuming that "North" (code_orientation = 1) ranges from -22.5° to 22.5°
  # etc

  bin_edges <- data.frame(
    bin_low = c(0,
                seq(360 / 8 / 2, 360 - 360 / 8 / 2, length.out = 8))) %>%
    mutate(
      bin_up = case_when(
        bin_low %in% c(0, 337.5) ~ bin_low + 360 / 8 / 2, # North
        TRUE ~ (bin_low + 360 / 8)),
      code_orientation = case_when(
        bin_low %in% c(0, 337.5) ~ 1, # North
        TRUE ~ (bin_low + 360 / 8 / 2) / 45 + 1))

  slpasp <- read.csv(dir) %>%
    as_tibble %>%
    select(-ID) %>%
    mutate(
      slope = round(slope, 1),
      aspect = round(aspect, 1)) %>%
    rowwise() %>%
    mutate(
      code_orientation = bin_edges %>%
        filter(aspect >= bin_low & aspect < bin_up) %>%
        pull(code_orientation) %>%
        first()) %>%
    ungroup








  # Retrieve manually harmonised WRB and EFTC and aggregate per plot

  dir <- paste0("./data/additional_data/",
                "SO_PRF_ADDS.xlsx")

  assertthat::assert_that(file.exists(dir),
                          msg = paste0("'", dir, "' does not exist."))

  so_prf_adds_agg <-
    openxlsx::read.xlsx(dir,
                        sheet = 1) %>%
    rename(bs_class = "BS.(high/low)") %>%
    mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
    filter(!is.na(plot_id)) %>%
    # Some columns contain a "_" which is used as a separator below.
    # Replace by a dot.
    mutate_at(vars(c("RSGu", "QUALu", "SPECu", "METHOD_RSGu", "DEPTHSTOCK",
                     "bs_class", "EFTC_harmonised", "Source_EFTC",
                     "code_humus", "unified_humus", "remark")),
              ~str_replace_all(., "_", ".")) %>%
    mutate(soil_wrb = paste0(RSGu, "_",
                             QUALu, "_",
                             SPECu, "_",
                             METHOD_RSGu, "_",
                             DEPTHSTOCK, "_",
                             bs_class, "_",
                             EFTC_harmonised, "_",
                             Source_EFTC, "_",
                             main.tree.species.code, "_",
                             code_humus, "_",
                             unified_humus, "_",
                             remark)) %>%
    # Filter for the most recent survey_year
    group_by(plot_id) %>%
    filter(survey_year == max(survey_year)) %>%
    ungroup() %>%
    group_by(plot_id) %>%
    # Sometimes there are different options, e.g. plot_id 60_9
    # No good way to solve this - we just have to pick one
    reframe(soil_wrb =
                names(which.max(table(soil_wrb[!is.na(soil_wrb)])))) %>%
    ungroup() %>%
    # Split the data back into the original columns
    separate(soil_wrb,
             into = c("code_wrb_soil_group",
                      "code_wrb_qualifier_1",
                      "code_wrb_spezifier_1",
                      "method_wrb_harmonisation_fscc",
                      "depth_stock",
                      "bs_class",
                      "code_forest_type",
                      "eftc_source",
                      "code_tree_species",
                      "code_humus",
                      "humus_type",
                      "remark_harmonisation_fscc"),
             sep = "_") %>%
    mutate(depth_stock = as.numeric(depth_stock)) %>%
    mutate(code_tree_species = as.numeric(code_tree_species)) %>%
    mutate_if(
      function(x) !is.Date(x),
      ~ifelse(. == "NA" | . == "", NA_character_, .)) %>%
    select(-code_wrb_spezifier_1,
           # Since the harmonised humus form is already included
           -code_humus)





  ### Generate eff_soil_depth ----

  assertthat::assert_that(
    n_distinct(get_env("data_availability_so")$plot_id) ==
      nrow(get_env("data_availability_so")))

  # Estimate the exact "effective soil depth"

  so_eff_soil_depth_sources <- get_env("data_availability_so") %>%
    select(country, code_country, code_plot, plot_id) %>%
    # depth_stock
    # Using the harmonised soil depths reported in so_prf_adds ("depth_stock")
    # The depths in this file are limited to a maximum of 100 cm and
    # the plausibility of these depths has been manually cross-checked.
    # For example, WRB soil classification information was taken into account
    # as follows:
    # - If soil is classified as "Leptosol" without other information available
    #   → depth is set to 25 cm;
    # - If soil is classified as "Lithic leptosol" without other information
    #   available → depth is set to 10 cm;
    # - If soil has "lithic" as qualifier → depth should be shallower than
    #   100 cm.
    # These harmonised depths are compared to alternative options.
    left_join(so_prf_adds_agg %>%
                select(plot_id, depth_stock),
              by = "plot_id") %>%
    # eff_soil_depth_orig
    # Add originally reported effective soil depths aggregated per plot_id
    left_join(
      get_env("so_prf") %>%
        # In case so_prf has already been harmonised with so_prf_adds,
        # then the original soil depth data have been copied to
        # a column eff_soil_depth_orig. Else, such column does not exist, and
        # eff_soil_depth is supposed to contain the original values.
        mutate(
          eff_soil_depth_orig =
            if ("eff_soil_depth_orig" %in% colnames(.)) {
              eff_soil_depth_orig
            } else {
              eff_soil_depth
            }
        ) %>%
        mutate(eff_soil_depth_orig = ifelse(.data$eff_soil_depth_orig == 0,
                                            NA,
                                            .data$eff_soil_depth_orig)) %>%
        filter(!is.na(eff_soil_depth)) %>%
        arrange(code_country, code_plot) %>%
        # Filter for records with the most recent change_date
        # per plot
        group_by(plot_id) %>%
        slice_max(order_by =
                    as.Date(change_date, format = "%Y-%m-%d")) %>%
        ungroup() %>%
        arrange(code_country, code_plot) %>%
        # Filter for the most abundantly reported depths
        group_by(plot_id, eff_soil_depth_orig) %>%
        summarise(count = n(),
                  .groups = "drop") %>%
        group_by(plot_id) %>%
        filter(count == max(count)) %>%
        summarise(
          eff_soil_depth_orig = first(eff_soil_depth_orig)) %>%
        ungroup() %>%
        select(plot_id, eff_soil_depth_orig),
      by = "plot_id") %>%
    # depth_top_r
    # Add the upper depth limit of any zone with consecutive R horizons
    # at the bottom of the profile in so_pfh, aggregated per plot_id
    left_join(
      get_env("so_pfh") %>%
        arrange(unique_survey_profile, layer_number) %>%
        # Create a group for consecutive layers within each profile
        group_by(unique_survey_profile, plot_id) %>%
        # Ensure correct ordering within groups
        arrange(unique_survey_profile, plot_id, .by_group = TRUE) %>%
        mutate(
          # Create a variable to indicate whether a layer contains R
          new_group = grepl("R", horizon_master, ignore.case = TRUE),
          # Identify transitions between high and low coarse_fragment_vol
          transition = (new_group != lag(new_group, default = new_group[1])),
          # Create group_id based on transitions
          group_id = cumsum(transition)
        ) %>%
        ungroup() %>%
        # Identify ranges that do or do not contain "R"
        group_by(unique_survey_profile, plot_id, group_id) %>%
        reframe(
          depth_top = first(horizon_limit_up),
          depth_bottom = last(horizon_limit_low),
          contains_R = any(grepl("R", horizon_master, ignore.case = TRUE))) %>%
        ungroup() %>%
        # Find the bottom cluster for each profile
        group_by(unique_survey_profile, plot_id) %>%
        # Arrange so the deepest cluster is first
        arrange(desc(group_id)) %>%
        # Get the deepest cluster
        slice_head(n = 1) %>%
        ungroup() %>%
        # Keep only those where the bottom cluster contains "R"
        filter(contains_R) %>%
        select(unique_survey_profile, plot_id,
               depth_top_r = depth_top) %>%
        # Take the average depth per plot
        group_by(plot_id) %>%
        reframe(depth_top_r = mean(depth_top_r, na.rm = TRUE)) %>%
        ungroup(),
      by = "plot_id") %>%
    # max_alternative_depth
    # Add the maximum of rooting_depth, rock_depth, obstacle_depth in so_prf
    # aggregated per plot_id
    # Sometimes this information is available instead of eff_soil_depth
    # because the parameter "eff_soil_depth" has only been requested in the
    # ICP Forests manual since 2016. Before that, partners had to report
    # rooting_depth, rock_depth and obstacle_depth. The parameter
    # "eff_soil_depth" was then introduced to replace these three parameters,
    # so these three parameters are no longer requested nowadays.
    left_join(
      get_env("so_prf") %>%
        rowwise() %>%
        mutate(rooting_depth = ifelse(.data$rooting_depth == 0 |
                                        .data$rooting_depth == 999,
                                      NA,
                                      .data$rooting_depth),
               rock_depth = ifelse(.data$rock_depth == 0 |
                                     .data$rock_depth == 999,
                                   NA,
                                   .data$rock_depth),
               obstacle_depth = ifelse(.data$obstacle_depth == 0 |
                                         .data$obstacle_depth == 999,
                                       NA,
                                       .data$obstacle_depth)) %>%
        mutate(max_alternative_depth =
                 ifelse(any(!is.na(c(.data$rooting_depth,
                                     .data$rock_depth,
                                     .data$obstacle_depth))),
                        # Maximum of these three depths
                        max(c(.data$rooting_depth,
                              .data$rock_depth,
                              .data$obstacle_depth),
                            na.rm = TRUE),
                        NA_real_)) %>%
        ungroup %>%
        filter(!is.na(max_alternative_depth)) %>%
        arrange(code_country, code_plot) %>%
        # Filter for records with the most recent change_date
        # per plot
        group_by(plot_id) %>%
        slice_max(order_by =
                    as.Date(change_date, format = "%Y-%m-%d")) %>%
        ungroup() %>%
        arrange(code_country, code_plot) %>%
        # Filter for the most abundantly reported depths
        group_by(plot_id, max_alternative_depth) %>%
        summarise(count = n(),
                  .groups = "drop") %>%
        group_by(plot_id) %>%
        filter(count == max(count)) %>%
        summarise(
          max_alternative_depth = first(max_alternative_depth)) %>%
        ungroup() %>%
        select(plot_id, max_alternative_depth),
      by = "plot_id") %>%
    # depth_top_stones
    # Add the upper depth limit of any zone with consecutive high stone content
    # (>= 80 %) at the bottom of the profile in so_som and so_pfh,
    # aggregated per plot_id
    # We use 80 % as the threshold, as according to its definition,
    # the continuous rock - from which the upper depth is considered as the
    # eff_soil_depth - can contain up to 20 vol% of cracks.
    left_join(
      bind_rows(
        # Combine profiles from "so_som" and "so_pfh" to evaluate
        # the stone content
        get_env("so_som") %>%
          mutate(unique_survey_profile =
                   paste0("so_som_", unique_survey_repetition)) %>%
          select(unique_survey_profile, plot_id, layer_number,
                 layer_limit_superior, layer_limit_inferior,
                 coarse_fragment_vol),
        get_env("so_pfh") %>%
          # Check if the column "coarse_fragment_vol" is missing
          mutate(
            coarse_fragment_vol =
              if (!("coarse_fragment_vol" %in% colnames(.))) {
                # Create `coarse_fragment_vol` by combining
                # `coarse_fragment_vol_converted` and `coarse_fragment_vol_avg`
                coalesce(coarse_fragment_vol_converted, coarse_fragment_vol_avg)
              } else {
                # If the column exists, keep it as it is
                coarse_fragment_vol
              }
          ) %>%
          mutate(unique_survey_profile =
                   paste0("so_pfh_", unique_survey_profile)) %>%
          select(unique_survey_profile, plot_id, layer_number,
                 layer_limit_superior = horizon_limit_up,
                 layer_limit_inferior = horizon_limit_low,
                 coarse_fragment_vol)) %>%
        # Create a group for consecutive layers within each profile
        group_by(unique_survey_profile, plot_id) %>%
        # Filter to include only profiles with at least one non-NA in
        # coarse_fragment_vol
        filter(any(!is.na(coarse_fragment_vol))) %>%
        # Ensure correct ordering within groups
        arrange(unique_survey_profile, plot_id, layer_number,
                .by_group = TRUE) %>%
        mutate(
          # Create a variable to indicate whether the coarse_fragment_vol
          # crosses the threshold
          new_group = !is.na(coarse_fragment_vol) & coarse_fragment_vol >= 80,
          # Identify transitions between high and low coarse_fragment_vol
          transition = (new_group != lag(new_group, default = new_group[1])),
          # Create group_id based on transitions
          group_id = cumsum(transition)
        ) %>%
        ungroup() %>%
        # Identify ranges that do or do not contain a lot of stones
        group_by(unique_survey_profile, plot_id, group_id) %>%
        reframe(
          depth_top = first(layer_limit_superior),
          depth_bottom = last(layer_limit_inferior),
          much_stones = any(!is.na(coarse_fragment_vol) &
                              coarse_fragment_vol >= 80)) %>%
        ungroup() %>%
        # Find the bottom cluster for each profile
        group_by(unique_survey_profile, plot_id) %>%
        # Arrange so the deepest cluster is first
        arrange(desc(group_id)) %>%
        # Get the deepest cluster
        slice_head(n = 1) %>%
        ungroup() %>%
        # Keep only those where the bottom cluster has a lot of stones
        filter(much_stones) %>%
        select(unique_survey_profile, plot_id,
               depth_top_stones = depth_top) %>%
        # Take the average depth per plot
        group_by(plot_id) %>%
        reframe(depth_top_stones = mean(depth_top_stones, na.rm = TRUE)) %>%
        ungroup(),
      by = "plot_id") %>%
    # depth_bottom_pfh
    # Add the lowest depth of the deepest "so_pfh" profile
    # (excluding R horizons) per plot
    left_join(
      get_env("so_pfh") %>%
        # Remove any horizons containing "R"
        filter(!(grepl("R", horizon_master, ignore.case = TRUE))) %>%
        group_by(unique_survey_profile, plot_id) %>%
        # Retrieve the lowest depth of the profile
        reframe(
          depth_bottom = ifelse(
            any(!is.na(horizon_limit_low)),
            max(horizon_limit_low, na.rm = TRUE),
            NA_real_),
          bg = any(!is.na(horizon_limit_low) &
                     horizon_limit_low > 0),
          change_date = min(as.Date(change_date,
                                    format = "%Y-%m-%d"))) %>%
        ungroup() %>%
        # Filter for profiles with at least any below-ground records
        filter(bg == TRUE) %>%
        select(-bg) %>%
        arrange(plot_id) %>%
        # Filter for records with the most recent change_date
        # per plot
        group_by(plot_id) %>%
        slice_max(order_by =
                    as.Date(change_date, format = "%Y-%m-%d")) %>%
        ungroup() %>%
        # Take the maximum depth per plot_id
        group_by(plot_id) %>%
        reframe(depth_bottom_pfh = max(depth_bottom, na.rm = TRUE)),
      by = "plot_id") %>%
    # Combine the different data sources into one harmonised value for
    # eff_soil_depth
    mutate(
      # Record the data source for the final value
      eff_soil_depth_source = case_when(
        !is.na(depth_top_stones) ~ "Top of stony zone at bottom",
        !is.na(depth_top_r) ~ "Top of parent material layers at bottom",
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig > 0 &
          eff_soil_depth_orig < 500 &
          eff_soil_depth_orig != 100 ~ "Original non-default eff_soil_depth",
        !is.na(depth_stock) &
          depth_stock < 100 ~ "Harmonised shallow (< 100) depth for stocks",
        !is.na(max_alternative_depth) &
          max_alternative_depth > 100 ~
          "Maximum of rooting, rock and obstacle depth",
        !is.na(depth_bottom_pfh) & depth_bottom_pfh > 100 ~
          "Bottom of deep (> 100) profile ('pfh')",
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig == 100 ~ "Original eff_soil_depth reporting 100",
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig >= 500 ~ "Original eff_soil_depth reporting 999",
        TRUE ~ "No info (default 999)"),
      eff_soil_depth = case_when(
        # Priority 1:
        # If there is a consecutive zone with >= 80 % coarse fragments
        # at the bottom of the profile (so_som or so_pfh), use its upper depth.
        # This information usually seems reliable and convincing, and consistent
        # with the definition of the upper limit of continuous rock.
        # (Note: this can be 0! Also note that any intermediate zones with
        #  >= 80 % coarse fragments, i.e. with a zone with lower stone content
        #  below, are ignored)
        !is.na(depth_top_stones) ~ depth_top_stones,
        # Priority 2:
        # If there is a consecutive zone with "R" in its horizon_master in
        # so_pfh at the bottom of the profile, use its upper depth.
        # This information usually seems reliable and convincing, although
        # there is sometimes a zone with >= 80 % coarse fragments on top
        # (which then receives the priority).
        !is.na(depth_top_r) ~ depth_top_r,
        # Priority 3:
        # If the originally reported eff_soil_depth is > 0 and < 500
        # and != 100, use this, since its quality seems quite reliable if
        # no default value is given.
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig > 0 &
          eff_soil_depth_orig < 500 &
          eff_soil_depth_orig != 100 ~ eff_soil_depth_orig,
        # Priority 4:
        # If the manually harmonised depth (which is supposed to be used for
        # stock calculations and which is limited to a maximum of 100 cm) is
        # shallower than 100 cm, use this.
        !is.na(depth_stock) & depth_stock < 100 ~ depth_stock,
        # Priority 5:
        # If the maximum of the alternative depths (rooting_depth, rock_depth,
        # obstacle_depth) is > 100 cm, use this.
        !is.na(max_alternative_depth) &
          max_alternative_depth > 100 ~ max_alternative_depth,
        # Priority 6:
        # If the depth of the bottom of the so_pfh profile is > 100 cm,
        # use this.
        !is.na(depth_bottom_pfh) & depth_bottom_pfh > 100 ~ depth_bottom_pfh,
        # Priority 7:
        # If the originally reported eff_soil_depth is equal to 100,
        # use this.
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig == 100 ~ eff_soil_depth_orig,
        # Priority 8:
        # If the originally reported eff_soil_depth is >= 500,
        # use this.
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig >= 500 ~ eff_soil_depth_orig,
        # Priority 9:
        # If none of those conditions are met (i.e. default), report 999.
        TRUE ~ 999)) %>%
    mutate(eff_soil_depth = round(eff_soil_depth, 1)) %>%
    select(plot_id, eff_soil_depth)



  ### Compose the final data ----


  df_strat <- get_env("data_availability_so") %>%
    select(country, partner_short, plot_id, code_plot) %>%
    # This includes the coordinates
    left_join(clim_sf, by = "plot_id") %>%
    as_sf %>%
    # Add biogeographical region
    st_join(biogeo_sf) %>%
    rename(biogeo = code) %>%
    st_drop_geometry() %>%
    distinct(plot_id, .keep_all = TRUE) %>%
    arrange(partner_short, code_plot) %>%
    # Add manually harmonised stratifiers
    left_join(so_prf_adds_agg,
              by = "plot_id") %>%
    # Add WRB etc
    left_join(get_env("so_prf") %>%
                select(plot_id, code_wrb_soil_group,
                       code_wrb_qualifier_1, code_wrb_spezifier_1,
                       change_date,
                       date_profile_desc) %>%
                filter(!is.na(code_wrb_soil_group)) %>%
                # Filter for records with the most recent change_date
                # per plot
                group_by(plot_id) %>%
                slice_max(order_by =
                            as.Date(change_date, format = "%Y-%m-%d")) %>%
                ungroup() %>%
                # Take the most abundant value
                # Sometimes there are different WRBs of the same most recent
                # change_date for a plot. In that case it is at this stage
                # not yet clear which WRB is more reliable at the plot level.
                # To do: clarify this
                group_by(plot_id, code_wrb_soil_group,
                         code_wrb_qualifier_1, code_wrb_spezifier_1) %>%
                summarise(count = n(),
                          date_profile_desc = first(date_profile_desc),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                filter(count == max(count)) %>%
                summarise(
                  uncertain_soil_group_options =
                    ifelse(n_distinct(code_wrb_soil_group) > 1,
                           paste0(unique(code_wrb_soil_group), collapse = "_"),
                           NA),
                  code_wrb_soil_group = first(code_wrb_soil_group),
                  code_wrb_qualifier_1 = first(code_wrb_qualifier_1),
                  code_wrb_spezifier_1 = first(code_wrb_spezifier_1),
                  date_profile_desc = first(date_profile_desc)) %>%
                ungroup() %>%
                select(plot_id, code_wrb_soil_group,
                       code_wrb_qualifier_1, code_wrb_spezifier_1,
                       uncertain_soil_group_options,
                       date_profile_desc) %>%
                # with 0 referring to "layer 0"
                rename(code_wrb_soil_group_0 = code_wrb_soil_group) %>%
                rename(code_wrb_qualifier_1_0 = code_wrb_qualifier_1),
              by = "plot_id") %>%
    # Combine sources (i.e. harmonised data and layer 0)
    mutate(
      uncertain_soil_group_options = ifelse(
        is.na(code_wrb_soil_group) & !is.na(uncertain_soil_group_options),
        uncertain_soil_group_options,
        NA),
      code_wrb_qualifier_1 = ifelse(
        !is.na(code_wrb_soil_group),
        code_wrb_qualifier_1,
        code_wrb_qualifier_1_0),
      code_wrb_soil_group = coalesce(code_wrb_soil_group,
                                     code_wrb_soil_group_0)) %>%
    select(-code_wrb_soil_group_0,
           -code_wrb_qualifier_1_0,
           -code_wrb_spezifier_1) %>% # actually not needed since not harmonised
    left_join(d_soil_group,
              by = join_by(code_wrb_soil_group == code)) %>%
    rename(wrb_soil_group = description) %>%
    left_join(d_soil_adjective %>%
                select(code, description) %>%
                rename(code_wrb_qualifier_1 = code) %>%
                rename(wrb_qualifier_1 = description),
              by = "code_wrb_qualifier_1") %>%
    # Add forest type
    left_join(get_env("si_sta") %>%
                select(plot_id, code_forest_type) %>%
                filter(code_forest_type != 99) %>%
                filter(!is.na(code_forest_type)) %>%
                group_by(plot_id, code_forest_type) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id, code_forest_type) %>%
                mutate(code_forest_type = as.character(code_forest_type)) %>%
                rename(code_forest_type_0 = code_forest_type),
              by = "plot_id") %>%
    mutate(code_forest_type = coalesce(code_forest_type,
                                       code_forest_type_0)) %>%
    select(-code_forest_type_0) %>%
    left_join(d_forest_type,
              by = join_by(code_forest_type == code)) %>%
    rename(forest_type = short_descr) %>%
    # Add humus form
    left_join(get_env("so_prf") %>%
                select(plot_id, code_humus_orig) %>%
                rename(code_humus = code_humus_orig) %>%
                filter(code_humus != 99) %>%
                filter(!is.na(code_humus)) %>%
                group_by(plot_id, code_humus) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id, code_humus),
              by = "plot_id") %>%
    left_join(d_humus,
              by = join_by(code_humus == code)) %>%
    rename(humus_type_0 = description) %>%
    mutate(humus_type_0 = case_when(
      humus_type_0 == "Amphi (or Amphihumus)" ~ "Amphi",
      humus_type_0 %in% c("Histomull", "Histomoder") ~ "Semi-terrestrial",
      TRUE ~ humus_type_0)) %>%
    mutate(humus_type = coalesce(humus_type, humus_type_0)) %>%
    select(-code_humus, -humus_type_0) %>%
    # Add parent_material
    left_join(get_env("so_prf") %>%
                select(plot_id, code_parent_material_1) %>%
                filter(code_parent_material_1 != 0) %>%
                filter(!is.na(code_parent_material_1)) %>%
                group_by(plot_id, code_parent_material_1) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                # Manual correction for plot 5_16 (according to old system)
                mutate(code_parent_material_1 =
                         ifelse(plot_id == "5_16",
                                3210,
                                code_parent_material_1)) %>%
                # Take the first digit of the code (only major classes)
                mutate(code_parent_material_dom1 =
                         as.integer(substr(code_parent_material_1, 1, 1))) %>%
                mutate(code_parent_material_dom1 =
                         1000 * code_parent_material_dom1) %>%
                select(plot_id,
                       code_parent_material_1,
                       code_parent_material_dom1),
              by = "plot_id") %>%
    left_join(d_parent_material,
              by = join_by(code_parent_material_dom1 == code)) %>%
    rename(parent_material = description) %>%
    mutate(parent_material = coalesce(parent_material,
                                      parent_material_esd)) %>%
    # Add main tree species
    left_join(get_env("si_sta") %>%
                select(plot_id, code_tree_species) %>%
                filter(code_tree_species != -9) %>%
                filter(!is.na(code_tree_species)) %>%
                group_by(plot_id, code_tree_species) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id, code_tree_species) %>%
                rename(code_tree_species_pcc = code_tree_species),
              by = "plot_id") %>%
    mutate(code_tree_species = coalesce(code_tree_species,
                                        code_tree_species_pcc)) %>%
    left_join(d_tree_spec,
              by = join_by(code_tree_species == code)) %>%
    rename(main_tree_species = description) %>%
    # Add data from Copernicus World-DEM30
    left_join(slpasp %>%
                select(plot_id,
                       slope_copdem30 = slope,
                       code_orientation_copdem30 = code_orientation),
              by = "plot_id") %>%
    # Add code_orientation (to create slope and aspect)
    left_join(get_env("si_plt") %>%
                select(plot_id, code_orientation) %>%
                filter(!is.na(code_orientation)) %>%
                group_by(plot_id, code_orientation) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id,
                       code_orientation_pcc = code_orientation),
              by = "plot_id") %>%
    # Add slope
    left_join(get_env("si_plt") %>%
                select(plot_id, slope) %>%
                filter(!is.na(slope)) %>%
                group_by(plot_id, slope) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id, slope),
              by = "plot_id") %>%
    mutate(
      slope = case_when(
        !is.na(slope) ~ slope,
        !is.na(slope_copdem30) ~ slope_copdem30,
        code_orientation_pcc == 9 ~ 0,
        TRUE ~ NA_integer_)) %>%
    # Add aspect
    mutate(
      code_orientation = coalesce(code_orientation_pcc,
                                  code_orientation_copdem30),
      aspect = case_when(
        code_orientation == 1 ~ 360,
        # aspect equal to 999 indicates "flat"
        code_orientation == 9 ~ 999,
        TRUE ~ code_orientation * 360 / 8 - 45)) %>%
    # Add altitude
    left_join(get_env("si_plt") %>%
                select(plot_id, altitude_m) %>%
                filter(!is.na(altitude_m)) %>%
                group_by(plot_id, altitude_m) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id, altitude_m),
              by = "plot_id") %>%
    mutate(altitude = coalesce(altitude_m,
                               elev)) %>%
    # Add effective soil depth
    left_join(so_eff_soil_depth_sources,
              by = "plot_id") %>%
    # Add rooting depth
    left_join(get_env("so_prf") %>%
                mutate(rooting_depth = ifelse(.data$rooting_depth == 0,
                                              NA,
                                              .data$rooting_depth)) %>%
                filter(!is.na(rooting_depth)) %>%
                # Filter for records with the most recent change_date
                # per plot
                group_by(plot_id) %>%
                slice_max(order_by =
                            as.Date(change_date, format = "%Y-%m-%d")) %>%
                ungroup() %>%
                group_by(plot_id, rooting_depth) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                filter(count == max(count)) %>%
                summarise(
                  rooting_depth = first(rooting_depth)) %>%
                ungroup() %>%
                select(plot_id, rooting_depth),
              by = "plot_id") %>%
    # Base saturation class
    # Add base saturation
    left_join(df_bs %>%
                filter(layer_limit_superior >= 20) %>%
                filter(!is.na(base_saturation)) %>%
                group_by(plot_id) %>%
                reframe(base_saturation = mean(base_saturation)) %>%
                ungroup(),
              by = "plot_id") %>%
    mutate(bs_class_0 = case_when(
      grepl("eutr", wrb_qualifier_1, ignore.case = TRUE) ~ ">= 50 %",
      grepl("dystr", wrb_qualifier_1, ignore.case = TRUE) ~ "< 50 %",
      (!is.na(base_saturation) & base_saturation >= 0.5) ~ ">= 50 %",
      (!is.na(base_saturation) & base_saturation < 0.5) ~ "< 50 %")) %>%
    mutate(bs_class = coalesce(bs_class, bs_class_0)) %>%
    mutate(bs_class = case_when(
      .data$bs_class == "low" ~ "< 50 %",
      .data$bs_class == "high" ~ ">= 50 %",
      TRUE ~ NA_character_)) %>%
    select(country,
           plot_id,
           longitude_dec,
           latitude_dec,
           tavg,
           prec,
           altitude,
           slope,
           aspect,
           eff_soil_depth,
           rooting_depth,
           wrb_soil_group,
           forest_type,
           humus_type,
           parent_material,
           biogeo,
           main_tree_species,
           bs_class,
           wrb_qualifier_1) %>%
    rename(mat = tavg,
           map = prec,
           wrb_ref_soil_group = wrb_soil_group,
           eftc = forest_type,
           humus_form = humus_type,
           biogeographical_region = biogeo)

  if ("uncertain_soil_group_options" %in% names(df_strat) &&
      length(which(!is.na(df_strat$uncertain_soil_group_options))) == 0) {

    df_strat <- df_strat %>%
      select(-uncertain_soil_group_options)
  }

}




  # 2. "s1" ----

  if (level == "LI") {



  ### Import source data ----

  # Retrieve data for base saturation

  if (!"base_saturation" %in% names(get_env("s1_som"))) {
    df_bs <- read.csv("./data/layer1_data/s1/s1_som.csv",
                      sep = ";")
  } else {
    df_bs <- get_env("s1_som")
  }

  if (!"base_saturation" %in% names(df_bs)) {
    df_bs <- df_bs %>%
      mutate(base_saturation =
               ifelse(!is.na(sum_base_cations) &
                        !is.na(sum_acid_cations),
                      sum_base_cations / (sum_base_cations +
                                            sum_acid_cations),
                      NA))
  }


  # Retrieve manually harmonised WRB and EFTC and aggregate per plot

  dir <- paste0("./data/additional_data/",
                "S1_PRF_ADDS.csv")

  assertthat::assert_that(file.exists(dir),
                          msg = paste0("'", dir, "' does not exist."))

  # Retrieve harmonised stratifiers and aggregate per plot

  s1_prf_adds_agg <-
    read.csv(dir, sep = ";") %>%
    as_tibble() %>%
    mutate(date_profile_desc =
             as.Date(parsedate::parse_iso_8601(parsedate::parse_date(
               date_profile_desc)))) %>%
    mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
    mutate(soil_wrb = paste0(WRB14RSG, "_",
                             WRB14QUAL, "_",
                             EFTC_final, "_",
                             Unified_humus, "_",
                             STOCKDEPTH)) %>%
    # Filter for the most recent survey_year
    group_by(plot_id) %>%
    filter(is.na(survey_year) |
             survey_year == max(survey_year)) %>%
    ungroup() %>%
    group_by(plot_id) %>%
    # Sometimes there are different options
    # No good way to solve this - we just have to pick one profile
    reframe(soil_wrb =
              names(which.max(table(soil_wrb[!is.na(soil_wrb)]))),
            survey_year = max(survey_year),
            date_profile_desc = ifelse(
              all(is.na(.data$date_profile_desc)),
              NA_Date_,
              as.Date(max(as.Date(date_profile_desc, format = "%Y-%m-%d"),
                          na.rm = TRUE),
                      origin = "1970-01-01",
                      format = "%Y-%m-%d"))) %>%
    ungroup() %>%
    mutate(date_profile_desc = as.Date(date_profile_desc,
                                       format = "%Y-%m-%d")) %>%
    # Split the data back into the original columns
    separate(soil_wrb,
             into = c("code_wrb_soil_group",
                      "code_wrb_qualifier_1",
                      "code_forest_type",
                      "humus_type",
                      "depth_stock"),
             sep = "_") %>%
    mutate(depth_stock = ifelse(
      depth_stock == "NA",
      NA_character_,
      depth_stock)) %>%
    mutate(depth_stock = as.numeric(depth_stock)) %>%
    mutate_if(
      function(x) !is.Date(x),
      ~ifelse(. == "NA" | . == "", NA_character_, .))







  ### Generate eff_soil_depth ----

  assertthat::assert_that(
    n_distinct(get_env("data_availability_s1")$plot_id) ==
      nrow(get_env("data_availability_s1")))

  # Estimate the exact "effective soil depth"

  s1_eff_soil_depth_sources <- get_env("data_availability_s1") %>%
    select(country, code_country, code_plot, plot_id) %>%
    # depth_stock
    # Using the harmonised soil depths reported in s1_prf_adds ("depth_stock")
    # The depths in this file are limited to a maximum of 100 cm and
    # the plausibility of these depths has been manually cross-checked.
    # For example, WRB soil classification information was taken into account
    # as follows:
    # - If soil is classified as "Leptosol" without other information available
    #   → depth is set to 25 cm;
    # - If soil is classified as "Lithic leptosol" without other information
    #   available → depth is set to 10 cm;
    # - If soil has "lithic" as qualifier → depth should be shallower than
    #   100 cm.
    # These harmonised depths are compared to alternative options.
    left_join(s1_prf_adds_agg %>%
                select(plot_id, depth_stock),
              by = "plot_id") %>%
    # eff_soil_depth_orig
    # Add originally reported effective soil depths aggregated per plot_id
    left_join(
      get_env("s1_prf") %>%
        # In case s1_prf has already been harmonised with s1_prf_adds,
        # then the original soil depth data have been copied to
        # a column eff_soil_depth_orig. Else, such column does not exist, and
        # eff_soil_depth is supposed to contain the original values.
        mutate(
          eff_soil_depth_orig =
            if ("eff_soil_depth_orig" %in% colnames(.)) {
              eff_soil_depth_orig
            } else {
              eff_soil_depth
            }
        ) %>%
        mutate(eff_soil_depth_orig = ifelse(.data$eff_soil_depth_orig == 0,
                                            NA,
                                            .data$eff_soil_depth_orig)) %>%
        filter(!is.na(eff_soil_depth)) %>%
        arrange(code_country, code_plot) %>%
        # Filter for records with the most recent change_date
        # per plot
        group_by(plot_id) %>%
        slice_max(order_by =
                    as.Date(change_date, format = "%Y-%m-%d")) %>%
        ungroup() %>%
        arrange(code_country, code_plot) %>%
        # Filter for the most abundantly reported depths
        group_by(plot_id, eff_soil_depth_orig) %>%
        summarise(count = n(),
                  .groups = "drop") %>%
        group_by(plot_id) %>%
        filter(count == max(count)) %>%
        summarise(
          eff_soil_depth_orig = first(eff_soil_depth_orig)) %>%
        ungroup() %>%
        select(plot_id, eff_soil_depth_orig),
      by = "plot_id") %>%
    # depth_top_r
    # Add the upper depth limit of any zone with consecutive R horizons
    # at the bottom of the profile in s1_pfh, aggregated per plot_id
    left_join(
      get_env("s1_pfh") %>%
        arrange(unique_survey_profile, layer_number) %>%
        # Create a group for consecutive layers within each profile
        group_by(unique_survey_profile, plot_id) %>%
        # Ensure correct ordering within groups
        arrange(unique_survey_profile, plot_id, .by_group = TRUE) %>%
        mutate(
          # Create a variable to indicate whether a layer contains R
          new_group = grepl("R", horizon_master, ignore.case = TRUE),
          # Identify transitions between high and low coarse_fragment_vol
          transition = (new_group != lag(new_group, default = new_group[1])),
          # Create group_id based on transitions
          group_id = cumsum(transition)
        ) %>%
        ungroup() %>%
        # Identify ranges that do or do not contain "R"
        group_by(unique_survey_profile, plot_id, group_id) %>%
        reframe(
          depth_top = first(horizon_limit_up),
          depth_bottom = last(horizon_limit_low),
          contains_R = any(grepl("R", horizon_master, ignore.case = TRUE))) %>%
        ungroup() %>%
        # Find the bottom cluster for each profile
        group_by(unique_survey_profile, plot_id) %>%
        # Arrange so the deepest cluster is first
        arrange(desc(group_id)) %>%
        # Get the deepest cluster
        slice_head(n = 1) %>%
        ungroup() %>%
        # Keep only those where the bottom cluster contains "R"
        filter(contains_R) %>%
        select(unique_survey_profile, plot_id,
               depth_top_r = depth_top) %>%
        # Take the average depth per plot
        group_by(plot_id) %>%
        reframe(depth_top_r = mean(depth_top_r, na.rm = TRUE)) %>%
        ungroup(),
      by = "plot_id") %>%
    # max_alternative_depth
    # Add the maximum of rooting_depth, rock_depth, obstacle_depth in s1_prf
    # aggregated per plot_id
    # Sometimes this information is available instead of eff_soil_depth
    # because the parameter "eff_soil_depth" has only been requested in the
    # ICP Forests manual since 2016. Before that, partners had to report
    # rooting_depth, rock_depth and obstacle_depth. The parameter
    # "eff_soil_depth" was then introduced to replace these three parameters,
    # so these three parameters are no longer requested nowadays.
    left_join(
      get_env("s1_prf") %>%
        rowwise() %>%
        mutate(rooting_depth = ifelse(.data$rooting_depth == 0 |
                                        .data$rooting_depth == 999,
                                      NA,
                                      .data$rooting_depth),
               rock_depth = ifelse(.data$rock_depth == 0 |
                                     .data$rock_depth == 999,
                                   NA,
                                   .data$rock_depth),
               obstacle_depth = ifelse(.data$obstacle_depth == 0 |
                                         .data$obstacle_depth == 999,
                                       NA,
                                       .data$obstacle_depth)) %>%
        mutate(max_alternative_depth =
                 ifelse(any(!is.na(c(.data$rooting_depth,
                                     .data$rock_depth,
                                     .data$obstacle_depth))),
                        # Maximum of these three depths
                        max(c(.data$rooting_depth,
                              .data$rock_depth,
                              .data$obstacle_depth),
                            na.rm = TRUE),
                        NA_real_)) %>%
        ungroup %>%
        filter(!is.na(max_alternative_depth)) %>%
        arrange(code_country, code_plot) %>%
        # Filter for records with the most recent change_date
        # per plot
        group_by(plot_id) %>%
        slice_max(order_by =
                    as.Date(change_date, format = "%Y-%m-%d")) %>%
        ungroup() %>%
        arrange(code_country, code_plot) %>%
        # Filter for the most abundantly reported depths
        group_by(plot_id, max_alternative_depth) %>%
        summarise(count = n(),
                  .groups = "drop") %>%
        group_by(plot_id) %>%
        filter(count == max(count)) %>%
        summarise(
          max_alternative_depth = first(max_alternative_depth)) %>%
        ungroup() %>%
        select(plot_id, max_alternative_depth),
      by = "plot_id") %>%
    # depth_top_stones
    # Add the upper depth limit of any zone with consecutive high stone content
    # (>= 80 %) at the bottom of the profile in s1_som and s1_pfh,
    # aggregated per plot_id
    # We use 80 % as the threshold, as according to its definition,
    # the continuous rock - from which the upper depth is considered as the
    # eff_soil_depth - can contain up to 20 vol% of cracks.
    left_join(
      bind_rows(
        # Combine profiles from "s1_som" and "s1_pfh" to evaluate
        # the stone content
        get_env("s1_som") %>%
          mutate(unique_survey_profile =
                   paste0("s1_som_", unique_survey_repetition)) %>%
          select(unique_survey_profile, plot_id, layer_number,
                 layer_limit_superior, layer_limit_inferior,
                 coarse_fragment_vol),
        get_env("s1_pfh") %>%
          # Check if the column "coarse_fragment_vol" is missing
          mutate(
            coarse_fragment_vol =
              if (!("coarse_fragment_vol" %in% colnames(.))) {
                # Create `coarse_fragment_vol` by combining
                # `coarse_fragment_vol_converted` and `coarse_fragment_vol_avg`
                coalesce(coarse_fragment_vol_converted, coarse_fragment_vol_avg)
              } else {
                # If the column exists, keep it as it is
                coarse_fragment_vol
              }
          ) %>%
          mutate(unique_survey_profile =
                   paste0("s1_pfh_", unique_survey_profile)) %>%
          select(unique_survey_profile, plot_id, layer_number,
                 layer_limit_superior = horizon_limit_up,
                 layer_limit_inferior = horizon_limit_low,
                 coarse_fragment_vol)) %>%
        # Create a group for consecutive layers within each profile
        group_by(unique_survey_profile, plot_id) %>%
        # Filter to include only profiles with at least one non-NA in
        # coarse_fragment_vol
        filter(any(!is.na(coarse_fragment_vol))) %>%
        # Ensure correct ordering within groups
        arrange(unique_survey_profile, plot_id, layer_number,
                .by_group = TRUE) %>%
        mutate(
          # Create a variable to indicate whether the coarse_fragment_vol
          # crosses the threshold
          new_group = !is.na(coarse_fragment_vol) & coarse_fragment_vol >= 80,
          # Identify transitions between high and low coarse_fragment_vol
          transition = (new_group != lag(new_group, default = new_group[1])),
          # Create group_id based on transitions
          group_id = cumsum(transition)
        ) %>%
        ungroup() %>%
        # Identify ranges that do or do not contain a lot of stones
        group_by(unique_survey_profile, plot_id, group_id) %>%
        reframe(
          depth_top = first(layer_limit_superior),
          depth_bottom = last(layer_limit_inferior),
          much_stones = any(!is.na(coarse_fragment_vol) &
                              coarse_fragment_vol >= 80)) %>%
        ungroup() %>%
        # Find the bottom cluster for each profile
        group_by(unique_survey_profile, plot_id) %>%
        # Arrange so the deepest cluster is first
        arrange(desc(group_id)) %>%
        # Get the deepest cluster
        slice_head(n = 1) %>%
        ungroup() %>%
        # Keep only those where the bottom cluster has a lot of stones
        filter(much_stones) %>%
        select(unique_survey_profile, plot_id,
               depth_top_stones = depth_top) %>%
        # Take the average depth per plot
        group_by(plot_id) %>%
        reframe(depth_top_stones = mean(depth_top_stones, na.rm = TRUE)) %>%
        ungroup(),
      by = "plot_id") %>%
    # depth_bottom_pfh
    # Add the lowest depth of the deepest "s1_pfh" profile
    # (excluding R horizons) per plot
    left_join(
      get_env("s1_pfh") %>%
        # Remove any horizons containing "R"
        filter(!(grepl("R", horizon_master, ignore.case = TRUE))) %>%
        group_by(unique_survey_profile, plot_id) %>%
        # Retrieve the lowest depth of the profile
        reframe(
          depth_bottom = ifelse(
            any(!is.na(horizon_limit_low)),
            max(horizon_limit_low, na.rm = TRUE),
            NA_real_),
          bg = any(!is.na(horizon_limit_low) &
                     horizon_limit_low > 0),
          change_date = min(as.Date(change_date,
                                    format = "%Y-%m-%d"))) %>%
        ungroup() %>%
        # Filter for profiles with at least any below-ground records
        filter(bg == TRUE) %>%
        select(-bg) %>%
        arrange(plot_id) %>%
        # Filter for records with the most recent change_date
        # per plot
        group_by(plot_id) %>%
        slice_max(order_by =
                    as.Date(change_date, format = "%Y-%m-%d")) %>%
        ungroup() %>%
        # Take the maximum depth per plot_id
        group_by(plot_id) %>%
        reframe(depth_bottom_pfh = max(depth_bottom, na.rm = TRUE)),
      by = "plot_id") %>%
    # Combine the different data sources into one harmonised value for
    # eff_soil_depth
    mutate(
      # Record the data source for the final value
      eff_soil_depth_source = case_when(
        !is.na(depth_top_stones) ~ "Top of stony zone at bottom",
        !is.na(depth_top_r) ~ "Top of parent material layers at bottom",
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig > 0 &
          eff_soil_depth_orig < 500 &
          eff_soil_depth_orig != 100 ~ "Original non-default eff_soil_depth",
        !is.na(depth_stock) &
          depth_stock < 100 ~ "Harmonised shallow (< 100) depth for stocks",
        !is.na(max_alternative_depth) &
          max_alternative_depth > 100 ~
          "Maximum of rooting, rock and obstacle depth",
        !is.na(depth_bottom_pfh) & depth_bottom_pfh > 100 ~
          "Bottom of deep (> 100) profile ('pfh')",
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig == 100 ~ "Original eff_soil_depth reporting 100",
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig >= 500 ~ "Original eff_soil_depth reporting 999",
        TRUE ~ "No info (default 999)"),
      eff_soil_depth = case_when(
        # Priority 1:
        # If there is a consecutive zone with >= 80 % coarse fragments
        # at the bottom of the profile (s1_som or s1_pfh), use its upper depth.
        # This information usually seems reliable and convincing, and consistent
        # with the definition of the upper limit of continuous rock.
        # (Note: this can be 0! Also note that any intermediate zones with
        #  >= 80 % coarse fragments, i.e. with a zone with lower stone content
        #  below, are ignored)
        !is.na(depth_top_stones) ~ depth_top_stones,
        # Priority 2:
        # If there is a consecutive zone with "R" in its horizon_master in
        # s1_pfh at the bottom of the profile, use its upper depth.
        # This information usually seems reliable and convincing, although
        # there is sometimes a zone with >= 80 % coarse fragments on top
        # (which then receives the priority).
        !is.na(depth_top_r) ~ depth_top_r,
        # Priority 3:
        # If the originally reported eff_soil_depth is > 0 and < 500
        # and != 100, use this, since its quality seems quite reliable if
        # no default value is given.
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig > 0 &
          eff_soil_depth_orig < 500 &
          eff_soil_depth_orig != 100 ~ eff_soil_depth_orig,
        # Priority 4:
        # If the manually harmonised depth (which is supposed to be used for
        # stock calculations and which is limited to a maximum of 100 cm) is
        # shallower than 100 cm, use this.
        !is.na(depth_stock) & depth_stock < 100 ~ depth_stock,
        # Priority 5:
        # If the maximum of the alternative depths (rooting_depth, rock_depth,
        # obstacle_depth) is > 100 cm, use this.
        !is.na(max_alternative_depth) &
          max_alternative_depth > 100 ~ max_alternative_depth,
        # Priority 6:
        # If the depth of the bottom of the s1_pfh profile is > 100 cm,
        # use this.
        !is.na(depth_bottom_pfh) & depth_bottom_pfh > 100 ~ depth_bottom_pfh,
        # Priority 7:
        # If the originally reported eff_soil_depth is equal to 100,
        # use this.
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig == 100 ~ eff_soil_depth_orig,
        # Priority 8:
        # If the originally reported eff_soil_depth is >= 500,
        # use this.
        !is.na(eff_soil_depth_orig) &
          eff_soil_depth_orig >= 500 ~ eff_soil_depth_orig,
        # Priority 9:
        # If none of those conditions are met (i.e. default), report 999.
        TRUE ~ 999)) %>%
    mutate(eff_soil_depth = round(eff_soil_depth, 1)) %>%
    select(plot_id, eff_soil_depth)






  ### Compose the final data ----


  # Compile a stratifier list for all plots in the survey

  df_strat <- get_env("data_availability_s1") %>%
    select(country, partner_short, plot_id, code_plot) %>%
    left_join(clim_sf, by = "plot_id") %>%
    as_sf %>%
    # Add biogeographical region
    st_join(biogeo_sf) %>%
    rename(biogeo = code) %>%
    st_drop_geometry() %>%
    distinct(partner_short, plot_id, longitude_dec, latitude_dec,
             .keep_all = TRUE) %>%
    arrange(partner_short, code_plot) %>%
    # Add manually harmonised stratifiers
    left_join(s1_prf_adds_agg %>%
                select(-survey_year),
              by = "plot_id") %>%
    # Add WRB etc
    left_join(get_env("s1_prf") %>%
                select(plot_id, code_wrb_soil_group,
                       code_wrb_qualifier_1, code_wrb_spezifier_1,
                       change_date,
                       date_profile_desc) %>%
                filter(!is.na(code_wrb_soil_group)) %>%
                # Filter for records with the most recent change_date
                # per plot
                group_by(plot_id) %>%
                slice_max(order_by =
                            as.Date(change_date, format = "%Y-%m-%d")) %>%
                ungroup() %>%
                # Take the most abundant value
                # Sometimes there are different WRBs of the same most recent
                # change_date for a plot. In that case it is at this stage
                # not yet clear which WRB is more reliable at the plot level.
                # To do: clarify this
                group_by(plot_id, code_wrb_soil_group,
                         code_wrb_qualifier_1, code_wrb_spezifier_1) %>%
                summarise(count = n(),
                          date_profile_desc = first(date_profile_desc),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                filter(count == max(count)) %>%
                summarise(
                  uncertain_soil_group_options =
                    ifelse(n_distinct(code_wrb_soil_group) > 1,
                           paste0(unique(code_wrb_soil_group), collapse = "_"),
                           NA),
                  code_wrb_soil_group = first(code_wrb_soil_group),
                  code_wrb_qualifier_1 = first(code_wrb_qualifier_1),
                  code_wrb_spezifier_1 = first(code_wrb_spezifier_1),
                  date_profile_desc = first(date_profile_desc)) %>%
                ungroup() %>%
                select(plot_id, code_wrb_soil_group,
                       code_wrb_qualifier_1, code_wrb_spezifier_1,
                       uncertain_soil_group_options,
                       date_profile_desc) %>%
                # with 0 referring to "layer 0"
                rename(code_wrb_soil_group_0 = code_wrb_soil_group) %>%
                rename(code_wrb_qualifier_1_0 = code_wrb_qualifier_1) %>%
                rename(date_profile_desc_0 = date_profile_desc),
              by = "plot_id") %>%
    # Combine sources (i.e. harmonised data and layer 0)
    mutate(
      uncertain_soil_group_options = ifelse(
        is.na(code_wrb_soil_group) & !is.na(uncertain_soil_group_options),
        uncertain_soil_group_options,
        NA),
      date_profile_desc = coalesce(as.Date(date_profile_desc),
                                   as.Date(date_profile_desc_0)),
      code_wrb_qualifier_1 = ifelse(
        !is.na(code_wrb_soil_group),
        code_wrb_qualifier_1,
        code_wrb_qualifier_1_0),
      code_wrb_soil_group = coalesce(code_wrb_soil_group,
                                     code_wrb_soil_group_0)) %>%
    select(-code_wrb_soil_group_0,
           -code_wrb_qualifier_1_0,
           -code_wrb_spezifier_1, # actually not needed since not harmonised
           -date_profile_desc_0) %>%
    left_join(d_soil_group,
              by = join_by(code_wrb_soil_group == code)) %>%
    rename(wrb_soil_group = description) %>%
    left_join(d_soil_adjective %>%
                select(code, description) %>%
                rename(code_wrb_qualifier_1 = code) %>%
                rename(wrb_qualifier_1 = description),
              by = "code_wrb_qualifier_1") %>%
    # Add forest type
    left_join(get_env("y1_st1") %>%
                select(plot_id, code_forest_type) %>%
                filter(code_forest_type != 99) %>%
                filter(!is.na(code_forest_type)) %>%
                group_by(plot_id, code_forest_type) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id, code_forest_type) %>%
                mutate(code_forest_type = as.character(code_forest_type)) %>%
                rename(code_forest_type_0 = code_forest_type),
              by = "plot_id") %>%
    mutate(code_forest_type = coalesce(code_forest_type,
                                       code_forest_type_0)) %>%
    select(-code_forest_type_0) %>%
    left_join(d_forest_type,
              by = join_by(code_forest_type == code)) %>%
    rename(forest_type = short_descr) %>%
    # Add humus form
    left_join(bind_rows(get_env("y1_st1") %>%
                          select(plot_id, code_humus),
                        get_env("s1_prf") %>%
                          select(plot_id, code_humus_orig) %>%
                          rename(code_humus = code_humus_orig)) %>%
                filter(code_humus != 99) %>%
                filter(!is.na(code_humus)) %>%
                group_by(plot_id, code_humus) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id, code_humus),
              by = "plot_id") %>%
    left_join(d_humus,
              by = join_by(code_humus == code)) %>%
    rename(humus_type_0 = description) %>%
    mutate(humus_type_0 = case_when(
      humus_type_0 == "Amphi (or Amphihumus)" ~ "Amphi",
      humus_type_0 %in% c("Histomull", "Histomoder") ~ "Semi-terrestrial",
      TRUE ~ humus_type_0)) %>%
    mutate(humus_type = coalesce(humus_type, humus_type_0)) %>%
    select(-code_humus, -humus_type_0) %>%
    # Add parent_material
    left_join(get_env("s1_prf") %>%
                select(plot_id, code_parent_material_1) %>%
                filter(code_parent_material_1 != 0) %>%
                filter(!is.na(code_parent_material_1)) %>%
                group_by(plot_id, code_parent_material_1) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                # Take the first digit of the code (only major classes)
                mutate(code_parent_material_dom1 =
                         as.integer(substr(code_parent_material_1, 1, 1))) %>%
                mutate(code_parent_material_dom1 =
                         1000 * code_parent_material_dom1) %>%
                select(plot_id,
                       code_parent_material_1,
                       code_parent_material_dom1),
              by = "plot_id") %>%
    left_join(d_parent_material,
              by = join_by(code_parent_material_dom1 == code)) %>%
    rename(parent_material = description) %>%
    mutate(parent_material = coalesce(parent_material,
                                      parent_material_esd)) %>%
    # Add main tree species
    left_join(get_env("y1_st1") %>%
                select(plot_id, code_tree_species) %>%
                filter(code_tree_species != -9) %>%
                filter(!is.na(code_tree_species)) %>%
                group_by(plot_id, code_tree_species) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id, code_tree_species),
              by = "plot_id") %>%
    left_join(d_tree_spec,
              by = join_by(code_tree_species == code)) %>%
    rename(main_tree_species = description) %>%
    # Add code_orientation (to create slope and aspect)
    left_join(get_env("y1_pl1") %>%
                select(plot_id, code_orientation) %>%
                filter(!is.na(code_orientation)) %>%
                group_by(plot_id, code_orientation) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id,
                       code_orientation),
              by = "plot_id") %>%
    # Add slope
    left_join(get_env("y1_pl1") %>%
                select(plot_id, slope) %>%
                filter(!is.na(slope)) %>%
                group_by(plot_id, slope) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id, slope),
              by = "plot_id") %>%
    mutate(
      slope = case_when(
        !is.na(slope) ~ slope,
        code_orientation == 9 ~ 0,
        TRUE ~ NA_integer_)) %>%
    # Add aspect
    mutate(
      aspect = case_when(
        code_orientation == 1 ~ 360,
        # aspect equal to 999 indicates "flat"
        code_orientation == 9 ~ 999,
        TRUE ~ code_orientation * 360 / 8 - 45)) %>%
    # Add altitude
    left_join(get_env("y1_pl1") %>%
                select(plot_id, altitude_m) %>%
                filter(!is.na(altitude_m)) %>%
                group_by(plot_id, altitude_m) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                arrange(-count) %>%
                slice_head() %>%
                ungroup() %>%
                select(plot_id, altitude_m),
              by = "plot_id") %>%
    mutate(altitude = coalesce(altitude_m,
                               elev)) %>%
    # Add soil depth
    left_join(s1_eff_soil_depth_sources,
              by = "plot_id") %>%
    # Base saturation class
    # Add base saturation
    left_join(df_bs %>%
                filter(layer_limit_superior >= 20) %>%
                filter(!is.na(base_saturation)) %>%
                group_by(plot_id) %>%
                reframe(base_saturation = mean(base_saturation)) %>%
                ungroup(),
              by = "plot_id") %>%
    mutate(bs_class = case_when(
      grepl("eutr", wrb_qualifier_1, ignore.case = TRUE) ~ ">= 50 %",
      grepl("dystr", wrb_qualifier_1, ignore.case = TRUE) ~ "< 50 %",
      (!is.na(base_saturation) & base_saturation >= 0.5) ~ ">= 50 %",
      (!is.na(base_saturation) & base_saturation < 0.5) ~ "< 50 %")) %>%
    select(country,
           plot_id,
           longitude_dec,
           latitude_dec,
           tavg,
           prec,
           altitude,
           slope,
           aspect,
           eff_soil_depth,
           wrb_soil_group,
           forest_type,
           humus_type,
           parent_material,
           biogeo,
           main_tree_species,
           bs_class,
           wrb_qualifier_1) %>%
    rename(mat = tavg,
           map = prec,
           wrb_ref_soil_group = wrb_soil_group,
           eftc = forest_type,
           humus_form = humus_type,
           biogeographical_region = biogeo)

  if ("uncertain_soil_group_options" %in% names(df_strat) &&
      length(which(!is.na(df_strat$uncertain_soil_group_options))) == 0) {

    df_strat <- df_strat %>%
      select(-uncertain_soil_group_options)
  }


}

return(df_strat)

}
