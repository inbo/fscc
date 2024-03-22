
get_stratifiers <- function(level) {

  # Note: this function should be updated with the manually
  # harmonised WRB data for LI by Nathalie

  assertthat::assert_that(level %in% c("LI", "LII"))

  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("aqp"),
            require("sf"),
            require("elevatr"))

  source("./src/functions/as_sf.R")
  source("./src/functions/get_env.R")
  source("./src/functions/overlay_tif.R")

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


  # 1. "so" ----

  if (level == "LII") {

  # Base saturation source
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



  # Get manually harmonised WRB and EFTC

  assertthat::assert_that(file.exists(paste0("./data/additional_data/",
                                             "SO_PRF_ADDS.xlsx")),
                          msg = paste0("'./data/additional_data/",
                                       "SO_PRF_ADDS.xlsx' ",
                                       "does not exist."))

  so_prf_adds_agg <-
    openxlsx::read.xlsx(paste0("./data/additional_data/",
                               "SO_PRF_ADDS.xlsx"),
                        sheet = 1) %>%
    rename(bs_class = "BS.(high/low)") %>%
    mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
    filter(!is.na(plot_id)) %>%
    mutate(soil_wrb = paste0(RSGu, "_",
                             QUALu, "_",
                             SPECu, "_",
                             METHOD_RSGu, "_",
                             DEPTHSTOCK, "_",
                             bs_class, "_",
                             EFTC, "_",
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
                      "eff_soil_depth",
                      "bs_class",
                      "code_forest_type",
                      "humus_type",
                      "remark_harmonisation_fscc"),
             sep = "_") %>%
    mutate(eff_soil_depth = as.numeric(eff_soil_depth)) %>%
    mutate_if(
      function(x) !is.Date(x),
      ~ifelse(. == "NA" | . == "", NA_character_, .)) %>%
    select(-code_wrb_spezifier_1)


  assertthat::assert_that(
    n_distinct(get_env("data_availability_so")$plot_id) ==
      nrow(get_env("data_availability_so")))

  df_strat_sf <- get_env("data_availability_so") %>%
    select(country, partner_short, plot_id, code_plot) %>%
    left_join(get_env("coordinates_so"), by = "plot_id") %>%
    as_sf



  df_strat <-
    # Add biogeographical region
    st_join(df_strat_sf, biogeo_sf) %>%
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
    # Add humus type
    left_join(get_env("so_prf") %>%
                select(plot_id, code_humus) %>%
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
      humus_type_0 %in% c("Histomull", "Histomoder") ~ "Peat",
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
                select(plot_id, code_parent_material_1),
              by = "plot_id") %>%
    left_join(d_parent_material,
              by = join_by(code_parent_material_1 == code)) %>%
    rename(parent_material = description) %>%
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
                select(plot_id, code_tree_species),
              by = "plot_id") %>%
    left_join(d_tree_spec,
              by = join_by(code_tree_species == code)) %>%
    rename(main_tree_species = description) %>%
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
    # Add soil depth
    left_join(get_env("so_prf") %>%
                mutate(eff_soil_depth_orig =
                         ifelse("eff_soil_depth" %in% colnames(.),
                                eff_soil_depth,
                                NA)) %>%
                rowwise() %>%
                mutate(eff_soil_depth = ifelse(.data$eff_soil_depth_orig == 0,
                                               NA,
                                               .data$eff_soil_depth_orig),
                       rooting_depth = ifelse(.data$rooting_depth == 0 |
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
                mutate(eff_soil_depth =
                         ifelse(is.na(.data$eff_soil_depth) &
                                  any(!is.na(c(.data$rooting_depth,
                                               .data$rock_depth,
                                               .data$obstacle_depth))),
                                # Maximum of these three depths
                                max(c(.data$rooting_depth,
                                      .data$rock_depth,
                                      .data$obstacle_depth),
                                    na.rm = TRUE),
                                .data$eff_soil_depth)) %>%
                ungroup %>%
                filter(!is.na(eff_soil_depth)) %>%
                # Filter for records with the most recent change_date
                # per plot
                group_by(plot_id) %>%
                slice_max(order_by =
                            as.Date(change_date, format = "%Y-%m-%d")) %>%
                ungroup() %>%
                group_by(plot_id, eff_soil_depth) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                filter(count == max(count)) %>%
                summarise(
                  eff_soil_depth = first(eff_soil_depth)) %>%
                ungroup() %>%
                select(plot_id, eff_soil_depth) %>%
                rename(eff_soil_depth_0 = eff_soil_depth),
              by = "plot_id") %>%
    mutate(eff_soil_depth = coalesce(eff_soil_depth,
                                     eff_soil_depth_0)) %>%
    mutate(eff_soil_depth = ifelse(!is.na(eff_soil_depth) &
                                     (eff_soil_depth > 100),
                                   100,
                                   eff_soil_depth)) %>%
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
           slope,
           eff_soil_depth,
           rooting_depth,
           wrb_soil_group,
           forest_type,
           humus_type,
           parent_material,
           biogeo,
           main_tree_species,
           bs_class,
           wrb_qualifier_1,
           code_wrb_soil_group,
           uncertain_soil_group_options,
           code_wrb_qualifier_1,
           code_forest_type,
           code_parent_material_1,
           code_tree_species,
           eff_soil_depth_0,
           date_profile_desc)

  if (length(which(!is.na(df_strat$uncertain_soil_group_options))) == 0) {
    df_strat <- df_strat %>%
      select(-uncertain_soil_group_options)
  }

}

  # 2. "s1" ----

  if (level == "LI") {

  # Base saturation source
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

  assertthat::assert_that(file.exists(paste0("./data/additional_data/",
                                             "S1_PRF_ADDS.csv")),
                          msg = paste0("'./data/additional_data/",
                                       "S1_PRF_ADDS.csv' ",
                                       "does not exist."))

  # Retrieve harmonised stratifiers and aggregate per plot

  s1_prf_adds_agg <-
    read.csv(paste0("./data/additional_data/",
                    "S1_PRF_ADDS.csv"), sep = ";") %>%
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
    filter(survey_year == max(survey_year)) %>%
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
                      "eff_soil_depth"),
             sep = "_") %>%
    mutate(eff_soil_depth = as.numeric(eff_soil_depth)) %>%
    mutate_if(
      function(x) !is.Date(x),
      ~ifelse(. == "NA" | . == "", NA_character_, .))

  # Compile a stratifier list for all plots in the survey

  assertthat::assert_that(
    n_distinct(get_env("data_availability_s1")$plot_id) ==
      nrow(get_env("data_availability_s1")))

  df_strat_sf <- get_env("data_availability_s1") %>%
    select(country, partner_short, plot_id, code_plot) %>%
    left_join(get_env("coordinates_s1"), by = "plot_id") %>%
    as_sf

  df_strat <-
    # Add biogeographical region
    st_join(df_strat_sf, biogeo_sf) %>%
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
    # Add humus type
    left_join(bind_rows(get_env("y1_st1") %>%
                          select(plot_id, code_humus),
                        get_env("s1_prf") %>%
                          select(plot_id, code_humus)) %>%
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
      humus_type_0 %in% c("Histomull", "Histomoder") ~ "Peat",
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
                select(plot_id, code_parent_material_1),
              by = "plot_id") %>%
    left_join(d_parent_material,
              by = join_by(code_parent_material_1 == code)) %>%
    rename(parent_material = description) %>%
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
    # Add soil depth
    left_join(get_env("s1_prf") %>%
                rowwise() %>%
                mutate(eff_soil_depth = ifelse(.data$eff_soil_depth == 0,
                                               NA,
                                               .data$eff_soil_depth),
                       rooting_depth = ifelse(.data$rooting_depth == 0 |
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
                mutate(eff_soil_depth =
                         ifelse(is.na(.data$eff_soil_depth) &
                                 any(!is.na(c(.data$rooting_depth,
                                              .data$rock_depth,
                                              .data$obstacle_depth))),
                               # Maximum of these three depths
                               max(c(.data$rooting_depth,
                                     .data$rock_depth,
                                     .data$obstacle_depth),
                                   na.rm = TRUE),
                               .data$eff_soil_depth)) %>%
                ungroup %>%
                filter(!is.na(eff_soil_depth)) %>%
                # Filter for records with the most recent change_date
                # per plot
                group_by(plot_id) %>%
                slice_max(order_by =
                            as.Date(change_date, format = "%Y-%m-%d")) %>%
                ungroup() %>%
                group_by(plot_id, eff_soil_depth) %>%
                summarise(count = n(),
                          .groups = "drop") %>%
                group_by(plot_id) %>%
                filter(count == max(count)) %>%
                summarise(
                  eff_soil_depth = first(eff_soil_depth)) %>%
                ungroup() %>%
                select(plot_id, eff_soil_depth) %>%
                rename(eff_soil_depth_0 = eff_soil_depth),
              by = "plot_id") %>%
    mutate(eff_soil_depth = coalesce(eff_soil_depth,
                                     eff_soil_depth_0)) %>%
    mutate(eff_soil_depth = ifelse(!is.na(eff_soil_depth) &
                                     (eff_soil_depth > 100),
                                   100,
                                   eff_soil_depth)) %>%
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
           slope,
           eff_soil_depth,
           wrb_soil_group,
           forest_type,
           humus_type,
           parent_material,
           biogeo,
           main_tree_species,
           bs_class,
           wrb_qualifier_1,
           code_wrb_soil_group,
           uncertain_soil_group_options,
           code_wrb_qualifier_1,
           code_forest_type,
           code_parent_material_1,
           code_tree_species,
           eff_soil_depth_0,
           date_profile_desc)

  if (length(which(!is.na(df_strat$uncertain_soil_group_options))) == 0) {
    df_strat <- df_strat %>%
      select(-uncertain_soil_group_options)
  }


}

return(df_strat)

}
