
get_stratifiers <- function(level) {

  # Note: this function should be updated with the manually
  # harmonised WRB data for LI by Nathalie

  assertthat::assert_that(level %in% c("LI", "LII"))

  stopifnot(require("tidyverse"),
            require("assertthat"),
            require("aqp"),
            require("mpspline2"),
            require("sf"))

  source("./src/functions/get_env.R")

  d_forest_type <-
    read.csv2("./data/additional_data/d_forest_type.csv") %>%
    select(code, short_descr) %>%
    mutate(code = as.character(code))

  d_soil_group <-
    read.csv2("./data/raw_data/so/adds/dictionaries/d_soil_group.csv") %>%
    select(code, description)

  d_humus <-
    read.csv2("./data/raw_data/so/adds/dictionaries/d_humus.csv") %>%
    filter(is.na(valid_to_survey_year)) %>%
    arrange(code) %>%
    select(code, description)

  d_tree_spec <-
    read.csv2("./data/raw_data/si/adds/dictionaries/d_tree_spec.csv") %>%
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

  # Get manually harmonised WRB and EFTC

  assertthat::assert_that(file.exists(paste0("./data/additional_data/",
                                             "SO_PRF_ADDS.xlsx")),
                          msg = paste0("'./data/additional_data/",
                                       "SO_PRF_ADDS.xlsx' ",
                                       "does not exist."))

  so_prf_adds <-
    openxlsx::read.xlsx(paste0("./data/additional_data/",
                               "SO_PRF_ADDS.xlsx"),
                        sheet = 2) %>%
    rename(bs_class = "BS.(high/low)",
           plot_id = PLOT_ID)

  df_strat <- so_prf_adds %>%
    mutate(soil_wrb = paste0(RSGu, "_",
                             QUALu, "_",
                             SPECu, "_",
                             METHOD_RSGu, "_",
                             DEPTHSTOCK, "_",
                             bs_class, "_",
                             EFTC, "_",
                             remark)) %>%
    group_by(plot_id) %>%
    # Sometimes there are different options, e.g. plot_id 60_9
    # No good way to solve this - we just have to pick one
    summarise(soil_wrb =
                names(which.max(table(soil_wrb[!is.na(soil_wrb)])))) %>%
    # Split the data back into the original columns
    separate(soil_wrb,
             into = c("code_wrb_soil_group",
                      "code_wrb_qualifier_1",
                      "code_wrb_spezifier_1",
                      "method_wrb_harmonisation_fscc",
                      "eff_soil_depth",
                      "bs_class",
                      "code_forest_type",
                      "remark_harmonisation_fscc"),
             sep = "_") %>%
    left_join(data_availability_so %>%
                select(plot_id, partner_short),
              by = "plot_id") %>%
    relocate(partner_short, .before = plot_id) %>%
    mutate(eff_soil_depth = as.numeric(eff_soil_depth)) %>%
    mutate_all(~ifelse((.) == "NA", NA, .)) %>%
    mutate_all(~ifelse((.) == "", NA, .)) %>%
    left_join(d_forest_type,
              by = join_by(code_forest_type == code)) %>%
    rename(forest_type = short_descr) %>%
    left_join(d_soil_group,
              by = join_by(code_wrb_soil_group == code)) %>%
    rename(wrb_soil_group = description)


  # Add coordinates

  source("./src/functions/as_sf.R")

  df_strat_sf <- df_strat %>%
    left_join(get_env("coordinates_so"), by = "plot_id") %>%
    as_sf

  # Add biogeographical region

  df_strat <- st_join(df_strat_sf, biogeo_sf) %>%
    rename(biogeo = code) %>%
    st_drop_geometry() %>%
    distinct(plot_id, .keep_all = TRUE) %>%
    arrange(partner_short) %>%
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
    rename(humus_type = description) %>%
    select(partner_short,
           plot_id,
           longitude_dec,
           latitude_dec,
           wrb_soil_group,
           forest_type,
           humus_type,
           biogeo,
           main_tree_species,
           bs_class,
           code_wrb_soil_group,
           code_wrb_qualifier_1,
           code_wrb_spezifier_1,
           code_forest_type,
           code_humus,
           code_tree_species)

}

  # 2. "s1" ----

  if (level == "LI") {

  df_strat <- get_env("s1_som") %>%
    distinct(plot_id) %>%
    left_join(get_env("data_availability_s1") %>%
                select(plot_id, partner_short),
              by = "plot_id") %>%
    relocate(partner_short, .before = plot_id)

  df_strat_sf <- df_strat %>%
    left_join(get_env("coordinates_s1"), by = "plot_id") %>%
    as_sf

  df_strat <-
    st_join(df_strat_sf, biogeo_sf) %>%
    rename(biogeo = code) %>%
    st_drop_geometry() %>%
    distinct(partner_short, plot_id, longitude_dec, latitude_dec,
             .keep_all = TRUE) %>%
    arrange(partner_short) %>%
    # Add WRB etc
    left_join(get_env("s1_prf") %>%
                select(plot_id, code_wrb_soil_group,
                       code_wrb_qualifier_1, code_wrb_spezifier_1,
                       change_date) %>%
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
                  code_wrb_spezifier_1 = first(code_wrb_spezifier_1)) %>%
                ungroup() %>%
                select(plot_id, code_wrb_soil_group,
                       code_wrb_qualifier_1, code_wrb_spezifier_1,
                       uncertain_soil_group_options),
              by = "plot_id") %>%
    left_join(d_soil_group,
              by = join_by(code_wrb_soil_group == code)) %>%
    rename(wrb_soil_group = description) %>%
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
                mutate(code_forest_type = as.character(code_forest_type)),
              by = "plot_id") %>%
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
    rename(humus_type = description) %>%
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
    mutate(bs_class = NA) %>%
    select(partner_short,
           plot_id,
           longitude_dec,
           latitude_dec,
           wrb_soil_group,
           forest_type,
           humus_type,
           biogeo,
           main_tree_species,
           bs_class,
           code_wrb_soil_group,
           uncertain_soil_group_options,
           code_wrb_qualifier_1,
           code_wrb_spezifier_1,
           code_forest_type,
           code_humus,
           code_tree_species)


}

return(df_strat)

}
