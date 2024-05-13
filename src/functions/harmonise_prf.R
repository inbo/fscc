

harmonise_prf <- function(survey_form,
                          data_frame = NULL,
                          save_to_env = FALSE) {


  # Import survey forms ----

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }


  # s1_prf ----

  if (survey_form == "s1_prf") {

    cat("Gap-fill 's1_prf' using manually harmonised profile data\n")

    dir <- "./data/additional_data/S1_PRF_ADDS.csv"

    assertthat::assert_that(file.exists(dir),
                            msg = paste0("'", dir, "' ",
                                         "does not exist."))

    s1_prf_adds <- read.csv(dir, sep = ";") %>%
      mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
      mutate(date_profile_desc =
               as.Date(parsedate::parse_iso_8601(parsedate::parse_date(
                 date_profile_desc)))) %>%
      mutate(unique_survey_profile =
               paste0(code_country, "_",
                      survey_year, "_",
                      code_plot, "_",
                      profile_pit_id)) %>%
      mutate(unique_survey =
               paste0(code_country, "_",
                      survey_year, "_",
                      code_plot)) %>%
      mutate(unique_profile =
               paste0(code_country, "_",
                      code_plot, "_",
                      profile_pit_id))

    # Aggregate per plot_id

    s1_prf_adds_agg <- s1_prf_adds %>%
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



    df <- df %>%
      select(-survey_year, -date_profile_desc) %>%
      rename(code_wrb_soil_group_orig = code_wrb_soil_group,
             code_wrb_qualifier_1_orig = code_wrb_qualifier_1,
             eff_soil_depth_orig = eff_soil_depth,
             code_humus_orig = code_humus) %>%
      left_join(s1_prf_adds_agg,
                by = "plot_id")

  } # End of "if s1_prf"




  # so_prf ----

  if (survey_form == "so_prf") {

    cat("Gap-fill 'so_prf' using manually harmonised profile data\n")

    assertthat::assert_that(file.exists(paste0("./data/additional_data/",
                                               "SO_PRF_ADDS.xlsx")),
                            msg = paste0("'./data/additional_data/",
                                         "SO_PRF_ADDS.xlsx' ",
                                         "does not exist."))

    df_humus <-
      openxlsx::read.xlsx(paste0("./data/additional_data/",
                                 "SO_PRF_ADDS 20231213.xlsx"),
                          sheet = 1) %>%
      mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
      mutate(key = paste0(plot_id, "_", profile_pit_id)) %>%
      select(key, unified_humus)

    # This file was created by Nathalie on 17 Oct 2023

    so_prf_adds <-
      openxlsx::read.xlsx(paste0("./data/additional_data/",
                                 "SO_PRF_ADDS.xlsx"),
                          sheet = 1) %>%
      mutate(unique_survey_profile =
               paste0(code_country, "_",
                      survey_year, "_",
                      code_plot, "_",
                      profile_pit_id)) %>%
      mutate(unique_survey =
               paste0(code_country, "_",
                      survey_year, "_",
                      code_plot)) %>%
      mutate(unique_profile =
               paste0(code_country, "_",
                      code_plot, "_",
                      profile_pit_id)) %>%
      rename(bs_class = "BS.(high/low)",
             plot_id = PLOT_ID) %>%
      filter(!is.na(plot_id)) %>%
      mutate(key = paste0(plot_id, "_", profile_pit_id)) %>%
      left_join(df_humus,
                by = "key") %>%
      select(-key)


    # Aggregate per plot_id

    so_prf_adds_agg <- so_prf_adds %>%
      mutate(soil_wrb = paste0(RSGu, "_",
                               QUALu, "_",
                               SPECu, "_",
                               METHOD_RSGu, "_",
                               DEPTHSTOCK, "_",
                               bs_class, "_",
                               EFTC, "_",
                               remark, "_",
                               unified_humus)) %>%
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
                        "forest_type",
                        "remark_harmonisation_fscc",
                        "humus_type"),
               sep = "_") %>%
      mutate(eff_soil_depth = as.numeric(eff_soil_depth)) %>%
      mutate_all(function(x) ifelse((x) == "NA", NA, x)) %>%
      mutate_all(function(x) ifelse((x) == "", NA, x))


    df <- df %>%
      rename(code_wrb_soil_group_orig = code_wrb_soil_group,
             code_wrb_qualifier_1_orig = code_wrb_qualifier_1,
             code_wrb_spezifier_1_orig = code_wrb_spezifier_1,
             eff_soil_depth_orig = eff_soil_depth,
             code_humus_orig = code_humus) %>%
      left_join(so_prf_adds_agg,
                by = "plot_id")

  } # End of "if so_prf"



  # Save ----

  if (save_to_env == TRUE) {
    assign_env(survey_form, df)

  } else {
    return(df)

  }
}
