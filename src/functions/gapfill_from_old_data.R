
gapfill_from_old_data <- function(survey_form,
                                  data_frame = NULL,
                                  save_to_env = FALSE,
                                  parameters = c("bulk_density",
                                                 "organic_carbon_total",
                                                 "organic_layer_weight",
                                                 "coarse_fragment_vol",
                                                 "part_size_clay",
                                                 "part_size_silt",
                                                 "part_size_sand")) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")
  source("./src/functions/expand_unique_survey_vec_adjacent_years.R")


  # Import survey forms ----

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }

  # so_som ----

  if (survey_form == "so_som") {

    ## Gap-filling existing records ----

    ### Prepare so_som_afscdb ----

    # Column names in AFSCDB differ from those in so_som

    # Corresponding column names
    corresponding_cols <- data.frame(
      so_som_afscdb =
        c("SURVEYYEAR", "CODECOUNTRY", "CODEPLOT", "PLOTID",
          "CODELAYER", "REPETITION", "LAYTOP", "LAYBOT",
          "SUBSAMPLES", "DATEANAL", "MOISTURE", "CLAY",
          "SILT", "SAND", "TEXCLASS", "BD",
          "BDEST", "CFMASS", "CFVOL", "ORGLAY",
          "PHCACL2", "PHH2O", "OC", "TON",
          "CARBONATES", "EXCHACID", "EXCHAL", "EXCHCA",
          "EXCHFE", "EXCHK", "EXCHMG", "EXCHMN",
          "EXCHNA", "FREEH", "EXTRAL", "EXTRCA",
          "EXTRCD", "EXTRCR", "EXTRCU", "EXTRFE",
          "EXTRHG", "EXTRK", "EXTRMG", "EXTRMN",
          "EXTRNA", "EXTRNI", "EXTRP", "EXTRPB",
          "EXTRS", "EXTRZN", "TOTAL", "TOTCA",
          "TOTFE", "TOTK", "TOTMG", "TOTMN",
          "TOTNA", "REACAL", "REACFE", "OBSERVATION"),
      so_som =
        c("survey_year", "code_country", "code_plot", "plot_id",
          "code_layer", "repetition",
          "layer_limit_superior", "layer_limit_inferior",
          "subsamples", "date_labor_analyses", "moisture_content",
          "part_size_clay", "part_size_silt", "part_size_sand",
          "code_texture_class", "bulk_density",
          "bulk_density_est", "coarse_fragment_mass",
          "coarse_fragment_vol", "organic_layer_weight",
          "ph_cacl2", "ph_h2o", "organic_carbon_total", "n_total",
          "carbonates", "exch_acidiy", "exch_al", "exch_ca",
          "exch_fe", "exch_k", "exch_mg", "exch_mn",
          "exch_na", "free_h", "extrac_al", "extrac_ca",
          "extrac_cd", "extrac_cr", "extrac_cu", "extrac_fe",
          "extrac_hg", "extrac_k", "extrac_mg", "extrac_mn",
          "extrac_na", "extrac_ni", "extrac_p", "extrac_pb",
          "extrac_s", "extrac_zn", "tot_al", "tot_ca",
          "tot_fe", "tot_k", "tot_mg", "tot_mn",
          "tot_na", "rea_al", "rea_fe", "other_obs"))

    # Import afscdb so_som data

    file_path <- paste0("./data/additional_data/afscdb_LII_2_2/repetitions/",
                        "FSCDB_LII_2_2012_SOM.xlsx")

    assertthat::assert_that(file.exists(file_path),
                            msg = paste0("'", file_path, "' ",
                                         "does not exist."))

    so_som_afscdb <- openxlsx::read.xlsx(file_path)



    # Update the column names

    # Iterate over columns of so_som_afscdb
    for (col_name in names(so_som_afscdb)) {

      # Find corresponding column name in corresponding_cols
      corresponding_name <-
        corresponding_cols$so_som[corresponding_cols$so_som_afscdb == col_name]

      # If corresponding name is not NA, rename the column
      if (!is.na(corresponding_name)) {

        names(so_som_afscdb)[names(so_som_afscdb) == col_name] <-
          corresponding_name

      } else {

        # If corresponding name is NA, remove the column
        so_som_afscdb[[col_name]] <- NULL
      }
    }



    # Find the nearby survey_year in so_som to be matched with

    so_som_afscdb <- so_som_afscdb %>%
      # unique_survey
      mutate(unique_survey = paste0(code_country, "_",
                                    survey_year, "_",
                                    code_plot))

    matching_unique_surveys <-
      data.frame(so_som_afscdb = unique(so_som_afscdb$unique_survey),
                 so_som = NA)

    for (i in seq_len(nrow(matching_unique_surveys))) {

      if (unlist(strsplit(matching_unique_surveys$so_som_afscdb[i],
                          "_"))[1] == "50") {

        surveys_i <- expand_unique_survey_vec_adjacent_years(
          matching_unique_surveys$so_som_afscdb[i], 20)

      } else {

        surveys_i <- expand_unique_survey_vec_adjacent_years(
          matching_unique_surveys$so_som_afscdb[i], 3)
      }

      survey_match_i <- surveys_i[which(
        surveys_i %in% unique(so_som$unique_survey))]

      if (all(!identical(survey_match_i, character(0))) &&
          length(survey_match_i) > 1) {

        year_i <- as.numeric(gsub(".*_(\\d{4})_.*", "\\1",
                                  matching_unique_surveys$so_som_afscdb[i]))
        years <- as.numeric(gsub(".*_(\\d{4})_.*", "\\1", survey_match_i))

        assertthat::assert_that(length(which.min(abs(years - year_i))) == 1)

        survey_match_i <- survey_match_i[which.min(abs(years - year_i))]

      }

      if (!identical(survey_match_i, character(0))) {
        matching_unique_surveys$so_som[i] <- survey_match_i
      }
    }



    so_som_afscdb <- so_som_afscdb %>%
      # Add the corresponding survey in so_som to be matched with
      left_join(matching_unique_surveys %>%
                  rename(unique_survey_so_som = so_som),
                by = join_by("unique_survey" == "so_som_afscdb")) %>%
      # Add unique_layer_repetition to match with
      mutate(unique_layer_repetition = paste0(
        .data$code_country, "_",
        gsub(".*_(\\d{4})_.*", "\\1", .data$unique_survey_so_som), "_",
        .data$code_plot, "_",
        .data$code_layer, "_",
        .data$repetition)) %>%
      # plot_id
      mutate(plot_id = paste0(code_country, "_",
                              code_plot)) %>%
      # Combine bulk_density columns
      mutate(bulk_density = coalesce(.data$bulk_density,
                                     .data$bulk_density_est)) %>%
      select(-bulk_density_est) %>%
      # Convert coarse_fragment_mass to "coarse_fragment_vol_from_mass"
      mutate(coarse_fragment_aid =
               ifelse(!is.na(bulk_density) & !is.na(coarse_fragment_mass),
                      (.data$bulk_density *
                         (.data$coarse_fragment_mass /
                            (100 - .data$coarse_fragment_mass))) / 2650,
                      NA)) %>%
      mutate(coarse_fragment_vol_from_mass =
               ifelse(!is.na(.data$coarse_fragment_aid),
                      round(
                      as.numeric((.data$coarse_fragment_aid /
                                    (1 + .data$coarse_fragment_aid)) * 100), 2),
                      NA)) %>%
      # Combine coarse_fragments columns
      mutate(coarse_fragment_vol =
               coalesce(.data$coarse_fragment_vol,
                        .data$coarse_fragment_vol_from_mass)) %>%
      select(-coarse_fragment_aid,
             -coarse_fragment_vol_from_mass,
             -coarse_fragment_mass)







    ### Prepare df ----

    # At the moment, this script is only elaborated for the following parameters
    # To do: expand for other parameters

    assertthat::assert_that(
      identical(parameters, c("bulk_density",
                              "organic_carbon_total",
                              "organic_layer_weight",
                              "coarse_fragment_vol",
                              "part_size_clay",
                              "part_size_silt",
                              "part_size_sand")))

    # Check for "_orig" columns and create if not exists
    for (col in parameters) {
      orig_col <- paste0(col, "_orig")
      if (!orig_col %in% names(df)) {
        df[[orig_col]] <- df[[col]]
      }
    }

    # Check for "_source" columns and create with NA if not exists
    for (col in parameters) {
      source_col <- paste0(col, "_source")
      if (!source_col %in% names(df)) {
        df[[source_col]] <- ifelse(!is.na(df[[col]]), "som (layer 0)", NA)
      }
    }





    ### Compile ----

    cat(paste0(" \nGap-fill existing records from '", survey_form,
               "' using a previously harmonised data source ",
               "(FSCDB.LII.2).\n"))

    # Filter for records with matching so_som records

    so_som_existing_rec <- so_som_afscdb %>%
      filter(!is.na(.data$unique_survey_so_som))

    assertthat::assert_that(
      length(unique(so_som_existing_rec$unique_layer_repetition)) ==
                              nrow(so_som_existing_rec))





    # Add "afscdb" data to df

    df <- df %>%
      left_join(so_som_existing_rec %>%
                  select(unique_layer_repetition,
                         bulk_density,
                         organic_carbon_total,
                         organic_layer_weight,
                         coarse_fragment_vol,
                         part_size_clay,
                         part_size_silt,
                         part_size_sand) %>%
                  rename(bulk_density_afscdb = bulk_density,
                         organic_carbon_total_afscdb = organic_carbon_total,
                         organic_layer_weight_afscdb = organic_layer_weight,
                         coarse_fragment_vol_afscdb = coarse_fragment_vol,
                         part_size_clay_afscdb = part_size_clay,
                         part_size_silt_afscdb = part_size_silt,
                         part_size_sand_afscdb = part_size_sand),
                by = "unique_layer_repetition")








    # Merge the columns

    df <- df %>%
      # Bulk density
      mutate(
        bulk_density_source = case_when(
          !is.na(bulk_density_source) ~ bulk_density_source,
          !is.na(.data$bulk_density) ~ "som (same year)",
          !is.na(.data$bulk_density_afscdb) ~ "FSCDB.LII (2012)",
          TRUE ~ NA_character_),
        bulk_density = coalesce(
          .data$bulk_density,
          .data$bulk_density_afscdb)) %>%
      # Organic carbon total
      mutate(
        organic_carbon_total_source = case_when(
          !is.na(organic_carbon_total_source) ~ organic_carbon_total_source,
          !is.na(.data$organic_carbon_total) ~ "som (same year)",
          !is.na(.data$organic_carbon_total_afscdb) ~ "FSCDB.LII (2012)",
          TRUE ~ NA_character_),
        organic_carbon_total = coalesce(
          .data$organic_carbon_total,
          .data$organic_carbon_total_afscdb)) %>%
      # Organic layer weight
      mutate(
        organic_layer_weight_source = case_when(
          !is.na(organic_layer_weight_source) ~ organic_layer_weight_source,
          !is.na(.data$organic_layer_weight) ~ "som (same year)",
          !is.na(.data$organic_layer_weight_afscdb) ~ "FSCDB.LII (2012)",
          TRUE ~ NA_character_),
        organic_layer_weight = coalesce(
          .data$organic_layer_weight,
          .data$organic_layer_weight_afscdb)) %>%
      # Coarse fragments
      mutate(
        coarse_fragment_vol_source = case_when(
          !is.na(coarse_fragment_vol_source) ~ coarse_fragment_vol_source,
          !is.na(.data$coarse_fragment_vol) ~ "som (same year)",
          !is.na(.data$coarse_fragment_vol_afscdb) ~ "FSCDB.LII (2012)",
          TRUE ~ NA_character_),
        coarse_fragment_vol = coalesce(
          .data$coarse_fragment_vol,
          .data$coarse_fragment_vol_afscdb)) %>%
      # Clay
      mutate(
        part_size_clay_source = case_when(
          !is.na(part_size_clay_source) ~ part_size_clay_source,
          !is.na(.data$part_size_clay) ~ "som (same year)",
          !is.na(.data$part_size_clay_afscdb) ~ "FSCDB.LII (2012)",
          TRUE ~ NA_character_),
        part_size_clay = coalesce(
          .data$part_size_clay,
          .data$part_size_clay_afscdb)) %>%
      # Silt
      mutate(
        part_size_silt_source = case_when(
          !is.na(part_size_silt_source) ~ part_size_silt_source,
          !is.na(.data$part_size_silt) ~ "som (same year)",
          !is.na(.data$part_size_silt_afscdb) ~ "FSCDB.LII (2012)",
          TRUE ~ NA_character_),
        part_size_silt = coalesce(
          .data$part_size_silt,
          .data$part_size_silt_afscdb)) %>%
      # Sand
      mutate(
        part_size_sand_source = case_when(
          !is.na(part_size_sand_source) ~ part_size_sand_source,
          !is.na(.data$part_size_sand) ~ "som (same year)",
          !is.na(.data$part_size_sand_afscdb) ~ "FSCDB.LII (2012)",
          TRUE ~ NA_character_),
        part_size_sand = coalesce(
          .data$part_size_sand,
          .data$part_size_sand_afscdb))




      # mutate(bulk_density =
      #          ifelse(!is.na(.data$bulk_density),
      #                 .data$bulk_density,
      #                 .data$bulk_density_afscdb)) %>%
      # select(-bulk_density_afscdb) %>%
      # # Organic carbon total
      # mutate(organic_carbon_total =
      #          ifelse(!is.na(.data$organic_carbon_total),
      #                 .data$organic_carbon_total,
      #                 .data$organic_carbon_total_afscdb)) %>%
      # select(-organic_carbon_total_afscdb) %>%
      # # Organic layer weight
      # mutate(organic_layer_weight =
      #          ifelse(!is.na(.data$organic_layer_weight),
      #                 .data$organic_layer_weight,
      #                 .data$organic_layer_weight_afscdb)) %>%
      # select(-organic_layer_weight_afscdb) %>%
      # # Coarse fragments
      # mutate(coarse_fragment_vol =
      #          ifelse(!is.na(.data$coarse_fragment_vol),
      #                 .data$coarse_fragment_vol,
      #                 .data$coarse_fragment_vol_afscdb)) %>%
      # select(-coarse_fragment_vol_afscdb) %>%
      # # Clay
      # mutate(part_size_clay =
      #          ifelse(!is.na(.data$part_size_clay),
      #                 .data$part_size_clay,
      #                 .data$part_size_clay_afscdb)) %>%
      # select(-part_size_clay_afscdb) %>%
      # # Silt
      # mutate(part_size_silt =
      #          ifelse(!is.na(.data$part_size_silt),
      #                 .data$part_size_silt,
      #                 .data$part_size_silt_afscdb)) %>%
      # select(-part_size_silt_afscdb) %>%
      # # Sand
      # mutate(part_size_sand =
      #          ifelse(!is.na(.data$part_size_sand),
      #                 .data$part_size_sand,
      #                 .data$part_size_sand_afscdb)) %>%
      # select(-part_size_sand_afscdb)




    # df <- df %>%
    #   mutate(
    #     bulk_density_source = case_when(
    #       !is.na(.data$bulk_density) ~ "som (same year)",
    #       !is.na(.data$bulk_density_sw_swc_sameyear) ~ "sw_swc (same year)",
    #       !is.na(.data$bulk_density_pfh_sameyear) ~ "pfh (same year)",
    #       !is.na(.data$bulk_density_som_otheryear) ~ "som (other year)",
    #       !is.na(.data$bulk_density_sw_swc_otheryear) ~ "sw_swc (other year)",
    #       !is.na(.data$bulk_density_pfh_otheryear) ~ "pfh (other year)",
    #       !is.na(.data$bulk_density_layer_weight) ~ "som (layer_weight)",
    #       TRUE ~ NA_character_),
    #     bulk_density = coalesce(
    #       .data$bulk_density,
    #       .data$bulk_density_sw_swc_sameyear,
    #       .data$bulk_density_pfh_sameyear,
    #       .data$bulk_density_som_otheryear,
    #       .data$bulk_density_sw_swc_otheryear,
    #       .data$bulk_density_pfh_otheryear,
    #       .data$bulk_density_layer_weight)) %>%
    #   select(-bulk_density_sw_swc_sameyear,
    #          -bulk_density_pfh_sameyear,
    #          -bulk_density_som_otheryear,
    #          -bulk_density_sw_swc_otheryear,
    #          -bulk_density_pfh_otheryear,
    #          -bulk_density_layer_weight)




    ## Adding missing records ----

    #  From the repetition-specific data (so_som_afscdb)
    file_path <-
      paste0("./data/additional_data/afscdb_LII_2_2/plot-aggregated/",
                        "AFSCDB_LII_2_2_080515_som.csv")

    assertthat::assert_that(file.exists(file_path),
                            msg = paste0("'", file_path, "' ",
                                         "does not exist."))


    so_som_afscdb_avg <- read.csv(file_path,
                              sep = ";", na.strings = "") %>%
      # unique_survey
      mutate(unique_survey = paste0(code_country, "_",
                                    survey_year, "_",
                                    code_plot)) %>%
      # plot_id
      mutate(plot_id = paste0(code_country, "_",
                              code_plot)) %>%
      # Convert coarse_fragment_mass to "coarse_fragment_vol_from_mass"
      mutate(coarse_fragment_aid =
               ifelse(!is.na(bulk_density) & !is.na(coarse_fragment_mass),
                      (.data$bulk_density *
                         (.data$coarse_fragment_mass /
                            (100 - .data$coarse_fragment_mass))) / 2650,
                      NA)) %>%
      mutate(coarse_fragment_vol_from_mass =
               ifelse(!is.na(.data$coarse_fragment_aid),
                      round(
                        as.numeric((.data$coarse_fragment_aid /
                                      (1 + .data$coarse_fragment_aid)) * 100),
                        2),
                      NA)) %>%
      # Combine coarse_fragments columns
      mutate(coarse_fragment_vol =
               coalesce(.data$coarse_fragment_vol,
                        .data$coarse_fragment_vol_from_mass)) %>%
      select(-coarse_fragment_aid,
             -coarse_fragment_vol_from_mass,
             -coarse_fragment_mass)




    # Identify which unique surveys are missing in so_som

    unique_surveys_so_som <- unique(df$unique_survey)

    source("./src/functions/expand_unique_survey_vec_adjacent_years.R")

    unique_surveys <-
      expand_unique_survey_vec_adjacent_years(unique_survey_vec =
                                                unique_surveys_so_som,
                                              number_of_years = 3)


    unique_surveys_missing <- so_som_afscdb_avg %>%
      # Manually verified: no need to add Swiss data anymore
      filter((code_country != 50) |
               (plot_id %in% c("50_6", "50_11", "50_16"))) %>%
      filter(!(unique_survey %in% unique_surveys)) %>%
      pull(unique_survey)



    # If there are any unique surveys in afscdb which are missing in so_som

    if (!identical(unique_surveys_missing, character(0))) {

      cat(paste0(" \nAdd missing records from '", survey_form,
                 "' using a previously harmonised data source ",
                 "(AFSCDB.LII.2.2).\n"))


      # Create a list of tables with the partner codes for each plot_id

      diff_partner_codes <- df %>%
        distinct(partner_code, .keep_all = TRUE) %>%
        filter(.data$partner_code != .data$code_country) %>%
        distinct(code_country) %>%
        pull(code_country)

      partner_codes <- df %>%
        filter(!partner_code %in% diff_partner_codes) %>%
        filter(code_country %in% diff_partner_codes) %>%
        distinct(plot_id, .keep_all = TRUE) %>%
        select(plot_id, partner_code)




      # Harmonise the data

      so_som_afscdb_to_add <- so_som_afscdb_avg %>%
        # Filter for the missing unique surveys
        filter(.data$unique_survey %in% unique_surveys_missing) %>%
        mutate(date_labor_analyses =
                 as.character(as.Date(.data$date_labor_analyses)),
               download_date = NA,
               layer_type = ifelse(.data$laytype == "Min",
                                   "mineral",
                                   ifelse(.data$laytype == "FF",
                                          "forest_floor",
                                          ifelse(.data$laytype == "Pea",
                                                 "peat",
                                                 NA))),
               repetition = 1,
               plot_id = paste0(code_country, "_",
                                code_plot),
               unique_survey = paste0(code_country, "_",
                                      survey_year, "_",
                                      code_plot),
               unique_survey_repetition = paste0(code_country, "_",
                                                 survey_year, "_",
                                                 code_plot, "_",
                                                 repetition),
               unique_survey_layer = paste0(code_country, "_",
                                            survey_year, "_",
                                            code_plot, "_",
                                            code_layer),
               unique_layer_repetition = paste0(code_country, "_",
                                                survey_year, "_",
                                                code_plot, "_",
                                                code_layer, "_",
                                                repetition),
               unique_layer = paste0(code_country, "_",
                                     code_plot, "_",
                                     code_layer),
               change_date = as.character(as.Date("2008-05-15")),
               code_line = NA,
               code_plot_orig = code_plot,
               bulk_density_orig = bulk_density,
               organic_carbon_total_orig = organic_carbon_total,
               organic_layer_weight_orig = organic_layer_weight,
               coarse_fragment_vol_orig = coarse_fragment_vol,
               part_size_clay_orig = part_size_clay,
               part_size_silt_orig = part_size_silt,
               part_size_sand_orig = part_size_sand,
               n_total_orig = n_total,
               code_layer_orig = code_layer,
               code_soil_horizon_sample_c = NA,
               elec_cond = NA,
               line_nr = NA,
               ni = NA,
               origin = NA,
               origin_merge_info = NA,
               origin_merged = NA,
               p_ox = NA,
               q_flag = NA,
               qif_key = NA,
               subsamples = NA,
               other_obs = paste0("Record inserted from AFSCDB.LII. ",
                                  .data$other_obs),
               bulk_density_afscdb = bulk_density,
               organic_carbon_total_afscdb = organic_carbon_total,
               organic_layer_weight_afscdb = organic_layer_weight,
               coarse_fragment_vol_afscdb = coarse_fragment_vol,
               part_size_clay_afscdb = part_size_clay,
               part_size_silt_afscdb = part_size_silt,
               part_size_sand_afscdb = part_size_sand,
               bulk_density_source = ifelse(!is.na(.data$bulk_density),
                                            "AFSCDB.LII.2.2 (2015)",
                                            NA_character_),
               organic_carbon_total_source =
                 ifelse(!is.na(.data$organic_carbon_total),
                        "AFSCDB.LII.2.2 (2015)",
                        NA_character_),
               organic_layer_weight_source =
                 ifelse(!is.na(.data$organic_layer_weight),
                        "AFSCDB.LII.2.2 (2015)",
                        NA_character_),
               coarse_fragment_vol_source =
                 ifelse(!is.na(.data$coarse_fragment_vol),
                        "AFSCDB.LII.2.2 (2015)",
                        NA_character_),
               n_total_source =
                 ifelse(!is.na(.data$n_total),
                        "AFSCDB.LII.2.2 (2015)",
                        NA_character_),
               part_size_clay_source = ifelse(!is.na(.data$part_size_clay),
                                              "AFSCDB.LII.2.2 (2015)",
                                              NA_character_),
               part_size_silt_source = ifelse(!is.na(.data$part_size_silt),
                                              "AFSCDB.LII.2.2 (2015)",
                                              NA_character_),
               part_size_sand_source = ifelse(!is.na(.data$part_size_sand),
                                              "AFSCDB.LII.2.2 (2015)",
                                              NA_character_)) %>%
        left_join(partner_codes,
                  by = "plot_id") %>%
        mutate(partner_code = ifelse(!is.na(.data$partner_code),
                                     .data$partner_code,
                                     .data$code_country)) %>%
        left_join(d_country[, c("code", "lib_country")],
                  by = join_by(code_country == code)) %>%
        rename(country = lib_country) %>%
        left_join(d_partner[, c("code", "desc_short", "description")],
                  by = join_by(partner_code == code)) %>%
        rename(partner_short = desc_short) %>%
        rename(partner = description) %>%
        # mutate(country = as.factor(country)) %>%
        # mutate(partner_short = as.factor(partner_short)) %>%
        # mutate(partner = as.factor(partner)) %>%
        rename(base_saturation = bs) %>%
        # Replace empty strings with NA
        mutate_all(~ replace(., . == "", NA)) %>%
        select(
          country, partner_short, partner, survey_year,
          code_country, code_plot, code_layer, repetition,
          layer_limit_superior, layer_limit_inferior, subsamples,
          date_labor_analyses, moisture_content,
          part_size_clay, part_size_silt, part_size_sand,
          code_texture_class, bulk_density, coarse_fragment_vol,
          organic_layer_weight,
          ph_cacl2, ph_h2o, organic_carbon_total, n_total,
          carbonates, exch_acidiy, exch_al, exch_ca,
          exch_fe, exch_k, exch_mg, exch_mn,
          exch_na, free_h, extrac_al, extrac_ca,
          extrac_cd, extrac_cr, extrac_cu, extrac_fe,
          extrac_hg, extrac_k, extrac_mg, extrac_mn,
          extrac_na, extrac_ni, extrac_p, extrac_pb,
          extrac_s, extrac_zn, tot_al, tot_ca,
          tot_fe, tot_k, tot_mg, tot_mn,
          tot_na, rea_al, rea_fe, exch_bce,
          exch_ace, exch_cec, elec_cond, ni,
          base_saturation, origin, code_soil_horizon_sample_c, p_ox,
          other_obs, partner_code, q_flag, change_date,
          code_line, line_nr, qif_key, code_plot_orig,
          download_date, layer_type, plot_id, unique_survey,
          unique_survey_repetition,
          unique_survey_layer, unique_layer_repetition, unique_layer,
          bulk_density_source, organic_layer_weight_source,
          organic_carbon_total_source, n_total_source,
          code_layer_orig,
          part_size_clay_orig, part_size_silt_orig,
          part_size_sand_orig,
          bulk_density_orig, coarse_fragment_vol_orig,
          organic_layer_weight_orig, organic_carbon_total_orig,
          n_total_orig,
          origin_merged, origin_merge_info,
          coarse_fragment_vol_source,
          part_size_clay_source, part_size_silt_source,
          part_size_sand_source,
          bulk_density_afscdb, organic_carbon_total_afscdb,
          organic_layer_weight_afscdb, coarse_fragment_vol_afscdb,
          part_size_clay_afscdb, part_size_silt_afscdb,
          part_size_sand_afscdb)

      assertthat::assert_that(all(names(df) == names(so_som_afscdb_to_add)))

      df <- rbind(df,
                  so_som_afscdb_to_add)

    }

    } # End of if so_som





  # Add data for so_pfh or s1?





  # Save ----

  if (save_to_env == TRUE) {
    assign_env(survey_form, df)

  } else {
    return(df)

  }
}
