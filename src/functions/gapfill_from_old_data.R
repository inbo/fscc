
gapfill_from_old_data <- function(survey_form,
                                  data_frame = NULL,
                                  save_to_env = FALSE,
                                  parameters = c("bulk_density",
                                                 "organic_carbon_total",
                                                 "n_total",
                                                 "organic_layer_weight",
                                                 "coarse_fragment_vol",
                                                 "part_size_clay",
                                                 "part_size_silt",
                                                 "part_size_sand")) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")
  source("./src/functions/expand_unique_survey_vec_adjacent_years.R")
  source("./src/functions/add_dec_coord_columns.R")


  # Import survey forms ----

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }

  # so_som ----

  if (survey_form == "so_som") {

    ## Prepare so_som_afscdb ----

    file_path <- paste0("./data/additional_data/afscdb_LII_2_2/",
                        "repetitions/",
                        "so_afscdb_harmonised_r.csv")

    if (file.exists(file_path)) {

      so_som_afscdb <- read.csv(file_path, sep = ";") %>%
        mutate_all(~ifelse((.) == "", NA, .)) %>%
        mutate(
          no_data = rowSums(!is.na(across(code_texture_class:extrac_al)))) %>%
        filter(no_data > 0)

    } else {

      # Create the file

      ## Prepare Spanish data

      so_som_esp <-
        # Physical data
        read_excel(paste0(
          "data/additional_data/partner_comm/so_spain_correct/",
          "1401 ANF SUELOS 1993-1996 + 2008.xlsx")) %>%
        select(-SEC) %>%
        # Chemical data
        left_join(
          read_excel(paste0(
            "data/additional_data/partner_comm/so_spain_correct/",
            "1402 ANQ SUELOS 1993-1996 + 2008.xlsx")) %>%
            select(-SEC),
          by = c("PARC", "AÑO", "CALIC", "HORIZ_AN")) %>%
        mutate(
          OBS = case_when(
            !is.na(OBS.x) & !is.na(OBS.y) ~ paste(OBS.x, OBS.y, sep = ". "),
            !is.na(OBS.x) ~ OBS.x,
            !is.na(OBS.y) ~ OBS.y,
            TRUE ~ NA_character_)) %>%
        select(-OBS.x, -OBS.y) %>%
        mutate(CALIC = ifelse(CALIC == "1 a 4",
                              1,
                              CALIC))


      corresponding_cols_esp <- data.frame(
        so_som_esp = c(
          "AÑO", "PARC", "CALIC", "HORIZ_AN",
          "LIM_SUP", "LIM_INF", "HUM",
          "ARCILLA", "LIMO", "ARENA",
          "TEXTURA", "DENSIDAD", "GRUESOS",
          "CAPA_ORG", "pH_H2O", "pH_CaCl2", "C_Org",
          "C_Total", "N", "CaCO3", "Ac",
          "BCE", "ACE", "CEC",
          "Sat_Bas", "Al interc", "Ca interc", "Fe interc",
          "K interc", "Mg interc", "Mn inter", "Na inter",
          "H+ libre",
          "Al extrai", "Ca extrai", "Cd extrai", "Cr extrai",
          "Cu extrai", "Fe extrai", "K extrai", "Mg extrai",
          "Mn extrai", "Ni extrai", "P extrai",
          "Pb extrai", "Zn extrai", "OBS"),
        so_som = c(
          "survey_year", "code_plot", "repetition", "code_layer",
          "layer_limit_superior", "layer_limit_inferior", "moisture_content",
          "part_size_clay", "part_size_silt", "part_size_sand",
          "code_texture_class", "bulk_density", "coarse_fragment_vol",
          "organic_layer_weight", "ph_h2o", "ph_cacl2", "organic_carbon_total",
          NA, "n_total", "carbonates", "exch_acidiy",
          "exch_bce", "exch_ace", "exch_cec",
          "base_saturation", "exch_al", "exch_ca", "exch_fe",
          "exch_k", "exch_mg", "exch_mn", "exch_na",
          "free_h",
          "extrac_al", "extrac_ca", "extrac_cd", "extrac_cr",
          "extrac_cu", "extrac_fe", "extrac_k", "extrac_mg",
          "extrac_mn", "extrac_ni", "extrac_p",
          "extrac_pb", "extrac_zn", "other_obs"))

      # Update the column names

      # Iterate over columns of so_som_afscdb
      for (col_name in names(so_som_esp)) {

        # Find corresponding column name in corresponding_cols
        corresponding_name <-
          corresponding_cols_esp$so_som[which(
            corresponding_cols_esp$so_som_esp == col_name)]

        # If corresponding name is not NA, rename the column
        if (!is.na(corresponding_name)) {

          names(so_som_esp)[names(so_som_esp) == col_name] <-
            corresponding_name

        } else {

          # If corresponding name is NA, remove the column
          so_som_esp[[col_name]] <- NULL
        }
      }

      so_som_esp$code_country <- 11


      ## Prepare so_som_afscdb

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

      assertthat::assert_that(file.exists(
        paste0("./data/additional_data/afscdb_LII_2_2/repetitions/",
               "FSCDB_LII_2_2012_SOM.xlsx")),
        msg = paste0("'", file_path, "' ",
                     "does not exist."))

      so_som_afscdb <-
        openxlsx::read.xlsx(paste0("./data/additional_data/afscdb_LII_2_2/",
                                   "repetitions/FSCDB_LII_2_2012_SOM.xlsx"))



      # Update the column names

      # Iterate over columns of so_som_afscdb
      for (col_name in names(so_som_afscdb)) {

        # Find corresponding column name in corresponding_cols
        corresponding_name <-
          corresponding_cols$so_som[which(
            corresponding_cols$so_som_afscdb == col_name)]

        # If corresponding name is not NA, rename the column
        if (!is.na(corresponding_name)) {

          names(so_som_afscdb)[names(so_som_afscdb) == col_name] <-
            corresponding_name

        } else {

          # If corresponding name is NA, remove the column
          so_som_afscdb[[col_name]] <- NULL
        }
      }


      # Correct a small mistake in Austrian organic_layer_weight
      # for plot 14_17 (copy paste error issue raised by Austrian partner
      # on 2024-06-10)

      so_som_afscdb <- so_som_afscdb %>%
        mutate(
          organic_layer_weight = ifelse(
            code_country == 14 & code_plot == 17,
            case_when(
              code_layer == "OL" & repetition == 1 ~ 109 * 1E-3 * 8,
              code_layer == "OFH" & repetition == 1 ~ 118 * 1E-3 * 8,
              code_layer == "OL" & repetition == 2 ~ 105 * 1E-3 * 8,
              code_layer == "OFH" & repetition == 2 ~ 305 * 1E-3 * 8,
              code_layer == "OL" & repetition == 3 ~ 37 * 1E-3 * 8,
              code_layer == "OFH" & repetition == 3 ~ 232 * 1E-3 * 8,
              code_layer == "OL" & repetition == 4 ~ 80 * 1E-3 * 8,
              code_layer == "OFH" & repetition == 4 ~ 218 * 1E-3 * 8,
              .default = organic_layer_weight),
            organic_layer_weight))


      # Replace Spanish records by the Spanish records from
      # their national database

      so_som_afscdb <- so_som_afscdb %>%
        filter(code_country != 11)

      # Add missing columns to so_som_esp and fill them with NA
      for (col in names(so_som_afscdb)) {
        if (!(col %in% names(so_som_esp))) {
          so_som_esp[[col]] <- NA
        }
      }

      # Reorder the columns of so_som_esp to match the order of so_som_afscdb
      so_som_esp <- so_som_esp[names(so_som_afscdb)]

      # Function to convert the class of a column
      convert_class <- function(column, target_class) {
        if (target_class == "character") {
          return(as.character(column))
        } else if (target_class == "numeric") {
          if (is.character(column)) {
            # Convert commas to dots before converting to numeric
            column <- gsub(",", ".", column)
            # Convert "<0" to "0" before converting to numeric
            column <- gsub("<0", "0", column)
          }
          return(as.numeric(column))
        } else if (target_class == "integer") {
          return(as.integer(column))
        } else if (target_class == "factor") {
          return(as.factor(column))
        } else if (target_class == "logical") {
          return(as.logical(column))
        } else if (target_class == "Date") {
          return(as.Date(column))
        } else {
          return(column)  # If the target class is unknown, return the column as is
        }
      }

      # Check and convert the data types of each column in so_som_esp
      # to match so_som_afscdb
      for (col in names(so_som_afscdb)) {
        target_class <- class(so_som_afscdb[[col]])
        so_som_esp[[col]] <- convert_class(so_som_esp[[col]], target_class)
      }


      # Combine the two data frames
      so_som_afscdb <- bind_rows(so_som_afscdb, so_som_esp)


      write.table(so_som_afscdb,
                  file = file_path,
                  row.names = FALSE,
                  na = "",
                  sep = ";",
                  dec = ".")



    } # End of "create the so_som_afscdb file"









    ## Find the nearby survey_year in so_som to be matched with ----

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
        surveys_i %in% unique(df$unique_survey))]

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
             -coarse_fragment_mass) %>%
      mutate(date_labor_analyses =
               # 1900 date system in Excel
               as.Date(date_labor_analyses, origin = "1899-12-30"))



    ## Gap-fill existing records ----

    ### Prepare df ----

    # At the moment, this script is only elaborated for the following parameters
    # To do: expand for other parameters

    assertthat::assert_that(
      identical(parameters, c("bulk_density",
                              "organic_carbon_total",
                              "n_total",
                              "organic_layer_weight",
                              "coarse_fragment_vol",
                              "part_size_clay",
                              "part_size_silt",
                              "part_size_sand")))

    # Check for "_orig" columns and create if not existing
    for (col in parameters) {
      orig_col <- paste0(col, "_orig")
      if (!orig_col %in% names(df)) {
        df[[orig_col]] <- df[[col]]
      }
    }

    # Check for "_source" columns and create with NA if not existing
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
                         n_total,
                         organic_layer_weight,
                         coarse_fragment_vol,
                         part_size_clay,
                         part_size_silt,
                         part_size_sand) %>%
                  rename(bulk_density_afscdb = bulk_density,
                         organic_carbon_total_afscdb = organic_carbon_total,
                         n_total_afscdb = n_total,
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
      # N total
      mutate(
        n_total_source = case_when(
          !is.na(n_total_source) ~ n_total_source,
          !is.na(.data$n_total) ~ "som (same year)",
          !is.na(.data$n_total_afscdb) ~ "FSCDB.LII (2012)",
          TRUE ~ NA_character_),
        n_total = coalesce(
          .data$n_total,
          .data$n_total_afscdb)) %>%
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







    ## Adding missing records ----


    # Filter for records which are currently missing in so_som

    so_som_missing_records <- so_som_afscdb %>%
      filter(is.na(.data$unique_survey_so_som)) %>%
      select(-unique_survey_so_som, -unique_layer_repetition)

    # Some Romanian records were missing in both databases,
    # but the partner confirmed that they should be inserted.

    layers_to_check <- c("52_2009_13_M12_3",
                         "52_2009_13_M24_3",
                         "52_2009_13_M48_3")

    if (all(!layers_to_check %in% df$unique_layer_repetition)) {

      assertthat::assert_that(
        all(layers_to_check %in% so_som_afscdb$unique_layer_repetition))

      so_som_missing_records <- rbind(
        so_som_missing_records,
        so_som_afscdb %>%
          filter(unique_layer_repetition %in% layers_to_check) %>%
          select(-unique_survey_so_som, -unique_layer_repetition)
      )
    }


    # If there are any unique surveys in afscdb which are missing in so_som

    if (nrow(so_som_missing_records) > 0) {

      cat(paste0(" \nAdd missing records from '", survey_form,
                 "' using a previously harmonised data source ",
                 "(FSCDB.LII.2).\n"))


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

      so_som_afscdb_to_add <- so_som_missing_records %>%
        mutate(date_labor_analyses =
                 as.character(as.Date(.data$date_labor_analyses)),
               download_date = NA,
               layer_type = case_when(
                 startsWith(code_layer, "O") | layer_limit_superior < 0 ~
                   "forest_floor",
                 startsWith(code_layer, "H") & layer_limit_superior >= 0 ~
                   "peat",
                 TRUE ~ "mineral"),
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
               change_date = as.character(as.Date("2012-01-01")),
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
               other_obs = ifelse(!is.na(.data$other_obs),
                                  paste0("Record inserted from FSCDB.LII. ",
                                         .data$other_obs),
                                  "Record inserted from FSCDB.LII."),
               bulk_density_afscdb = bulk_density,
               organic_carbon_total_afscdb = organic_carbon_total,
               n_total_afscdb = n_total,
               organic_layer_weight_afscdb = organic_layer_weight,
               coarse_fragment_vol_afscdb = coarse_fragment_vol,
               part_size_clay_afscdb = part_size_clay,
               part_size_silt_afscdb = part_size_silt,
               part_size_sand_afscdb = part_size_sand,
               bulk_density_source = ifelse(!is.na(.data$bulk_density),
                                            "FSCDB.LII.2. (2012)",
                                            NA_character_),
               organic_carbon_total_source =
                 ifelse(!is.na(.data$organic_carbon_total),
                        "FSCDB.LII.2. (2012)",
                        NA_character_),
               organic_layer_weight_source =
                 ifelse(!is.na(.data$organic_layer_weight),
                        "FSCDB.LII.2. (2012)",
                        NA_character_),
               coarse_fragment_vol_source =
                 ifelse(!is.na(.data$coarse_fragment_vol),
                        "FSCDB.LII.2. (2012)",
                        NA_character_),
               n_total_source =
                 ifelse(!is.na(.data$n_total),
                        "FSCDB.LII.2. (2012)",
                        NA_character_),
               part_size_clay_source = ifelse(!is.na(.data$part_size_clay),
                                              "FSCDB.LII.2. (2012)",
                                              NA_character_),
               part_size_silt_source = ifelse(!is.na(.data$part_size_silt),
                                              "FSCDB.LII.2. (2012)",
                                              NA_character_),
               part_size_sand_source = ifelse(!is.na(.data$part_size_sand),
                                              "FSCDB.LII.2. (2012)",
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
        mutate(base_saturation = NA_real_,
               exch_bce = NA_real_,
               exch_ace = NA_real_,
               exch_cec = NA_real_) %>%
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

          part_size_clay_source, part_size_silt_source,
          part_size_sand_source,
          bulk_density_source,
          coarse_fragment_vol_source,
          organic_layer_weight_source,
          organic_carbon_total_source, n_total_source,
          code_layer_orig,
          part_size_clay_orig, part_size_silt_orig,
          part_size_sand_orig,
          bulk_density_orig, coarse_fragment_vol_orig,
          organic_layer_weight_orig, organic_carbon_total_orig,
          n_total_orig,
          origin_merged, origin_merge_info,

          bulk_density_afscdb, organic_carbon_total_afscdb, n_total_afscdb,
          organic_layer_weight_afscdb, coarse_fragment_vol_afscdb,
          part_size_clay_afscdb, part_size_silt_afscdb,
          part_size_sand_afscdb)

      assertthat::assert_that(all(names(df) == names(so_som_afscdb_to_add)))

      df <- rbind(df,
                  so_som_afscdb_to_add)

    }

    } # End of if so_som



  # s1_som ----

  if (survey_form == "s1_som") {

    ## Prepare s1_som_fscdb ----

    file_path <- paste0("./data/additional_data/fscdb_LI/",
                        "original_access_versions/",
                        "s1_fscdb_access_harmonised_r.csv")

    if (file.exists(file_path)) {

      s1_som_fscdb <- read.csv(file_path, sep = ";") %>%
        mutate_all(~ifelse((.) == "", NA, .)) %>%
        mutate(
          no_data = rowSums(!is.na(across(code_texture_class:extrac_al)))) %>%
        filter(no_data > 0) %>%
        mutate(repetition = ifelse(code_country == 58 & code_plot == 2255,
                                2,
                                repetition),
               plot_id = ifelse(code_country == 58 & code_plot == 2255,
                                "58_255",
                                plot_id),
               code_plot = ifelse(code_country == 58 & code_plot == 2255,
                                  255,
                                  code_plot)) %>%
        # do not apply this because then, there seem to be two profiles
        # with 58_188 as plot_id
        # mutate(repetition = ifelse(code_country == 58 & code_plot == 2188,
        #                         2,
        #                         repetition),
        #        plot_id = ifelse(code_country == 58 & code_plot == 2188,
        #                         "58_188",
        #                         plot_id),
        #        code_plot = ifelse(code_country == 58 & code_plot == 2188,
        #                           188,
        #                           code_plot)) %>%
        mutate(
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
                                code_layer))

    } else {

      # Create the file

      assertthat::assert_that(
        file.exists(paste0("data/additional_data/fscdb_LI/",
                           "original_access_versions/",
                           "VW_CHEMICAL_PARAMETERS_DATA.xlsx")))
      assertthat::assert_that(
        file.exists(paste0("data/additional_data/fscdb_LI/",
                           "original_access_versions/",
                           "VW_PLOT_DATA.xlsx")))
      assertthat::assert_that(
        file.exists(paste0("data/additional_data/fscdb_LI/",
                           "original_access_versions/",
                           "SYN_DATA_PLOT.xlsx")))
      assertthat::assert_that(
        file.exists(paste0("data/additional_data/fscdb_LI/",
                           "original_access_versions/",
                           "VW_PHYSICAL_PARAMETERS_DATA.xlsx")))
      assertthat::assert_that(
        file.exists(paste0("data/additional_data/fscdb_LI/",
                           "original_access_versions/",
                           "SYN_DATA_CHEMICAL_PARAMETER.xlsx")))
      assertthat::assert_that(
        file.exists(paste0("data/additional_data/fscdb_LI/",
                           "original_access_versions/",
                           "SYN_DATA_SAMPLING_DATE.xlsx")))



      s1_chemical_data <- openxlsx::read.xlsx(
        paste0("data/additional_data/fscdb_LI/original_access_versions/",
               "VW_CHEMICAL_PARAMETERS_DATA.xlsx"), sheet = 1) %>%
        mutate_at(vars((which(names(.) == "HORIZONID") + 1):ncol(.)),
                  as.numeric)

      s1_plot_prf_data <- openxlsx::read.xlsx(
        paste0("data/additional_data/fscdb_LI/original_access_versions/",
               "VW_PLOT_DATA.xlsx"), sheet = 1)

      s1_plot_data <- openxlsx::read.xlsx(
        paste0("data/additional_data/fscdb_LI/original_access_versions/",
               "SYN_DATA_PLOT.xlsx"), sheet = 1) %>%
        rename(longitude = LONGITUDE) %>%
        rename(latitude = LATITUDE) %>%
        add_dec_coord_columns

      s1_physical_data <- openxlsx::read.xlsx(
        paste0("data/additional_data/fscdb_LI/original_access_versions/",
               "VW_PHYSICAL_PARAMETERS_DATA.xlsx"), sheet = 1) %>%
        mutate_at(vars((which(names(.) == "Texture") + 1):ncol(.)), as.numeric)

      s1_analysis_dates <- openxlsx::read.xlsx(
        paste0("data/additional_data/fscdb_LI/original_access_versions/",
               "SYN_DATA_CHEMICAL_PARAMETER.xlsx"), sheet = 1) %>%
        mutate(date_labor_analyses =
                 # 1900 date system in Excel
                 as.Date(ANADATE, origin = "1899-12-30")) %>%
        filter(!is.na(date_labor_analyses)) %>%
        group_by(PLOTID) %>%
        summarise(date_labor_analyses = min(date_labor_analyses, na.rm = TRUE))

      s1_sampling_dates <- openxlsx::read.xlsx(
        paste0("data/additional_data/fscdb_LI/original_access_versions/",
               "SYN_DATA_SAMPLING_DATE.xlsx"), sheet = 1) %>%
        mutate(date_sampling =
                 # 1900 date system in Excel
                 as.Date(SAMPLEDATE, origin = "1899-12-30"))


      # Create a dataframe to map column names from s1_som_fscdb to s1_som

      mapping_df <- data.frame(
        s1_som_fscdb = c(
          "PLOTID", "COUNTRYID", "PLOTNR", "date_sampling",
          "date_labor_analyses", "HORIZONID",  "CEC", "BCE",
          "ACE", "Exchangeable.acidity", "Base.saturation", "Fe",
          "Cr", "Ni", "Mn", "Zn",
          "Cu", "Pb", "Cd", "Total.nitrogen",
          "P", "Ca", "Mg", "K",
          "Organic.carbon", "Na", "ph_cacl2", "Aluminium",
          "CaCO3", "unique_survey_layer", "Texture", "Bulk.density",
          "Coarse.fragments", "Weight.organic.layer"
        ),
        s1_som = c(
          "plot_id", "code_country", "code_plot", "date_sampling",
          "date_labor_analyses", "code_layer", "exch_cec", "exch_bce",
          "exch_ace", "exch_acidiy", "base_saturation", "extrac_fe",
          "extrac_cr", "extrac_ni", "extrac_mn", "extrac_zn",
          "extrac_cu", "extrac_pb", "extrac_cd", "n_total",
          "extrac_p", "extrac_ca", "extrac_mg", "extrac_k",
          "organic_carbon_total", "extrac_na", "ph_cacl2", "extrac_al",
          "carbonates", "unique_survey_layer", "code_texture_class",
          "bulk_density",
          "coarse_fragment_vol", "organic_layer_weight"
        )
      )

      # Combine data

      s1_som_fscdb <- s1_chemical_data %>%
        mutate(unique_survey_layer = paste0(PLOTID, "_", HORIZONID)) %>%
        select(-SURVEYID) %>%
        full_join(s1_physical_data %>%
                    mutate(unique_survey_layer =
                             paste0(PLOTID, "_", HORIZONID)) %>%
                    select(-SURVEYID, -PLOTID, -HORIZONID),
                  by = "unique_survey_layer") %>%
        mutate(PLOTID = ifelse(is.na(PLOTID),
                               as.numeric(str_split(unique_survey_layer, "_",
                                                    simplify = TRUE)[, 1]),
                               PLOTID),
               HORIZONID = ifelse(is.na(HORIZONID),
                                  str_split(unique_survey_layer, "_",
                                            simplify = TRUE)[, 2],
                                  HORIZONID)) %>%
        full_join(s1_plot_data %>%
                    select(PLOTID, COUNTRYID, PLOTNR),
                  by = "PLOTID") %>%
        full_join(s1_sampling_dates %>%
                    filter(!is.na(date_sampling)) %>%
                    select(PLOTID, date_sampling),
                  by = "PLOTID") %>%
        full_join(s1_analysis_dates,
                  by = "PLOTID") %>%
        rename(ph_cacl2 = `pH(CaCl2)`) %>%
        relocate(any_of(c("COUNTRYID", "PLOTNR", "date_sampling",
                          "oldest_date")),
                 .after = PLOTID) %>%
        mutate_at(vars(-contains("date")), ~ifelse(. == "", NA, .))

      # Update the column names

      # Iterate over columns of so_som_afscdb
      for (col_name in names(s1_som_fscdb)) {

        # Find corresponding column name in corresponding_cols
        corresponding_name <-
          mapping_df$s1_som[which(mapping_df$s1_som_fscdb == col_name)]

        # If corresponding name is not NA, rename the column
        if (!is.na(corresponding_name)) {

          names(s1_som_fscdb)[names(s1_som_fscdb) == col_name] <-
            corresponding_name

        } else {

          # If corresponding name is NA, remove the column
          s1_som_fscdb[[col_name]] <- NULL
        }
      }


      assertthat::assert_that(all(!is.na(s1_som_fscdb$code_country)))
      assertthat::assert_that(all(!is.na(s1_som_fscdb$code_plot)))

      d_depth_level_soil <-
        read.csv("./data/additional_data/d_depth_level_soil.csv",
                 sep = ";")

      diff_partner_codes <- get_env("data_availability_s1") %>%
        distinct(partner_code, .keep_all = TRUE) %>%
        filter(.data$partner_code != .data$code_country) %>%
        distinct(code_country) %>%
        pull(code_country)

      partner_codes <- get_env("data_availability_s1") %>%
        filter(!partner_code %in% diff_partner_codes) %>%
        filter(code_country %in% diff_partner_codes) %>%
        distinct(plot_id, .keep_all = TRUE) %>%
        select(plot_id, partner_code)


      s1_som_fscdb <- s1_som_fscdb %>%
        mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
        mutate(survey_year = case_when(
          !is.na(date_sampling) ~ format(date_sampling, "%Y"),
          !is.na(date_labor_analyses) ~ format(date_labor_analyses, "%Y"),
          .default = NA)) %>%
        left_join(d_depth_level_soil %>%
                    rename(code_layer = code) %>%
                    select(code_layer, layer_limit_superior,
                           layer_limit_inferior),
                  by = "code_layer") %>%
        mutate(layer_type = case_when(
          startsWith(code_layer, "O") | layer_limit_superior < 0 ~
            "forest_floor",
          startsWith(code_layer, "H") & layer_limit_superior >= 0 ~
            "peat",
          TRUE ~ "mineral"
        )) %>%
        left_join(d_country %>%
                    rename(code_country = code) %>%
                    rename(country = lib_country) %>%
                    select(code_country, country),
                  by = "code_country") %>%
        left_join(partner_codes,
                  by = "plot_id") %>%
        mutate(partner_code = ifelse(!is.na(.data$partner_code),
                                     .data$partner_code,
                                     .data$code_country)) %>%
        left_join(d_partner[, c("code", "desc_short", "description")],
                  by = join_by(partner_code == code)) %>%
        rename(partner_short = desc_short) %>%
        rename(partner = description) %>%
        mutate(repetition = 1) %>%
        select(-date_sampling) %>%
        select(country, partner_short, partner, survey_year, code_country,
               code_plot, plot_id, repetition, code_layer, layer_type,
               layer_limit_superior, layer_limit_inferior, date_labor_analyses,
               code_texture_class, bulk_density, coarse_fragment_vol,
               organic_layer_weight, ph_cacl2, organic_carbon_total,
               n_total, carbonates,
               everything()) %>%
        select(-unique_survey_layer) %>%
        mutate(
          no_data = rowSums(!is.na(across(code_texture_class:extrac_al)))) %>%
        filter(no_data > 0) %>%
        mutate(repetition = ifelse(code_country == 58 & code_plot == 2255,
                                   2,
                                   repetition),
               plot_id = ifelse(code_country == 58 & code_plot == 2255,
                                "58_255",
                                plot_id),
               code_plot = ifelse(code_country == 58 & code_plot == 2255,
                                  255,
                                  code_plot)) %>%
        mutate(repetition = ifelse(code_country == 58 & code_plot == 2188,
                                   2,
                                   repetition),
               plot_id = ifelse(code_country == 58 & code_plot == 2188,
                                "58_188",
                                plot_id),
               code_plot = ifelse(code_country == 58 & code_plot == 2188,
                                  188,
                                  code_plot)) %>%
        mutate(
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
                                code_layer)) %>%
        arrange(country, code_plot, layer_limit_superior)

      write.table(s1_som_fscdb,
                  file = paste0("./data/additional_data/fscdb_LI/",
                                "original_access_versions/",
                                "s1_fscdb_access_harmonised_r.csv"),
                  row.names = FALSE,
                  na = "",
                  sep = ";",
                  dec = ".")


    }



    ## Find the nearby survey_year in s1_som to be matched with ----

    matching_unique_surveys <-
      data.frame(s1_som_fscdb = unique(s1_som_fscdb$unique_survey),
                 s1_som = NA)

    survey_year_range <- sort(unique(na.omit(s1_som_fscdb$survey_year)))

    for (i in seq_len(nrow(matching_unique_surveys))) {

      # If the survey_year is NA (i.e. not specified in Access)

      if (grepl("_",
                gsub(".*_(\\d{4})_.*", "\\1",
                     matching_unique_surveys$s1_som_fscdb[i])) &&
          grepl("_NA_", matching_unique_surveys$s1_som_fscdb[i])) {

        surveys_i <-
          paste0(unlist(strsplit(matching_unique_surveys$s1_som_fscdb[i],
                                 "_"))[1], "_",
                 survey_year_range, "_",
                 unlist(strsplit(matching_unique_surveys$s1_som_fscdb[i],
                                 "_"))[3])
      } else {

        surveys_i <- expand_unique_survey_vec_adjacent_years(
          matching_unique_surveys$s1_som_fscdb[i], 3)
      }

      survey_match_i <- surveys_i[which(
        surveys_i %in% unique(df$unique_survey))]

      if (all(!identical(survey_match_i, character(0))) &&
          length(survey_match_i) > 1) {

        year_i <- as.numeric(gsub(".*_(\\d{4})_.*", "\\1",
                                  matching_unique_surveys$s1_som_fscdb[i]))
        years <- as.numeric(gsub(".*_(\\d{4})_.*", "\\1", survey_match_i))

        assertthat::assert_that(length(which.min(abs(years - year_i))) == 1)

        survey_match_i <- survey_match_i[which.min(abs(years - year_i))]

      }

      if (!identical(survey_match_i, character(0))) {
        matching_unique_surveys$s1_som[i] <- survey_match_i
      }
    }



    s1_som_fscdb <- s1_som_fscdb %>%
      # Add the corresponding survey in so_som to be matched with
      left_join(matching_unique_surveys %>%
                  rename(unique_survey_s1_som = s1_som),
                by = join_by("unique_survey" == "s1_som_fscdb")) %>%
      # Add unique_layer_repetition to match with
      mutate(unique_layer_repetition_s1_som = ifelse(
        !is.na(.data$unique_survey_s1_som),
        paste0(
          .data$code_country, "_",
          gsub(".*_(\\d{4})_.*", "\\1", .data$unique_survey_s1_som), "_",
          .data$code_plot, "_",
          .data$code_layer, "_",
          .data$repetition),
        NA))



    ## Gap-fill existing records ----

    ### Prepare df ----

    # At the moment, this script is only elaborated for the following parameters
    # To do: expand for other parameters

    assertthat::assert_that(
      identical(parameters, c("bulk_density",
                              "organic_carbon_total",
                              "n_total",
                              "organic_layer_weight",
                              "coarse_fragment_vol",
                              "part_size_clay",
                              "part_size_silt",
                              "part_size_sand")))

    # Check for "_orig" columns and create if not existing
    for (col in parameters) {
      orig_col <- paste0(col, "_orig")
      if (!orig_col %in% names(df)) {
        df[[orig_col]] <- df[[col]]
      }
    }

    # Check for "_source" columns and create with NA if not existing
    for (col in parameters) {
      source_col <- paste0(col, "_source")
      if (!source_col %in% names(df)) {
        df[[source_col]] <- ifelse(!is.na(df[[col]]), "som (layer 0)", NA)
      }
    }



    ### Compile ----

    cat(paste0(" \nGap-fill existing records from '", survey_form,
               "' using a previously harmonised data source ",
               "(FSCDB.LI - Access database version).\n"))

    # Filter for records with matching so_som records

    s1_som_existing_rec <- s1_som_fscdb %>%
      filter(!is.na(.data$unique_survey_s1_som))

    assertthat::assert_that(
      length(unique(s1_som_existing_rec$unique_layer_repetition)) ==
        nrow(s1_som_existing_rec))





    # Add "afscdb" data to df

    df <- df %>%
      left_join(s1_som_existing_rec %>%
                  select(unique_layer_repetition_s1_som,
                         bulk_density,
                         organic_carbon_total,
                         n_total,
                         organic_layer_weight,
                         coarse_fragment_vol) %>%
                  rename(bulk_density_fscdb = bulk_density,
                         organic_carbon_total_fscdb = organic_carbon_total,
                         n_total_fscdb = n_total,
                         organic_layer_weight_fscdb = organic_layer_weight,
                         coarse_fragment_vol_fscdb = coarse_fragment_vol),
                by = join_by("unique_layer_repetition" ==
                               "unique_layer_repetition_s1_som"))








    # Merge the columns

    df <- df %>%
      # Bulk density
      mutate(
        bulk_density_source = case_when(
          !is.na(bulk_density_source) ~ bulk_density_source,
          !is.na(.data$bulk_density) ~ "som (same year)",
          !is.na(.data$bulk_density_fscdb) ~ "FSCDB.LI (2002)",
          TRUE ~ NA_character_),
        bulk_density = coalesce(
          .data$bulk_density,
          .data$bulk_density_fscdb)) %>%
      # Organic carbon total
      mutate(
        organic_carbon_total_source = case_when(
          !is.na(organic_carbon_total_source) ~ organic_carbon_total_source,
          !is.na(.data$organic_carbon_total) ~ "som (same year)",
          !is.na(.data$organic_carbon_total_fscdb) ~ "FSCDB.LI (2002)",
          TRUE ~ NA_character_),
        organic_carbon_total = coalesce(
          .data$organic_carbon_total,
          .data$organic_carbon_total_fscdb)) %>%
      # N total
      mutate(
        n_total_source = case_when(
          !is.na(n_total_source) ~ n_total_source,
          !is.na(.data$n_total) ~ "som (same year)",
          !is.na(.data$n_total_fscdb) ~ "FSCDB.LI (2002)",
          TRUE ~ NA_character_),
        n_total = coalesce(
          .data$n_total,
          .data$n_total_fscdb)) %>%
      # Organic layer weight
      mutate(
        organic_layer_weight_source = case_when(
          !is.na(organic_layer_weight_source) ~ organic_layer_weight_source,
          !is.na(.data$organic_layer_weight) ~ "som (same year)",
          !is.na(.data$organic_layer_weight_fscdb) ~ "FSCDB.LI (2002)",
          TRUE ~ NA_character_),
        organic_layer_weight = coalesce(
          .data$organic_layer_weight,
          .data$organic_layer_weight_fscdb)) %>%
      # Coarse fragments
      mutate(
        coarse_fragment_vol_source = case_when(
          !is.na(coarse_fragment_vol_source) ~ coarse_fragment_vol_source,
          !is.na(.data$coarse_fragment_vol) ~ "som (same year)",
          !is.na(.data$coarse_fragment_vol_fscdb) ~ "FSCDB.LI (2002)",
          TRUE ~ NA_character_),
        coarse_fragment_vol = coalesce(
          .data$coarse_fragment_vol,
          .data$coarse_fragment_vol_fscdb))





    ## Adding missing records ----


    # Identify which unique surveys are missing in so_som

    unique_surveys_s1_som <- s1_som_fscdb %>%
      filter(is.na(unique_survey_s1_som)) %>%
      filter(code_country != 58) %>%
      left_join(data_availability_s1 %>%
                  select(plot_id, survey_years), by = "plot_id") %>%
      relocate(survey_years, .after = survey_year) %>%
      pull(unique_survey)

    unique_surveys_missing <- s1_som_fscdb %>%
      filter(unique_survey %in% unique_surveys_s1_som) %>%
      mutate(
        no_data = rowSums(!is.na(across(code_texture_class:extrac_al)))) %>%
      filter(no_data > 0) %>%
      pull(unique_survey)

    # If there are any unique surveys in afscdb which are missing in so_som

    if (!identical(unique_surveys_missing, character(0))) {

      cat(paste0(" \nAdd missing records from '", survey_form,
                 "' using a previously harmonised data source ",
                 "(FSCDB.LI - Access database version).\n"))

      # Harmonise the data

      s1_som_fscdb_to_add <- s1_som_fscdb %>%
        # Filter for the missing unique surveys
        filter(.data$unique_survey %in% unique_surveys_missing) %>%
        mutate(download_date = NA,
               change_date = as.character(as.Date("2002-01-01")),
               code_line = NA,
               code_plot_orig = code_plot,
               bulk_density_orig = bulk_density,
               organic_carbon_total_orig = organic_carbon_total,
               organic_layer_weight_orig = organic_layer_weight,
               coarse_fragment_vol_orig = coarse_fragment_vol,
               part_size_clay = NA,
               part_size_silt = NA,
               part_size_sand = NA,
               part_size_clay_orig = part_size_clay,
               part_size_silt_orig = part_size_silt,
               part_size_sand_orig = part_size_sand,
               n_total_orig = n_total,
               code_layer_orig = code_layer,
               line_nr = NA,
               p_ox = NA,
               q_flag = NA,
               qif_key = NA,
               subsamples = NA,
               moisture_content = NA,
               ph_h2o = NA,
               exch_al = NA,
               exch_ca = NA,
               exch_fe = NA,
               exch_k = NA,
               exch_mg = NA,
               exch_mn = NA,
               exch_na = NA,
               extrac_hg = NA,
               extrac_s = NA,
               free_h = NA,
               tot_al = NA,
               tot_ca = NA,
               tot_fe = NA,
               tot_k = NA,
               tot_mg = NA,
               tot_mn = NA,
               tot_na = NA,
               rea_al = NA,
               rea_fe = NA,
               other_obs = "Record inserted from FSCDB.LI.",
               bulk_density_fscdb = bulk_density,
               organic_carbon_total_fscdb = organic_carbon_total,
               n_total_fscdb = n_total,
               organic_layer_weight_fscdb = organic_layer_weight,
               coarse_fragment_vol_fscdb = coarse_fragment_vol,
               part_size_clay_fscdb = part_size_clay,
               part_size_silt_fscdb = part_size_silt,
               part_size_sand_fscdb = part_size_sand,
               bulk_density_source = ifelse(!is.na(.data$bulk_density),
                                            "FSCDB.LI (2002)",
                                            NA_character_),
               organic_carbon_total_source =
                 ifelse(!is.na(.data$organic_carbon_total),
                        "FSCDB.LI (2002)",
                        NA_character_),
               organic_layer_weight_source =
                 ifelse(!is.na(.data$organic_layer_weight),
                        "FSCDB.LI (2002)",
                        NA_character_),
               coarse_fragment_vol_source =
                 ifelse(!is.na(.data$coarse_fragment_vol),
                        "FSCDB.LI (2002)",
                        NA_character_),
               n_total_source =
                 ifelse(!is.na(.data$n_total),
                        "FSCDB.LI (2002)",
                        NA_character_),
               part_size_clay_source = ifelse(!is.na(.data$part_size_clay),
                                              "FSCDB.LI (2002)",
                                              NA_character_),
               part_size_silt_source = ifelse(!is.na(.data$part_size_silt),
                                              "FSCDB.LI (2002)",
                                              NA_character_),
               part_size_sand_source = ifelse(!is.na(.data$part_size_sand),
                                              "FSCDB.LI (2002)",
                                              NA_character_)) %>%
        select(
            country, partner_short, partner, survey_year, code_country,
            code_plot, code_layer, repetition, layer_limit_superior,
            layer_limit_inferior, subsamples, date_labor_analyses,
            moisture_content, part_size_clay, part_size_silt,
            part_size_sand, code_texture_class, bulk_density,
            coarse_fragment_vol, organic_layer_weight, ph_cacl2,
            ph_h2o, organic_carbon_total, n_total, carbonates,
            exch_acidiy, exch_al, exch_ca, exch_fe,
            exch_k, exch_mg, exch_mn, exch_na,
            free_h, extrac_al, extrac_ca, extrac_cd,
            extrac_cr, extrac_cu, extrac_fe, extrac_hg,
            extrac_k, extrac_mg, extrac_mn, extrac_na,
            extrac_ni, extrac_p, extrac_pb, extrac_s,
            extrac_zn, tot_al, tot_ca, tot_fe,
            tot_k, tot_mg, tot_mn, tot_na,
            rea_al, rea_fe, p_ox, other_obs,
            partner_code, q_flag, change_date, code_line,
            line_nr, qif_key, code_plot_orig, download_date,
            layer_type, plot_id,
            unique_survey, unique_survey_repetition, unique_survey_layer,
            unique_layer_repetition, unique_layer, part_size_clay_source,
            part_size_silt_source, part_size_sand_source, bulk_density_source,
            coarse_fragment_vol_source, organic_layer_weight_source,
            organic_carbon_total_source,
            n_total_source, code_layer_orig, part_size_clay_orig,
            part_size_silt_orig, part_size_sand_orig, bulk_density_orig,
            coarse_fragment_vol_orig, organic_layer_weight_orig,
            organic_carbon_total_orig,
            n_total_orig, bulk_density_fscdb, organic_carbon_total_fscdb,
            n_total_fscdb,
            organic_layer_weight_fscdb, coarse_fragment_vol_fscdb)

      assertthat::assert_that(all(names(df) == names(s1_som_fscdb_to_add)))

      df <- rbind(df,
                  s1_som_fscdb_to_add)

    }



  } # End of "if s1_som"


  # Add data for pfh?





  # Save ----

  if (save_to_env == TRUE) {
    assign_env(survey_form, df)

  } else {
    return(df)

  }
}
