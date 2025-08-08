
get_data_availability <- function(code_survey) {

  stopifnot(require("tidyverse"),
            require("assertthat"))

  source("./src/functions/as_sf.R")
  source("./src/functions/get_env.R")

  # Create a list with names of the different survey forms per survey

  assertthat::assert_that(code_survey %in% c("s1", "so"))

  survey_forms <- paste0(code_survey, "_",
                         c("pfh", "som", "prf", "pls", "lqa"))


  data_availability_long <- NULL

  for (i in seq_along(survey_forms)) {

    df <- get_env(survey_forms[i])

    if (!"survey_year" %in% colnames(df)) {
      df$survey_year <- NA
    }

    data_availability_long <- bind_rows(data_availability_long,
                                        df %>%
                                          select(partner_code,
                                                 code_country,
                                                 code_plot,
                                                 plot_id,
                                                 survey_year) %>%
                                          mutate(
                                            survey_form = survey_forms[i]))
  }

  # Add old plots "so"

  if (code_survey == "so") {

    # Add extra plot_ids from AFSCDB

    dir_afscdb <-
      paste0("./data/additional_data/afscdb_LII_2_2/repetitions/",
             "so_afscdb_harmonised_r.csv")

    assertthat::assert_that(file.exists(dir_afscdb),
                            msg = paste0("'", dir_afscdb, "' ",
                                         "does not exist."))

    # Get the partner_codes of these plots
    # None of the plot_ids in AFSCDB which are not reported in "so"
    # belong to the countries with multiple partner codes (2, 4, 13)

    diff_partner_codes <- data_availability_long %>%
      distinct(partner_code, .keep_all = TRUE) %>%
      filter(.data$partner_code != .data$code_country) %>%
      distinct(code_country) %>%
      pull(code_country)

    partner_codes <- data_availability_long %>%
      filter(!partner_code %in% diff_partner_codes) %>%
      filter(code_country %in% diff_partner_codes) %>%
      distinct(plot_id, .keep_all = TRUE) %>%
      select(plot_id, partner_code)

    df <- read.csv(dir_afscdb,
                   sep = ";",
                   na.strings = "") %>%
      mutate_all(~ifelse((.) == "", NA, .)) %>%
      mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
      left_join(partner_codes,
                by = "plot_id") %>%
      mutate(partner_code = ifelse(!is.na(.data$partner_code),
                                   .data$partner_code,
                                   .data$code_country))

    data_availability_long <- bind_rows(data_availability_long,
                                        df %>%
                                          select(partner_code,
                                                 code_country,
                                                 code_plot,
                                                 plot_id,
                                                 survey_year) %>%
                                          mutate(survey_form = "so_som"))
  }

  # Add old plots "s1"

  if (code_survey == "s1") {

    # Add extra plot_ids from AFSCDB

    dir_fscdb <-
      paste0("./data/additional_data/fscdb_LI/",
             "original_access_versions/",
             "s1_fscdb_access_harmonised_r.csv")

    assertthat::assert_that(file.exists(dir_fscdb),
                            msg = paste0("'", dir_fscdb, "' ",
                                         "does not exist."))

    # Get the partner_codes of these plots
    # None of the plot_ids in AFSCDB which are not reported in "so"
    # belong to the countries with multiple partner codes (2, 4, 13)

    df <- read.csv(dir_fscdb,
                   sep = ";",
                   na.strings = "") %>%
      mutate_all(~ifelse((.) == "", NA, .)) %>%
      mutate(
        no_data = rowSums(!is.na(across(code_texture_class:extrac_al)))) %>%
      filter(no_data > 0) %>%
      mutate(plot_id = ifelse(code_country == 58 & code_plot == 2255,
                              "58_255",
                              plot_id),
             code_plot = ifelse(code_country == 58 & code_plot == 2255,
                                255,
                                code_plot)) %>%
      mutate(plot_id = ifelse(code_country == 58 & code_plot == 2188,
                              "58_188",
                              plot_id),
             code_plot = ifelse(code_country == 58 & code_plot == 2188,
                                188,
                                code_plot))

    data_availability_long <- bind_rows(data_availability_long,
                                        df %>%
                                          select(partner_code,
                                                 code_country,
                                                 code_plot,
                                                 plot_id,
                                                 survey_year) %>%
                                          mutate(survey_form = "s1_som"))
  }


  # Define a function to process partner_codes
  process_partner_code <- function(partner_code) {
    codes <- unlist(strsplit(partner_code, "_"))
    codes <- as.numeric(codes[!codes %in% c("98", "11")])
    if (length(codes) == 1) {
      return(codes)
    } else {
      return(NA)
    }
  }


  data_availability <- data_availability_long %>%
    group_by(plot_id, code_country, code_plot) %>%
    reframe(survey_years = paste(unique(sort(survey_year)), collapse = "_"),
            survey_years_som = {
              som_years <- survey_year[survey_form %>% grepl("_som$", .)]
              if (length(som_years) > 0) paste(unique(sort(som_years)),
                                               collapse = "_")
              else NA_character_
            },
            partner_codes_n_distinct = n_distinct(partner_code),
            partner_codes =
              paste(unique(sort(partner_code)), collapse = "_")) %>%
    ungroup() %>%
    mutate(code_country = as.integer(code_country)) %>%
    left_join(d_country[, c("code", "lib_country")],
              by = join_by(code_country == code)) %>%
    rename(country = lib_country) %>%
    mutate(country = as.character(country)) %>%
    mutate(partner_code = NA) %>%
    rowwise() %>%
    mutate(
      partner_code = ifelse(
        partner_codes_n_distinct == 1,
        as.character(partner_codes),
        as.character(process_partner_code(partner_codes))
      )
    ) %>%
    ungroup() %>%
    select(-partner_codes, -partner_codes_n_distinct) %>%
    mutate(partner_code = as.integer(partner_code)) %>%
    left_join(d_partner[, c("code", "desc_short", "description")],
              by = join_by(partner_code == code)) %>%
    rename(partner_short = desc_short) %>%
    rename(partner = description) %>%
    mutate(partner_short = as.character(partner_short)) %>%
    mutate(partner = as.character(partner)) %>%
    relocate(partner_code, .after = code_country) %>%
    arrange(code_country, code_plot)


  return(data_availability)

}


