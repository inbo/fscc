

get_pir <- function() {

  # Get and harmonise partner inconsistency reports with validated
  # feedback from partners

  # Import checked pir ----

    plot_coord_poland_lii <-
      read.csv(paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                      "LII_53_plot_coord_harmonisation_key_Poland.csv"),
               sep = ";") %>%
      filter(survey_year != 2017) %>%
      filter(plot_id_orig != "53_150") %>%
      select(plot_id, plot_id_orig)


    assertthat::assert_that(file.exists(paste0("./data/additional_data/",
                                               "20230302_checked_pirs.xlsx")),
                            msg = paste0("'./data/additional_data/",
                                         "20230302_checked_pirs.xlsx' ",
                                         "does not exist."))

    pir_checked <-
      openxlsx::read.xlsx("./data/additional_data/20230302_checked_pirs.xlsx",
                          sheet = 1)

    # Checked pir uses "partner_code" + "code_plot" as "plot_id"
    # We use "code_country" as "code_plot" as "plot_id"
    # Correct this
    pir_checked <- pir_checked %>%
      mutate(plot_id = paste0(code_country, "_",
                              code_plot)) %>%
      select(-plot_id_response) %>%
      left_join(plot_coord_poland_lii %>%
                  rename(plot_id_response = plot_id_orig),
                by = "plot_id") %>%
      mutate(plot_id_response =
               ifelse(code_country == 53 &
                        survey_form %in% c("si_sta", "so_prf", "so_pls") &
                        survey_year %in% c(1994, 1995, 1996,
                                           1998, 1999, 2000),
                      ifelse(plot_id == "53_810" &
                               survey_year %in% c(1998, 1999, 2000),
                             "53_150",
                             plot_id_response),
                      plot_id))



    # Rename rule_id if needed
    if ("rule_ID" %in% colnames(pir_checked)) {
      pir_checked <- rename(pir_checked,
                            rule_id = rule_ID)
    }

    # Remove " (estimated by FSCC)"
    pir_checked <- pir_checked %>%
      mutate(parameter_value = gsub(" \\(estimated by FSCC\\)",
                                    "",
                                    parameter_value))

    # "1 - The reported value is extreme but correct"
    # "2 - The correct value is resubmitted/the inconsistency is solved"
    # "3 - The reported value is removed (e.g. resubmitted as NA)"
    # "4 - No (good) data are available for this parameter"
    # "5 - Other"

    pir_checked$code_nfc_action_taken <-
      gsub("^1.*", "1", pir_checked$code_nfc_action_taken)
    pir_checked$code_nfc_action_taken <-
      gsub("^2.*", "2", pir_checked$code_nfc_action_taken)
    pir_checked$code_nfc_action_taken <-
      gsub("^3.*", "3", pir_checked$code_nfc_action_taken)
    pir_checked$code_nfc_action_taken <-
      gsub("^4.*", "4", pir_checked$code_nfc_action_taken)
    pir_checked$code_nfc_action_taken <-
      gsub("^5.*", "5", pir_checked$code_nfc_action_taken)
    pir_checked$code_nfc_action_taken <-
      as.integer(pir_checked$code_nfc_action_taken)

    assign_env("pir_checked",
               pir_checked)

    return(pir_checked)


}
