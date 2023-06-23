
#' Read PIRs with partner responses
#'
#' After manual checking and harmonisation of the PIRs with responses by
#' partners, this function imports these PIRs and binds them together.
#' Then, a column "unique_rule_id" is added, based on the survey_form, plot_id,
#' survey_year, code_layer_horizon_master, repetition_profile_pit_id, parameter,
#' parameter_value and rule_id.
#'
#' @param overwrite This indicates whether or not "pir_checked" in the global
#' environment needs to be overwritten in case this object already exists
#'
#' Output - This function returns the combined dataframe with PIR responses to
#' the global environment
#'
#' @return A list with "unique_rule_id" for which no corrections in the central
#' database are required
#'
#' @examples
#'

get_checked_inconsistencies <- function(overwrite = NULL) {

  # If no "overwrite" argument is provided, set it to "TRUE" (by default)

  if (is.null(overwrite)) {
    overwrite <- TRUE
    }

  # If "pir_checked" already exists in the global environment and
  # overwrite = FALSE

  assertthat::assert_that(overwrite == TRUE ||
                            !exists("pir_checked"),
                          msg = paste0("The object 'pir_checked' already",
                                       " exists in the global environment",
                                       " and should not be overwritten."))


  # If "overwrite" is TRUE of if the "pir_checked" does not exist yet

if (overwrite == TRUE ||
    !exists("pir_checked")) {

pir_checked <- NULL

for (i in seq_along(list.files("./data/pir_checked/"))) {
  pir_checked_new <- openxlsx::read.xlsx(paste0("./data/pir_checked/",
                                         list.files("./data/pir_checked/")[i]),
                                         sheet = 1)

  if (ncol(pir_checked_new) == 22) {
    pir_checked_new$updated_value <- NA
    }

  pir_checked <- rbind(pir_checked,
                       pir_checked_new[, 1:23])
  }


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



pir_checked$unique_rule_id <- paste0(pir_checked$survey_form, "_",
                                     pir_checked$plot_id, "_",
                                     pir_checked$survey_year, "_",
                                     pir_checked$code_layer_horizon_master, "_",
                                     pir_checked$repetition_profile_pit_id, "_",
                                     pir_checked$parameter, "_",
                                     pir_checked$parameter_value, "_",
                                     pir_checked$rule_id)

### Correct wrong codes by NFCs ----

if (pir_checked$code_nfc_action_taken[
  which(pir_checked$unique_rule_id ==
  "so_prf_3004_301_2007_NA_LS^BU^Inv2007WR01_longitude_101560_FSCC_4")] == 1) {

  pir_checked$code_nfc_action_taken[
  which(pir_checked$unique_rule_id ==
    "so_prf_3004_301_2007_NA_LS^BU^Inv2007WR01_longitude_101560_FSCC_4")] <- 2

  }

vec_wrong <- grep("supplemented", pir_checked$nfc_remark, ignore.case = TRUE)
vec_wrong <- vec_wrong[which(pir_checked$rule_id[vec_wrong] == "FSCC_12" &
                             pir_checked$partner_code[vec_wrong] == 3004)]
pir_checked$code_nfc_action_taken[which(
  pir_checked$code_nfc_action_taken[vec_wrong] == 5)] <- 2



# Export "pir_checked" to global environment ----

assign("pir_checked", pir_checked, envir = globalenv())

# Return list of unique_rule_id's for which the partners either confirmed that
# the value is correct or indicated that the value cannot be corrected

inconsistencies_ignore <- pir_checked$unique_rule_id[
          which(pir_checked$code_nfc_action_taken %in% c(1, 4, 5))]

return(inconsistencies_ignore)
}

}
