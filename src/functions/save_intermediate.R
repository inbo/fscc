
#' Save intermediate solid soil data
#'
#' Save intermediate data for specified survey forms to the provided path.
#'
#' @param survey_forms Character vector of survey forms to save (either survey
#' codes, e.g. "so", or survey forms, e.g. "so_som"). Default is NULL,
#' which saves all solid soil-related survey forms.
#' @param path_name Path where the data will be saved. Default is NULL, which
#' saves to the default path 
#' "./output/intermediate_data/(download_date)_survey_forms_intermediate/".
#' @return None
#'
#' @examples
#' # Save all solid soil-related survey forms to the default path
#' save_intermediate()
#'
#' # Save specific survey forms to a custom path
#' save_intermediate(survey_forms = c("so_som", "so_prf"), 
#' path_name = "./custom_path/")
#'
#' @details
#' This function uses the function "gives_warning()", which checks whether a
#' given expression generates a warning.


save_intermediate <- function(survey_forms = NULL, path_name = NULL) {
  
  
  survey_forms_all <- c("so_som", "so_prf", "so_pls", "so_pfh", "so_lqa", 
                        "s1_som", "s1_prf", "s1_pls", "s1_pfh", "s1_lqa", 
                        "si_eve", "si_plt", "si_sta", "si_tco", "y1_pl1", 
                        "y1_st1", "sw_swa", "sw_swc")
  
  list_data_tables <- list(so = c("som","prf","pls","pfh","lqa"),
                           s1 = c("som","prf","pls","pfh","lqa"),
                           si = c("eve","plt","sta","tco"),
                           y1 = c("pl1","st1"),
                           sw = c("swa","swc"))
  
  # If no "survey_forms" argument is provided,
  # save all solid soil-related survey forms (by default)
  
  if (is.null(survey_forms)) {
    survey_forms <- survey_forms_all}  
  
  # If "survey_forms" argument is provided, replace any survey codes (e.g. "so")
  # by the actual survey forms (e.g. "so_som", "so_prf"...)
  
  if (!is.null(survey_forms)) {
    
    survey_forms_extra <- NULL
    
    # Check for each of the reported characters whether they represent a code
    # of a survey (length = 1) or a survey form (length = 2)
    
    for (i in seq_along(survey_forms)) {
      
      if (length(unlist(strsplit(survey_forms[i], "_"))) == 1) {
        
        # Replace the code by the actual survey forms
        survey_forms_extra <- c(survey_forms_extra,
                                paste0(survey_forms[i], "_",
                                list_data_tables[[which(
                                  names(list_data_tables) == survey_forms[i])]]))
        
        survey_forms[i] <- NA}}
    
    if (!identical(survey_forms_extra, NULL)) {
      
      survey_forms <- c(survey_forms, survey_forms_extra)
      survey_forms <- survey_forms[which(!is.na(survey_forms))]}}
  
  
  # Create a vector with the codes of the different surveys
  
  survey_codes <- NULL
  
  for (i in seq_along(survey_forms)) {
    survey_codes <- c(survey_codes,
                      unlist(strsplit(survey_forms[i], "_"))[1])}
  
  survey_codes <- unique(survey_codes)
  
  
  # If no "path_name" argument is provided,
  # save to the following default path
  
  if (is.null(path_name)) {
    path_name <- paste0("./output/intermediate_data/",
                        download_date,
                        "_survey_forms_intermediate/")}  
  

  # Create path if this does not exist yet
  
  if (!file.exists(path_name)) {dir.create(path_name);
    path_did_not_exist <- TRUE} else {path_did_not_exist <- FALSE}
  
  # Check if there are any data in the path in which the data will be saved
  
  if (!path_did_not_exist &&
      !identical(list.files(path_name), character(0))) {
    
    # Prompt the user for confirmation
    overwrite <- 
      askYesNo("Data already exists in the folder. Do you want to overwrite it?")}

  
  
  
  # Save the data if overwriting is confirmed or if there are no data
  
  if (overwrite == TRUE ||
      path_did_not_exist == TRUE) {
    
    # Check if folders exist and overwrite them if needed
    
    source("./src/functions/gives_warning.R")
    
    for (i in seq_along(survey_codes)) {
      
      survey_forms_code <- grep(paste0("^", survey_codes[i]), 
                                survey_forms, value = TRUE)
      
      path_name_survey <- paste0(path_name, download_date, "_",
                                 survey_codes[i], "/")
      
      # If there already exists a subfolder with the given name:
      # delete subfolder
      
      if (gives_warning(dir.create(path_name_survey)) == TRUE) {
        unlink(path_name_survey, recursive = TRUE)}
      
      dir.create(path_name_survey)
      
      # Save each of the survey forms
      
      for (j in seq_along(survey_forms_code)) {
        
        df <- get(survey_forms_code[j], envir = .GlobalEnv)
        write.csv2(df, paste0(path_name_survey, survey_forms_code[j], ".csv"),
                  row.names = FALSE,
                  na = "")}
      
      # Save coordinate data form if this exists
      
      coordinate_df_name <- paste0("coordinates_", survey_codes[i])
      
      if (exists(coordinate_df_name)) {
        coordinate_df <- get(coordinate_df_name, envir = .GlobalEnv)
        write.csv2(coordinate_df, 
                  paste0(path_name_survey, coordinate_df_name, ".csv"),
                  row.names = FALSE,
                  na = "")};
      
      # Save data availability form if this exists
      
      data_avail_name <- paste0("data_availability_", survey_codes[i])
      
      if (exists(data_avail_name)) {
        data_avail_df <- get(data_avail_name, envir = .GlobalEnv)
        write.csv2(data_avail_df, 
                  paste0(path_name_survey, data_avail_name, ".csv"),
                  row.names = FALSE,
                  na = "")}
      }
    
    # Save additional data forms if they exist
    
    list_additional <- c("d_country", "d_partner")
    list_additional_exists <- sapply(list_additional, exists)
    
    if (any(list_additional_exists == TRUE)) {
    list_additional <- list_additional[which(list_additional_exists)]
    
        # If there already exists a subfolder with the given name:
        # delete subfolder
    
    if (gives_warning(dir.create(
      paste0(path_name, download_date, "_additional_data/"))) == TRUE) {
      unlink(paste0(path_name, download_date, "_additional_data/"), 
             recursive = TRUE)}
    
    dir.create(paste0(path_name, download_date, "_additional_data/"))
    
    for (i in seq_along(list_additional)) {
      df <- get(list_additional[i], envir = .GlobalEnv)
      write.csv2(df, 
                paste0(path_name, download_date, "_additional_data/",
                       list_additional[i], ".csv"),
                row.names = FALSE,
                na = "")
    }}
    
    
    }

}

