


harmonise_below_loqs <- function(survey_form,
                                 data_frame = NULL,
                                 parameters = c("part_size_clay",
                                                "part_size_silt",
                                                "part_size_sand",
                                                "organic_carbon_total",
                                                "n_total"),
                                 solve = TRUE,
                                 save_to_env = FALSE) {
  
  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  # 1. Prepare dataframes ----
  
  ## 1.1 Get the survey_form data ----
  
  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }
  
  ## 1.2 Get "lqa" survey form ----
  
  lqa <- get_env(paste0(unlist(str_split(survey_form, "_"))[1],
                        "_lqa"))
    
    # Define the mapping between parameters in so_lqa and so_som
    lqa_parameters <- data.frame(
      lqa_parameter = c(
        "Pb_extr", "Zn_extr", "Pclay", "Psilt", "Mn_exch",
        "CaCO3", "Al_extr", "Mg_exch", "Fe_exch", "Na_extr",
        "Ca_exch", "pH_CaCl2", "Total_N", "K_extr", "Al_exch",
        "Mg_tot", "K_exch", "S_extr", "org_C", "Na_exch",
        "Cr_extr", "Fe_extr", "K_tot", "pH_H2O", "Psand",
        "Cu_extr", "Free_H+", "Cd_extr", "Hg_extr", "Ca_extr",
        "Mg_extr", "Ni_extr", "P_extr", "Al_react", "Mn_extr",
        "Fe_react", "Ca_tot", "P_ox", "Acid_exch", "Na_tot",
        "Mn_tot", "Fe_tot", "Al_tot"),
      som_parameter = c(
        "extrac_pb", "extrac_zn", "part_size_clay", "part_size_silt", "exch_mn",
        "carbonates", "extrac_al", "exch_mg", "exch_fe", "extrac_na",
        "exch_ca", "ph_cacl2", "n_total", "extrac_k", "exch_al",
        "tot_mg", "exch_k", "extrac_s", "organic_carbon_total", "exch_na",
        "extrac_cr", "extrac_fe", "tot_k", "ph_h2o", "part_size_sand",
        "extrac_cu", "free_h", "extrac_cd", "extrac_hg", "extrac_ca",
        "extrac_mg", "extrac_ni", "extrac_p", "extrac_al", "extrac_mn",
        "extrac_fe", "tot_ca", "p_ox", "exch_acidiy", "tot_na",
        "tot_mn", "tot_fe", "tot_al"),
      pfh_parameter = c(
        NA, NA, "horizon_clay", "horizon_silt", NA,
        "horizon_caco3_total", NA, "horizon_exch_mg", NA, NA,
        "horizon_exch_ca", NA, "horizon_n_total", NA, NA,
        NA, "horizon_exch_k", NA, "horizon_c_organic_total", "horizon_exch_na",
        NA, NA, NA, "horizon_ph", "horizon_sand",
        NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA,
        NA, NA, NA))
    
    
    # Add corresponding "som"/"pfh" parameter names
    
    if (unlist(str_split(survey_form, "_"))[2] == "som") {
      
    lqa <- lqa %>%
      left_join(lqa_parameters %>%
                  select(-pfh_parameter),
                by = join_by("code_parameter" == "lqa_parameter")) %>%
      rename(parameter_harmonised = som_parameter) %>%
      relocate(parameter_harmonised, .after = "code_parameter")
    }
    
    if (unlist(str_split(survey_form, "_"))[2] == "pfh") {
      
      lqa <- lqa %>%
        left_join(lqa_parameters %>%
                    select(-som_parameter) %>%
                    filter(!is.na(pfh_parameter)),
                  by = join_by("code_parameter" == "lqa_parameter")) %>%
        rename(parameter_harmonised = pfh_parameter) %>%
        relocate(parameter_harmonised, .after = "code_parameter") %>%
        # Remove lqa info about parameters absent in pfh
        filter(!is.na(.data$parameter_harmonised))
      }
    
  ## 1.3 Get ranges_qaqc table ----
  
  # Source:
  # Table 6 of the ICP Forests manual (Part XVI Quality Assurance and Control
  # in Laboratories, Version 2020-1) [based on the BioSoil survey dataset]
  
  ranges_qaqc <- read.csv2("./data/additional_data/ranges_qaqc.csv")
  
  ## 1.4 Get the "qif" form ----
  
    if (unlist(str_split(survey_form, "_"))[2] == "som") {
    
  qif <-
    read.csv2(paste0("./data/raw_data/",
                     unlist(str_split(survey_form, "_"))[1],
                     "/adds/qif_",
                     unlist(str_split(survey_form, "_"))[1],
                     ".csv")) %>%
    # Replace "" by NA
    mutate_all(~ replace(., . == "", NA))
  
  # Identify columns ending with "_loq" or "_rt"
  loq_rt_cols <- names(qif)[grepl("_(loq|rt)$", names(qif))]
  
  # Convert identified columns to numeric
  qif[loq_rt_cols] <- lapply(qif[loq_rt_cols], function(x) {
    as.numeric(as.character(x))
  })
  
  # ATTRIBUTE CATALOGUE*:
  # - qif_key:		PRIMARY KEY
  # - lqa_info:		"0" - no LQA information is available for this period /
  #               "1" LQA information is available
  # - *_rt: 		columns ending with "_rt" contain the ringtest results
  #             of the attribute for the corresponding period
  #             - "1" Passed
  #             - "2" Failed, passed re-qualification
  #             - "3" Failed (ringtest, fail re-qualification)
  #             - "0" Not participated
  #             - "9999" No ringtest conducted for this attribute
  # - *_loq:		limit of qualification for this attribute in the
  #             corresponding period (same unit as measurement value)


  # Select columns mentioned in "parameters"
    
  qif_sub <- qif %>%
    select(qif_key,
           matches(paste(parameters, collapse = "|")))
  
  }
  
      
  # 2. Join qif with df ----
  
  if (unlist(str_split(survey_form, "_"))[2] == "som") {
    
  df <- df %>%
    left_join(qif_sub,
              by = "qif_key")
  
  }
  
  
  # 3. Gap-fill "loq" data in df ----
  
  for (i in seq_along(parameters)) {
    
    parameter_i <- parameters[i]
    
    # Derive theoretical loqs from ranges_qaqc
    
    if (unlist(str_split(survey_form, "_"))[2] == "som") {
      
    loq_qaqc_i <- data.frame(
      layer_type = c("mineral", "forest_floor", "peat"),
      loq_qaqc = c(
        # mineral
        ranges_qaqc$LOQ_mineral[
          which(ranges_qaqc$parameter_som == parameter_i)],
        # forest_floor: organic
        ranges_qaqc$LOQ_org[
          which(ranges_qaqc$parameter_som == parameter_i)],
        # peat: organic
        ranges_qaqc$LOQ_org[
          which(ranges_qaqc$parameter_som == parameter_i)]))
    }
    
    if (unlist(str_split(survey_form, "_"))[2] == "pfh") {
      
      loq_qaqc_i <- data.frame(
        layer_type = c("mineral", "forest_floor", "peat"),
        loq_qaqc = c(
          # mineral
          ranges_qaqc$LOQ_mineral[
            which(ranges_qaqc$parameter_pfh == parameter_i)],
          # forest_floor: organic
          ranges_qaqc$LOQ_org[
            which(ranges_qaqc$parameter_pfh == parameter_i)],
          # peat: organic
          ranges_qaqc$LOQ_org[
            which(ranges_qaqc$parameter_pfh == parameter_i)]))
    }
    
    # Derive maximum loqs per partner
    
    suppressWarnings({
      
    lqa_i <- lqa %>%
      filter(.data$parameter_harmonised == parameter_i) %>%
      select(code_country, partner_code, survey_year,
             code_plot, quantification_limit) %>%
      group_by(partner_code) %>%
      summarise(loq_max = max(.data$quantification_limit, na.rm = TRUE))
    })
      
    # Add additional LOQ data sources to df
    
    df <- df %>%
      left_join(lqa_i,
                by = "partner_code") %>%
      left_join(loq_qaqc_i,
                by = "layer_type")
    
    # Identify the column with loq to be gapfilled
    
    if (unlist(str_split(survey_form, "_"))[2] == "som") {
      
    assertthat::assert_that((!identical(
      which(names(df) == paste0(parameter_i, "_loq")),
      integer(0))),
      msg = paste0("A column with 'loq' does not seem to be found ",
                   "in the 'qif' form for the given parameter '",
                   parameter_i,
                   "'."))
    
    col_ind_i <- which(names(df) == paste0(parameter_i, "_loq"))
    
    names(df)[col_ind_i] <- "loq_col_to_be_gapfilled"
    
    }
    
    if (unlist(str_split(survey_form, "_"))[2] == "pfh") {
      
      df$loq_col_to_be_gapfilled <- NA
      col_ind_i <- which(names(df) == "loq_col_to_be_gapfilled")
    }
    
    
    # Combine
    
    df <- df %>%
      mutate(loq_col_to_be_gapfilled =
               # Priority 1: qif
               ifelse(!is.na(.data$loq_col_to_be_gapfilled),
                      .data$loq_col_to_be_gapfilled,
                      # Priority 2: maximum of loq_qaqc and loq_max from "lqa"
                      ifelse(!is.na(.data$loq_max),
                             pmax(.data$loq_max, .data$loq_qaqc, na.rm = TRUE),
                             # Priority 3: loq from qaqc
                             .data$loq_qaqc))) %>%
      mutate(loq_col_to_be_gapfilled = as.numeric(loq_col_to_be_gapfilled))
    
    # Rename the column to the original name and remove the other columns
    
    names(df)[col_ind_i] <- paste0(parameter_i, "_loq")
    
    df <- select(df,
                  -loq_max, -loq_qaqc)
    }
  
  
# 4. Update parameter values below LOQ to half of LOQ ----
  
  if (solve == TRUE) {
  
  for (i in seq_along(parameters)) {
    
    parameter_i <- parameters[i]
    
    # Rename active column
    col_ind_i <- which(names(df) == parameter_i)
    names(df)[col_ind_i] <- "active_column"
    
    # Rename corresponding loq column
    col_ind_loq_i <- which(names(df) == paste0(parameter_i, "_loq"))
    names(df)[col_ind_loq_i] <- "active_loq"
    
    # Update the parameter values
    
    df <- df %>%
      mutate(active_column =
               # Whether or not it is indicated as "-1",
               # it is anyway smaller than the loq
               ifelse(.data$active_column < .data$active_loq,
                      0.5 * .data$active_loq,
                      .data$active_column))
    
    # Rename column names back to original
    names(df)[col_ind_i] <- parameter_i
    names(df)[col_ind_loq_i] <- paste0(parameter_i, "_loq")
  }
  }
  
  
  
  if (save_to_env == TRUE) {
    assign_env(survey_form, df)
  } else {
    return(df)
  }
  
}
