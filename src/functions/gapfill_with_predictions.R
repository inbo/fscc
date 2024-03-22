

gapfill_with_predictions <- function(survey_form,
                                     data_frame = NULL,
                                     save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  cat(paste0(" \nGap-fill '", survey_form, "' with predictions\n"))


  code_survey <- unlist(str_split(survey_form, "_"))[1]
  survey_form_type <- unlist(str_split(survey_form, "_"))[2]


  # Import survey forms ----

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }


  if (survey_form_type == "pfh") {

    # Harmonise the variable names with those of "som"

    df <- df %>%
      rename(layer_limit_superior = horizon_limit_up) %>%
      rename(layer_limit_inferior = horizon_limit_low) %>%
      rename(code_layer = horizon_master) %>%
      rename(organic_carbon_total = horizon_c_organic_total) %>%
      rename(unique_survey_repetition = unique_survey_profile)

  }


  # Pedotransfer function for bulk density in mineral layers ----

  # Expected relationship (BD-PTF) with TOC:
  # BD = 1511 – (81,1*sqrt(TOC))

  # This is based on the analysis of layer 1 "s1" data (using different models
  # and predictors) and validation using the layer 1 "so" data
  # (in Feb/March 2024) - cfr. script [TO DO: ADD SCRIPT]

  bd_ptf <- function(toc) {
    ifelse(is.na(toc), NA, 1511 - (81.1 * sqrt(toc)))
  }


  # Mean bulk density for forest floor

  source("./src/functions/get_bulk_density_stats.R")

  bd_ff <- get_bulk_density_stats(mode = "mean",
                                  layer_type = "forest_floor")

  # Mean bulk density for peat

  source("./src/functions/get_bulk_density_stats.R")

  bd_peat <- get_bulk_density_stats(mode = "mean",
                                    layer_type = "peat")




  # Uncertainty in PTF ----

  uncertainty_ptf <- 160 # kg m-3

  bd_ptf_upper <- function(bd) {
    ifelse(is.na(bd),
           NA,
           ifelse((round(bd + uncertainty_ptf) > 2650),
                  2650,
                  round(bd + uncertainty_ptf)))
  }

  bd_ptf_lower <- function(bd) {
    ifelse(is.na(bd),
           NA,
           ifelse((round(bd - uncertainty_ptf) < 0),
                  0,
                  round(bd - uncertainty_ptf)))
  }


  #  Gap-fill bulk densities for mineral layers ----

  df <- df %>%
    mutate(bd_ptf = case_when(
      # Mineral layers: ptf
      layer_type == "mineral" ~ round(bd_ptf(.data$organic_carbon_total)),
      # Peat layers: bd_peat
      layer_type == "peat" ~ round(bd_peat),
      # Forest floor layers: bd_ff
      layer_type == "forest_floor" ~ round(bd_ff),
      .default = NA_integer_)) %>%
    # Bulk density: combine columns
    mutate(
      bulk_density_source = case_when(
        !is.na(bulk_density) ~ bulk_density_source,
        !is.na(bd_ptf) ~ "PTF",
        .default = NA_character_),
      bulk_density_min = case_when(
        !is.na(bulk_density) ~ bulk_density_min,
        !is.na(bd_ptf) ~ bd_ptf_lower(bd_ptf),
        .default = NA_integer_),
      bulk_density_max = case_when(
        !is.na(bulk_density) ~ bulk_density_max,
        !is.na(bd_ptf) ~ bd_ptf_upper(bd_ptf),
        .default = NA_integer_),
      bulk_density = coalesce(bulk_density,
                              bd_ptf)) %>%
    # Calculate organic_layer_weight based on these bulk densities
    mutate(organic_layer_weight_ptf =
             ifelse(!is.na(.data$layer_thickness) &
                      (.data$layer_type %in% c("forest_floor", "peat")),
                    # kg m-2
                    round(.data$bd_ptf * (.data$layer_thickness * 1e-2), 2),
                    NA_real_),
           organic_layer_weight_ptf_min =
             ifelse(!is.na(.data$layer_thickness) &
                      (.data$layer_type %in% c("forest_floor", "peat")),
                    # kg m-2
                    round(.data$bulk_density_min *
                            (.data$layer_thickness * 1e-2), 2),
                    NA_real_),
           organic_layer_weight_ptf_max =
             ifelse(!is.na(.data$layer_thickness) &
                      (.data$layer_type %in% c("forest_floor", "peat")),
                    # kg m-2
                    round(.data$bulk_density_max *
                            (.data$layer_thickness * 1e-2), 2),
                    NA_real_)) %>%
    # Organic layer weight: combine columns
    mutate(
      organic_layer_weight_source = ifelse(
        layer_type %in% c("peat", "forest_floor"),
        case_when(
          !is.na(organic_layer_weight) ~ organic_layer_weight_source,
          !is.na(organic_layer_weight_ptf) ~
            paste0("bulk_density (", .data$bulk_density_source, ")")),
        NA_character_),
      organic_layer_weight_min = ifelse(
        layer_type %in% c("peat", "forest_floor"),
        case_when(
          !is.na(organic_layer_weight) ~ organic_layer_weight_min,
          !is.na(organic_layer_weight_ptf_min) ~ organic_layer_weight_ptf_min),
        NA_integer_),
      organic_layer_weight_max = ifelse(
        layer_type %in% c("peat", "forest_floor"),
        case_when(
          !is.na(organic_layer_weight) ~ organic_layer_weight_max,
          !is.na(organic_layer_weight_ptf_max) ~ organic_layer_weight_ptf_max),
        NA_integer_),
      organic_layer_weight = ifelse(
        layer_type %in% c("peat", "forest_floor"),
        coalesce(organic_layer_weight,
                 organic_layer_weight_ptf),
        NA_integer_))



  cat("\nData sources 'bulk_density' after gap-filling with PTFs:\n")
  print(table(
    df %>%
      mutate(bulk_density_source_harm = case_when(
        grepl("fscdb", bulk_density_source, ignore.case = TRUE) ~
          "old database versions",
        grepl("pir", bulk_density_source, ignore.case = TRUE) ~ "PIRs",
        grepl("ptf", bulk_density_source, ignore.case = TRUE) ~ "PTF",
        grepl("som", bulk_density_source) ~ "som",
        grepl("swc", bulk_density_source) ~ "swc",
        grepl("pfh.*measure|measure.*pfh", bulk_density_source) ~
          "pfh (measured)",
        grepl("pfh.*est|est.*pfh", bulk_density_source) ~ "pfh (estimated)",
        .default = bulk_density_source)) %>%
      pull(bulk_density_source_harm)))



  if (survey_form_type == "pfh") {

    # Change the variable names back to the original names

    df <- df %>%
      rename(horizon_limit_up = layer_limit_superior) %>%
      rename(horizon_limit_low = layer_limit_inferior) %>%
      rename(horizon_master = code_layer) %>%
      rename(horizon_c_organic_total = organic_carbon_total) %>%
      rename(unique_survey_profile = unique_survey_repetition)

  }


  # Save ----

  if (save_to_env == TRUE) {
    assign_env(survey_form, df)
  } else {
    return(df)
  }

}
