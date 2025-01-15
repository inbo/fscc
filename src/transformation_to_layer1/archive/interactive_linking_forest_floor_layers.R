
# Interactive linking of forest floor layers in "pfh" and "som"

# Script initiation date: 20231122

stopifnot(require("tidyverse"),
          require("assertthat"),
          require("aqp"))

source("./src/functions/get_env.R")
source("./src/functions/assign_env.R")

source("./src/functions/expand_unique_survey_vec_adjacent_years.R")


# Specify "code_survey" ----

code_survey <- "s1"


# Prepare data ----

pfh <- get_env(paste0(code_survey, "_pfh")) %>%
  mutate(survey_form = paste0(code_survey, "_pfh")) %>%
  mutate(bulk_density = ifelse(!is.na(.data$horizon_bulk_dens_measure),
                               .data$horizon_bulk_dens_measure,
                               .data$horizon_bulk_dens_est)) %>%
  filter(!is.na(.data$layer_number)) %>%
  rename(profile_id = unique_survey_profile) %>%
  rename(depth_top = horizon_limit_up) %>%
  rename(depth_bottom = horizon_limit_low) %>%
  rename(code_layer = horizon_master) %>%
  rename(repetition = profile_pit_id) %>%
  mutate(repetition = as.character(.data$repetition),
         code_layer = as.character(.data$code_layer)) %>%
  select(survey_form,
         plot_id,
         survey_year,
         unique_survey,
         profile_id,
         repetition,
         code_layer,
         layer_number,
         layer_type,
         depth_top,
         depth_bottom,
         layer_number_ff,
         layer_number_bg,
         colour_moist_hex,
         bulk_density)

som <- get_env(paste0(code_survey, "_som")) %>%
  mutate(colour_moist_hex = NA) %>%
  mutate(survey_form = paste0(code_survey, "_som")) %>%
  filter(!is.na(.data$layer_number)) %>%
  rename(profile_id = unique_survey_repetition) %>%
  rename(depth_top = layer_limit_superior) %>%
  rename(depth_bottom = layer_limit_inferior) %>%
  mutate(repetition = as.character(.data$repetition),
         code_layer = as.character(.data$code_layer)) %>%
  select(survey_form,
         plot_id,
         survey_year,
         unique_survey,
         profile_id,
         repetition,
         code_layer,
         layer_number,
         layer_type,
         depth_top,
         depth_bottom,
         layer_number_ff,
         layer_number_bg,
         colour_moist_hex,
         bulk_density)



link_forest_floors <- NULL

# Evaluate for each of the plot surveys ----

for (i in seq_along(unique(som$unique_survey))) {

  unique_survey_i <- unique(som$unique_survey)[i]

  survey_expanded_i <-
    expand_unique_survey_vec_adjacent_years(unique_survey_i, 3)

  profiles_i <-
    bind_rows(som %>%
                filter(unique_survey == unique_survey_i),
              pfh %>%
                filter(unique_survey %in% survey_expanded_i))  %>%
    filter(!is.na(.data$layer_number_ff) |
             (.data$layer_number_bg == 1)) %>%
    mutate(prof_name = paste0(.data$survey_form, "_",
                              .data$survey_year, "_",
                              .data$repetition)) %>%
    # Some code_layers (especially in "pfh") are not unique within the profile
    # Add the layer_number for those
    group_by(.data$prof_name) %>%
    mutate(duplicated_code_layer =
             duplicated(code_layer) |
             duplicated(code_layer, fromLast = TRUE)) %>%
    ungroup() %>%
    mutate(code_layer_unique = ifelse(.data$duplicated_code_layer == TRUE,
                                      paste0(.data$code_layer, "_",
                                             .data$layer_number),
                                      .data$code_layer)) %>%
    relocate(code_layer_unique, .after = code_layer) %>%
    select(-duplicated_code_layer)



  ## Is it necessary to link these data? ----

      # If this survey exists in "pfh"
  if (!identical(which(grepl("pfh", profiles_i$survey_form)),
                 integer(0)) &&
      # If "pfh" contains any forest floor layer
      any(grepl("pfh", profiles_i$survey_form) &
          profiles_i$layer_type == "forest_floor") &&
      # If "som" contains any forest floor layer
      any(grepl("som", profiles_i$survey_form) &
          profiles_i$layer_type == "forest_floor") &&
      # If any "pfh" forest floor does not have bulk density
      (any(grepl("pfh", profiles_i$survey_form) &
           profiles_i$layer_type == "forest_floor" &
           is.na(profiles_i$bulk_density)) ||
       # If any forest floor layer does not have known layer depths
       any(profiles_i$layer_type == "forest_floor" &
           (is.na(profiles_i$depth_top) |
            is.na(profiles_i$depth_bottom))))) {

    ## Prepare to print the data ----

    profiles_i_summary <- profiles_i %>%
      mutate(layer_type_short = case_when(
        layer_type == "forest_floor" ~ "ff",
        layer_type == "mineral" ~ "min",
        layer_type == "peat" ~ "peat",
        TRUE ~ layer_type)) %>%
      mutate(layer_features = paste0(code_layer_unique, " (",
                                     layer_type_short, "): from ",
                                     depth_top, " to ",
                                     depth_bottom, "     ")) %>%
      select(prof_name, layer_features) %>%
      # Create a unique identifier for each group
      group_by(prof_name) %>%
      mutate(row_ind = row_number(),
             row_ind_max_profile = max(.data$row_ind, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(row_ind_max = max(.data$row_ind, na.rm = TRUE),
             row_ind = .data$row_ind +
               (.data$row_ind_max - .data$row_ind_max_profile)) %>%
      select(-row_ind_max_profile, -row_ind_max) %>%
      # Spread the data for easy printing
      pivot_wider(names_from = prof_name,
                  values_from = layer_features) %>%
      arrange(row_ind) %>%
      select(-row_ind) %>%
      mutate_all(function(x) ifelse(is.na(x), "", x)) %>%
      as.data.frame

    # Print the summary

    cat(paste0(unique_survey_i, "\n \n"))

    # Print "som" columns
    print(profiles_i_summary[, grep("som", names(profiles_i_summary)),
                             drop = FALSE],
    row.names = FALSE,
    right = FALSE)

    cat(" \n")

    # Print "pfh" columns
    print(profiles_i_summary[, grep("pfh", names(profiles_i_summary)),
                             drop = FALSE],
          row.names = FALSE,
          right = FALSE)



    # Collect info for each profile ----

    profiles_ff_i <- profiles_i %>%
      filter(!is.na(.data$layer_number_ff)) %>%
      mutate(code_layers_link = NA)

    for (j in seq_len(nrow(profiles_ff_i))) {


    # If any "pfh" forest floor does not have bulk density

    if (grepl("pfh", profiles_ff_i$survey_form[j]) &&
             is.na(profiles_ff_i$bulk_density[j])) {

      # Prompt for user input
      additional_info <-
        readline(prompt = paste0("Enter code_layer(s) in 'som' ",
                                 "corresponding with '",
                                 profiles_ff_i$code_layer_unique[j],
                                 "' in '",
                                 profiles_ff_i$prof_name[j],
                                 "': "))

      # Add the information
      profiles_ff_i$code_layers_link[j] <-
        str_to_upper(gsub(" ", "_", additional_info))

    }



    # If any forest floor layer does not have known layer depths
    if (is.na(profiles_ff_i$code_layers_link[j]) &&
            (is.na(profiles_ff_i$depth_top[j]) ||
                  is.na(profiles_ff_i$depth_bottom[j]))) {

      # Prompt for user input
      additional_info <-
        ifelse(grepl("pfh", profiles_ff_i$survey_form[j]),
               readline(prompt = paste0("Enter code_layer(s) in 'som' ",
                                        "corresponding with '",
                                        profiles_ff_i$code_layer_unique[j],
                                        "' in '",
                                        profiles_ff_i$prof_name[j],
                                        "': ")),
               readline(prompt = paste0("Enter code_layer(s) in 'som' ",
                                        "corresponding with '",
                                        profiles_ff_i$code_layer_unique[j],
                                        "' in '",
                                        profiles_ff_i$prof_name[j],
                                        "': ")))

      # Add the information
      profiles_ff_i$code_layers_link[j] <-
        str_to_upper(gsub(" ", "_", additional_info))

    }

    } # End of evaluation profiles within unique survey

    link_forest_floors <- rbind(link_forest_floors,
                                profiles_ff_i)

  } # End of check if it is necessary

} # End of evaluation unique surveys


# Export ----

link_forest_floors$change_date <- as.character(Sys.Date())

write.csv2(link_forest_floors,
           paste0("./data/additional_data/",
                  code_survey, "_link_forest_floors.csv"),
           row.names = FALSE,
           na = "")





