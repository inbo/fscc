
#' Get plot x coordinates co-location table
#'
#' This function generates a table that shows the spatial relationships between
#' plots in different survey forms and survey years of a given ICP Forests
#' partner and survey code. Plots on the same location are grouped in the
#' same row.
#' 
#' @param code_survey Character string - The name/code of the ICP Forests
#' survey (e.g. "so").
#' @param partner_code Numeric - partner code of the partner to evaluate
#' @param dist_threshold Numeric - the maximum distance threshold (in meters)
#' for proximity-matching features between groups. Every combination of plots
#' with a distance above this threshold will be considered as located on a
#' different location. Default is 2000 (meter).
#' @param extra_forms Additional forms to include in the co-location table,
#' e.g. an overview with the theoretically existing plots from a partner
#' representative.
#' @details
#' This function looks into survey forms with coordinates of the given survey
#' code (e.g. so_pls and so_prf for survey code "so") as well as the survey
#' forms with coordinates in the associated system installment survey (i.e.
#' si_plt for system installment survey "si" for survey code "so").
#' 
#' The function first groups all plot_ids with coordinates based on survey_year
#' and survey_form (si_plt, so_pls, so_prf). For si_plt, which does not have
#' a survey_year, the column "last_year" is used as survey_year.
#' 
#' The purpose is to create a table with these groups (survey_form x
#' survey_year) as columns and different locations as rows.
#' 
#' In particular, each group of plot_ids is examined: for each plot_id of a
#' certain group (combination of survey form and survey year, e.g. s1_pls 1995),
#' the function searches for co-located plot_ids within a specified distance
#' (default: 2000 meter) in the other groups.
#' 
#' It then creates a new row with the "reference plot_id" in the table, and
#' places the obtained co-located plot_ids in the same row under the
#' corresponding survey_form x survey_year column.
#' 
#' If a certain plot_id is already present in the column of its group (due
#' to being co-located with previous plot_ids from an earlier group),
#' the function skips that plot_id.
#' 
#' In the end, the function creates some extra columns in the table:
#' - latitude_most_abundant and longitude_most_abundant: numeric representing
#'   the most common decimal latitude and longitude associated with the
#'   plot_id's and groups in the given row.
#' - coords_duplicated: this column contains a logical indicating whether the
#'   combination of most abundant latitude and longitude of the given row
#'   has been reported earlier on in the table (duplicated function)
#' - all_equal_survey: if all plot_id's of the groups of only the
#'   given code_survey (i.e. only groups of "so_prf" and "so_pls" and not
#'   those of "si_plt" for "so") reported in the given row are equal, this
#'   column is a character string with the unique plot_id of these groups.
#'   Else, if not all the plot_id's in those groups are the same, this column
#'   is empty.
#' - all_equal: if all plot_id's of all groups associated with the
#'   given code_survey (i.e. also those of "si_plt" for "so") reported in
#'   the given row are equal, this column is a character string with the
#'   unique plot_id of these groups. Else, if not all the plot_id's in
#'   those groups are the same, this column is empty.
#' - plot_id_survey_duplicated: this column contains a logical indicating
#'   whether the plot_id reported in the column "all_equal_survey" in the given
#'   row has been reported earlier on in the table (duplicated function).
#' 
#' @return A dataframe containing the resulting co-location overview table.
#' 
#' @examples
#' df_table_test <-
#' get_plot_coord_colocation_table(code_survey = "so",
#'                                 partner_code = 53,
#'                                 extra_forms = c("extra_so_plot_1995",
#'                                                 "extra_so_plot_1999",
#'                                                 "ks_data"))
#' @export

# Create function get_plot_coord_colocation_table ----

get_plot_coord_colocation_table <- function(code_survey,
                                            partner_code,
                                            dist_threshold = 2000,
                                            extra_forms = NULL) {
  
  source("./src/functions/get_env.R")
  source("./src/functions/as_character_summary.R")
  source("./src/functions/as_sf.R")
  
  stopifnot(require("sf"))
  stopifnot(require("tidyverse"))
  
  
  # Identify survey forms with coordinates to check ----
  
  # Create a list with names of the different survey forms per survey
  
  list_data_tables <- list(so = c("som", "prf", "pls", "pfh", "lqa"),
                           s1 = c("som", "prf", "pls", "pfh", "lqa"),
                           si = c("eve", "plt", "sta", "tco"),
                           y1 = c("pl1", "st1"),
                           sw = c("swa", "swc"),
                           ss = c("lqa", "pss", "ssm"),
                           bd = c("gpl", "dbh", "tht", "dwd", "can", "gvg"),
                           lf = c("lfp", "lfm", "lqa"),
                           c1 = c("tre", "trf"),
                           f1 = c("plf", "fot", "fom", "lqa"),
                           aq = c("pac", "pps", "aqa", "aqp",
                                  "col", "aqb"),
                           cc = c("trc", "trd"),
                           dp = c("pld", "dem", "lqa"),
                           fo = c("plf", "fot", "fom", "lqa"),
                           gr = c("pli", "ipm", "inv", "ira",
                                  "irh", "irm", "irp", "iev"),
                           gv = c("plv", "vem"),
                           gb = c("pgb", "gbm", "gbh", "lqa"),
                           la = c("pla", "lam", "lap", "llf"),
                           li = c("lip", "lit", "lis", "lia"),
                           mm = c("plm", "mem", "meh"),
                           oz = c("pll", "ltf", "lss", "ots",
                                  "ozp"),
                           ph = c("phe", "plp", "phi", "phd",
                                  "phc"))
  
  # Identify the level of the survey (Level I versus Level II)
  
  if (code_survey %in% c("s1", "y1", "bd", "c1", "f1")) {
    survey_level <- "LI"
  }
  if (code_survey %in% c("so", "si", "sw", "ss", "lf",
                         "aq", "cc", "dp", "fo", "gr",
                         "gv", "gb", "la", "li", "mm",
                         "oz", "ph")) {
    survey_level <- "LII"
  }
  
  
  survey_forms <- paste0(code_survey, "_",
                         list_data_tables[[which(names(list_data_tables) ==
                                                   code_survey)]])
  
  
  if (survey_level == "LI" &&
      code_survey != "y1") {
    survey_forms <- c(survey_forms,
                      paste0("y1", "_",
                             list_data_tables[[which(names(list_data_tables) ==
                                                       "y1")]]))
  }
  
  if (survey_level == "LII" &&
      code_survey != "si") {
    survey_forms <- c(survey_forms,
                      paste0("si", "_",
                             list_data_tables[[which(names(list_data_tables) ==
                                                       "si")]]))
  }
  
  
  survey_forms_with_coordinates <- NULL
  
  for (i in seq_along(survey_forms)) {
    if (survey_forms[i] %in% ls(envir = .GlobalEnv)) {
      if (!identical(get_env(survey_forms[i])$latitude_dec, NULL) &&
          !identical(get_env(survey_forms[i])$longitude_dec, NULL)) {
        
        if (is.null(survey_forms_with_coordinates)) {
          survey_forms_with_coordinates <- survey_forms[i]} else
          {survey_forms_with_coordinates <- c(survey_forms_with_coordinates,
                                              survey_forms[i])
          }
      }
    }
  }
  
  
  
  # List all the possible coordinates for plot_ids per unique_survey ----
  
  list_coords <- NULL
  partner_code_value <- partner_code
  
  for (i in seq_along(survey_forms_with_coordinates)) {
    
    list_coords_i <- get_env(survey_forms_with_coordinates[i]) %>%
      filter(partner_code == partner_code_value) %>%
      mutate(unique_survey =
               if (exists("unique_survey")) unique_survey else plot_id) %>%
      mutate(survey_year =
               if (exists("survey_year")) survey_year else last_year) %>%
      mutate(unique_survey_survey_form =
               paste0(survey_forms_with_coordinates[i], "_",
                      unique_survey),
             unique_survey_year =
               paste0(survey_forms_with_coordinates[i], "_",
                      partner_code, "_",
                      survey_year)) %>%
      select(partner_short, partner_code, survey_year, code_plot,
             plot_id, unique_survey, unique_survey_survey_form,
             unique_survey_year, latitude_dec, longitude_dec) %>%
      arrange(unique_survey_survey_form)
    
    list_coords <- rbind(list_coords,
                         list_coords_i)
  }
  
  if (any(is.na(list_coords$latitude_dec)) ||
      any(is.na(list_coords$longitude_dec))) {
    cat(paste0("Some coordinates are missing for partner ",
               d_partner$desc_short[which(d_partner$code == partner_code)],
               " (partner_code: ",
               partner_code,
               ") in survey '",
               code_survey, "'.\n"))
    list_coords <- list_coords %>%
      filter(!is.na(latitude_dec)) %>%
      filter(!is.na(longitude_dec))
  }

  # Check if all plot_ids have one unique coordinate pair
  
  if (length(unique(list_coords$unique_survey_survey_form)) < 
      length(unique(paste0(list_coords$unique_survey_survey_form, "_",
                           list_coords$latitude_dec, "_",
                           list_coords$longitude_dec)))) {
    
    # If not: check where
    
    df_dupl_coord <- list_coords %>%
      mutate(comb = paste0(unique_survey_survey_form, "_",
                           latitude_dec, "_",
                           longitude_dec)) %>%
      group_by(unique_survey_survey_form, code_plot, unique_survey_year) %>%
      summarise(count = n(), .groups = "drop") %>%
      filter(count > 1)
    
    vec_dupl_coord <- df_dupl_coord$code_plot
    vec_dupl_coord_plot_id <- paste0(partner_code, "_", vec_dupl_coord)
    vec_dupl_coord_survey <- df_dupl_coord$unique_survey_survey_form
  
    cat(paste0("Coordinates for plot_id(s) ",
               as_character_summary(vec_dupl_coord_plot_id),
               " are not unique in ",
               as_character_summary(df_dupl_coord$unique_survey_year),
               ".\n"))
    
    # And update list_coords 
    for (j in seq_along(vec_dupl_coord_survey)) {
      
      ind_rows_to_update_fscc <-
        which(list_coords$unique_survey_survey_form ==
                vec_dupl_coord_survey[j])
      
      vec_columns_to_update <- which(names(list_coords) %in%
                                       c("code_plot",
                                         "plot_id",
                                         "unique_survey",
                                         "unique_survey_survey_form"))
      
      for (k in vec_columns_to_update) {
        list_coords[ind_rows_to_update_fscc, k] <-
          paste0(list_coords[ind_rows_to_update_fscc, k],
                 "fscc",
                 seq_along(ind_rows_to_update_fscc))
      }
    }
  }
  
  
  
  # Convert to spatial
  
  df_spat <- list_coords %>%
    st_as_sf(coords = c("longitude_dec", "latitude_dec"),
             crs = 4326) %>%
    st_transform(crs = 3035)
  
  
  # Identify unique combinations of survey_forms and survey_years
  
  unique_survey_years <- unique(list_coords$unique_survey_year)
  
  # To know the relative positions (col indices) in the tables below
  col_indices <- data.frame(unique_survey_year = unique_survey_years,
                            col_ind = seq_along(unique_survey_years))
  
  # Table with plot_ids and one row per location
  df_table <- data.frame(matrix(NA,
                                nrow = 0,
                                ncol = length(unique_survey_years)))
  names(df_table) <- unique_survey_years
  
  # Table with match type: either proximity match
  # or plot_id match (in case of no coords)
  df_match_type <- data.frame(matrix(NA,
                                     nrow = 0,
                                     ncol = length(unique_survey_years)))
  names(df_match_type) <- unique_survey_years
  
  # dataframe for plot_ids without coordinates
  
  df_without_coords <- data.frame(matrix(NA,
                                         nrow = 0,
                                         ncol = ncol(list_coords)))
  names(df_without_coords) <- names(list_coords)
  
  # Evaluate for each of the reported coordinate x plot_id combinations ----
  
  # Set progress bar
  progress_bar <- txtProgressBar(min = 0,
                                 max = length(unique_survey_years),
                                 style = 3)
  
  for (i in seq_along(unique_survey_years)) {
    
    df_i <- list_coords[which(
      list_coords$unique_survey_year == unique_survey_years[i]), ]
    df_spat_i <- df_spat %>%
      filter(unique_survey_year != unique_survey_years[i])
    col_i <- which(names(df_table) == unique_survey_years[i])


    for (j in seq_along(unique(df_i$plot_id))) {
      
      plot_id_j <- (unique(df_i$plot_id))[j]
      
      # If the given plot_id_j has not been matched to an earlier plot_id yet
      
      if (identical(which(df_table[, col_i] == plot_id_j), integer(0))) {
        
        lat_j <- df_i$latitude_dec[which(df_i$plot_id == plot_id_j)]
        long_j <- df_i$longitude_dec[which(df_i$plot_id == plot_id_j)]
        
        if (all(is.na(lat_j)) &&
            all(is.na(long_j))) {
          
          df_without_coords <- rbind(df_without_coords,
                                     df_i[which(df_i$plot_id ==
                                                  plot_id_j)[1], ])
          
          # At least one coordinate reported
        } else {
          
          lat_j <- as.numeric(lat_j[!is.na(lat_j)])
          long_j <- as.numeric(long_j[!is.na(long_j)])
          
          if (isFALSE(length(unique(lat_j)) == 1 &&
              length(unique(long_j)) == 1)) {
          cat(paste0("Coordinates for plot_id '",
                         plot_id_j,
                         "' are not unique in '",
                         unique_survey_years[i],
                         "'."))
          }
          
          lat_j <- unique(lat_j)
          long_j <- unique(long_j)
          
          coord_j <- data.frame(plot_id = plot_id_j,
                                latitude_dec = lat_j,
                                longitude_dec = long_j) %>%
            st_as_sf(coords = c("longitude_dec", "latitude_dec"),
                     crs = 4326) %>%
            st_transform(crs = 3035)
          
          # Search for proximity matches
          
          sf::st_is_within_distance(coord_j,
                                    df_spat_i,
                                    dist = dist_threshold,
                                    sparse = FALSE) |>
            which() -> vec
          
          nearby_rows <- st_drop_geometry(df_spat_i[vec, ]) %>%
            distinct(unique_survey_year, .keep_all = TRUE) %>%
            left_join(col_indices, by = "unique_survey_year")
          
          # Update df_table and df_match_type
          
          df_table_j <- data.frame(matrix(NA,
                                          nrow = 1,
                                          ncol = length(unique_survey_years)))
          names(df_table_j) <- unique_survey_years
          
          df_match_type_j <- data.frame(matrix(NA,
                                            nrow = 1,
                                            ncol = length(unique_survey_years)))
          names(df_match_type_j) <- unique_survey_years
          
          
          df_table_j[1, col_i] <- plot_id_j
          df_match_type_j[1, col_i] <- "reference"
          
          for (k in seq_len(nrow(nearby_rows))) {
            df_table_j[1, nearby_rows$col_ind[k]] <- nearby_rows$plot_id[k]
            df_match_type_j[1, nearby_rows$col_ind[k]] <- "spatial"
          }
          
          df_table <- rbind(df_table,
                            df_table_j)
          
          df_match_type <- rbind(df_match_type,
                                 df_match_type_j)
          
        }
      }
    }
    
    # Update progress bar
    setTxtProgressBar(progress_bar, i)
  }
  
  close(progress_bar)
  
  
  # Add coordinates ----
  
  # Create most_abundant function
  
  most_abundant <- function(x) {
    ux <- unique(na.omit(x))
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Create find_row_indices function
  
  find_row_indices <- function(data, subset) {
    row_indices <- lapply(names(subset), function(name) {
      # Find matching rows based on unique_survey_year and plot_id
      row_ind <-
        which(data$unique_survey_year == name &
                data$plot_id == subset[[name]], arr.ind = TRUE)
      return(row_ind)
    })
    
    # Combine all the row indices
    combined_row_indices <- do.call(c, row_indices)
    # Remove any duplicates and return the result
    return(unique(combined_row_indices))  
  }
  
  df_table$latitude_most_abundant <- NA
  df_table$longitude_most_abundant <- NA
  
  for (i in seq_len(nrow(df_table))) {
    
    subset <- df_table[i, which(!is.na(df_table[i, ]))]
    # subset <- df_table[i, which(!is.na(df_table[i, vec_cols]))]
    
    if (length(subset) == 1) {
      names(subset) <- names(df_table)[which(!is.na(df_table[i, ]))]
    }
    
    # Find row indices in list with coordinates list_coords
    row_indices <- find_row_indices(list_coords, subset)
    
    # Identify most abundant coordinates
    df_table$latitude_most_abundant[i] <-
      most_abundant(x = as.numeric(list_coords$latitude_dec[row_indices]))
    df_table$longitude_most_abundant[i] <-
      most_abundant(x = as.numeric(list_coords$longitude_dec[row_indices]))
    
  }
  
  
  
  # Add extra data ----
  
  if (!is.null(extra_forms)) {
    
    source("./src/functions/distance_join.R")
    
    # Get extra forms if any
    
    assertthat::assert_that(is.character(extra_forms),
                            msg = paste0("Argument 'extra_forms' should be ",
                                         "a character vector."))
    extra_column_names <- extra_forms
    
    assertthat::assert_that(length(extra_forms) <= 3,
                            msg = paste0("Length of argument 'extra_forms' ",
                                         "is longer than 3. Adjust function ",
                                         "script to account for this."))
    
    
    
    if (length(extra_forms) >= 1) {
      
      extra_df <- get_env(extra_forms[1])
      
      if (is.data.frame(extra_df)) {
        extra_df <- extra_df %>%
          st_as_sf(coords = c("longitude_dec", "latitude_dec"),
                   crs = 4326) %>%
          st_transform(crs = 3035)
      }
      
      df_table_spat <- df_table %>%
        mutate(longitude_dec = longitude_most_abundant) %>%
        mutate(latitude_dec = latitude_most_abundant) %>%
        st_as_sf(coords = c("longitude_dec", "latitude_dec"),
                 crs = 4326) %>%
        st_transform(crs = 3035)
      
      df_table <- distance_join(sf1 = df_table_spat,
                                sf2 = extra_df,
                                join_column = "plot_id",
                                dist_threshold = dist_threshold) %>%
        st_drop_geometry()
      
      df_table$plot_id <- as.character(df_table$plot_id)
      names(df_table)[which(names(df_table) == "plot_id")] <- extra_forms[1]
      
    }
    
    
    
    
    if (length(extra_forms) >= 2) {
      
      extra_df <- get_env(extra_forms[2])
      
      if (is.data.frame(extra_df)) {
        extra_df <- extra_df %>%
          st_as_sf(coords = c("longitude_dec", "latitude_dec"),
                   crs = 4326) %>%
          st_transform(crs = 3035)
      }
      
      df_table_spat <- df_table %>%
        mutate(longitude_dec = longitude_most_abundant) %>%
        mutate(latitude_dec = latitude_most_abundant) %>%
        st_as_sf(coords = c("longitude_dec", "latitude_dec"),
                 crs = 4326) %>%
        st_transform(crs = 3035)
      
      df_table <- distance_join(sf1 = df_table_spat,
                                sf2 = extra_df,
                                join_column = "plot_id",
                                dist_threshold = dist_threshold) %>%
        st_drop_geometry()
      
      df_table$plot_id <- as.character(df_table$plot_id)
      names(df_table)[which(names(df_table) == "plot_id")] <- extra_forms[2]
      
    }
    
    
    
    
    if (length(extra_forms) >= 3) {
      
      extra_df <- get_env(extra_forms[3])
      
      if (is.data.frame(extra_df)) {
        extra_df <- extra_df %>%
          st_as_sf(coords = c("longitude_dec", "latitude_dec"),
                   crs = 4326) %>%
          st_transform(crs = 3035)
      }
      
      df_table_spat <- df_table %>%
        mutate(longitude_dec = longitude_most_abundant) %>%
        mutate(latitude_dec = latitude_most_abundant) %>%
        st_as_sf(coords = c("longitude_dec", "latitude_dec"),
                 crs = 4326) %>%
        st_transform(crs = 3035)
      
      df_table <- distance_join(sf1 = df_table_spat,
                                sf2 = extra_df,
                                join_column = "plot_id",
                                dist_threshold = dist_threshold) %>%
        st_drop_geometry()
      
      df_table$plot_id <- as.character(df_table$plot_id)
      names(df_table)[which(names(df_table) == "plot_id")] <- extra_forms[3]
      
    }
    
    
    df_table <- df_table %>%
      mutate(across(everything(), ~ ifelse(.x == "NA", NA, .x))) %>%
      relocate(longitude_most_abundant, .after = ncol(.)) %>%
      relocate(latitude_most_abundant, .after = ncol(.))
  }


  # Final data processing ----

  
  df_table$combined_coords <-
    apply(df_table[, which(names(df_table) %in% c("latitude_most_abundant",
                                                  "longitude_most_abundant"))],
          1, function(x) paste0(x, collapse = "_"))
  
  df_table <- df_table %>%
    mutate(coords_duplicated = duplicated(combined_coords)) %>%
    select(-combined_coords)
  
  all_equal_function <- function(x) {
    if (length(unique(x[!is.na(x)])) == 1) {
      plot_id_unique <- (unique(x[!is.na(x)]))
      return(gsub("fscc\\d+$", "", plot_id_unique))
    } else {
      return(NA)
    }
  }
  
  vec_cols_survey <- which(grepl(code_survey, unique_survey_years))
  
  df_table <- df_table %>%
    mutate(all_equal_survey = {
      cols <- across(
        starts_with(unique_survey_years[1]):starts_with(
          unique_survey_years[max(vec_cols_survey)]))
      apply(cols, 1, all_equal_function)
    }) %>%
    mutate(all_equal = {
      cols <- across(
        starts_with(unique_survey_years[1]):starts_with(
          unique_survey_years[length(unique_survey_years)]))
      apply(cols, 1, all_equal_function)
    }) %>%
    mutate(plot_id_survey_duplicated = ifelse(!is.na(all_equal_survey),
                                              duplicated(all_equal_survey),
                                              NA)) %>%
    mutate(arrange_col = ifelse(!is.na(all_equal_survey),
                                as.numeric(gsub(paste0("^", partner_code, "_"),
                                                "",
                                                all_equal_survey)),
                                NA)) %>%
    arrange(arrange_col) %>%
    select(-arrange_col)

  if (any(df_table$plot_id_survey_duplicated[
    which(!is.na(df_table$plot_id_survey_duplicated))] == TRUE)) {

    ind_plot_id_dupl <- which(df_table$plot_id_survey_duplicated == TRUE)
    df_table$dist_plot_move_meter <- NA

    for (i in ind_plot_id_dupl) {

      ind_reference <- which(df_table$all_equal_survey ==
                               df_table$all_equal_survey[i] &
                               df_table$plot_id_survey_duplicated == FALSE)

      df_sub_spat <- df_table[c(ind_reference, i), ] %>%
        mutate(latitude_dec = latitude_most_abundant) %>%
        mutate(longitude_dec = longitude_most_abundant) %>%
        as_sf

      df_table$dist_plot_move_meter[i] <-
        as.numeric(st_distance(df_sub_spat[1, ],
                               df_sub_spat[2, ]))
    }
  }

  return(df_table)
  
}
