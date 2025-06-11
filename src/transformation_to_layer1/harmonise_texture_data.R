
# Harmonise texture data
# ----------------------

# Harmonise Level II soil texture data, collected during a specific call
# in December 2024


# Define required packages
stopifnot(require("tidyverse"),
          require("openxlsx"),
          require("readxl"),
          require("assertthat"),
          require("soiltexture"))

source("./src/functions/get_env.R")
source("./src/functions/depth_join.R")
source("./src/functions/as_sf.R")
source("./src/functions/distance_join.R")


columns_key <- c("country", "partner_short", "partner", "survey_year",
                 "code_country", "partner_code", "code_plot", "plot_id",
                 "repetition",
                 "code_layer", "layer_type", "layer_number",
                 "layer_limit_superior", "layer_limit_inferior")


# Croatia ----

# (BD no, stones no, texture class no)

croatia_so <- read.csv(paste0("./data/additional_data/partner_comm/",
                              "texture_data_collection_dec2024/croatia_so.csv"),
                       sep = ";") %>%
  as_tibble %>%
  rename("frac_.2_.02" = "frac_.2._.02") %>%
  mutate(
    part_size_clay = NA,
    part_size_silt = NA,
    part_size_sand = NA) %>%
  mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
  group_by(plot_id) %>%
  arrange(layer_limit_superior, .by_group = TRUE) %>%
  mutate(layer_number = row_number()) %>%
  ungroup()

# Convert texture boundaries

  for (i in seq_len(nrow(croatia_so))) {

    fractions_i <-
      TT.text.transf.X(tri.data = croatia_so[i, ] %>%
                         # Ascending order!
                       select("frac_.002_",
                              "frac_.02_.002",
                              "frac_.2_.02",
                              "frac_2_.2"),
                     # Original limits
                     dat.ps.lim = c(0, 2, 20, 200, 2000),
                     # Target limits (sand fraction should increase)
                     base.ps.lim = c(0, 2, 63, 2000)) %>%
      rename(part_size_clay = C1,
             part_size_silt = C2,
             part_size_sand = C3)

    croatia_so$part_size_clay[i] <- fractions_i$part_size_clay
    croatia_so$part_size_silt[i] <- fractions_i$part_size_silt
    croatia_so$part_size_sand[i] <- fractions_i$part_size_sand
  }

# Add bulk density and coarse fragments in order to be able to take
# the soil mass into account, when converting the data to the correct depths

croatia_so <- croatia_so %>%
  select(-starts_with("frac_")) %>%
  depth_join(
    df2 = get_env("so_som") %>%
      filter(plot_id %in% c("57_103", "57_104")) %>%
      filter(survey_year == 1995) %>%
      select(any_of(columns_key),
             bulk_density, coarse_fragment_vol),
    mode = "constant_physical_parameters")

# Convert to the correct depths

df_croatia <- get_env("so_som") %>%
  filter(plot_id %in% c("57_103", "57_104")) %>%
  filter(survey_year == 1995) %>%
  select(any_of(columns_key)) %>%
  depth_join(df2 = croatia_so,
             mode = "constant_physical_parameters") %>%
  mutate(
    part_size_clay = round(part_size_clay, 1),
    part_size_silt = round(part_size_silt, 1),
    part_size_sand = round(part_size_sand, 1)) %>%
  select(-bulk_density, -coarse_fragment_vol)











# Sweden IM ----

# (BD yes, stones yes, texture class no)

# Filter for surveys after 1985. Do not filter for BioSoil profiles only,
# as these do not contain texture data

# IM Gårdsjön F1    - 6451 - ICP Forests plot code: 6451
# IM Aneboda         - 5751 - ICP Forests plot code: 5751
# IM Kindla              - 6851 - ICP Forests plot code: 6851
# IM Gammtratten - 7251 - ICP Forests plot code: 7251

plots_sweden <- data.frame(
  Site = c("Aneboda", "Gammtratten", "Gårdsjön", "Kindla"),
  code_plot = c(5751, 7251, 6451, 6851))


# Create a function that harmonises the depths in column
# `Mineral soil depth (cm)`

parse_depth_range <- function(depth_range, code_layer, layer_type) {
  # Remove whitespace around dash
  # depth_range <- gsub("\\s*–\\s*", "–", depth_range)
  # depth_range <- gsub("\\s*-\\s*", "–", depth_range)
#depth_range <- gsub("(?<![\\s])-((?=[\\d]|$))", "–", depth_range, perl = TRUE)

  # Handle special cases
  if (is.na(depth_range)) return(c(NA, NA))

  # Handle '>60' and similar cases
  if (grepl("^>60", depth_range)) {
    return(c(60, NA))
  }

  if (grepl("^>-40", depth_range)) {
    return(c(40, NA))
  }

  if (depth_range == "55-") {
    return(c(55, NA))
  }

  depth_range <- gsub("(?<!^)(?<![\\s])-((?=[\\d]|$))", "–",
                      depth_range, perl = TRUE)

  # Split the range
  parts <- unlist(strsplit(depth_range, "–"))

  # return(paste(parts, collapse = "_"))

  # Handle cases with only one value
  if (length(parts) == 1) {
    # If forest floor layer, assume 0 as the upper limit
    if ((!is.na(code_layer) & code_layer == "O") |
        layer_type == "forest_floor") {
      return(c((-1) * abs(as.numeric(parts)), 0))
    }

    if (layer_type == "peat") {
      return(c(0, abs(as.numeric(parts))))
    }

  }

  if (layer_type == "forest_floor" &
      all(as.numeric(parts) >= 0)) {
    parts <- (-1) * abs(as.numeric(parts))
  }

  if (layer_type != "forest_floor" &
      all(as.numeric(parts) <= 0)) {
    parts <- abs(as.numeric(parts))
  }

  # Normal case with two values
  upper <- min(as.numeric(parts))
  lower <- max(as.numeric(parts))

  return(c(upper, lower))
}




# Harmonise data


sweden_so <-
  read_excel(paste0("./data/additional_data/partner_comm/",
                    "texture_data_collection_dec2024/original/",
                    "Swe-IM-Soil-Physics.xlsx"),
             skip = 2) %>%
  left_join(
    plots_sweden,
    by = "Site") %>%
  mutate(code_country = 13) %>%
  rename(depth_range = `Mineral soil depth (cm)`,
         code_layer = `Field estimated genetic soil horizon`,
         survey_year = `Sampling year`) %>%
  filter(survey_year > 1985) %>%
  mutate(
    layer_type = case_when(
      `Soil type` %in% c("O-hor", "OE-hor") ~ "forest_floor",
      `Soil type` %in% c("Organic", "Peat") ~ "peat",
      `Soil type` %in% c("Ah-hor", "Mineral") ~ "mineral",
      TRUE ~ `Soil type`),
    layer_limit_superior = NA,
    layer_limit_inferior = NA) %>%
  relocate(layer_limit_superior, layer_limit_inferior,
           .before = depth_range) %>%
  rowwise() %>%
  # Harmonise the depth ranges based on the above-mentioned function
  mutate(
    layer_limit_superior = parse_depth_range(depth_range,
                                             code_layer,
                                             layer_type)[1],
    layer_limit_inferior = parse_depth_range(depth_range,
                                             code_layer,
                                             layer_type)[2]) %>%
  ungroup() %>%
  mutate(
    profile_pit_id =
      case_when(
        Site == "Gammtratten" ~ `Location 1`,
        TRUE ~ paste0(`Location 1`, "_", `Location 2`))) %>%
  mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
  group_by(plot_id) %>%
  arrange(profile_pit_id, .by_group = TRUE) %>%
  mutate(profile_pit_id =
           as.numeric(factor(profile_pit_id,
                             levels = unique(profile_pit_id)))) %>%
  ungroup() %>%
  relocate(profile_pit_id, .after = `Location 2`) %>%
  mutate(profile_id = paste0(survey_year, "_",
                             plot_id, "_",
                             profile_pit_id)) %>%
  mutate(
    bulk_density = ifelse(
      `Bulk density measured (g/cm3)` > 2.65,
      `Bulk density measured (g/cm3)`,
      `Bulk density measured (g/cm3)` * 1E3),
    coarse_fragment_vol = `Stones+ boulders, % (model 2, PAM+LL)` * 1E-2) %>%
  rename(
    gravel = `Gravel (%), >2mm`,
    "frac_.002_" = `Clay  (%), d<0.002 mm`,
    "frac_.006_.002" = `Fine silt  (%), 0.002≤d<0.006 mm`,
    "frac_.02_.006" = `Course silt  (%), 0.006≤d<0.02 mm`,
    "frac_.06_.02" = `Fine sand I  (%) (Finmo), 0.02≤d<0.06 mm`,
    "frac_.2_.06" = `Fine sand II  (%) (Grovmo),  0.06≤d<0.2 mm`,
    "frac_.6_.2" = `Intermediate sand  (%), 0.2≤d<0.6 mm`,
    "frac_2_.6" = `Course sand  (%), 0.6≤d<2 mm`) %>%
  mutate(
    part_size_clay = NA,
    part_size_silt = NA,
    part_size_sand = NA) %>%
  group_by(profile_id) %>%
  arrange(layer_limit_superior, .by_group = TRUE) %>%
  mutate(layer_number = row_number()) %>%
  ungroup()


# Convert texture boundaries

ind_tex <- which(!is.na(sweden_so$`frac_.002_`))
cols_tex <- c("frac_.002_",
              "frac_.006_.002",
              "frac_.02_.006",
              "frac_.06_.02",
              "frac_.2_.06",
              "frac_.6_.2",
              "frac_2_.6")


for (i in ind_tex) {

  # Normalise the texture fractions to obtain a sum of 100 %
  # (because gravel is also included in the current 100 %)

  sum_texture_i <- sweden_so[i, ] %>%
    select(any_of(cols_tex)) %>%
    rowSums

  sweden_so_i <- sweden_so[i, ] %>%
    mutate_at((cols_tex),
            ~ ifelse(
              !is.na(.),
              (.) / sum_texture_i * 100,
              NA))

  fractions_i <-
    TT.text.transf.X(tri.data = sweden_so_i %>%
                       # Ascending order!
                       select("frac_.002_",
                              "frac_.006_.002",
                              "frac_.02_.006",
                              "frac_.06_.02",
                              "frac_.2_.06",
                              "frac_.6_.2",
                              "frac_2_.6"),
                     # Original limits
                     dat.ps.lim = c(0, 2, 6, 20, 60, 200, 600, 2000),
                     # Target limits (sand fraction should increase)
                     base.ps.lim = c(0, 2, 63, 2000)) %>%
    rename(part_size_clay = C1,
           part_size_silt = C2,
           part_size_sand = C3)

  sweden_so$part_size_clay[i] <- fractions_i$part_size_clay
  sweden_so$part_size_silt[i] <- fractions_i$part_size_silt
  sweden_so$part_size_sand[i] <- fractions_i$part_size_sand
}


# Harmonise the vol% coarse fragments

sweden_so <- sweden_so %>%
  select(-starts_with("frac_")) %>%
  mutate(
    # Convert column "gravel" to volumetric
    gravel_vol_converted =
      ifelse(!is.na(bulk_density) & !is.na(gravel),
             round(
             (.data$gravel * .data$bulk_density) /
               (2650 - .01 * .data$gravel * (2650 - .data$bulk_density)),
             3),
             NA)) %>%
  mutate(
    coarse_fragment_vol = case_when(
      !is.na(coarse_fragment_vol) & !is.na(gravel_vol_converted) ~
        coarse_fragment_vol + gravel_vol_converted,
      !is.na(gravel_vol_converted) ~ gravel_vol_converted,
      TRUE ~ coarse_fragment_vol))


# Convert to the correct depths

df_sweden <- get_env("so_som") %>%
  filter(plot_id %in% paste0("13_", plots_sweden$code_plot)) %>%
  select(any_of(columns_key)) %>%
  # Give priority to BioSoil data for bulk density
  # (no texture data available in BioSoil records)
  depth_join(df2 = sweden_so %>%
               filter(Project == "BioSoil"),
             prefix_parameters_in_df1 = "biosoil_",
             mode = "constant_physical_parameters",
             parameters = c("bulk_density")) %>%
  depth_join(df2 = sweden_so,
             mode = "constant_physical_parameters") %>%
  # Add the other coarse fragments data from the other Excel tab to see
  # if they are better
  depth_join(df2 =
               read_excel(paste0("./data/additional_data/partner_comm/",
                                 "texture_data_collection_dec2024/original/",
                                 "Swe-IM-Soil-Physics.xlsx"),
                          sheet = "Stones", skip = 2) %>%
               left_join(
                 plots_sweden,
                 by = "Site") %>%
               mutate(code_country = 13,
                      layer_limit_superior = 0,
                      layer_limit_inferior = 30,
                      layer_number = 1,
                      layer_type = "mineral",
                      bulk_density = 750) %>%
               mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
               rename(coarse_fragment_vol =
                        `Stones+ boulders, % (model 2, PAM+LL)`),
             prefix_parameters_in_df1 = "stones_",
             mode = "constant_physical_parameters",
             parameters = c("coarse_fragment_vol")) %>%
  mutate(
    part_size_clay = round(part_size_clay, 1),
    part_size_silt = round(part_size_silt, 1),
    part_size_sand = round(part_size_sand, 1),
    # The other coarse fragment data based on gravel on the first sheet
    # seems less reliable
    coarse_fragment_vol = round(stones_coarse_fragment_vol, 1),
    bulk_density = round(coalesce(biosoil_bulk_density,
                                  bulk_density), 1)) %>%
  select(-biosoil_bulk_density,
         -stones_coarse_fragment_vol)










# Sweden Swethro ----

# (BD no,
#  stones yes but not accurate and smaller than 2 cm but better than nothing,
#  texture class no)

path_swethro <- paste0("./data/additional_data/partner_comm/",
                       "texture_data_collection_dec2024/original/",
                       "sweden_swethro_texture/")

plots_swethro <- list.dirs(path_swethro, full.names = FALSE)
plots_swethro <- plots_swethro[which(!plots_swethro == "")]





process_texture_files <- function(raw_data) {

    # 1. Extract sample ID

    sample_id_row <- which(raw_data == "Provets ID", arr.ind = TRUE)[1, "row"]
    sample_id_col <- which(raw_data == "Provets ID", arr.ind = TRUE)[1, "col"]
    sample_id <- raw_data[sample_id_row, sample_id_col + 1]

    # If sample ID is NA, look one more column to the right
    # (handling possible empty cell)
    if (is.na(sample_id)) {
      sample_id <- raw_data[sample_id_row, sample_id_col + 2]
    }

    texture_values <- data.frame()


    # 2. Texture fractions

    # Find the column containing "Mängd"
    # which indicates our texture data section
    mengd_col <- which(raw_data == "Mängd", arr.ind = TRUE)[1, "col"]

    # Find the column indices for our texture classes
    texture_classes <- data.frame(
      swed = c("Ler", "Silt", "Sand", "Grus+"),
      eng = c("part_size_clay", "part_size_silt", "part_size_sand", "gravel"))

    for (class_name in texture_classes$swed) {

      class_pos <- which(raw_data == class_name, arr.ind = TRUE)

      # Should be more to the right than that graph
      # (with "Mellangrus" at the right side)
      class_pos <- class_pos[which(
        class_pos[, "col"] > which(raw_data == "Mellangrus",
                                   arr.ind = TRUE)[1, "col"]), ]

      assertthat::assert_that(length(class_pos) == 2)

      row_idx <- class_pos["row"]

      texture_values <- bind_rows(
        texture_values,
        data.frame(
          sample_id = as.character(sample_id),
          parameter = texture_classes$eng[which(
            texture_classes$swed == class_name)],
          value = as.numeric(raw_data[row_idx, mengd_col])))
    }

    # Normalise clay, silt and sand

    sum_texture <- texture_values %>%
      filter(parameter %in% c("part_size_clay", "part_size_silt",
                              "part_size_sand")) %>%
      reframe(value = sum(value)) %>%
      pull(value)

    texture_values <- texture_values %>%
      mutate(value = ifelse(
        parameter %in% c("part_size_clay", "part_size_silt", "part_size_sand"),
        .data$value / sum_texture * 100,
        value))


    # 3. Cumulative particle size fractions

    accum_row <-
      which(grepl("Ackumulera", pull(raw_data[, mengd_col]), fixed = FALSE))

    unit_col <- mengd_col - 1

    # Particle size boundaries

    particle_sizes <- c()

    vec_units <- raw_data[seq(accum_row + 1, nrow(raw_data)), unit_col]
    vec_units_num <- !is.na(suppressWarnings(as.numeric(pull(vec_units))))

    ind_potential <- which(vec_units_num &
                             (c(FALSE, head(vec_units_num, -1)) |
                                c(tail(vec_units_num, -1), FALSE)))

    for (k in seq_along(ind_potential)) {

      if (k == 1 || (ind_potential[k - 1] == ind_potential[k] - 1)) {

        particle_sizes <- c(particle_sizes,
                            pull(vec_units)[ind_potential[k]])
      } else {
        stop
      }
    }

    particle_sizes <- as.data.frame(particle_sizes) %>%
      mutate(
        part_size_num = as.numeric(particle_sizes),
        part_size_harm = case_when(
          part_size_num < 80 & part_size_num > 70 ~ 75,
          part_size_num < 60 & part_size_num > 50 ~ 53,
          part_size_num < 40 & part_size_num > 35 ~ 38,
          part_size_num < 27 & part_size_num > 22 ~ 25,
          part_size_num <= 2 & part_size_num > 1 ~ 2,
          TRUE ~ round(part_size_num, 0)),
        parameter = paste0(
          "part_size_cumfrac_",
          # ifelse(abs(part_size_num - round(part_size_num, 0)) >
          #          .Machine$double.eps^0.5,
          #        case_when(
          #          abs(part_size_num - round(part_size_num, 0)) < .05 ~
          #            paste0(as.character(round(part_size_num, 1)), ".0"),
          #          TRUE ~ as.character(round(part_size_num, 1))),
          #        as.character(round(part_size_num, 0)))
          round(part_size_harm, 0)
          ))


    for (part_size_name in particle_sizes$particle_sizes) {

      class_pos <- which(raw_data == part_size_name, arr.ind = TRUE)

      # Should be more to the right than that graph
      # (with "Mellangrus" at the right side)
      class_pos <- class_pos[which(
        class_pos[, "col"] > which(raw_data == "Mellangrus",
                                   arr.ind = TRUE)[1, "col"]), ]

      assertthat::assert_that(length(class_pos) == 2)

      row_idx <- class_pos["row"]

      texture_values <- bind_rows(
        texture_values,
        data.frame(
          sample_id = as.character(sample_id),
          parameter = particle_sizes$parameter[which(
            particle_sizes$particle_sizes == part_size_name)],
          value = as.numeric(raw_data[row_idx, mengd_col])))
    }


  return(texture_values)

}




# Compile the data from the folders

swethro_so_long <- NULL

# Set up a progress bar to track processing

if (!isTRUE(getOption("knitr.in.progress"))) {
  progress_bar <-
    txtProgressBar(min = 0,
                   max = length(plots_swethro),
                   style = 3)
}


for (i in seq_along(plots_swethro)) {

  path_i <- paste0(path_swethro, plots_swethro[i], "/")

  # xlsm files

  files_i <- list.files(path_i, pattern = "\\.xlsm$", full.names = TRUE)

  if (!identical(files_i, character(0))) {

       for (j in seq_along(files_i)) {

      #raw_data <- read_excel(files_i[j])

      swethro_so_long <- bind_rows(
        swethro_so_long,
        process_texture_files(suppressMessages(read_excel(files_i[j]))))

    } # End of loop along samples (Excel files)

  } else {

    # xlsx files for M13 and L07 (compiled by FSCC)

    data_i <- read_excel(list.files(path_i, pattern = "\\.xlsx$",
                                    full.names = TRUE)) %>%
      mutate(
        part_size_num = 1E3 * as.numeric(gsub(",", ".", part_size)),
        part_size_harm = case_when(
          part_size_num < 80 & part_size_num > 70 ~ 75,
          part_size_num < 60 & part_size_num > 50 ~ 53,
          part_size_num < 40 & part_size_num > 35 ~ 38,
          part_size_num < 27 & part_size_num > 22 ~ 25,
          part_size_num <= 2 & part_size_num > 1 ~ 2,
          TRUE ~ round(part_size_num, 0)),
        parameter = paste0(
          "part_size_cumfrac_",
          part_size_harm))

    samples_i <- unique(data_i$sample_id)

    for (j in seq_along(samples_i)) {

      data_j <- data_i %>%
        filter(sample_id == samples_i[j]) %>%
        mutate(
          value = as.numeric(gsub(",", ".", value))) %>%
        arrange(desc(part_size_num)) %>%  # ensure descending order of size
        mutate(fraction = value - lead(value, default = 0)) %>%
        arrange(part_size_num)

      # Calculate sum of fractions < 2000 µm (fine earth)
      # and > 2000 µm (gravel)

      cols_tex <- data_j %>%
        filter(part_size_num <= 2000) %>%
        pull(parameter)

      cols_gravel <- data_j %>%
        filter(part_size_num > 2000) %>%
        pull(parameter)

      table_fractions <- data_j %>%
        select(parameter, fraction) %>%
        pivot_wider(names_from = parameter, values_from = fraction)

      sum_texture_j <- table_fractions %>%
        select(any_of(cols_tex)) %>%
        rowSums

      table_fractions <- table_fractions %>%
        mutate_at((cols_tex),
                  ~ ifelse(
                    !is.na(.),
                    (.) / sum_texture_j * 100,
                    NA))

      limits_j <- c(
        0,
        data_j %>%
          filter(part_size_num <= 2000) %>%
          pull(part_size_num))

      fractions_j <-
        TT.text.transf.X(tri.data = table_fractions %>%
                           # Ascending order!
                           select(any_of(cols_tex)),
                         # Original limits
                         dat.ps.lim = limits_j,
                         # Target limits
                         base.ps.lim = c(0, 2, 63, 2000)) %>%
        rename(part_size_clay = C1,
               part_size_silt = C2,
               part_size_sand = C3) %>%
        pivot_longer(
          cols = everything(),
          names_to = "parameter",
          values_to = "value") %>%
        mutate(
          sample_id = as.character(samples_i[j])) %>%
        relocate(sample_id, .before = "parameter")


      # Add to the long dataframe

      swethro_so_long <- bind_rows(
        swethro_so_long,
        fractions_j,
        # Gravel
        data.frame(
          sample_id = as.character(samples_i[j]),
          parameter = "gravel",
          value = table_fractions %>%
            select(any_of(cols_gravel)) %>%
            rowSums),
        data_j %>%
          arrange(desc(part_size_num)) %>%
          mutate(sample_id = as.character(sample_id)) %>%
          select(sample_id, parameter, value))

    } # End of loop along samples

  } # End of xlsx files M13 and L07


  # Update the progress bar

  if (!isTRUE(getOption("knitr.in.progress"))) {
    setTxtProgressBar(progress_bar, i)
  }

} # End of loop along plots (folders)

if (!isTRUE(getOption("knitr.in.progress"))) {
  close(progress_bar)
}


swethro_so <- swethro_so_long %>%
  pivot_wider(
    names_from = parameter,
    values_from = value) %>%
  mutate(
    sample_id_long = sample_id,
    sample_id = sub("^([0-9]+).*", "\\1", sample_id_long)) %>%
  relocate(sample_id_long, .after = sample_id)





# Add the correct metadata (depths, ICP Forests plot codes)

# These depths were first compiled in Excel based on the hand-written
# layer thicknesses of the field protocols inside each folder per plot

swethro_depths <- read_excel(paste0("./data/additional_data/partner_comm/",
                                    "texture_data_collection_dec2024/",
                                    "sweden_swethro_depths.xlsx")) %>%
  # Removes layer BC as instructed by our national contact person
  filter(is.na(layer_thickness) | layer_thickness != "0") %>%
  mutate(sample_id = as.character(`Sample ID`)) %>%
  rename(swethro_site_code = `SWETHRO Site code`,
         swethro_site_name = `SWEHRO Site name`,
         code_layer = Layer) %>%
  select(sample_id, swethro_site_code, swethro_site_name, code_layer,
         layer_limit_superior, layer_limit_inferior)

icp_plots <- read_excel(paste0(path_swethro,
                               "SWETHRO_sites_to_ICP.xlsx")) %>%
  rename(swethro_site_name = `SWEHRO Site name`) %>%
  mutate(swethro_site_code = str_replace_all(`SWETHRO Site code`,
                                     "^([A-Z]+)\\s*([0-9]+)\\s*[A-Z]*$",
                                     "\\1\\2")) %>%
  mutate(swethro_site_code = ifelse(
    swethro_site_code == "H03", "H03B", swethro_site_code)) %>%
  select(code_country, partner_code, code_plot, swethro_site_code) %>%
  mutate(survey_year = ifelse(
    swethro_site_code %in% c("M13", "L07"), 2010, 2020))

swethro_depths <- swethro_depths %>%
  left_join(icp_plots,
            by = "swethro_site_code") %>%
  mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
  relocate(code_country, partner_code, code_plot, plot_id,
           .before = "swethro_site_code") %>%
  relocate(sample_id, survey_year, .after = "swethro_site_name")


swethro_so <- swethro_depths %>%
  full_join(swethro_so,
            by = "sample_id") %>%
  select(-sample_id_long) %>%
  group_by(plot_id) %>%
  arrange(layer_limit_superior, .by_group = TRUE) %>%
  mutate(layer_number = row_number()) %>%
  ungroup() %>%
  relocate(layer_number, .after = "code_layer") %>%
  select(-part_size_cumfrac_63000, -part_size_cumfrac_500,
         -part_size_cumfrac_200)

write.table(swethro_so,
            file = paste0("./data/additional_data/partner_comm/",
                          "texture_data_collection_dec2024/",
                          "swethro_texture_compiled.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")



# Add bulk density and coarse fragments in order to be able to take
# the soil mass into account, when converting the data to the correct depths

swethro_so2 <- swethro_so %>%
  select(-starts_with("part_size_cumfrac_")) %>%
  mutate(layer_type = "mineral") %>%
  depth_join(
    df2 = get_env("so_som") %>%
      filter(plot_id %in% unique(swethro_so$plot_id)) %>%
      select(any_of(columns_key),
             bulk_density),
    mode = "constant_physical_parameters") %>%
  mutate(coarse_fragment_vol = gravel) %>%
  select(-gravel)

# Convert to the correct depths


df_swethro <- get_env("so_som") %>%
  filter(plot_id %in% unique(swethro_so$plot_id)) %>%
  select(any_of(columns_key)) %>%
  depth_join(df2 = swethro_so2,
             mode = "constant_physical_parameters") %>%
  mutate(
    part_size_clay = round(part_size_clay, 1),
    part_size_silt = round(part_size_silt, 1),
    part_size_sand = round(part_size_sand, 1)) %>%
  select(-bulk_density)

write.table(df_swethro,
            file = paste0("./data/additional_data/partner_comm/",
                          "texture_data_collection_dec2024/",
                          "so_som_swethro_texture.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")











# Sweden FA ----

# (BD no, stones no, texture class yes)

data_fa <- read_excel(paste0("./data/additional_data/partner_comm/",
                             "texture_data_collection_dec2024/original/",
                             "sweden_swethro_texture/",
                             "texture_observation_plots.xlsx")) %>%
  rename(code_plot = Ytnr) %>%
  mutate(plot_id = paste0("13_", code_plot)) %>%
  filter(!is.na(Textf)) %>%
  # The Swedish texture classes do not entirely correspond with the FAO classes
  # but they are a better approximation than "no data".
  # As such, we assume the following links with FAO texture classes:
  # - 1: grus (gravel) → FAO "Coarse"
  # - 2: Grovsand (Coarse sand) → FAO "Coarse"
  # - 3: Mellansand (Medium sand) → FAO "Medium"
  # - 4: Grovmo/finmo/mjäla (Coarse silt/fine silt/very fine silt)
  #      → FAO Medium fine
  # - 5: Ler (Clay) → FAO "Fine"
  # (no class corresponds with FAO "Very fine"
  # since there can just be one class)
  mutate(
    code_texture_class = case_when(
      Textf %in% c(1, 2) ~ "Coarse",
      Textf == 3 ~ "Medium",
      Textf == 4 ~ "Medium Fine",
      Textf == 5 ~ "Fine")) %>%
  select(plot_id, code_texture_class)

df_sweden_fa <- get_env("so_som") %>%
  filter(plot_id %in% data_fa$plot_id) %>%
  select(any_of(columns_key)) %>%
  left_join(data_fa,
            by = "plot_id")







# Hungary ----

# (BD no, stones no, texture class no)

# The Hungarian texture file contains the following data:
# - plot 1, 3, 10, 19: to be inserted as extra data
#   (in function gapfill_from_old_data())
#   (2007)
# - plot 1, 2, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17:
#   based on other particle size boundaries, which therefore have to be
#   converted
#   (1997)
# - plot 15, 16, 17, 18, 20, 21: already with correct boundaries
#   (2011), already in "so_som", so no need to do anything.

# Data from 1997 for which limits should be converted

hung_so <- suppressMessages(
  read_excel(paste0("./data/additional_data/partner_comm/",
                    "texture_data_collection_dec2024/original/",
                    "51_SOM_all.xlsx"),
             range = "A31:Q115")) %>%
  rename(
    code_country = `...1`,
    code_plot = `...2`,
    code_layer = `...3`,
    repetition = `...4`,
    layer_limit_superior = `...5`,
    layer_limit_inferior = `...6`) %>%
  select(-`...7`, -`...8`, -`...9`) %>%
  filter(!is.na(`<0,002`)) %>%
  mutate(
    part_size_clay = NA,
    part_size_silt = NA,
    part_size_sand = NA) %>%
  mutate(plot_id = paste0(code_country, "_", code_plot)) %>%
  group_by(plot_id) %>%
  arrange(layer_limit_superior, .by_group = TRUE) %>%
  mutate(layer_number = row_number()) %>%
  ungroup() %>%
  mutate(survey_year = 1997)

# Convert texture boundaries

for (i in seq_len(nrow(hung_so))) {

  fractions_i <-
    TT.text.transf.X(tri.data = hung_so[i, ] %>%
                       # Ascending order!
                       select("<0,002", "0,02-0,002", "0,063-0,02",
                              "0,125-0,063", "0,20- 0,125", "0,63- 0,20",
                              "1,25-0,63", ">1.25"),
                     # Original limits
                     dat.ps.lim = c(0, 2, 20, 63, 125, 200, 630, 1250, 2000),
                     # Target limits (sand fraction should increase)
                     base.ps.lim = c(0, 2, 63, 2000)) %>%
    rename(part_size_clay = C1,
           part_size_silt = C2,
           part_size_sand = C3)

  hung_so$part_size_clay[i] <- round(fractions_i$part_size_clay, 1)
  hung_so$part_size_silt[i] <- round(fractions_i$part_size_silt, 1)
  hung_so$part_size_sand[i] <- round(fractions_i$part_size_sand, 1)
}




# Convert to the correct depths

df_hung <- get_env("so_som") %>%
  filter(plot_id %in% hung_so$plot_id) %>%
  group_by(plot_id) %>%
  filter(
    !((!any(layer_type == "mineral")) |
        (any(!is.na(part_size_clay) &
               !is.na(part_size_silt) &
               !is.na(part_size_sand))))) %>%
  ungroup() %>%
  filter(survey_year < 2000) %>%
  mutate(
    unique_layer = paste0(code_country, "_",
                          code_plot, "_", code_layer)) %>%
  select(any_of(columns_key), unique_layer) %>%
  left_join(
    hung_so %>%
      mutate(
        unique_layer = paste0(code_country, "_",
                              code_plot, "_", code_layer)) %>%
      select(unique_layer, contains("part_size_")),
    by = "unique_layer")








# Luxemburg ----

# (BD yes, stones yes, texture class no)

lux_lab <- suppressMessages(
  read_excel(paste0("./data/additional_data/partner_comm/",
                             "texture_data_collection_dec2024/original/",
                             "NEC Article 9 data reporting 23Oct19 LUX_v3",
                             ".xlsx"),
                      sheet = "Appendix 4", range = "D5:BS65"))[-c(1:4), ] %>%
  mutate(
    latitude_dec = as.numeric(.data$`y...12`),
    longitude_dec = as.numeric(.data$`x...11`)) %>%
  rename(site_name = `Site name`,
         profile_pit_id = `N°of profil or sample`,
         sample_id_long = `sample ID`)

lux_coord <- lux_lab %>%
  filter(!is.na(latitude_dec) & !is.na(longitude_dec)) %>%
  group_by(site_name, profile_pit_id) %>%
  reframe(
    latitude_dec =
      if (n_distinct(latitude_dec) == 1) unique(latitude_dec) else NA_real_,
    longitude_dec =
      if (n_distinct(longitude_dec) == 1) unique(longitude_dec) else
        NA_real_) %>%
  as_sf

distance_join(sf1 = lux_coord,
              sf2 = coordinates_so %>%
                filter(grepl("^12_", plot_id)) %>%
                as_sf,
              join_column = "plot_id") %>%
  st_drop_geometry() %>%
  select(site_name, profile_pit_id, plot_id)


lux_bd <- suppressMessages(
  read_excel(paste0("./data/additional_data/partner_comm/",
                    "texture_data_collection_dec2024/original/",
                    "NEC Article 9 data reporting 23Oct19 LUX_v3",
                    ".xlsx"),
             sheet = "Appendix 4", range = "S69:AA137"))[-c(1:2), ] %>%
  mutate(
    bulk_density = 1E3 * as.numeric(`Average apparente density`)) %>%
  rename(profile_pit_id = `N° of profil and/or sample`,
         sample_id = `sample ID`) %>%
  filter(!is.na(bulk_density)) %>%
  mutate(
    plot_id = case_when(
      profile_pit_id == "EUF 19-01" ~ "12_1",
      profile_pit_id == "EUF 19-06" ~ "12_2")) %>%
  filter(!is.na(plot_id)) %>%
  select(plot_id, profile_pit_id, sample_id, bulk_density) %>%
  mutate(
    # Extract numeric limits
    layer_limit_superior = as.numeric(str_extract(sample_id, "^[0-9]+")),
    layer_limit_inferior =
      as.numeric(str_extract(sample_id, "(?<=-)[0-9]+"))) %>%
  group_by(profile_pit_id) %>%
  arrange(layer_limit_superior, .by_group = TRUE) %>%
  mutate(layer_number = row_number()) %>%
  ungroup() %>%
  mutate(layer_type = "mineral")



lux_so <- lux_lab %>%
  mutate(
    plot_id = case_when(
      site_name == "HET L1" ~ "12_1",
      site_name == "HET L2" ~ "12_2")) %>%
  filter(!is.na(plot_id)) %>%
  mutate(
    # Remove trailing underscores and ensure proper spacing
    sample_id = str_remove(sample_id_long, "_\\d+$"),
    sample_id = str_replace(sample_id, "cm\\s*$", " cm"),
    # Extract numeric limits
    layer_limit_superior = as.numeric(str_extract(sample_id, "^[0-9]+")),
    layer_limit_inferior =
      as.numeric(str_extract(sample_id, "(?<=-)[0-9]+")),
    # Extract repetition number or assign 4 if missing
    repetition = if_else(
      str_detect(sample_id_long, "_\\d+$"),
      as.numeric(str_extract(sample_id_long, "\\d+$")),
      4
    )) %>%
  relocate(repetition, sample_id, layer_limit_superior, layer_limit_inferior,
           .after = "sample_id_long") %>%
  mutate(unique_repetition = paste0(plot_id, "_", repetition)) %>%
  group_by(unique_repetition) %>%
  arrange(layer_limit_superior, .by_group = TRUE) %>%
  mutate(layer_number = row_number()) %>%
  ungroup() %>%
  mutate(layer_type = "mineral") %>%
  mutate(
    clay = as.numeric(`Clay 2000-50micron`),
    silt = as.numeric(`Silt 2000-50micron`),
    sand = as.numeric(`Sand 2000-50micron`),
    cf_mass = as.numeric(`Pebble franction (EG >2mm)`)) %>%
  select(plot_id, site_name, profile_pit_id, repetition, unique_repetition,
         layer_number,
         layer_type, sample_id_long, sample_id,
         layer_limit_superior, layer_limit_inferior,
         clay, silt, sand, cf_mass) %>%
  mutate(code_layer = sample_id) %>%
  depth_join(df2 = lux_bd %>%
               mutate(code_layer = sample_id),
             mode = "constant_physical_parameters") %>%
  group_by(unique_repetition) %>%
  arrange(layer_number) %>%
  fill(bulk_density, .direction = "down") %>%
  ungroup() %>%
  mutate(
    # Convert coarse fragments to volumetric
    coarse_fragment_vol =
      ifelse(!is.na(bulk_density) & !is.na(cf_mass),
             round(
               (.data$cf_mass * .data$bulk_density) /
                 (2650 - .01 * .data$cf_mass * (2650 - .data$bulk_density)),
               3),
             NA),
    part_size_clay = NA,
    part_size_silt = NA,
    part_size_sand = NA)


# Convert texture boundaries


for (i in seq_len(nrow(lux_so))) {

  fractions_i <-
    TT.text.transf.X(tri.data = lux_so[i, ] %>%
                       # Ascending order!
                       select("clay",
                              "silt",
                              "sand"),
                     # Original limits
                     dat.ps.lim = c(0, 2, 50, 2000),
                     # Target limits (sand fraction should increase)
                     base.ps.lim = c(0, 2, 63, 2000)) %>%
    rename(part_size_clay = C1,
           part_size_silt = C2,
           part_size_sand = C3)

  lux_so$part_size_clay[i] <- fractions_i$part_size_clay
  lux_so$part_size_silt[i] <- fractions_i$part_size_silt
  lux_so$part_size_sand[i] <- fractions_i$part_size_sand
}


# Convert to the correct depths

df_luxemburg <- get_env("so_som") %>%
  filter(plot_id %in% unique(lux_so$plot_id)) %>%
  select(any_of(columns_key)) %>%
  depth_join(df2 = lux_so,
             mode = "constant_physical_parameters") %>%
  mutate(
    part_size_clay = round(part_size_clay, 1),
    part_size_silt = round(part_size_silt, 1),
    part_size_sand = round(part_size_sand, 1))






# Germany Brandenburg ----

# (BD yes, stones yes, texture class no)


brandenburg_texture <- read.csv(paste0("./data/additional_data/partner_comm/",
                                       "texture_data_collection_dec2024/",
                                       "original/brandenburg_texture.csv"),
                                skip = 1,
                                sep = ";") %>%
  as_tibble %>%
  mutate(plot_id = paste0("4_", plot)) %>%
  rename(layer_limit_superior = upper,
         layer_limit_inferior = lower,
         coarse_fragment_vol = gravel,
         part_size_clay = clay,
         part_size_silt = silt,
         part_size_sand = sand) %>%
  mutate(bulk_density = 1E3 * BD,
         layer_limit_superior = ifelse(
           layer_limit_superior > layer_limit_inferior,
           (-1) * layer_limit_superior,
           layer_limit_superior),
         layer_type = ifelse(
           layer_limit_superior < 0,
           "forest_floor",
           "mineral")) %>%
  group_by(plot_id) %>%
  arrange(layer_limit_superior, .by_group = TRUE) %>%
  mutate(layer_number = row_number()) %>%
  ungroup() %>%
  mutate(
    code_layer = ifelse(
      layer_type == "mineral",
      paste0("M", layer_number),
      paste0("O", layer_number)))


# No conversion of texture boundaries needed

# Convert to the correct depths

df_brandenburg <- get_env("so_som") %>%
  filter(plot_id %in% unique(brandenburg_texture$plot_id)) %>%
  select(any_of(columns_key)) %>%
  depth_join(df2 = brandenburg_texture,
             mode = "constant_physical_parameters") %>%
  mutate(
    part_size_clay = round(part_size_clay, 1),
    part_size_silt = round(part_size_silt, 1),
    part_size_sand = round(part_size_sand, 1))







# Germany Bavaria ----

# (BD yes, stones yes, texture class no)


plot_codes_bavaria <- read.csv(paste0("./data/additional_data/partner_comm/",
                                      "texture_data_collection_dec2024/",
                                      "original/codes_bavaria.csv"),
                               sep = ";") %>%
  rename(code_plot = si_code) %>%
  select(-si_code_long)

soil_site_bavaria1 <- read.csv(paste0("./data/additional_data/partner_comm/",
                                      "texture_data_collection_dec2024/",
                                      "original/soil_site_bavaria1.csv"),
                               sep = ";") %>%
  left_join(plot_codes_bavaria,
            by = "Plot_ID") %>%
  rename(code_plot_bavaria = "Plot_ID") %>%
  mutate(plot_id = paste0("4_", code_plot)) %>%
  relocate(code_plot, plot_id, .before = Horizont_Nr) %>%
  rename(code_layer = Horizont,
         layer_number = Horizont_Nr,
         layer_limit_superior = von_cm,
         layer_limit_inferior = bis_cm,
         bulk_density = TRD_Feinboden_kgm3) %>%
  mutate(
    layer_type = ifelse(
      layer_limit_superior < 0,
      "forest_floor",
      "mineral"),
    coarse_fragment_vol = 1E2 * Skelett_VAnteil,
    part_size_sand = Sand_mAnteil,
    part_size_silt = Schluff_mAnteil,
    part_size_clay = Ton_mAnteil)


# Convert texture boundaries

# Convert to the correct depths

df_bavaria <- get_env("so_som") %>%
  filter(plot_id %in% unique(soil_site_bavaria1$plot_id)) %>%
  # Filter for plots without texture data
  group_by(plot_id) %>%
  filter(
    !((!any(layer_type == "mineral")) |
        (any(!is.na(part_size_clay) &
               !is.na(part_size_silt) &
               !is.na(part_size_sand))))
  ) %>%
  ungroup() %>%
  select(any_of(columns_key)) %>%
  depth_join(df2 = soil_site_bavaria1,
             mode = "constant_physical_parameters") %>%
  mutate(
    part_size_clay = round(part_size_clay, 1),
    part_size_silt = round(part_size_silt, 1),
    part_size_sand = round(part_size_sand, 1))








# Germany Rheinland-Pfalz ----

# (BD no, stones no, texture class no)


data_rp <- read_excel(paste0("./data/additional_data/partner_comm/",
                             "texture_data_collection_dec2024/original/",
                             "DE_Overlay_soil_texture_deposition_RP.xlsx"),
                      sheet = "texture_7xx_RP",
                      skip = 1) %>%
  rename(
    profile_pit_id = PROFILBEZ,
    code_plot = `plot_ID ICPF`,
    survey_year = Year,
    layer_limit_superior = OT,
    layer_limit_inferior = UT) %>%
  mutate(
    part_size_clay = Clay,
    part_size_silt = rowSums(across(c(FU, MU, GU)), na.rm = TRUE),
    part_size_sand = rowSums(across(c(FS, MS, GS)), na.rm = TRUE)) %>%
  select(
    code_plot, profile_pit_id, survey_year,
    layer_limit_superior, layer_limit_inferior,
    starts_with("part_size")) %>%
  mutate(plot_id = paste0("4_", code_plot)) %>%
  mutate(layer_type = ifelse(
           layer_limit_superior < 0,
           "forest_floor",
           "mineral")) %>%
  group_by(plot_id, profile_pit_id) %>%
  arrange(layer_limit_superior, .by_group = TRUE) %>%
  mutate(layer_number = row_number()) %>%
  ungroup() %>%
  mutate(
    code_layer = ifelse(
      layer_type == "mineral",
      paste0("M", layer_number),
      paste0("O", layer_number)))

# No conversion of texture boundaries needed

# Add bulk density and coarse fragments in order to be able to take
# the soil mass into account, when converting the data to the correct depths

data_rp <- data_rp %>%
  depth_join(
    df2 = get_env("so_som") %>%
      filter(plot_id %in% unique(data_rp$plot_id)) %>%
      group_by(plot_id) %>%
      filter(
        !((!any(layer_type == "mineral")) |
            (any(!is.na(part_size_clay) &
                   !is.na(part_size_silt) &
                   !is.na(part_size_sand))))) %>%
      ungroup() %>%
      select(any_of(columns_key),
             bulk_density, coarse_fragment_vol),
    mode = "constant_physical_parameters")



# Convert to the correct depths

df_rp <- get_env("so_som") %>%
  filter(plot_id %in% unique(data_rp$plot_id)) %>%
  group_by(plot_id) %>%
  filter(
    !((!any(layer_type == "mineral")) |
        (any(!is.na(part_size_clay) &
               !is.na(part_size_silt) &
               !is.na(part_size_sand))))) %>%
  ungroup() %>%
  select(any_of(columns_key)) %>%
  depth_join(df2 = data_rp,
             mode = "constant_physical_parameters") %>%
  mutate(
    part_size_clay = round(part_size_clay, 1),
    part_size_silt = round(part_size_silt, 1),
    part_size_sand = round(part_size_sand, 1)) %>%
  select(-bulk_density, -coarse_fragment_vol)





# Combine ----

so_som_texture <-
  bind_rows(
    # Croatia
    df_croatia,
    # Sweden IM (also bulk density and stones)
    df_sweden,
    # Sweden Swethro
    df_swethro,
    # Sweden FA (only texture class) (some also have textures from df_swethro)
    df_sweden_fa,
    # Hungary
    df_hung,
    # Luxemburg (also bulk density and stones)
    df_luxemburg,
    # Germany Brandenburg (also bulk density and stones)
    df_brandenburg,
    # Germany Bavaria (also bulk density and stones)
    df_bavaria,
    # Germany Rheinland-Pfalz
    df_rp)




write.table(so_som_texture,
            file = paste0("./data/additional_data/partner_comm/",
                          "texture_data_collection_dec2024/",
                          "so_som_texture.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")






tex_avail_so_som <- get_env("so_som") %>%
  group_by(plot_id) %>%
  reframe(any_tex = (!any(layer_type == "mineral") &
                       any(layer_type == "peat")) |
            (any(!is.na(part_size_clay) &
                   !is.na(part_size_silt) &
                   !is.na(part_size_sand))),
          any_tex_class = (!any(layer_type == "mineral") &
                             any(layer_type == "peat")) |
            any(!is.na(code_texture_class))) %>%
  # Add extra texture data
  left_join(
    so_som_texture %>%
      group_by(plot_id) %>%
      reframe(any_tex2 = (!any(layer_type == "mineral") &
                            any(layer_type == "peat")) |
                (any(!is.na(part_size_clay) &
                       !is.na(part_size_silt) &
                       !is.na(part_size_sand))),
              any_tex_class2 = (!any(layer_type == "mineral") &
                                  any(layer_type == "peat")) |
                any(!is.na(code_texture_class))),
    by = "plot_id") %>%
  mutate(
    any_tex_after = case_when(
      any_tex == TRUE |
        (!is.na(any_tex2) & any_tex2 == TRUE) ~ TRUE,
      TRUE ~ FALSE),
    any_tex_class_after = case_when(
      any_tex_class == TRUE |
        (!is.na(any_tex_class2) & any_tex_class2 == TRUE) ~ TRUE,
      TRUE ~ FALSE)) %>%
  select(-any_tex2, -any_tex_class2) %>%
  left_join(get_env("data_availability_so") %>%
              select(country, plot_id, code_country, partner_code, code_plot),
            by = "plot_id") %>%
  relocate(starts_with("any_"), .after = code_plot) %>%
  arrange(code_country, code_plot)

write.table(tex_avail_so_som,
            file = paste0("./data/additional_data/partner_comm/",
                          "texture_data_collection_dec2024/",
                          "so_som_texture_availability.csv"),
            row.names = FALSE,
            na = "",
            sep = ";",
            dec = ".")




length(which(tex_avail_so_som$any_tex == TRUE))
length(which(tex_avail_so_som$any_tex_class == TRUE))

length(which(tex_avail_so_som$any_tex == TRUE |
               tex_avail_so_som$any_tex_class == TRUE))

length(which(tex_avail_so_som$any_tex_after == TRUE))
length(which(tex_avail_so_som$any_tex_class_after == TRUE))

length(which(tex_avail_so_som$any_tex_after == TRUE |
               tex_avail_so_som$any_tex_class_after == TRUE))

# Create a summary tibble
tex_summary <- tibble(
  ` ` = c("Any particle sizes (or peat soil)",
          "Any texture classes (or peat soil)",
          "Any texture data (or peat soil)"),
  before_addition = c(
    sum(tex_avail_so_som$any_tex, na.rm = TRUE),
    sum(tex_avail_so_som$any_tex_class, na.rm = TRUE),
    sum(tex_avail_so_som$any_tex | tex_avail_so_som$any_tex_class,
        na.rm = TRUE)),
  after_addition = c(
    sum(tex_avail_so_som$any_tex_after, na.rm = TRUE),
    sum(tex_avail_so_som$any_tex_class_after, na.rm = TRUE),
    sum(tex_avail_so_som$any_tex_after | tex_avail_so_som$any_tex_class_after,
        na.rm = TRUE)))


