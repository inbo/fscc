
#' List inconsistencies in depth layers
#'
#' This is a function that evaluates the consistency of depth layers in
#' soil monitoring data, such as information about the layer limits and the
#' connection of adjacent layers in the soil profile
#'
#' @param survey_form Character string - Name of the survey form (lower case
#' and separated by '_') to be evaluated
#' @param solve Logical - Indicates whether obvious mistakes can be solved
#' in this function (if TRUE). Default is FALSE.
#' @param save_to_env Logical which indicates whether the output dataframes
#' can be saved to the environment and override any existing objects
#' with the same name. Default is FALSE.
#'
#' @details
#' This function uses:
#' - the function "get_redundant_layers()"
#' which determines which layers of the soil profile are redundant,
#' i.e. can be left out without effect on the depths which are covered.
#' Indeed, within a given soil profile, some depth layers (partly) overlap
#' (i.e. some depths are covered by multiple depth layers).
#' - the function "as_character_summary"
#' which creates a character summary of the elements in a vector
#' For example, it turns a vector c(1, 2, 3, 1000) into "1, 2, 3 and 1000".
#'
#' Outputs - This function generates and returns:
#' - an inconsistency report ("list_layer_inconsistencies")
#' - the original data form, but with harmonised
#'    * layer number (from top to down) for each layer in the soil profile
#'      repetition (excluding redundant layers)
#'    * estimated layer limit (if solve == TRUE)
#' - "list_redundant_layers" (only for "pfh" survey forms)
#'
#' Which kind of inconsistencies are identified in this function?
#' FSCC_48: Is the presence/absence of a forest floor consistent over
#'          different profiles of the given plot?
#' FSCC_2:  Are layer limits of above-ground layers negative and
#'          of below-ground layers positive?
#' FSCC_40: Is the reported "code_layer" known?
#' FSCC_39: Do the reported layer limits correspond with the theoretical
#'          layer limits for the given code_layer?
#' FSCC_8:  Do the layers have known layer limits?
#' FSCC_9:  Do the layer limits equal 0 for multiple depth layers in the
#'          given profile?
#' FSCC_11: Does the superior layer limit equal the inferior layer limit?
#' FSCC_7:  Does none of the depth layers of the given profile have known
#'          layer limits?
#' FSCC_10: Is there a gap or overlap between adjacent layers?
#'
#' How are the survey forms exactly updated in this function
#' (if "solve" == TRUE)?
#' 1. "som" survey forms: Update "layer_limit_superior" and
#'    "layer_limit_inferior":
#'    -	Copy the column "layer_limit_superior" to a new column
#'      "layer_limit_superior_orig" + the column "layer_limit_inferior" to a
#'      new column "layer_limit_inferior_orig"
#'    -	Update the columns "layer_limit_superior" and "layer_limit_inferior":
#'      > Multiply records with wrong signs of above-ground or below-ground
#'        layers (see PIR for rule_id = "FSCC_2") with a factor *(-1)
#'      > Empty layer limits which are clearly a mistake: where layer limits
#'        equal 0 for multiple depth layers within a profile (see PIR for
#'        rule_id = "FSCC_9")
#'      > Empty layer limits which are clearly a mistake: where superior layer
#'        limit equals inferior layer limit (see PIR for rule_id = "FSCC_11")
#'    -	Gap-fill empty layer_limit_superiors and layer_limit_inferiors
#'      > Retrieve the layer limits from the corresponding "pfh" survey forms
#'        if possible (based on an equal "code_layer", so only for forest
#'        floor layers)
#'      > Fill these layer limits based on the theoretical layer limits
#'        ("d_depth_level_soil": add columns with theoretical superior and
#'        inferior layer limits based on the column "description" in
#'        "d_depth_level_soil")
#' 2. "pfh" survey forms: Update "horizon_limit_up" and "horizon_limit_low":
#'    -	Copy the column "horizon_limit_up" to a new column
#'      "horizon_limit_up_orig" + the column "horizon_limit_low" to a new
#'      column "horizon_limit_low_orig"
#'    -	Update the columns "horizon_limit_up" and "horizon_limit_low":
#'      >	Multiply records with wrong signs of above-ground or below-ground
#'        layers (see PIR for rule_id = "FSCC_2") with *(-1)
#' 3. Create a column "layer_number" (in "som" survey forms) or
#'    "layer_number" (in "pfh" survey forms) which represents the rank
#'    of a layer (from top to bottom) within a profile (unique "repetition" or
#'    "profile_pit_id"). "Redundant layers" are not included in this ranking
#'    (i.e. layers which cover the same depths like other layers from the same
#'    profile and which can therefore be "left out" without any effect on the
#'    depths which are covered by the data of the given profile). If there are
#'    multiple options when identifying these redundant layers (e.g. "M01"
#'    versus "M05" + "M51"): consider the least "detailed" layer(s) (i.e.
#'    layer(s) with largest depth range) as redundant (e.g. "M01"). In general,
#'    ranking happens based on the superior/upper and inferior/lower layer
#'    limits.
#'    -	"som" survey forms: Include forest floor without layer limits in the
#'      ranking. How the forest floor layer combinations are ranked (based
#'      on theoretical sequence of forest floor layers, e.g. "OL" on top etc +
#'      analysis of the total organic carbon content of the existing forest
#'      floor combinations), is shown in the tables
#'      "layer_number_two_forest_floor_layers" and
#'      "layer_number_three_forest_floor_layers" in this function.
#'    -	"pfh" survey forms: If there are no redundant layers:
#'      "layer_number" equals "horizon_number" if the ranking of
#'      "horizon_number" equals the ranking of the layer limits (which is not
#'      always the case); else "layer_number" is based on the ranking
#'      of the layer limits.
#'
#' WARNING - This function may not be optimally efficient and may ideally
#' require refactoring for better performance.
#'
#' @examples
#' get_layer_inconsistencies("so_som", solve = TRUE)
#'

get_layer_inconsistencies <- function(survey_form,
                                      data_frame = NULL,
                                      solve = FALSE,
                                      save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  cat(paste0(" \nSolve layer inconsistencies in '", survey_form, "'\n"))

  # Specify date on which 'layer 0' data were downloaded ----
  # from ICP Forests website

  source("./src/functions/get_date_local.R")
  download_date <- get_date_local(path = "./data/raw_data/",
                                  save_to_env = TRUE,
                                  collapsed = TRUE)
  download_date_pir <- as.Date(parsedate::parse_iso_8601(download_date))

  # Monitor how long it takes to run this function

  start_time_r <- Sys.time()

  # Import the inconsistency catalogue ----

  assertthat::assert_that(
    file.exists("./data/additional_data/inconsistency_catalogue.csv"),
    msg = paste0("There is no 'inconsistency catalogue' in the",
                 " '.data/additional_data/' folder."))

  inconsistency_catalogue <-
    read.csv("./data/additional_data/inconsistency_catalogue.csv", sep = ";")




  # Import checked pir ----
  # This is necessary to validate the layer type based on organic_carbon_total

  source("./src/functions/get_pir.R")

  if (!exists("pir_checked") ||
      (exists("pir_checked") &&
       any(!"data.frame" %in% class(pir_checked)) &&
       !"rule_id" %in% names(pir_checked))) {

    pir_checked <- get_pir()

  }

  # To determine the "layer_number" for forest floor layers without layer
  # limits:

  # The two dataframes below
  # (layer_number_two_forest_floor_layers and
  # layer_number_three_forest_floor_layers)
  # were made by checking the following for each "unique_repetition" in
  # "so_som":
  # - Which forest floor layers are reported?
  # - What must have been the sequence (layer_number) of the layers?

  # In case it was not clear which layer must have been on top from
  # "code_layer", this information was derived from the total soil
  # organic carbon content in the assumption that:
  # the more superior the layer, the higher the % soil organic carbon

  layer_number_two_forest_floor_layers <-
    data.frame(layer_number = c(1, 2),
               code_layer_harmonised = c("OL", "OFH"),
               combi_1 = c("O2", "O"),
               combi_1_3204 = c("O", "O2"),
               combi_2 = c("O", "O1"),
               combi_3 = c("O1", "O2"),
               combi_4 = c("OF", "OH"),
               combi_5 = c("OL", "OF"),
               combi_6 = c("OL", "OFH"),
               combi_7 = c("OL", "OH"),
               combi_8 = c("OLF", "OH"),
               combi_9 = c("O", "H"),
               combi_10 = c("HFS", "HS"),
               combi_11 = c("HF", "HFS"),
               combi_12 = c("HF", "HS"),
               combi_13 = c("OH", "H"),
               combi_14 = c("OFH", "HFS"),
               combi_15 = c("OF", "HS"),
               combi_16 = c("OF", "OFH"),
               combi_17 = c("OLF", "HS"),
               combi_18 = c("OLF", "OFH"),
               combi_19 = c("OL", "HFS"),
               combi_20 = c("OL", "HS"),
               combi_21 = c("OL/OF", "OH"),
               combi_22 = c("O", "H/A"),
               combi_23 = c("OF/H", "H"),
               combi_24 = c("OF/OH", "H"),
               combi_25 = c("OF/OH", "H/O"),
               combi_26 = c("OF/OH", "O"),
               combi_27 = c("OF", "H"),
               combi_28 = c("OL/OF", "H"),
               combi_29 = c("OL/OF", "OFH"),
               combi_30 = c("OL/OH", "H"),
               combi_31 = c("OL", "H"),
               combi_32 = c("OL", "OF/H"),
               combi_33 = c("OL", "OF/OH"),
               combi_34 = c("O", "HE"))

  layer_number_three_forest_floor_layers <-
    data.frame(layer_number = c(1, 2, 3),
               combi_1 = c("OL", "OF", "OH"),
               combi_2 = c("O", "O3", "O2"),
               combi_3 = c("HF", "HFS", "HS"),
               combi_4 = c("OF", "HFS", "HS"),
               combi_6 = c("OF", "OFH", "OH"),
               combi_7 = c("OL", "HFS", "HS"),
               combi_8 = c("OL", "OF", "HS"),
               combi_9 = c("OL", "OF", "OF/OH"),
               combi_10 = c("OLF", "OF", "OH"),
               combi_11 = c("OL", "OFH", "H"),
               combi_12 = c("OL", "OF", "H"),
               combi_13 = c("OL", "OF", "OH/A"),
               combi_14 = c("O", "H2", "H1"))



# . ----
# Part 1: processing "som" survey forms (with fixed depths) ----

  # Check if the given survey form is "som"

if (unlist(strsplit(survey_form, "_"))[2] == "som") {

  # Retrieve the survey_form data and the "pfh" data (to estimate depth
  # layer limits) and to cross-check forest floor layers in
  # all survey_forms (FSCC_48)

  if (is.null(data_frame)) {
    df <- get_env(survey_form)
  } else {
    df <- data_frame
  }

  pfh <- get_env(paste0(unlist(strsplit(survey_form, "_"))[1], "_pfh"))



  # Retrieve prf for information about the eff_soil_depth

  if (unlist(strsplit(survey_form, "_"))[1] == "so") {

    assertthat::assert_that(file.exists(paste0("./data/additional_data/",
                                               "SO_PRF_ADDS.xlsx")),
                            msg = paste0("'./data/additional_data/",
                                         "SO_PRF_ADDS.xlsx' ",
                                         "does not exist."))

    # This file was created by Nathalie in 2023

    prf <-
      openxlsx::read.xlsx(paste0("./data/additional_data/",
                                 "SO_PRF_ADDS.xlsx"),
                          sheet = 1) %>%
      rename(plot_id = PLOT_ID) %>%
      mutate(eff_soil_depth = as.numeric(DEPTHSTOCK))

  }


  if (unlist(strsplit(survey_form, "_"))[1] == "s1") {

    prf <- get_env(paste0(unlist(strsplit(survey_form, "_"))[1], "_prf")) %>%
      rowwise() %>%
      mutate(eff_soil_depth = ifelse(is.na(.data$eff_soil_depth) &
                                       any(!is.na(c(.data$rooting_depth,
                                                    .data$rock_depth,
                                                    .data$obstacle_depth))),
                                     # Maximum of these three depths
                                     max(c(.data$rooting_depth,
                                           .data$rock_depth,
                                           .data$obstacle_depth),
                                         na.rm = TRUE),
                                     .data$eff_soil_depth))
  }

  prf_agg <- prf %>%
    group_by(plot_id) %>%
    # Sometimes there are different options,
    # No good way to solve this - we just have to pick one
    summarise(eff_soil_depth = median(eff_soil_depth, na.rm = TRUE))



  # If the input survey form is "so_som" (Level II):
  # retrieve the "som" and "pfh" data from Level I ("s1_som" and "s1_pfh")
  # to cross-check forest floor layers in all survey_forms (FSCC_48)

if (unlist(strsplit(survey_form, "_"))[1] == "so") {

  survey_level <- "Level II"

  } else
if (unlist(strsplit(survey_form, "_"))[1] == "s1") {

  survey_level <- "Level I"

}



  # Which extreme TOC values are confirmed as correct by partners?
  # The layer type of these don't have to be converted.
  # For example, in Wallonia, this is due to the presence of charcoal

  survey_form_input <- survey_form

  pir_toc_confirmed <- pir_checked %>%
    filter(survey_form == survey_form_input) %>%
    filter(parameter == "organic_carbon_total") %>%
    # Outside plausible range
    filter(rule_id == "FSCC_14") %>%
    # "The reported value is extreme but correct"
    filter(code_nfc_action_taken == 1) %>%
    pull(code_line)




  # This script is automatically moving the null line of
  # the profile in accordance with the location/thickness of
  # the forest floor, peat and mineral layers.

  # However, sometimes this causes a mismatch between the
  # null line location in "som" versus "pfh".
  # This can be due to tiny differences in the thickness
  # of the organic layer, differences in the TOC concentration...

  # Therefore, based on manual comparison,
  # it appears best and most logical to make an exception (in
  # moving the null line) for the following profiles:

  s1_som_dont_move <- c(
    # Ireland
    "7_2007_702",
    # Latvia
    "64_2008_739",
    # Poland
    "53_2008_481683", "53_2008_520203", "53_2007_1081445",
    # Slovenia
    "60_2006_83",
    # UK
    "6_2008_75", "6_2008_84", "6_2008_103", "6_2008_111",
    "6_2008_113", "6_2008_145", "6_2008_80030", "6_2008_80033",
    "6_2008_47", "6_2008_49", "6_2008_63", "6_2008_81",
    "6_2008_104", "6_2008_114", "6_2008_146", "6_2008_159",
    "6_2008_160",
    "14_2006_88", "14_1987_89", "14_2006_89", "14_1987_108",
    "14_1987_110", "14_1987_119", "14_2008_119", "14_2006_124",
    "14_2006_126", "14_1987_136", "14_1988_141", "14_2007_141",
    "14_1989_150", "14_2008_150", "14_1989_175", "14_2008_175",
    "14_2007_179", "14_2007_184", "14_2007_212", "14_2008_301",
    "14_2007_315",
    "95_1995_2105",
    "57_1993_2520",
    "59_2022_152",
    "1_2007_1175", "1_1994_1246", "1_2007_1443", "1_2007_1659",
    "1_1994_1678", "1_1994_1744",
    "4_2008_1626", "4_2007_1689", "4_2007_1781", "4_2008_1877",
    "4_1992_8005", "4_1992_8048", "4_1992_8195", "4_2007_8208",
    "4_1992_8239", "4_1990_10025",
    "5_2007_153", "5_2007_961", "5_2007_2051",
    "64_2008_739",
    "56_1992_5", "56_1992_35", "56_1992_82", "56_1992_400",
    "56_1992_474", "56_1992_574", "56_1992_830", "56_1992_959",
    "55_1990_830",
    "53_1995_510", "53_1995_511", "53_2007_1520643",
    "67_2004_418",
    "60_1995_195", "60_2006_574", "60_2006_827", "60_1995_999",
    "60_2006_2574",
    "11_2008_16", "11_2008_38", "11_2009_60", "11_1995_64",
    "11_2009_278", "11_1994_286", "11_2009_286", "11_2009_324",
    "11_1995_584",
    "13_2008_2737", "13_1985_12781", "13_1986_18575", "13_1987_22154",
    "6_2008_62", "6_2008_124", "6_2008_135", "6_2008_158")


  so_som_dont_move <-
    c("14_2006_4", "14_1995_18", "14_2006_18",
      "57_1995_104",
      "15_1996_23",
      "1_2011_36", "1_2010_40", "1_2010_40", "1_2010_40", "1_2010_40",
      "1_2010_40", "1_2007_41", "1_2007_41", "1_2011_92",
      "4_1995_903", "4_1994_905", "4_1994_906", "4_1995_909", "4_1995_911",
      "4_1994_917", "4_1995_1604",
      "5_1995_17", "55_1991_11", "55_1992_14",
      "53_1995_514", "53_1995_610", "53_1995_622", "53_1999_702",
      "60_2004_2",
      "11_1995_51", "11_1995_52",
      "13_2010_7453",
      "50_1998_1", "50_1997_2", "50_1997_2")



  # Specific issues ----

  ## Manually correct layer type based on organic_carbon_total ----

  #  This was manually determined by checking all different profile surveys
  #  within a certain plot_id, while harmonising the depth of the null line

  if (unlist(strsplit(survey_form, "_"))[1] == "s1") {

    # Italian plot 5_52
    # Based on "pfh", the TOC content, and the fact that this is a histosol,
    # these layers should become peat layers.

    layers_to_check <- df %>%
      filter(plot_id == "5_52") %>%
      filter(layer_type == "mineral") %>%
      filter(organic_carbon_total >= 200) %>%
      pull(code_line)

    if (!identical(layers_to_check, character(0))) {

      df <- df %>%
        mutate(across(c(code_layer,
                        unique_survey_layer,
                        unique_layer,
                        unique_layer_repetition),
                      ~ ifelse(code_line %in% layers_to_check,
                               str_replace_all(., "M", "H"),
                               .))) %>%
       mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
                                  "peat",
                                  .data$layer_type))

    }
  }


  if (unlist(strsplit(survey_form, "_"))[1] == "s1") {

    # Irish plot 7_712
    # Based on the TOC content,
    # this layer should become a mineral layer.

    layers_to_check <- df %>%
      filter(plot_id == "7_712") %>%
      filter(layer_type == "peat") %>%
      filter(organic_carbon_total < 200) %>%
      pull(code_line)

    if (!identical(layers_to_check, character(0))) {

      df <- df %>%
        mutate(across(c(code_layer,
                        unique_survey_layer,
                        unique_layer,
                        unique_layer_repetition),
                      ~ ifelse(code_line %in% layers_to_check,
                               str_replace_all(., "H", "M"),
                               .))) %>%
        mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
                                   "mineral",
                                   .data$layer_type))

    }
  }

  if (unlist(strsplit(survey_form, "_"))[1] == "s1") {


  # Layers that should become forest_floor

  layers_to_check <- c(
    # Several Swedish H layers
    df %>%
      filter(code_country == 13) %>%
      filter(layer_type == "peat") %>%
      filter(startsWith(code_layer, "H")) %>%
      filter(!code_layer %in% c("H51", "H05", "H12", "H24", "H48", "H01",
                               "H7383", "H8393")) %>%
      filter(is.na(layer_limit_inferior) |
               layer_limit_inferior <= 0) %>%
      pull(code_line),
    # Other above-ground H layers
    # with known layer limits
    df %>%
      filter(code_country != 13) %>%
      filter(layer_type == "peat") %>%
      filter(startsWith(code_layer, "H")) %>%
      filter(!is.na(layer_limit_superior) &
               layer_limit_superior < 0) %>%
      pull(code_line),
    # Other above-ground H layers
    # without known layer limits
    df %>%
      filter(code_country != 13) %>%
      filter(layer_type == "peat") %>%
      filter(startsWith(code_layer, "H")) %>%
      filter(!code_layer %in% c("H51", "H05", "H12", "H24", "H48", "H01",
                               "H7383", "H8393")) %>%
      filter(is.na(layer_limit_superior)) %>%
      pull(code_line))

  if (!identical(layers_to_check, character(0))) {

    df <- df %>%
      mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
                                 "forest_floor",
                                 .data$layer_type))

  }

  }



  if (unlist(strsplit(survey_form, "_"))[1] == "so") {


    # Layers that should become forest_floor

    layers_to_check <- c(
      # H-layers above-ground or with unknown layer limits
      df %>%
        filter(layer_type == "peat") %>%
        filter(startsWith(code_layer, "H")) %>%
        filter(!code_layer %in% c("H51", "H05", "H12", "H24", "H48", "H01",
                                 "H7383", "H8393")) %>%
        filter(is.na(layer_limit_inferior) |
                 layer_limit_inferior <= 0) %>%
        pull(code_line))

    if (!identical(layers_to_check, character(0))) {

      df <- df %>%
        mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
                                   "forest_floor",
                                   .data$layer_type))

    }
  }



  ## Manually correct switched layer limits ----

  # Irish s1 plot 7_704:
  # Layer limits of OL and OF are switched

  if (unlist(strsplit(survey_form, "_"))[1] == "s1") {

    layer_ol <- df %>%
      filter(unique_survey_repetition == "7_2007_704_1") %>%
      filter(code_layer == "OL")

    layer_of <- df %>%
      filter(unique_survey_repetition == "7_2007_704_1") %>%
      filter(code_layer == "OF")

    if (pull(layer_ol, layer_limit_superior) == -23 &
        pull(layer_ol, layer_limit_inferior) == -5 &
        pull(layer_of, layer_limit_superior) == -28 &
        pull(layer_of, layer_limit_inferior) == -23) {

      df <- df %>%
        mutate(layer_limit_superior =
                 ifelse(code_line == pull(layer_ol, code_line),
                        -28,
                        ifelse(code_line == pull(layer_of, code_line),
                               -23,
                               layer_limit_superior))) %>%
        mutate(layer_limit_inferior =
                 ifelse(code_line == pull(layer_ol, code_line),
                        -23,
                        ifelse(code_line == pull(layer_of, code_line),
                               -5,
                               layer_limit_inferior)))
    }
  }




  # Set up a progress bar to track processing

if (!isTRUE(getOption("knitr.in.progress"))) {
progress_bar <-
  txtProgressBar(min = 0,
                 max = length(unique(df$unique_survey_repetition)),
                 style = 3)
}


  # This dataframe refers to "d_depth_level_soil" in the
  # adds/dictionaries folder of the raw data,
  # to which columns with the theoretical layer limits
  # ("layer_limit_superior" and "layer_limit_inferior") were added for
  # each code_layer

d_depth_level_soil <- read.csv("./data/additional_data/d_depth_level_soil.csv",
                               sep = ";")




  # Initialise variables to store layer numbers and limits

# df$layer_number_superior <- NA # Layer numbers by ranking layer_limit_superior
# df$layer_number_inferior <- NA # Layer numbers by ranking layer_limit_superior
df$layer_number <- NA # Final layer numbers
                      # If there are overlapping layers
                      # between 0 and 10 cm depth
                      # (i.e. M05 & M51 & M01),
                      # layer numbers are not given for the "redundant layer(s)"
                      # (in this case: least detailed redundant layer(s),
                      # i.e. M01)
# df$layer_number_combined_layers <- NA # If there are overlapping layers
#                                       # (e.g. M05 & M51 & M01),
#                                       # layer numbers of the least detailed
#                                       # profile (i.e. with M01)
#                                       # are represented here

  # In the end, only "layer_number" is retained in the saved survey form
  # TO DO: adjust script so that the other three layer number versions are
  # no longer generated?



  # Before estimating layer limits when needed,
  # duplicate the columns with layer limits (to keep the original layer limits)
  # Only the layer limit columns without "_orig" can be altered

if (!"layer_limit_inferior_orig" %in% names(df)) {
df$layer_limit_inferior_orig <- df$layer_limit_inferior
}

if (!"layer_limit_superior_orig" %in% names(df)) {
df$layer_limit_superior_orig <- df$layer_limit_superior
}

  # Same for code_layer, since this is absent for some Latvian plots in so_som

if (!"code_layer_orig" %in% names(df)) {
df$code_layer_orig <- df$code_layer
}

  # The intention is to create a list_layer_inconsistencies of
  # the following format to store inconsistencies found in the survey data:

list_layer_inconsistencies <- data.frame(survey_form = NULL,
                                  partner = NULL,
                                  partner_code = NULL,
                                  country = NULL,
                                  code_country = NULL,
                                  survey_year = NULL,
                                  code_plot = NULL,
                                  plot_id = NULL,
                                  code_layer_horizon_master = NULL,
                                  repetition_profile_pit_id = NULL,
                                  code_line = NULL,
                                  parameter = NULL,
                                  parameter_unit = NULL,
                                  parameter_value = NULL,
                                  inconsistency_reason = NULL,
                                  inconsistency_type = NULL,
                                  rule_id = NULL,
                                  non_duplicated_error_type_per_record = NULL,
                                  change_date = NULL,
                                  download_date = NULL)


# Check if forest floor matches between "som" and "pfh" may be found (manually)
# to allow gap-filling forest floor layer limits

if (survey_form == "s1_som") {

  s1_link_ff <- read.csv("./data/additional_data/s1_link_forest_floors.csv",
                         sep = ";")

  assertthat::assert_that(all(!is.na(s1_link_ff$depth_top)) &&
                            all(!is.na(s1_link_ff$depth_bottom)))

  # No forest floor layer limits can be gap-filled based on other survey form

}


if (survey_form == "so_som") {

  so_link_ff <- read.csv("./data/additional_data/so_link_forest_floors.csv",
                         sep = ";")

  assertthat::assert_that(all(!is.na(so_link_ff$depth_top)) &&
                            all(!is.na(so_link_ff$depth_bottom)))

  # No forest floor layer limits can be gap-filled based on other survey form

}



# The following needs to be asserted because the layer_type of these layers
# needs to be converted to forest_floor in this script (manually checked)

if (unlist(strsplit(survey_form, "_"))[1] == "so") {

  assertthat::assert_that(
    # From those five partners
    all((df %>%
           ungroup() %>%
           filter(layer_type == "peat") %>%
           filter(!code_layer %in% c("H05", "H51", "H12", "H24")) %>%
           filter(is.na(layer_limit_superior) |
                    is.na(layer_limit_inferior)) %>%
           distinct(partner_short) %>%
           pull) %in% c("DE_BW",
                        "DE_BB",
                        "DE_SN",
                        "Poland",
                        "Ireland")) &&
      # H layers
      all((df %>%
             ungroup() %>%
             filter(layer_type == "peat") %>%
             filter(!code_layer %in% c("H05", "H51", "H12", "H24")) %>%
             filter(is.na(layer_limit_superior) |
                      is.na(layer_limit_inferior)) %>%
             distinct(code_layer) %>%
             pull) %in% c("H")),
    msg = paste0("There seem to be peat layers with unknown layer limits ",
                 "that were not manually checked"))

}





# Evaluate soil profiles per unique_survey_repetition ----

for (i in seq_along(unique(df$unique_survey_repetition))) {

# Determine index of layers with the given unique_survey_repetition in df
vec <- which(unique(df$unique_survey_repetition)[i] ==
               df$unique_survey_repetition)

# Determine index of below-ground layers in df
vec_bg <- vec[which(df$layer_type[vec] == "mineral" |
                      df$layer_type[vec] == "peat")]

# Determine index of forest floor layers in df
vec_ff <- vec[which(df$layer_type[vec] == "forest_floor")]

# Special case: Latvian records without code_layer
# if (unique(df$unique_survey_repetition[vec]) == "64_2004_5_1") {
#   if (any(is.na(df$code_layer[vec]))) {
#     vec_ff <- vec[which(is.na(df$code_layer[vec]))]
#   }
# }




# Special case: Latvian records without code_layer
# There are two Latvian records without code_layer, layer_limits,
# and with only one organic_carbon_total value
# Assumption: since the other records of the same
# unique_survey_repetition are M01, M12, M24, M48, and
# organic_layer_weight is reported, we will assume that these are
# forest floor layers.
# Because of the organic_layer_weight information, these records
# are still valuable for C stock calculations.
# Deriving which of the records is on top
# and which is below can only happen based on
# analysis of the organic_layer_weight in so_som profiles
# with at least two forest floor layers. This teaches us that
# it is the most likely that the inferior layer usually has a
# higher organic_layer_weight than the superior layer.
# As such, we will name the code_layer of these records so that
# they will be sorted accordingly.
# This also matches with the values in the columns "code_line" and
# "line_nr".
#
# if (!identical(vec_ff, integer(0))) {
# if (unique(df$unique_survey_repetition[vec_ff]) == "64_2004_5_1") {
#   if (all(is.na(df$code_layer[vec_ff]))) {
#
#     ind_superior <-
#       vec_ff[which(df$organic_layer_weight[vec_ff] ==
#                      min(df$organic_layer_weight[vec_ff]))]
#
#     ind_inferior <-
#       vec_ff[which(df$organic_layer_weight[vec_ff] ==
#                      max(df$organic_layer_weight[vec_ff]))]
#
#     df$code_layer[ind_superior] <- "OL"
#     df$code_layer[ind_inferior] <- "OFH"
#
#     df$unique_survey_layer[vec_ff] <-
#       paste0(df$code_country[vec_ff], "_",
#              df$survey_year[vec_ff], "_",
#              df$code_plot[vec_ff], "_",
#              df$code_layer[vec_ff])
#
#     df$unique_layer_repetition[vec_ff] <-
#       paste0(df$code_country[vec_ff], "_",
#              df$survey_year[vec_ff], "_",
#              df$code_plot[vec_ff], "_",
#              df$code_layer[vec_ff], "_",
#              df$repetition[vec_ff])
#
#     df$unique_layer[vec_ff] <-
#       paste0(df$code_country[vec_ff], "_",
#              df$code_plot[vec_ff], "_",
#              df$code_layer[vec_ff])
#
#     df$layer_type[vec_ff] <- "forest_floor"
#   }
# }
# }





## FSCC_48: Inconsistent presence/absence forest floor layers ----

# This is only tested in "so_som"

if (unlist(strsplit(survey_form, "_"))[2] == "som") {

# Store the "unique_survey_repetition" and "plot_id" of i

unique_survey_repetition_i <- unique(df$unique_survey_repetition)[i]

plot_id_i <- unique(df$plot_id[vec])

# Determine which unique_survey_repetitions exist for the given plot_id
# in "so_som"

survey_repetitions_som <- unique(df$unique_survey_repetition[
  which(df$plot_id == plot_id_i)])

# Perform the check only if it is the first time that this plot_id appears
# (since the 'for' loop is based on unique_survey_repetition - not on plot_id)

if (which(survey_repetitions_som == unique_survey_repetition_i) == 1) {

# Determine indices of records with the given plot_id in the four survey_forms

vec_som <- which(df$plot_id == plot_id_i)
vec_pfh <- which(pfh$plot_id == plot_id_i)
# vec_som_other <- which(som_other$plot_id == plot_id_i)
# vec_pfh_other <- which(pfh_other$plot_id == plot_id_i)

# Determine which unique_survey_repetitions exist for the given plot_id
# in the three other survey_forms

survey_repetitions_pfh <-
  unique(pfh$unique_survey_profile[which(pfh$plot_id == plot_id_i)])
# survey_repetitions_som_other <-
#   unique(som_other$unique_survey_repetition[
#     which(som_other$plot_id == plot_id_i)])
# survey_repetitions_pfh_other <-
#   unique(pfh_other$unique_survey_profile[
#     which(pfh_other$plot_id == plot_id_i)])

# Create a list with layer names of the forest floor layers
# This step is not required to find out the inconsistencies

forest_floor_layers_som <- NULL
forest_floor_layers_pfh <- NULL
# forest_floor_layers_som_other <- NULL
# forest_floor_layers_pfh_other <- NULL

# Create a data frame with metadata of the repetitions that exist for the
# given plot_id

if (survey_level == "Level II") {
  survey_2 <- "so_pfh"
  }
if (survey_level == "Level I") {
  survey_2 <- "s1_pfh"
}

# Create dataframes for each survey_form (named with "_ff_i")
# with different unique_survey_repetitions and necessary meta-information

df_ff_i <- df[vec_som[
  which(!duplicated(df$unique_survey_repetition[vec_som]))],
  which(names(df) %in% c("partner_code", "survey_year",
                         "code_plot", "plot_id",
                         "repetition", "unique_survey_repetition"))]

names(df_ff_i)[which(names(df_ff_i) == "repetition")] <-
  "repetition_profile_pit_id"

df_ff_i$repetition_profile_pit_id <-
  as.character(df_ff_i$repetition_profile_pit_id)


pfh_ff_i <- pfh[vec_pfh[
  which(!duplicated(pfh$unique_survey_profile[vec_pfh]))],
  which(names(pfh) %in% c("partner_code", "survey_year",
                          "code_plot", "plot_id",
                          "profile_pit_id", "unique_survey_profile"))]

names(pfh_ff_i)[which(names(pfh_ff_i) == "profile_pit_id")] <-
  "repetition_profile_pit_id"

names(pfh_ff_i)[which(names(pfh_ff_i) == "unique_survey_profile")] <-
  "unique_survey_repetition"

pfh_ff_i$repetition_profile_pit_id <-
  as.character(pfh_ff_i$repetition_profile_pit_id)



# som_other_ff_i <- som_other[vec_som_other[
#   which(!duplicated(som_other$unique_survey_repetition[vec_som_other]))],
#         which(names(som_other) %in% c("partner_code", "survey_year",
#                                       "code_plot", "plot_id",
#                                       "repetition",
#                                       "unique_survey_repetition"))]
#
# names(som_other_ff_i)[
#   which(names(som_other_ff_i) == "repetition")] <-
#   "repetition_profile_pit_id"
#
# som_other_ff_i$repetition_profile_pit_id <-
#   as.integer(som_other_ff_i$repetition_profile_pit_id)
#
#
#
# pfh_other_ff_i <- pfh_other[vec_pfh_other[
#   which(!duplicated(pfh_other$unique_survey_profile[vec_pfh_other]))],
#           which(names(pfh_other) %in% c("partner_code", "survey_year",
#                                         "code_plot", "plot_id",
#                                         "profile_pit_id",
#                                         "unique_survey_profile"))]
#
# names(pfh_other_ff_i)[
#   which(names(pfh_other_ff_i) == "profile_pit_id")] <-
#   "repetition_profile_pit_id"
#
# names(pfh_other_ff_i)[
#   which(names(pfh_other_ff_i) == "unique_survey_profile")] <-
#   "unique_survey_repetition"
#
# pfh_other_ff_i$repetition_profile_pit_id <-
#   as.integer(pfh_other_ff_i$repetition_profile_pit_id)


# Merge these four dataframes to one data_frame "list_ff_meta_i"

list_ff_meta_i <-
   cbind(data.frame("survey_form" =
                    c(rep(survey_form, length(survey_repetitions_som)),
                      rep(survey_2, length(survey_repetitions_pfh))),
                 # rep(survey_3, length(survey_repetitions_som_other)),
                 # rep(survey_4, length(survey_repetitions_pfh_other))),
                   "survey_form_script" =
                     c(rep("df", length(survey_repetitions_som)),
                       rep("pfh", length(survey_repetitions_pfh)))),
                 # rep("som_other", length(survey_repetitions_som_other)),
                 # rep("pfh_other", length(survey_repetitions_pfh_other)))),
        rbind(df_ff_i, pfh_ff_i), # som_other_ff_i, pfh_other_ff_i),
        data.frame("number_ff_layers" =
                    rep(NA, (length(survey_repetitions_som) +
                             length(survey_repetitions_pfh)))))
                             # length(survey_repetitions_som_other) +
                             # length(survey_repetitions_pfh_other)))))


# For each of these unique_survey_repetitions:
# Store in "list_ff_meta_i" how many forest floor layers exist
# in the given unique_survey_repetition

for (j in 1:(length(survey_repetitions_som) +
             length(survey_repetitions_pfh))) {
             # length(survey_repetitions_som_other) +
             # length(survey_repetitions_pfh_other))) {

  # som

  if (j <= length(survey_repetitions_som)) {

    vec_som_j <- which(df$unique_survey_repetition == survey_repetitions_som[j])

    forest_floor_j <- as.character(df$code_layer[vec_som_j[
                which(df$layer_type[vec_som_j] == "forest_floor")]])

    list_ff_meta_i$number_ff_layers[j] <- length(forest_floor_j)

    forest_floor_layers_som <- append(forest_floor_layers_som,
                                      list(forest_floor_j))

    names(forest_floor_layers_som)[j] <-
      as.character(survey_repetitions_som[j])

  } else

  # pfh

  if ((length(survey_repetitions_pfh) > 0) &&
      (j > length(survey_repetitions_som)) &&
      (j <= length(c(survey_repetitions_som, survey_repetitions_pfh)))) {

    j_pfh <- (j - length(survey_repetitions_som))

    vec_pfh_j <- which(pfh$unique_survey_profile ==
                         survey_repetitions_pfh[j_pfh])

    forest_floor_j <- as.character(pfh$horizon_master[vec_pfh_j[
                which(pfh$layer_type[vec_pfh_j] == "forest_floor")]])

    list_ff_meta_i$number_ff_layers[j] <- length(forest_floor_j)

    forest_floor_layers_pfh <- append(forest_floor_layers_pfh,
                                      list(forest_floor_j))

    names(forest_floor_layers_pfh)[j_pfh] <-
      as.character(survey_repetitions_pfh[j_pfh])

    } # else

    # # som_other
    #
    # if ((length(survey_repetitions_som_other) > 0) &&
    #     (j > (length(survey_repetitions_som) +
    #           length(survey_repetitions_pfh))) &&
    #     (j <= length(c(survey_repetitions_som, survey_repetitions_pfh,
    #                    survey_repetitions_som_other)))) {
    #
    #   j_som_other <- (j - (length(survey_repetitions_som) +
    #                          length(survey_repetitions_pfh)))
    #
    #   vec_som_other_j <- which(som_other$unique_survey_repetition ==
    #                              survey_repetitions_som_other[j_som_other])
    #
    #   forest_floor_j <- as.character(som_other$code_layer[vec_som_other_j[
    #               which(som_other$layer_type[vec_som_other_j] ==
    #                       "forest_floor")]])
    #
    #   list_ff_meta_i$number_ff_layers[j] <- length(forest_floor_j)
    #
    #   forest_floor_layers_som_other <- append(forest_floor_layers_som_other,
    #                                     list(forest_floor_j))
    #
    #   names(forest_floor_layers_som_other)[j_som_other] <-
    #             as.character(survey_repetitions_som_other[j_som_other])
    #
    # } else
    #
    # # pfh_other
    #
    # if ((length(survey_repetitions_pfh_other) > 0) &&
    #     (j > (length(survey_repetitions_som) + length(survey_repetitions_pfh) +
    #           length(survey_repetitions_som_other)))) {
    #
    #   j_pfh_other <- (j - (length(survey_repetitions_som) +
    #                          length(survey_repetitions_pfh) +
    #                          length(survey_repetitions_som_other)))
    #
    #   vec_pfh_other_j <- which(pfh_other$unique_survey_profile ==
    #                              survey_repetitions_pfh_other[j_pfh_other])
    #
    #   forest_floor_j <- as.character(pfh_other$horizon_master[vec_pfh_other_j[
    #               which(pfh_other$layer_type[vec_pfh_other_j] ==
    #                       "forest_floor")]])
    #
    #   list_ff_meta_i$number_ff_layers[j] <- length(forest_floor_j)
    #
    #   forest_floor_layers_pfh_other <- append(forest_floor_layers_pfh_other,
    #                                     list(forest_floor_j))
    #
    #   names(forest_floor_layers_pfh_other)[j_pfh_other] <-
    #             as.character(survey_repetitions_pfh_other[j_pfh_other])
    #
    # }
  }


# Generate inconsistencies if forest floor layers are sometimes present and
# sometimes absent in the given plot_id

 # If at least 1 survey repetition does not have forest floor layers
 if ((length(which(list_ff_meta_i$number_ff_layers == 0)) > 0) &&

      # If at least 1 survey repetition does have forest floor layers
     (length(which(list_ff_meta_i$number_ff_layers > 0)) > 0)) {


   # This function creates a character summary of the elements in a vector
   # For example, it turns a vector c(1, 2, 3, 1000) into "1, 2, 3 and 1000".

   source("./src/functions/as_character_summary.R")

# FSCC_48: Caution: for the given plot_id, forest floor layers over different
   # profile pit ID's, repetitions, survey years and survey_forms are
   # not consistently reported: forest floor layers are not reported
   # (i.e. absent) for repetition/profile_pit_id = xxx

   # Retrieve information about the inconsistency from
   # "inconsistency_catalogue"

   rule_id <- "FSCC_48"
   inconsistency_reason <-
     inconsistency_catalogue$inconsistency_reason[
       which(inconsistency_catalogue$rule_id == rule_id)]
   inconsistency_type <-
     inconsistency_catalogue$inconsistency_type[
       which(inconsistency_catalogue$rule_id == rule_id)]

   # Change "inconsistency_reason" with specific information
   # (Name "inconsistency_reason" "caution_ff" for now)

   caution_ff <- inconsistency_reason
   caution_ff <- paste0(caution_ff,
                        ": forest floor layers are absent for ")

   # Add a column to "list_ff_meta_i" with a unique value per
   # survey_year per survey_form

   list_ff_meta_i$survey_form_year <-
     paste0(list_ff_meta_i$survey_form, "_", list_ff_meta_i$survey_year)

   # Determine index in "list_ff_meta_i" of unique_survey_repetitions without
   # forest floor layers

   vec_ff_absent <- which(list_ff_meta_i$number_ff_layers == 0)

   # Create a vector "vec_ff_absent_inconsistent" which tells,
   # using the "survey_form_year" column, whether there are repetitions
   # with forest floor layers + without
   # forest floor layers within the given survey year of the given survey form
   # (i.e. whether the presence of forest floor layers is inconsistent within
   # the given survey year of the given survey form)

   vec_ff_absent_inconsistent <- rep(NA, length(vec_ff_absent))

   for (j in seq_along(vec_ff_absent)) {

   vec_ff_present_inconsistent <-
     which(((list_ff_meta_i$survey_form_year ==
               list_ff_meta_i$survey_form_year[vec_ff_absent[j]]) &
              (list_ff_meta_i$number_ff_layers > 0)))


   if (!identical(vec_ff_present_inconsistent, integer(0))) {
   vec_ff_absent_inconsistent[j] <- TRUE
   }
   }

   # Determine indices in "list_ff_meta_i" of unique_survey_repetitions without
   # forest floor layers for which any other unique_survey_repetitions within
   # the same survey year and survey form DO contain forest floor layers

   vec_ff_inconsistent <-
     vec_ff_absent[which(vec_ff_absent_inconsistent == TRUE)]
   surveys_inconsistent <-
     unique(list_ff_meta_i$survey_form_year[vec_ff_inconsistent])

   # Determine indices in "list_ff_meta_i" of unique_survey_repetitions without
   # forest floor layers for which all other unique_survey_repetitions within
   # the same survey year and survey form also DO NOT contain forest floor
   # layers

   vec_ff_consistent <-
     vec_ff_absent[which(is.na(vec_ff_absent_inconsistent))]
   surveys_consistent <-
     unique(list_ff_meta_i$survey_form_year[vec_ff_consistent])

   # For the "inconsistently" absent forest floors
   # (i.e. where repetition/profile needs to be reported):
   # update "inconsistency_reason" ("caution_ff") with information about
   # unique_survey_repetitions without forest floors

   for (j in seq_along(surveys_inconsistent)) {

     vec_ff_inconsistent_j <-
       vec_ff_inconsistent[
         which(list_ff_meta_i$survey_form_year[vec_ff_inconsistent] ==
                                   surveys_inconsistent[j])]

     vec_ff_inconsistent_j_character <-
       as_character_summary(vec_ff_inconsistent_j)
     survey_year_j <- unique(list_ff_meta_i$survey_year[
       which(list_ff_meta_i$survey_form_year == surveys_inconsistent[j])])
     survey_form_j <- unique(list_ff_meta_i$survey_form[
       which(list_ff_meta_i$survey_form_year == surveys_inconsistent[j])])

     caution_ff <- paste0(caution_ff,
                          "repetition/profile_pit_id ",
                          vec_ff_inconsistent_j_character,
                          " in survey_year ",
                          survey_year_j,
                          " (",
                          survey_form_j,
                          ")")

     if (j < length(surveys_inconsistent)) {
       caution_ff <- paste0(caution_ff, ", ")
     }

     if (j == length(surveys_inconsistent) - 1) {
       caution_ff <- paste0(caution_ff, "and ")
     }
     }


   # For the "consistently" absent forest floors
   # (i.e. where repetition/profile does not need to be reported):
   # update "inconsistency_reason" ("caution_ff") with information about
   # unique_survey_repetitions without forest floors

   for (j in seq_along((surveys_consistent))) {

     survey_year_j <- unique(list_ff_meta_i$survey_year[
       which(list_ff_meta_i$survey_form_year == surveys_consistent[j])])

     survey_form_j <- unique(list_ff_meta_i$survey_form[
       which(list_ff_meta_i$survey_form_year == surveys_consistent[j])])

     if (!identical(vec_ff_inconsistent, integer(0)) && (j == 1)) {

       caution_ff <- paste0(caution_ff, ", and for ")
       }

     if (j == 1) {
       caution_ff <- paste0(caution_ff, "survey year ")
       }

     caution_ff <- paste0(caution_ff,
                          survey_year_j,
                          " (",
                          survey_form_j,
                          ")")

     if (j < length(surveys_consistent) - 1) {
       caution_ff <- paste0(caution_ff, ", ")
     }

     if (j == length(surveys_consistent) - 1) {
       caution_ff <- paste0(caution_ff, " and ")
     }
     }


   # Where have forest floor layers been reported?
   # update "inconsistency_reason" ("caution_ff") with information about
   # unique_survey_repetitions with forest floors

   vec_ff_present <- which(list_ff_meta_i$number_ff_layers > 0)

   caution_ff <- paste0(caution_ff,
                        ". Meanwhile, forest floor layers have been reported",
                        " for the records listed in PIR, i.e. in profiles",
                        " from survey year ")

   if (length(unique(list_ff_meta_i$survey_form[vec_ff_present])) == 1) {

     caution_ff <- paste0(caution_ff,
                          as_character_summary(unique(
                            list_ff_meta_i$survey_year[vec_ff_present])),
                          " (",
                          unique(list_ff_meta_i$survey_form[vec_ff_present]),
                          ").")
     }

   if (length(unique(list_ff_meta_i$survey_form[vec_ff_present])) > 1) {

     survey_year_present <- paste0(list_ff_meta_i$survey_year[vec_ff_present],
                                   " (",
                                   list_ff_meta_i$survey_form[vec_ff_present],
                                   ")")

     caution_ff <- paste0(caution_ff,
                          as_character_summary(unique(survey_year_present)),
                          ".")

     }


   # Create a vector with row indices of forest floor layers
   # (to add to inconsistency list)

   vec_ff_som <- vec_som[which(df$layer_type[vec_som] == "forest_floor")]
   vec_ff_pfh <- vec_pfh[which(pfh$layer_type[vec_pfh] == "forest_floor")]
   # vec_ff_som_other <-
   #   vec_som_other[which(som_other$layer_type[vec_som_other] == "forest_floor")]
   # vec_ff_pfh_other <-
   #   vec_pfh_other[which(pfh_other$layer_type[vec_pfh_other] == "forest_floor")]

   # Store information about the inconsistency in "list_layer_inconsistencies"

   inconsistency_reason <- caution_ff

   # Report original code_layer if code_layer may have been changed
   # (possibly in "s1_som" due to FSCC_47)

   # if ("code_layer_orig" %in% names(som_other)) {
   #     vec_code_layer <-
   #       as.character(som_other$code_layer_orig[vec_ff_som_other])
   #   } else {
   #     vec_code_layer <- as.character(som_other$code_layer[vec_ff_som_other])
   #   }

   # "so_som"

   if (!identical(vec_ff_som, integer(0))) {

   list_layer_inconsistencies <- rbind(
     list_layer_inconsistencies,
     data.frame(survey_form = (rep(survey_form, length(vec_ff_som))),
                partner = df$partner[vec_ff_som],
                partner_code = df$partner_code[vec_ff_som],
                country = df$country[vec_ff_som],
                code_country = df$code_country[vec_ff_som],
                survey_year = df$survey_year[vec_ff_som],
                code_plot = df$code_plot[vec_ff_som],
                plot_id = df$plot_id[vec_ff_som],
                code_layer_horizon_master = df$code_layer[vec_ff_som],
                repetition_profile_pit_id = df$repetition[vec_ff_som],
                code_line = df$code_line[vec_ff_som],
                parameter = (rep("code_layer", length(vec_ff_som))),
                parameter_unit = (rep(NA, length(vec_ff_som))),
                parameter_value = as.character(df$code_layer[vec_ff_som]),
                inconsistency_reason = inconsistency_reason,
                inconsistency_type = inconsistency_type,
                rule_id = rule_id,
                non_duplicated_error_type_per_record =
                  rep(FALSE, length(vec_ff_som)),
                change_date = df$change_date[vec_ff_som],
                download_date = rep(download_date_pir, length(vec_ff_som))))
   }

   # "so_pfh"

   if (!identical(vec_ff_pfh, integer(0))) {

   list_layer_inconsistencies <- rbind(
     list_layer_inconsistencies,
     data.frame(survey_form = (rep(survey_2, length(vec_ff_pfh))),
                partner = pfh$partner[vec_ff_pfh],
                partner_code = pfh$partner_code[vec_ff_pfh],
                country = pfh$country[vec_ff_pfh],
                code_country = pfh$code_country[vec_ff_pfh],
                survey_year = pfh$survey_year[vec_ff_pfh],
                code_plot = pfh$code_plot[vec_ff_pfh],
                plot_id = pfh$plot_id[vec_ff_pfh],
                code_layer_horizon_master = pfh$horizon_master[vec_ff_pfh],
                repetition_profile_pit_id = pfh$profile_pit_id[vec_ff_pfh],
                code_line = pfh$code_line[vec_ff_pfh],
                parameter = (rep("horizon_master", length(vec_ff_pfh))),
                parameter_unit = (rep(NA, length(vec_ff_pfh))),
                parameter_value = as.character(pfh$horizon_master[vec_ff_pfh]),
                inconsistency_reason = inconsistency_reason,
                inconsistency_type = inconsistency_type,
                rule_id = rule_id,
                non_duplicated_error_type_per_record =
                  rep(FALSE, length(vec_ff_pfh)),
                change_date = pfh$change_date[vec_ff_pfh],
                download_date = rep(download_date_pir, length(vec_ff_pfh))))
   }

   # "s1_som"

   # if (!identical(vec_ff_som_other, integer(0))) {
   # list_layer_inconsistencies <- rbind(
   #   list_layer_inconsistencies,
   #   data.frame(survey_form = (rep(survey_3, length(vec_ff_som_other))),
   #              partner = som_other$partner[vec_ff_som_other],
   #              partner_code = som_other$partner_code[vec_ff_som_other],
   #              country = som_other$country[vec_ff_som_other],
   #              code_country = som_other$code_country[vec_ff_som_other],
   #              survey_year = som_other$survey_year[vec_ff_som_other],
   #              code_plot = som_other$code_plot[vec_ff_som_other],
   #              plot_id = som_other$plot_id[vec_ff_som_other],
   #              code_layer_horizon_master = vec_code_layer,
   #              repetition_profile_pit_id =
   #                som_other$repetition[vec_ff_som_other],
   #              code_line = som_other$code_line[vec_ff_som_other],
   #              parameter = (rep("code_layer", length(vec_ff_som_other))),
   #              parameter_unit = (rep(NA, length(vec_ff_som_other))),
   #              parameter_value =  vec_code_layer,
   #              inconsistency_reason = inconsistency_reason,
   #              inconsistency_type = inconsistency_type,
   #              rule_id = rule_id,
   #              non_duplicated_error_type_per_record =
   #                rep(FALSE, length(vec_ff_som_other)),
   #              change_date = som_other$change_date[vec_ff_som_other],
   #              download_date = rep(download_date_pir,
   #                                  length(vec_ff_som_other))))
   # }
   #
   # # "s1_pfh"
   #
   # if (!identical(vec_ff_pfh_other, integer(0))) {
   #
   # list_layer_inconsistencies <- rbind(
   #   list_layer_inconsistencies,
   #   data.frame(survey_form = (rep(survey_4, length(vec_ff_pfh_other))),
   #              partner = pfh_other$partner[vec_ff_pfh_other],
   #              partner_code = pfh_other$partner_code[vec_ff_pfh_other],
   #              country = pfh_other$country[vec_ff_pfh_other],
   #              code_country = pfh_other$code_country[vec_ff_pfh_other],
   #              survey_year = pfh_other$survey_year[vec_ff_pfh_other],
   #              code_plot = pfh_other$code_plot[vec_ff_pfh_other],
   #              plot_id = pfh_other$plot_id[vec_ff_pfh_other],
   #              code_layer_horizon_master =
   #                pfh_other$horizon_master[vec_ff_pfh_other],
   #              repetition_profile_pit_id =
   #                pfh_other$profile_pit_id[vec_ff_pfh_other],
   #              code_line = pfh_other$code_line[vec_ff_pfh_other],
   #              parameter = (rep("horizon_master", length(vec_ff_pfh_other))),
   #              parameter_unit = (rep(NA, length(vec_ff_pfh_other))),
   #              parameter_value =
   #                as.character(pfh_other$horizon_master[vec_ff_pfh_other]),
   #              inconsistency_reason = inconsistency_reason,
   #              inconsistency_type = inconsistency_type,
   #              rule_id = rule_id,
   #              non_duplicated_error_type_per_record =
   #                rep(FALSE, length(vec_ff_pfh_other)),
   #              change_date = pfh_other$change_date[vec_ff_pfh_other],
   #              download_date = rep(download_date_pir,
   #                                  length(vec_ff_pfh_other))))
   # }

   # Change "non_duplicated_error_type_per_record" so that there is only one
   # unique inconsistency per plot_id

   length_inconsistency <-
     length(c(vec_ff_som, vec_ff_pfh)) #, vec_ff_som_other, vec_ff_pfh_other))

   list_layer_inconsistencies$non_duplicated_error_type_per_record[
     nrow(list_layer_inconsistencies) - length_inconsistency + 1] <- TRUE


 }
}
}












## FSCC_2: Caution: the reported layer limits in above-ground (negative) or ----
# below-ground (positive) layers have the wrong sign (positive/negative).

# Above-ground

# Check if layer limits of forest floor layers are known

if (any(!is.na(df$layer_limit_superior_orig[vec_ff])) &&
    any(!is.na(df$layer_limit_inferior_orig[vec_ff]))) {

  vec_ff_sup <- vec_ff[which(!is.na(df$layer_limit_superior_orig[vec_ff]))]
  vec_ff_inf <- vec_ff[which(!is.na(df$layer_limit_inferior_orig[vec_ff]))]

  vec_ff_non_empty <-
    vec_ff[which((!is.na(df$layer_limit_superior_orig[vec_ff])) &
                   (!is.na(df$layer_limit_inferior_orig[vec_ff])))]

  if (!identical(vec_bg, integer(0)) &&
      any(!is.na(df$layer_limit_superior_orig[vec_bg])) &&
      any(!is.na(df$layer_limit_inferior_orig[vec_bg]))) {

  # Depth range forest floor

  depth_range_ff <- seq(round(max(c(df$layer_limit_superior[vec_ff],
                                    df$layer_limit_inferior[vec_ff]),
                                  na.rm = TRUE)),
                        round(min(c(df$layer_limit_superior[vec_ff],
                                    df$layer_limit_inferior[vec_ff]),
                                  na.rm = TRUE)),
                        by = -0.5)

  depth_range_ff <- depth_range_ff[-c(1, length(depth_range_ff))]

  # Depth range below-ground

  depth_range_bg <- seq(round(max(c(df$layer_limit_superior[vec_bg],
                                    df$layer_limit_inferior[vec_bg]),
                                  na.rm = TRUE)),
                        round(min(c(df$layer_limit_superior[vec_bg],
                                    df$layer_limit_inferior[vec_bg]),
                                  na.rm = TRUE)),
                        by = -0.5)

  depth_range_bg <- depth_range_bg[-c(1, length(depth_range_bg))]

  }


  # Check if forest floor layers have limits with wrong sign
  # (i.e. all >= 0 and at least once > 0 so not equal to 0)
  # Only for profiles without peat since peat soils usually have a
  # special sequence
  # There should be an overlap in depth ranges between above-ground
  # and below-ground and
  # the superior layer limit should be higher than the inferior layer limit
  # (in the assumption that the signs are switched)

if (all(df$layer_limit_inferior_orig[vec_ff_sup] >= 0) &&
    all(df$layer_limit_superior_orig[vec_ff_inf] >= 0) &&
    any(c(df$layer_limit_inferior_orig[vec_ff_sup],
          df$layer_limit_superior_orig[vec_ff_inf]) > 0) &&
    (((!identical(vec_bg, integer(0)) &&
       any(!is.na(df$layer_limit_superior_orig[vec_bg])) &&
       any(!is.na(df$layer_limit_inferior_orig[vec_bg]))) &&
      any(depth_range_ff %in% depth_range_bg)) ||
     identical(vec_bg, integer(0))) &&
    all(df$layer_limit_superior_orig[vec_ff_non_empty] >
        df$layer_limit_inferior_orig[vec_ff_non_empty]) &&
    length(vec_ff) >= 1 &&
    all(df$layer_type[vec_bg] != "peat")) {

  # Store information about the inconsistency in "list_layer_inconsistencies"

  # superior

  rule_id <- "FSCC_2"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  if ("code_layer_orig" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_orig[vec_ff_sup])
    } else {
      vec_code_layer <- as.character(df$code_layer[vec_ff_sup])
    }

  list_layer_inconsistencies <- rbind(
    list_layer_inconsistencies,
    data.frame(survey_form = as.factor(rep(survey_form, length(vec_ff_sup))),
               partner = df$partner[vec_ff_sup],
               partner_code = df$partner_code[vec_ff_sup],
               country = df$country[vec_ff_sup],
               code_country = df$code_country[vec_ff_sup],
               survey_year = df$survey_year[vec_ff_sup],
               code_plot = df$code_plot[vec_ff_sup],
               plot_id = df$plot_id[vec_ff_sup],
               code_layer_horizon_master = vec_code_layer,
               repetition_profile_pit_id = df$repetition[vec_ff_sup],
               code_line = df$code_line[vec_ff_sup],
               parameter = (rep("layer_limit_superior", length(vec_ff_sup))),
               parameter_unit = (rep("cm", length(vec_ff_sup))),
               parameter_value = df$layer_limit_superior_orig[vec_ff_sup],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record =
                 rep(TRUE, length(vec_ff_sup)),
               change_date = df$change_date[vec_ff_sup],
               download_date = rep(download_date_pir, length(vec_ff_sup))))

  # Inferior

  rule_id <- "FSCC_2"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  if ("code_layer_orig" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_orig[vec_ff_inf])
    } else {
      vec_code_layer <- as.character(df$code_layer[vec_ff_inf])
    }

  list_layer_inconsistencies <- rbind(
    list_layer_inconsistencies,
    data.frame(survey_form = as.factor(rep(survey_form, length(vec_ff_inf))),
               partner = df$partner[vec_ff_inf],
               partner_code = df$partner_code[vec_ff_inf],
               country = df$country[vec_ff_inf],
               code_country = df$code_country[vec_ff_inf],
               survey_year = df$survey_year[vec_ff_inf],
               code_plot = df$code_plot[vec_ff_inf],
               plot_id = df$plot_id[vec_ff_inf],
               code_layer_horizon_master = vec_code_layer,
               repetition_profile_pit_id = df$repetition[vec_ff_inf],
               code_line = df$code_line[vec_ff_inf],
               parameter = (rep("layer_limit_inferior", length(vec_ff_inf))),
               parameter_unit = (rep("cm", length(vec_ff_inf))),
               parameter_value = df$layer_limit_inferior_orig[vec_ff_inf],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record =
                 rep(FALSE, length(vec_ff_inf)),
               change_date = df$change_date[vec_ff_inf],
               download_date = rep(download_date_pir, length(vec_ff_inf))))

  # If "solve" is TRUE, multiply the layer limits of the forest floor layers
  # with *(-1) so that they become negative

   if (solve == TRUE) {

    df$layer_limit_inferior[vec_ff_inf] <-
      (-1) * (df$layer_limit_inferior_orig[vec_ff_inf])
    df$layer_limit_superior[vec_ff_sup] <-
      (-1) * (df$layer_limit_superior_orig[vec_ff_sup])

   }
}
}


# Below-ground

# Check if layer limits of forest floor layers are known

if (any(!is.na(df$layer_limit_superior_orig[vec_bg])) &&
    any(!is.na(df$layer_limit_inferior_orig[vec_bg]))) {

  vec_bg_sup <- vec_bg[which(!is.na(df$layer_limit_superior_orig[vec_bg]))]
  vec_bg_inf <- vec_bg[which(!is.na(df$layer_limit_inferior_orig[vec_bg]))]

  vec_bg_non_empty <-
    vec_bg[which((!is.na(df$layer_limit_superior_orig[vec_bg])) &
                   (!is.na(df$layer_limit_inferior_orig[vec_bg])))]


  # Depth range forest floor

  if (!identical(vec_ff, integer(0)) &&
      (any(!is.na(df$layer_limit_inferior_orig[vec_ff])) &&
       any(!is.na(df$layer_limit_superior_orig[vec_ff])))) {

  depth_range_ff <- seq(round(max(c(df$layer_limit_superior[vec_ff],
                                    df$layer_limit_inferior[vec_ff]),
                                  na.rm = TRUE)),
                        round(min(c(df$layer_limit_superior[vec_ff],
                                    df$layer_limit_inferior[vec_ff]),
                                  na.rm = TRUE)),
                        by = -0.5)

  depth_range_ff <- depth_range_ff[-c(1, length(depth_range_ff))]

  # Depth range below-ground

  depth_range_bg <- seq(round(max(c(df$layer_limit_superior[vec_bg],
                                    df$layer_limit_inferior[vec_bg]),
                                  na.rm = TRUE)),
                        round(min(c(df$layer_limit_superior[vec_bg],
                                    df$layer_limit_inferior[vec_bg]),
                                  na.rm = TRUE)),
                        by = -0.5)

  depth_range_bg <- depth_range_bg[-c(1, length(depth_range_bg))]

  }

  # Check if below-ground (mineral or peat layers) have limits with wrong sign
  # (i.e. all <= 0 and at least once < 0 so not equal to 0)

if (all(df$layer_limit_inferior_orig[vec_bg_inf] <= 0) &&
    all(df$layer_limit_superior_orig[vec_bg_sup] <= 0) &&
    any(c(df$layer_limit_inferior_orig[vec_bg_inf],
          df$layer_limit_superior_orig[vec_bg_sup]) < 0) &&
    # Either (if there's a forest floor) there should be no overlap
    # or (if there is no forest floor), the superior layer limit should
    # be higher than the inferior layer limit (in the assumption
    # that the signs are switched)
    (((!identical(vec_ff, integer(0)) &&
       any(!is.na(df$layer_limit_superior_orig[vec_ff])) &&
       any(!is.na(df$layer_limit_inferior_orig[vec_ff]))) &&
      (any(depth_range_bg %in% depth_range_ff))) ||
     ((identical(vec_ff, integer(0)) ||
       (all(is.na(df$layer_limit_superior_orig[vec_ff])) ||
        all(is.na(df$layer_limit_inferior_orig[vec_ff])))) &&
      (all(df$layer_limit_superior_orig[vec_bg_non_empty] >
           df$layer_limit_inferior_orig[vec_bg_non_empty])))) &&
    length(vec_bg) >= 1) {

  # Store information about the inconsistency in "list_layer_inconsistencies"

  # Superior

  rule_id <- "FSCC_2"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  if ("code_layer_orig" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_orig[vec_bg_sup])
    } else {
      vec_code_layer <- as.character(df$code_layer[vec_bg_sup])
    }

  list_layer_inconsistencies <- rbind(
    list_layer_inconsistencies,
    data.frame(survey_form = as.factor(rep(survey_form, length(vec_bg_sup))),
               partner = df$partner[vec_bg_sup],
               partner_code = df$partner_code[vec_bg_sup],
               country = df$country[vec_bg_sup],
               code_country = df$code_country[vec_bg_sup],
               survey_year = df$survey_year[vec_bg_sup],
               code_plot = df$code_plot[vec_bg_sup],
               plot_id = df$plot_id[vec_bg_sup],
               code_layer_horizon_master = vec_code_layer,
               repetition_profile_pit_id = df$repetition[vec_bg_sup],
               code_line = df$code_line[vec_bg_sup],
               parameter = (rep("layer_limit_superior", length(vec_bg_sup))),
               parameter_unit = (rep("cm", length(vec_bg_sup))),
               parameter_value = df$layer_limit_superior_orig[vec_bg_sup],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record =
                 rep(TRUE, length(vec_bg_sup)),
               change_date = df$change_date[vec_bg_sup],
               download_date = rep(download_date_pir, length(vec_bg_sup))))

  # Inferior

  rule_id <- "FSCC_2"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  if ("code_layer_orig" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_orig[vec_bg_inf])
    } else {
      vec_code_layer <- as.character(df$code_layer[vec_bg_inf])
    }

  list_layer_inconsistencies <- rbind(
    list_layer_inconsistencies,
    data.frame(survey_form = as.factor(rep(survey_form, length(vec_bg_inf))),
               partner = df$partner[vec_bg_inf],
               partner_code = df$partner_code[vec_bg_inf],
               country = df$country[vec_bg_inf],
               code_country = df$code_country[vec_bg_inf],
               survey_year = df$survey_year[vec_bg_inf],
               code_plot = df$code_plot[vec_bg_inf],
               plot_id = df$plot_id[vec_bg_inf],
               code_layer_horizon_master = vec_code_layer,
               repetition_profile_pit_id = df$repetition[vec_bg_inf],
               code_line = df$code_line[vec_bg_inf],
               parameter = (rep("layer_limit_inferior", length(vec_bg_inf))),
               parameter_unit = (rep("cm", length(vec_bg_inf))),
               parameter_value = df$layer_limit_inferior_orig[vec_bg_inf],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record =
                 rep(FALSE, length(vec_bg_inf)),
               change_date = df$change_date[vec_bg_inf],
               download_date = rep(download_date_pir, length(vec_bg_inf))))

  # If "solve" is TRUE, multiply the layer limits of the below-ground layers
  # with *(-1) so that they become positive

  if (solve == TRUE) {

    df$layer_limit_inferior[vec_bg_inf] <-
      (-1) * (df$layer_limit_inferior_orig[vec_bg_inf])
    df$layer_limit_superior[vec_bg_sup] <-
      (-1) * (df$layer_limit_superior_orig[vec_bg_sup])

  }
}
}



## FSCC_40: the reported "code_layer" is unknown + ----
## FSCC_39: reported layer limits do not correspond with code_layer ----

  # Loop through indices in the vector `vec`

for (j in vec) {

  # Check if the necessary parameters are not missing

  if ("code_layer_orig" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_orig[j])
    } else {
      vec_code_layer <- as.character(df$code_layer[j])
    }

  if (!is.na(vec_code_layer) &&
      (!is.na(df$layer_limit_superior_orig[j]) ||
       !is.na(df$layer_limit_inferior_orig[j]))) {

  # Identify the index of `code_layer` in `d_depth_level_soil$code`

    ind <- which(vec_code_layer == d_depth_level_soil$code)

  # Check if the index is not found (i.e., `ind` is an empty integer vector)

    if (identical(ind, integer(0))) {

# FSCC_40: the reported "code_layer" is unknown

      # Store information about the inconsistency in
      # "list_layer_inconsistencies"

      rule_id <- "FSCC_40"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      list_layer_inconsistencies <- rbind(
        list_layer_inconsistencies,
        data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                   partner = df$partner[j],
                   partner_code = df$partner_code[j],
                   country = df$country[j],
                   code_country = df$code_country[j],
                   survey_year = df$survey_year[j],
                   code_plot = df$code_plot[j],
                   plot_id = df$plot_id[j],
                   code_layer_horizon_master = vec_code_layer,
                   repetition_profile_pit_id = df$repetition[j],
                   code_line = df$code_line[j],
                   parameter = (rep("code_layer", length(j))),
                   parameter_unit = (rep("-", length(j))),
                   parameter_value = vec_code_layer,
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(TRUE, length(j)),
                   change_date = df$change_date[j],
                   download_date = rep(download_date_pir, length(j))))

      } else {

# FSCC_39: reported layer limits do not correspond with code_layer

  # Retrieve the theoretical layer limits
  # corresponding to the `code_layer` value in `d_depth_level_soil`

   limit_superior_theoretical <- d_depth_level_soil$layer_limit_superior[ind]
   limit_inferior_theoretical <- d_depth_level_soil$layer_limit_inferior[ind]

   # Check if `layer_limit_superior_orig` is not missing
   # and does not match the theoretical value

   if (!is.na(limit_superior_theoretical)) {

   if (is.na(df$layer_limit_superior_orig[j]) ||
        (df$layer_limit_superior_orig[j] != limit_superior_theoretical)) {

   # Store information about the inconsistency in "list_layer_inconsistencies"

     rule_id <- "FSCC_39"
     inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
       which(inconsistency_catalogue$rule_id == rule_id)]
     inconsistency_reason <- paste0(inconsistency_reason,
                                    " (i.e. ",
                                    limit_superior_theoretical,
                                    " cm).")
     inconsistency_type <- inconsistency_catalogue$inconsistency_type[
       which(inconsistency_catalogue$rule_id == rule_id)]

     if ("code_layer_orig" %in% names(df)) {
         vec_code_layer <- as.character(df$code_layer_orig[j])
       } else {
         vec_code_layer <- as.character(df$code_layer[j])
       }

     list_layer_inconsistencies <- rbind(
       list_layer_inconsistencies,
       data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                  partner = df$partner[j],
                  partner_code = df$partner_code[j],
                  country = df$country[j],
                  code_country = df$code_country[j],
                  survey_year = df$survey_year[j],
                  code_plot = df$code_plot[j],
                  plot_id = df$plot_id[j],
                  code_layer_horizon_master = vec_code_layer,
                  repetition_profile_pit_id = df$repetition[j],
                  code_line = df$code_line[j],
                  parameter = (rep("layer_limit_superior", length(j))),
                  parameter_unit = (rep("cm", length(j))),
                  parameter_value =
                    as.character(df$layer_limit_superior_orig[j]),
                  inconsistency_reason = inconsistency_reason,
                  inconsistency_type = inconsistency_type,
                  rule_id = rule_id,
                  non_duplicated_error_type_per_record = rep(TRUE, length(j)),
                  change_date = df$change_date[j],
                  download_date = rep(download_date_pir, length(j))))
   }
   }

   # Check if `layer_limit_inferior_orig` is not missing
   # and does not match the theoretical value

   if (!is.na(limit_inferior_theoretical)) {

   if (is.na(df$layer_limit_inferior_orig[j]) ||
       (df$layer_limit_inferior_orig[j] != limit_inferior_theoretical)) {

     # Store information about the inconsistency in
     # "list_layer_inconsistencies"

     rule_id <- "FSCC_39"
     inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
       which(inconsistency_catalogue$rule_id == rule_id)]
     inconsistency_reason <- paste0(inconsistency_reason,
                                    " (i.e. ",
                                    limit_inferior_theoretical,
                                    " cm).")
     inconsistency_type <- inconsistency_catalogue$inconsistency_type[
       which(inconsistency_catalogue$rule_id == rule_id)]

     if ("code_layer_orig" %in% names(df)) {
         vec_code_layer <- as.character(df$code_layer_orig[j])
       } else {
         vec_code_layer <- as.character(df$code_layer[j])
       }

     list_layer_inconsistencies <- rbind(
       list_layer_inconsistencies,
       data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                  partner = df$partner[j],
                  partner_code = df$partner_code[j],
                  country = df$country[j],
                  code_country = df$code_country[j],
                  survey_year = df$survey_year[j],
                  code_plot = df$code_plot[j],
                  plot_id = df$plot_id[j],
                  code_layer_horizon_master = vec_code_layer,
                  repetition_profile_pit_id = df$repetition[j],
                  code_line = df$code_line[j],
                  parameter = (rep("layer_limit_inferior", length(j))),
                  parameter_unit = (rep("cm", length(j))),
                  parameter_value =
                    as.character(df$layer_limit_inferior_orig[j]),
                  inconsistency_reason = inconsistency_reason,
                  inconsistency_type = inconsistency_type,
                  rule_id = rule_id,
                  non_duplicated_error_type_per_record = rep(TRUE, length(j)),
                  change_date = df$change_date[j],
                  download_date = rep(download_date_pir, length(j))))
   }
   }

  }
  }
  }





## FSCC_8: No layer limit information ----

  # Check if any but not all superior layer limits are missing

if ((any(!is.na(df$layer_limit_superior_orig[vec]))) &&
    (any(is.na(df$layer_limit_superior_orig[vec])))) {

  # store the indices of the records without layer limits in vec_inconsistency.

  vec_inconsistency <- vec[which(is.na(df$layer_limit_superior_orig[vec]))]

  # Store information about the inconsistencies in "list_layer_inconsistencies"

  rule_id <- "FSCC_8"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  if ("code_layer_orig" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_orig[vec_inconsistency])
    } else {
      vec_code_layer <- as.character(df$code_layer[vec_inconsistency])
    }

  list_layer_inconsistencies <- rbind(
    list_layer_inconsistencies,
    data.frame(survey_form = as.factor(rep(survey_form,
                                           length(vec_inconsistency))),
               partner = df$partner[vec_inconsistency],
               partner_code = df$partner_code[vec_inconsistency],
               country = df$country[vec_inconsistency],
               code_country = df$code_country[vec_inconsistency],
               survey_year = df$survey_year[vec_inconsistency],
               code_plot = df$code_plot[vec_inconsistency],
               plot_id = df$plot_id[vec_inconsistency],
               code_layer_horizon_master = vec_code_layer,
               repetition_profile_pit_id = df$repetition[vec_inconsistency],
               code_line = df$code_line[vec_inconsistency],
               parameter = as.factor(rep("layer_limit_superior",
                                         length(vec_inconsistency))),
               parameter_unit = as.factor(rep("cm", length(vec_inconsistency))),
               parameter_value =
                 df$layer_limit_superior_orig[vec_inconsistency],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record =
                 rep(TRUE, length(vec_inconsistency)),
               change_date = df$change_date[vec_inconsistency],
               download_date = rep(download_date_pir,
                                   length(vec_inconsistency))))
  }


  # Check if any but not all inferior layer limits are missing

if ((any(!is.na(df$layer_limit_inferior_orig[vec]))) &&
    (any(is.na(df$layer_limit_inferior_orig[vec])))) {

  # store the indices of the records without layer limits in vec_inconsistency.

  vec_inconsistency <- vec[which(is.na(df$layer_limit_inferior_orig[vec]))]

  # Store information about the inconsistencies in "list_layer_inconsistencies"

  rule_id <- "FSCC_8"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  if ("code_layer_orig" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_orig[vec_inconsistency])
    } else {
      vec_code_layer <- as.character(df$code_layer[vec_inconsistency])
    }

  list_layer_inconsistencies <- rbind(
    list_layer_inconsistencies,
    data.frame(survey_form = as.factor(rep(survey_form,
                                           length(vec_inconsistency))),
               partner = df$partner[vec_inconsistency],
               partner_code = df$partner_code[vec_inconsistency],
               country = df$country[vec_inconsistency],
               code_country = df$code_country[vec_inconsistency],
               survey_year = df$survey_year[vec_inconsistency],
               code_plot = df$code_plot[vec_inconsistency],
               plot_id = df$plot_id[vec_inconsistency],
               code_layer_horizon_master = vec_code_layer,
               repetition_profile_pit_id = df$repetition[vec_inconsistency],
               code_line = df$code_line[vec_inconsistency],
               parameter = as.factor(rep("layer_limit_inferior",
                                         length(vec_inconsistency))),
               parameter_unit = as.factor(rep("cm", length(vec_inconsistency))),
               parameter_value =
                 df$layer_limit_inferior_orig[vec_inconsistency],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record =
                 rep(TRUE, length(vec_inconsistency)),
               change_date = df$change_date[vec_inconsistency],
               download_date = rep(download_date_pir,
                                   length(vec_inconsistency))))
  }











# Step 1: Empty layer limits that are clearly a mistake ----


## FSCC_9 Error reason: the layer limits equal 0 for multiple depth layers ----
# in the given profile

  # Superior

if (!identical(which(!is.na(df$layer_limit_superior_orig[vec])), integer(0))) {

  # Check if all layer limits are zeros for a profile, i.e.
  # check if all layer_limit_superior are the same value +
  # check if they all equal 0 +
  # check if layer_limit_superior equals 0 for more than one layer

if (length(unique(df$layer_limit_superior_orig[vec[
  which(!is.na(df$layer_limit_superior_orig[vec]))]])) == 1) {

if ((unique(df$layer_limit_superior_orig[vec[
      which(!is.na(df$layer_limit_superior_orig[vec]))]]) == 0) &&
    (length(which(df$layer_limit_superior_orig[vec[
      which(!is.na(df$layer_limit_superior_orig[vec]))]] == 0)) > 1)) {

      vec_error <- vec[which(!is.na(df$layer_limit_superior_orig[vec]))]

  # Store information about the inconsistency in "list_layer_inconsistencies"

        rule_id <- "FSCC_9"
        inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
          which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <- inconsistency_catalogue$inconsistency_type[
          which(inconsistency_catalogue$rule_id == rule_id)]

        if ("code_layer_orig" %in% names(df)) {
          vec_code_layer <- as.character(df$code_layer_orig[vec_error])
          } else {
            vec_code_layer <- as.character(df$code_layer[vec_error])
          }

        list_layer_inconsistencies <- rbind(
          list_layer_inconsistencies,
          data.frame(survey_form = as.factor(rep(survey_form,
                                                 length(vec_error))),
                     partner = df$partner[vec_error],
                     partner_code = df$partner_code[vec_error],
                     country = df$country[vec_error],
                     code_country = df$code_country[vec_error],
                     survey_year = df$survey_year[vec_error],
                     code_plot = df$code_plot[vec_error],
                     plot_id = df$plot_id[vec_error],
                     code_layer_horizon_master = vec_code_layer,
                     repetition_profile_pit_id = df$repetition[vec_error],
                     code_line = df$code_line[vec_error],
                     parameter = as.factor(rep("layer_limit_superior",
                                               length(vec_error))),
                     parameter_unit = as.factor(rep("cm", length(vec_error))),
                     parameter_value = df$layer_limit_superior_orig[vec_error],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       rep(TRUE, length(vec_error)),
                     change_date = df$change_date[vec_error],
                     download_date = rep(download_date_pir,
                                         length(vec_error))))

  # If "solve" is set to TRUE, set the layer limit to NA

        if (solve == TRUE) {
          df$layer_limit_superior[vec_error] <- NA
        }
}
}
}

  # Inferior

if (!identical(which(!is.na(df$layer_limit_inferior_orig[vec])), integer(0))) {

  # Check if all layer limits are zeros for a profile, i.e.
  # check if all layer_limit_inferior are the same value +
  # check if they all equal 0 +
  # check if layer_limit_inferior equals 0 for more than one layer

if (length(unique(df$layer_limit_inferior_orig[vec[
  which(!is.na(df$layer_limit_inferior_orig[vec]))]])) == 1) {

if ((unique(df$layer_limit_inferior_orig[vec[
      which(!is.na(df$layer_limit_inferior_orig[vec]))]]) == 0) &&
   (length(which(df$layer_limit_inferior_orig[vec[
      which(!is.na(df$layer_limit_inferior_orig[vec]))]] == 0)) > 1)) {

      vec_error <- vec[which(!is.na(df$layer_limit_inferior_orig[vec]))]

  # Store information about the inconsistency in "list_layer_inconsistencies"

      rule_id <- "FSCC_9"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      if ("code_layer_orig" %in% names(df)) {
          vec_code_layer <- as.character(df$code_layer_orig[vec_error])
        } else {
          vec_code_layer <- as.character(df$code_layer[vec_error])
        }

      list_layer_inconsistencies <- rbind(
         list_layer_inconsistencies,
         data.frame(survey_form = as.factor(rep(survey_form,
                                                length(vec_error))),
                    partner = df$partner[vec_error],
                    partner_code = df$partner_code[vec_error],
                    country = df$country[vec_error],
                    code_country = df$code_country[vec_error],
                    survey_year = df$survey_year[vec_error],
                    code_plot = df$code_plot[vec_error],
                    plot_id = df$plot_id[vec_error],
                    code_layer_horizon_master = vec_code_layer,
                    repetition_profile_pit_id = df$repetition[vec_error],
                    code_line = df$code_line[vec_error],
                    parameter = as.factor(rep("layer_limit_inferior",
                                              length(vec_error))),
                    parameter_unit = as.factor(rep("cm", length(vec_error))),
                    parameter_value = df$layer_limit_inferior_orig[vec_error],
                    inconsistency_reason = inconsistency_reason,
                    inconsistency_type = inconsistency_type,
                    rule_id = rule_id,
                    non_duplicated_error_type_per_record =
                      rep(TRUE, length(vec_error)),
                    change_date = df$change_date[vec_error],
                    download_date = rep(download_date_pir, length(vec_error))))

      # If "solve" is set to TRUE, set the layer limit to NA

        if (solve == TRUE) {
          df$layer_limit_inferior[vec_error] <- NA
        }
}
}
}



## FSCC_11 Error reason: layer_limit_superior == layer_limit_inferior ----

for (j in vec) {

  # Check if the superior and inferior layer limits are not empty
  # and equal to each other

if (!is.na(df$layer_limit_superior_orig[j]) &&
    !is.na(df$layer_limit_inferior_orig[j])) {

  if ((df$layer_limit_superior_orig[j] == df$layer_limit_inferior_orig[j])) {

    # Store information about the inconsistency in "list_layer_inconsistencies"

    # Superior

    rule_id <- "FSCC_11"
    inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
      which(inconsistency_catalogue$rule_id == rule_id)]
    inconsistency_type <- inconsistency_catalogue$inconsistency_type[
      which(inconsistency_catalogue$rule_id == rule_id)]

    if ("code_layer_orig" %in% names(df)) {
        vec_code_layer <- as.character(df$code_layer_orig[j])
      } else {
        vec_code_layer <- as.character(df$code_layer[j])
      }

    list_layer_inconsistencies <- rbind(
      list_layer_inconsistencies,
      data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                 partner = df$partner[j],
                 partner_code = df$partner_code[j],
                 country = df$country[j],
                 code_country = df$code_country[j],
                 survey_year = df$survey_year[j],
                 code_plot = df$code_plot[j],
                 plot_id = df$plot_id[j],
                 code_layer_horizon_master = vec_code_layer,
                 repetition_profile_pit_id = df$repetition[j],
                 code_line = df$code_line[j],
                 parameter = as.factor(rep("layer_limit_superior", length(j))),
                 parameter_unit = as.factor(rep("cm", length(j))),
                 parameter_value = df$layer_limit_superior_orig[j],
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(TRUE, length(j)),
                 change_date = df$change_date[j],
                 download_date = rep(download_date_pir, length(j))))

    # Inferior
    rule_id <- "FSCC_11"
    inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
      which(inconsistency_catalogue$rule_id == rule_id)]
    inconsistency_type <- inconsistency_catalogue$inconsistency_type[
      which(inconsistency_catalogue$rule_id == rule_id)]

    list_layer_inconsistencies <- rbind(
       list_layer_inconsistencies,
       data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                  partner = df$partner[j],
                  partner_code = df$partner_code[j],
                  country = df$country[j],
                  code_country = df$code_country[j],
                  survey_year = df$survey_year[j],
                  code_plot = df$code_plot[j],
                  plot_id = df$plot_id[j],
                  code_layer_horizon_master = vec_code_layer,
                  repetition_profile_pit_id = df$repetition[j],
                  code_line = df$code_line[j],
                  parameter = as.factor(rep("layer_limit_inferior", length(j))),
                  parameter_unit = as.factor(rep("cm", length(j))),
                  parameter_value = df$layer_limit_inferior_orig[j],
                  inconsistency_reason = inconsistency_reason,
                  inconsistency_type = inconsistency_type,
                  rule_id = rule_id,
                  non_duplicated_error_type_per_record = rep(FALSE, length(j)),
                  change_date = df$change_date[j],
                  download_date = rep(download_date_pir, length(j))))

    # If "solve" is set to TRUE, set the layer limit to NA

     if (solve == TRUE) {
       df$layer_limit_superior[j] <- NA
       df$layer_limit_inferior[j] <- NA
    }
  }
}
}


## FSCC_51: Layer limit equals -9999 ----

# Superior

vec_inconsistency <- vec[which(df$layer_limit_superior_orig[vec] < -500)]

if (!identical(vec_inconsistency, integer(0))) {

  # Store information about the inconsistency in "list_layer_inconsistencies"

  rule_id <- "FSCC_51"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  if ("code_layer_orig" %in% names(df)) {
    vec_code_layer <- as.character(df$code_layer_orig[j])
  } else {
    vec_code_layer <- as.character(df$code_layer[j])
  }

  list_layer_inconsistencies <- rbind(
    list_layer_inconsistencies,
    data.frame(survey_form = as.factor(rep(survey_form,
                                           length(vec_inconsistency))),
               partner = df$partner[vec_inconsistency],
               partner_code = df$partner_code[vec_inconsistency],
               country = df$country[vec_inconsistency],
               code_country = df$code_country[vec_inconsistency],
               survey_year = df$survey_year[vec_inconsistency],
               code_plot = df$code_plot[vec_inconsistency],
               plot_id = df$plot_id[vec_inconsistency],
               code_layer_horizon_master = vec_code_layer,
               repetition_profile_pit_id = df$repetition[vec_inconsistency],
               code_line = df$code_line[vec_inconsistency],
               parameter = as.factor(rep("layer_limit_superior",
                                         length(vec_inconsistency))),
               parameter_unit = as.factor(rep("cm", length(vec_inconsistency))),
               parameter_value =
                 df$layer_limit_superior_orig[vec_inconsistency],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record =
                 rep(TRUE, length(vec_inconsistency)),
               change_date = df$change_date[vec_inconsistency],
               download_date = rep(download_date_pir,
                                   length(vec_inconsistency))))

  # If "solve" is set to TRUE, set the layer limit to NA

  if (solve == TRUE) {
    df$layer_limit_superior[vec_inconsistency] <- NA
  }
}


# Inferior

vec_inconsistency <- vec[which(df$layer_limit_inferior_orig[vec] < -500)]

if (!identical(vec_inconsistency, integer(0))) {

  # Store information about the inconsistency in "list_layer_inconsistencies"

  rule_id <- "FSCC_51"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  if ("code_layer_orig" %in% names(df)) {
    vec_code_layer <- as.character(df$code_layer_orig[j])
  } else {
    vec_code_layer <- as.character(df$code_layer[j])
  }

  list_layer_inconsistencies <- rbind(
    list_layer_inconsistencies,
    data.frame(survey_form = as.factor(rep(survey_form,
                                           length(vec_inconsistency))),
               partner = df$partner[vec_inconsistency],
               partner_code = df$partner_code[vec_inconsistency],
               country = df$country[vec_inconsistency],
               code_country = df$code_country[vec_inconsistency],
               survey_year = df$survey_year[vec_inconsistency],
               code_plot = df$code_plot[vec_inconsistency],
               plot_id = df$plot_id[vec_inconsistency],
               code_layer_horizon_master = vec_code_layer,
               repetition_profile_pit_id = df$repetition[vec_inconsistency],
               code_line = df$code_line[vec_inconsistency],
               parameter = as.factor(rep("layer_limit_inferior",
                                         length(vec_inconsistency))),
               parameter_unit = as.factor(rep("cm", length(vec_inconsistency))),
               parameter_value =
                 df$layer_limit_inferior_orig[vec_inconsistency],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record =
                 rep(TRUE, length(vec_inconsistency)),
               change_date = df$change_date[vec_inconsistency],
               download_date = rep(download_date_pir,
                                   length(vec_inconsistency))))

  # If "solve" is set to TRUE, set the layer limit to NA

  if (solve == TRUE) {
    df$layer_limit_inferior[vec_inconsistency] <- NA
  }
}





# Step 2: Try to gap-fill any empty layer limits ----

  # If there is at least one layer without limit information

if ((any(is.na(df$layer_limit_inferior[vec])) ||
     any(is.na(df$layer_limit_superior[vec])))) {

  # Get the index of empty cells in layer_limit_superior and
  # layer_limit_inferior

vec_empty_layer_limit_superior <-
  vec[which(is.na(df$layer_limit_superior[vec]))]
vec_empty_layer_limit_inferior <-
  vec[which(is.na(df$layer_limit_inferior[vec]))]


# TO DO Option 2.1: Can we fill these layers based on pfh? ----
# (only for forest floor layers)

# vec_ff_empty_superior <- vec[which(
#   is.na(df$layer_limit_superior[vec]) &
#     df$layer_type[vec] == "forest_floor")]
#
# vec_ff_empty_inferior <- vec[which(
#   is.na(df$layer_limit_inferior[vec]) &
#     df$layer_type[vec] == "forest_floor")]

# If there are any forest floor layers with unknown layer limits

# if (!identical(vec_ff_empty_superior, integer(0)) ||
#     !identical(vec_ff_empty_inferior, integer(0))) {
#
#   # The survey_year may differ a bit. Determine the unique_surveys in which
#   # a match in "pfh" may be possible
#
# source("./src/functions/expand_unique_survey_vec_adjacent_years.R")
#
# unique_survey_i <- unique(df$unique_survey[vec])
# unique_survey_expanded_i <-
#   expand_unique_survey_vec_adjacent_years(unique_survey_vec = unique_survey_i,
#                                           number_of_years = 4)
#
#   # Get the index of the plot survey in pfh
#
# vec_pfh <- which(pfh$unique_survey %in% unique_survey_expanded_i) # In pfh
#
#   # Fill empty cells in layer_limit_superior
#
# if (!identical(vec_pfh, integer(0))) { # If this plot survey is in pfh
#
#   # If any
#
#   # match_key_forest_floors  <- rbind(
#   #   match_key_forest_floors ,
#   #   data.frame(unique_survey_repetition = NULL,
#   #              code_layer = NULL,
#   #              code_layers_pfh = NULL,
#   #              layer_limit_superior = NULL,
#   #              layer_limit_inferior = NULL))
#
#
#
#   # Check if there are empty cells in layer_limit_superior
#
# if (!identical(vec_empty_layer_limit_superior, integer(0))) {
#
# for (j in vec_empty_layer_limit_superior) {
#
#   if ("code_layer_orig" %in% names(df)) {
#     vec_code_layer <- as.character(df$code_layer_orig[j])
#   } else {
#     vec_code_layer <- as.character(df$code_layer[j])
#   }
#
#   # Check if code_layer is in pfh
#
#   vec_pfh_match <- which(vec_code_layer == pfh$horizon_master[vec_pfh])
#
#   if (!identical(vec_pfh_match, integer(0)) &&
#       length(vec_pfh_match) == 1) {
#
#     df$layer_limit_superior[j] <-
#       pfh$horizon_limit_up[vec_pfh[
#         match(vec_code_layer, pfh$horizon_master[vec_pfh])]]
#
#   } else
#
#           # For OLF and OFH layers,
#           # match the code_layer with OL-OF and OF-OH in pfh respectively
#
#           if (vec_code_layer == "OLF" &&
#               !is.na(match("OL", pfh$horizon_master[vec_pfh])) &&
#               !is.na(match("OF", pfh$horizon_master[vec_pfh]))) {
#
#               df$layer_limit_superior[j] <-
#                 pfh$horizon_limit_up[vec_pfh[
#                   match("OL", pfh$horizon_master[vec_pfh])]]
#
#               } else
#
#           if (vec_code_layer == "OFH" &&
#               !is.na(match("OF", pfh$horizon_master[vec_pfh])) &&
#               !is.na(match("OH", pfh$horizon_master[vec_pfh]))) {
#
#               df$layer_limit_superior[j] <-
#                 pfh$horizon_limit_up[vec_pfh[
#                   match("OF", pfh$horizon_master[vec_pfh])]]
#
#               }
#     }
#   }
#
#
#   # Fill empty cells in layer_limit_inferior
#
#   # Check if there are empty cells in layer_limit_inferior
#
#  if (!identical(vec_empty_layer_limit_inferior, integer(0))) {
#
#  for (j in vec_empty_layer_limit_inferior) {
#
#   # If code_layer is in pfh
#
#    if ("code_layer_orig" %in% names(df)) {
#      vec_code_layer <- as.character(df$code_layer_orig[j])
#    } else {
#        vec_code_layer <- as.character(df$code_layer[j])
#    }
#
#    # Check if code_layer is in pfh
#
#    vec_pfh_match <- which(vec_code_layer == pfh$horizon_master[vec_pfh])
#
#    if (!identical(vec_pfh_match, integer(0)) &&
#        length(vec_pfh_match) == 1) {
#
#      df$layer_limit_inferior[j] <-
#        pfh$horizon_limit_low[vec_pfh[
#          match(vec_code_layer, pfh$horizon_master[vec_pfh])]]
#
#     } else
#
#           # For OLF and OFH layers,
#           # match the code_layer with OL-OF and OF-OH in pfh respectively
#
#           if (vec_code_layer == "OLF" &&
#               !is.na(match("OL", pfh$horizon_master[vec_pfh])) &&
#               !is.na(match("OF", pfh$horizon_master[vec_pfh]))) {
#
#               df$layer_limit_inferior[j] <-
#                pfh$horizon_limit_low[vec_pfh[
#                  match("OF", pfh$horizon_master[vec_pfh])]]
#
#               } else
#           if (vec_code_layer == "OFH" &&
#               !is.na(match("OF", pfh$horizon_master[vec_pfh])) &&
#               !is.na(match("OH", pfh$horizon_master[vec_pfh]))) {
#
#               df$layer_limit_inferior[j] <-
#                 pfh$horizon_limit_low[vec_pfh[
#                   match("OH", pfh$horizon_master[vec_pfh])]]
#
#               }
#     }
#    }
# } # End of "if this plot survey is in 'pfh'"
# }


# Option 2.2: Can we fill these layers based on d_depth_level_soil? ----

  # Superior

  # Again select the indices of the empty superior layer limit and
  # inferior layer limit from vec

vec_empty_layer_limit_superior <-
  vec[which(is.na(df$layer_limit_superior[vec]))]
vec_empty_layer_limit_inferior <-
  vec[which(is.na(df$layer_limit_inferior[vec]))]

  # Check if vec_empty_layer_limit_superior is not empty
  # i.e. if there are actually layers with missing layer_limit_superior
  # in the given profile
  # and fill the empty superior layer limit

if (!identical(vec_empty_layer_limit_superior, integer(0))) {

for (j in vec_empty_layer_limit_superior) {

        if ("code_layer_orig" %in% names(df)) {
          vec_code_layer <- as.character(df$code_layer_orig[j])
          } else {
            vec_code_layer <- as.character(df$code_layer[j])
          }

  # Fill with theoretical layer limits of ICP Forests

  if (!is.na(match(vec_code_layer, d_depth_level_soil$code))) {

    df$layer_limit_superior[j] <-
        d_depth_level_soil$layer_limit_superior[match(vec_code_layer,
                                                      d_depth_level_soil$code)]
  }

  # Special case: German plots from 1995/1996 with non-conform code_layers
  # with layer limits different from the theoretical ones

  if (survey_form == "so_som") {

  if (df$partner_code[j] == 3204 &&
      df$survey_year[j] < 2000 &&
      vec_code_layer == "M25") {
    df$layer_limit_superior[j] <- 2
  }
}

}
}


  # Inferior

  # Check if vec_empty_layer_limit_inferior is not empty
  # and fill the empty inferior layer limit

if (!identical(vec_empty_layer_limit_inferior, integer(0))) {

  for (j in vec_empty_layer_limit_inferior) {

        if ("code_layer_orig" %in% names(df)) {
        vec_code_layer <- as.character(df$code_layer_orig[j])
        } else {
          vec_code_layer <- as.character(df$code_layer[j])
        }


    # Fill with theoretical layer limits of ICP Forests

    if (!is.na(match(vec_code_layer, d_depth_level_soil$code))) {

      df$layer_limit_inferior[j] <-
        d_depth_level_soil$layer_limit_inferior[match(vec_code_layer,
                                                      d_depth_level_soil$code)]

    }

    # Special case: German plots from 1995/1996 with non-conform code_layers
    # with layer limits different from the theoretical ones

    if (df$partner_code[j] == 3204 &&
        df$survey_year[j] < 2000) {

      if (vec_code_layer == "M02") {
        df$layer_limit_inferior[j] <- 2
      }

      if (vec_code_layer == "M25") {
        df$layer_limit_inferior[j] <- 5
      }
    }
}
}
}



## FSCC_2 Error reason: Typo observed ----

  # One value in the given profile seems to have the wrong sign

if (unique(df$unique_survey_repetition)[i] == "4_2009_503_1") {

  if (df$layer_limit_inferior_orig[vec[
    which(df$code_layer[vec] == "OF")]] == 3) {

  ind_error <- vec[which(df$code_layer[vec] == "OF")]

  # Store information about the inconsistency in "list_layer_inconsistencies"

rule_id <- "FSCC_2"
inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
  which(inconsistency_catalogue$rule_id == rule_id)]
inconsistency_type <- inconsistency_catalogue$inconsistency_type[
  which(inconsistency_catalogue$rule_id == rule_id)]

if ("code_layer_orig" %in% names(df)) {
  vec_code_layer <- as.character(df$code_layer_orig[ind_error])
  } else {
    vec_code_layer <- as.character(df$code_layer[ind_error])
  }

  list_layer_inconsistencies <- rbind(
    list_layer_inconsistencies,
    data.frame(survey_form = as.factor(rep(survey_form, length(ind_error))),
               partner = df$partner[ind_error],
               partner_code = df$partner_code[ind_error],
               country = df$country[ind_error],
               code_country = df$code_country[ind_error],
               survey_year = df$survey_year[ind_error],
               code_plot = df$code_plot[ind_error],
               plot_id = df$plot_id[ind_error],
               code_layer_horizon_master = vec_code_layer,
               repetition_profile_pit_id = df$repetition[ind_error],
               code_line = df$code_line[ind_error],
               parameter = as.factor(rep("layer_limit_inferior",
                                         length(ind_error))),
               parameter_unit = as.factor(rep("cm", length(ind_error))),
               parameter_value = df$layer_limit_inferior_orig[ind_error],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record = rep(TRUE,
                                                          length(ind_error)),
               change_date = df$change_date[ind_error],
               download_date = rep(download_date_pir, length(ind_error))))

  # If "solve" is set to TRUE, correct the layer limit

  if (solve == TRUE) {
    df$layer_limit_inferior[ind_error] <- (-3)
    }
}
}






## FSCC_2 Error reason: Typo observed ----

# Typo observed: This layer has to be moved individually

if (unique(df$unique_survey_repetition)[i] == "9_2007_4_2") {

  ind_error <- vec[which(df$code_layer[vec] == "OFH")]

  if (df$layer_limit_inferior_orig[ind_error] == -1) {

    # If "solve" is set to TRUE, correct the layer limit

    if (solve == TRUE) {
      df$layer_limit_inferior[ind_error] <- 0
    }
  }

  if (df$layer_limit_superior_orig[ind_error] == -4) {

    # If "solve" is set to TRUE, correct the layer limit

    if (solve == TRUE) {
      df$layer_limit_superior[ind_error] <- -3
    }
  }

}








## FSCC_7 Error reason: none of the layers has layer limit information ----

  # Check if none of the layers has superior layer limit information

if (all(is.na(df$layer_limit_superior[vec]))) {

  # Store information about the inconsistency in "list_layer_inconsistencies"

rule_id <- "FSCC_7"
inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
  which(inconsistency_catalogue$rule_id == rule_id)]
inconsistency_type <- inconsistency_catalogue$inconsistency_type[
  which(inconsistency_catalogue$rule_id == rule_id)]

if ("code_layer_orig" %in% names(df)) {
  vec_code_layer <- as.character(df$code_layer_orig[vec])
  } else {
    vec_code_layer <- as.character(df$code_layer[vec])
  }

  list_layer_inconsistencies <- rbind(
     list_layer_inconsistencies,
     data.frame(survey_form = as.factor(rep(survey_form, length(vec))),
                partner = df$partner[vec],
                partner_code = df$partner_code[vec],
                country = df$country[vec],
                code_country = df$code_country[vec],
                survey_year = df$survey_year[vec],
                code_plot = df$code_plot[vec],
                plot_id = df$plot_id[vec],
                code_layer_horizon_master = vec_code_layer,
                repetition_profile_pit_id = df$repetition[vec],
                code_line = df$code_line[vec],
                parameter = as.factor(rep("layer_limit_superior", length(vec))),
                parameter_unit = as.factor(rep("cm", length(vec))),
                parameter_value = df$layer_limit_superior_orig[vec],
                inconsistency_reason = inconsistency_reason,
                inconsistency_type = inconsistency_type,
                rule_id = rule_id,
                non_duplicated_error_type_per_record = rep(TRUE, length(vec)),
                change_date = df$change_date[vec],
                download_date = rep(download_date_pir, length(vec))))

  }


   # Check if none of the layers has inferior layer limit information

if (all(is.na(df$layer_limit_inferior[vec]))) {

   # Store information about the inconsistency in "list_layer_inconsistencies"

   rule_id <- "FSCC_7"
   inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
     which(inconsistency_catalogue$rule_id == rule_id)]
   inconsistency_type <- inconsistency_catalogue$inconsistency_type[
     which(inconsistency_catalogue$rule_id == rule_id)]

   if ("code_layer_orig" %in% names(df)) {
     vec_code_layer <- as.character(df$code_layer_orig[vec])
     } else {
       vec_code_layer <- as.character(df$code_layer[vec])
     }

  list_layer_inconsistencies <- rbind(
    list_layer_inconsistencies,
    data.frame(survey_form = as.factor(rep(survey_form, length(vec))),
               partner = df$partner[vec],
               partner_code = df$partner_code[vec],
               country = df$country[vec],
               code_country = df$code_country[vec],
               survey_year = df$survey_year[vec],
               code_plot = df$code_plot[vec],
               plot_id = df$plot_id[vec],
               code_layer_horizon_master = vec_code_layer,
               repetition_profile_pit_id = df$repetition[vec],
               code_line = df$code_line[vec],
               parameter = as.factor(rep("layer_limit_inferior", length(vec))),
               parameter_unit = as.factor(rep("cm", length(vec))),
               parameter_value = df$layer_limit_inferior_orig[vec],
               inconsistency_reason = inconsistency_reason,
               inconsistency_type = inconsistency_type,
               rule_id = rule_id,
               non_duplicated_error_type_per_record = rep(TRUE, length(vec)),
               change_date = df$change_date[vec],
               download_date = rep(download_date_pir, length(vec))))

  }




# Step intermediate - layer_type conversion for mineral to peat ----
# This depends on the TOC content.

# Exceptions:
# - if some repetitions of the same plot_id and the same layer have a TOC
#   within the mineral range (manually verified)
# - if the TOC has been confirmed as extreme but correct in the PIRs

# Note: We don't do the other way around: converting forest floor layers
# with too low TOC to mineral.

if (any(df$layer_type[vec] == "mineral")) {

  # organic_carbon_total - based on ranges_qaqc
  upper_limit_mineral <- 150
  upper_limit_mineral_top <- 200
  lower_limit_organic <- 93

  prof_i <- df[vec, ] %>%
    filter(!is.na(layer_limit_superior) |
             !is.na(layer_limit_inferior)) %>%
    mutate(depth_avg =
             rowMeans(select(., layer_limit_superior, layer_limit_inferior),
                      na.rm = TRUE)) %>%
    arrange(depth_avg) %>%
    filter(!is.na(organic_carbon_total)) %>%
    select(code_layer, layer_type, layer_limit_superior, layer_limit_inferior,
           organic_carbon_total) %>%
    mutate(convert_layer_type = case_when(
      layer_limit_superior == 0 &
        layer_type == "mineral" &
        organic_carbon_total > upper_limit_mineral_top ~ "to_organic",
      layer_limit_superior > 0 &
        layer_type == "mineral" &
        organic_carbon_total > upper_limit_mineral_top ~ "to_organic",
      layer_limit_superior > 0 &
        layer_type == "mineral" &
        organic_carbon_total > upper_limit_mineral &
        (lag(layer_type != "mineral",
              default = TRUE) |
           lag(organic_carbon_total > upper_limit_mineral_top,
                default = TRUE)) ~ "to_organic",
      TRUE ~ NA_character_))

  if (any(!is.na(pull(prof_i, convert_layer_type)))) {

    # Which layers may have to be converted?

    # code_layer

    layers_to_check <-
      pull(filter(prof_i, !is.na(convert_layer_type)), code_layer)

    # row indices

    vec_min_to_peat <-
      vec[which(df$code_layer[vec] %in% layers_to_check)]

    # Exception: don't convert if not all depths of this plot_id
    # are in the organic range.

    # They should belong to those unique surveys (as manually evaluated):

    so_som_dont_convert <- c("1_2011_92", "5_1995_17", "5_1997_2",
                             "60_2004_2", "50_1997_2")
    s1_som_dont_convert <- c("58_1995_285", "7_2007_208", "6_2008_62",
                             "6_2008_124", "6_2008_135", "6_2008_156",
                             "6_2008_158")

    # Retrieve those records of the same unique_survey

    lines_to_be_consistent <- df %>%
      filter(unique_survey ==
               unique(pull(df[vec_min_to_peat, ], unique_survey))) %>%
      filter(code_layer %in% layers_to_check)

    if (all(!is.na(pull(lines_to_be_consistent, organic_carbon_total))) &&
        any(pull(lines_to_be_consistent, organic_carbon_total) >
            upper_limit_mineral) &&
        any(pull(lines_to_be_consistent, organic_carbon_total) <
            upper_limit_mineral) &&
        ((survey_form == "so_som" &&
          !unique(pull(lines_to_be_consistent, unique_survey)) %in%
          so_som_dont_convert) ||
         (survey_form == "s1_som" &&
          !unique(pull(lines_to_be_consistent, unique_survey)) %in%
          s1_som_dont_convert))) {

      cat(paste0(unique(pull(lines_to_be_consistent, unique_survey)), "\n"))

      if (survey_form == "so_som") {

        # assertthat::assert_that(
        #   unique(pull(df[vec_min_to_peat, ], unique_survey)) %in%
        #     so_som_dont_convert)
      }

      if (survey_form == "s1_som") {

        # assertthat::assert_that(
        #   unique(pull(df[vec_min_to_peat, ], unique_survey)) %in%
        #     s1_som_dont_convert)
      }
    }

    # Convert the layer types and names if all conditions are met

    if ( # TOC is not confirmed as extreme by partners
         (!(any(pull(lines_to_be_consistent, code_line) %in%
               pir_toc_confirmed))) &&
         # All TOC values in different repetitions should be higher than 150
         # and known
         (all(!is.na(pull(lines_to_be_consistent, organic_carbon_total))) &&
          all(pull(lines_to_be_consistent, organic_carbon_total) >
              upper_limit_mineral)) &&
         # The unique_survey should not be manually listed as "dont convert"
         # (due to consistency reasons)
         ((survey_form == "so_som" &&
           !(any(unique(pull(lines_to_be_consistent, unique_survey)) %in%
                 so_som_dont_convert))) ||
          (survey_form == "s1_som" &&
           !(any(unique(pull(lines_to_be_consistent, unique_survey)) %in%
                 s1_som_dont_convert))))) {

      # Immediately convert all layers in lines_to_be_consistent

      df <- df %>%
        mutate(across(c(code_layer,
                        unique_survey_layer,
                        unique_layer,
                        unique_layer_repetition),
                      ~ ifelse(code_line %in% pull(lines_to_be_consistent,
                                                   code_line),
                               str_replace_all(., "M", "H"),
                               .))) %>%
        mutate(layer_type = ifelse(.data$code_line %in%
                                     pull(lines_to_be_consistent,
                                          code_line),
                                   "peat",
                                   .data$layer_type))

    }
  }
}







# Step intermediate - layer_type conversion for peat layers to ff ----
# If needed

# Rule for organic H layers:
# - if code_layer is H and no layer limits are provided,
#   the layer should be in the forest floor. Change layer_type to forest_floor.
# - if organic H layers are < 40 cm thick in total (and below any
#   forest floor or above any mineral soil), this layer(s) should be
#   considered as the forest floor. Change layer_type to forest_floor.
# - if organic H layers are >= 40 cm thick in total, they can be
#   considered as actual peat layers.

# Firstly, a specific case for H-layers of specific partners
# that belong to the forest floor
# (manually checked and asserted in the beginning of this script)

if (length(vec) >= 2) {

  if (unlist(strsplit(survey_form, "_"))[1] == "so") {

    if (any(df$layer_type[vec] == "peat")) {

      vec_peat_to_ff <- vec[which((df$layer_type[vec] == "peat") &
                                    (!df$code_layer[vec] %in%
                                       c("H05", "H51", "H12", "H24")) &
                                    (is.na(df$layer_limit_superior[vec]) |
                                       is.na(df$layer_limit_inferior[vec])))]

      if (!identical(vec_peat_to_ff, integer(0))) {
        df$layer_type[vec_peat_to_ff] <- "forest_floor"
      }
    }
  }

  if (any(df$layer_type[vec] == "peat") &&
      # Make an exception for some profiles
      ((survey_form == "so_som" &&
        !unique(df$unique_survey[vec]) %in% so_som_dont_move) ||
       (survey_form == "s1_som" &&
        !unique(df$unique_survey[vec]) %in% s1_som_dont_move))) {

    vec_peat <- vec[which(df$layer_type[vec] == "peat")]

    # Gap-fill layer limits of H05, H51 etc if needed

    vec_peat_empty <- vec[which(is.na(df$layer_limit_superior[vec_peat]))]

    if (!identical(vec_peat_empty, integer(0))) {

      df$layer_limit_superior[vec_peat_empty] <- case_when(
        df$code_layer[vec_peat_empty] == "H05" ~ 0,
        df$code_layer[vec_peat_empty] == "H51" ~ 5,
        df$code_layer[vec_peat_empty] == "H12" ~ 10,
        df$code_layer[vec_peat_empty] == "H24" ~ 20,
        df$code_layer[vec_peat_empty] == "H48" ~ 40,
        TRUE ~ df$layer_limit_superior[vec_peat_empty])
    }

    vec_peat_empty <- vec[which(is.na(df$layer_limit_inferior[vec_peat]))]

    if (!identical(vec_peat_empty, integer(0))) {

      df$layer_limit_inferior[vec_peat_empty] <- case_when(
        df$code_layer[vec_peat_empty] == "H05" ~ 5,
        df$code_layer[vec_peat_empty] == "H51" ~ 10,
        df$code_layer[vec_peat_empty] == "H12" ~ 20,
        df$code_layer[vec_peat_empty] == "H24" ~ 40,
        df$code_layer[vec_peat_empty] == "H48" ~ 80,
        TRUE ~ df$layer_limit_superior[vec_peat_empty])
    }

    source("./src/functions/summarise_profile_per_depth_layer_type.R")

    df_sub <- summarise_profile_per_depth_layer_type(df_profile = df[vec, ])


    vec_df_sub_peat <- which(df_sub$layer_type == "peat")

    for (j in vec_df_sub_peat) {

      assertthat::assert_that(!is.na(df_sub$total_thickness[j]))

      # First row (i.e. on top)
      if (((j == 1 ||
            # Below forest_floor
            df_sub$layer_type[j - 1] == "forest_floor") &&
           # The so-called below-ground should be at least 40 cm deep,
           # otherwise we can't know for sure if the peat layer is thicker
           # than 40
           (sum(df_sub$total_thickness, na.rm = TRUE) >= 40)) &&
          # The total_thickness is less than 40
          df_sub$total_thickness[j] < 40) {

        # Then it should be considered as forest_floor

        vec_peat_to_ff <-
          vec_peat[which(
            (df$layer_limit_superior[vec_peat] >=
               df_sub$layer_limit_superior[j]) &
              df$layer_limit_inferior[vec_peat] <=
              df_sub$layer_limit_inferior[j])]

        df$layer_type[vec_peat_to_ff] <- "forest_floor"
      }
    }
  }


  # Assure that no forest floors are thicker than 40 cm

  # if (any(df$layer_type[vec] == "forest_floor")) {
  #
  #   source("./src/functions/summarise_profile_per_depth_layer_type.R")
  #
  #   df_sub <- summarise_profile_per_depth_layer_type(df_profile = df[vec, ])
  #
  #   # Take the first forest floor layer only,
  #   # because there may be buried forest floor layers
  #
  #   if ("forest_floor" %in% df_sub$layer_type) {
  #   assertthat::assert_that(
  #     df_sub$total_thickness[which(df_sub$layer_type ==
  #      "forest_floor")][1] <= 40)
  #   }
  # }

  # Forest floors thicker than 40 cm ----

  if (any(df$layer_type[vec] == "forest_floor")) {

    vec_ff <- vec[which(df$layer_type[vec] == "forest_floor")]

    source("./src/functions/summarise_profile_per_depth_layer_type.R")

    df_sub <-
      summarise_profile_per_depth_layer_type(df[vec, ])

    if ("forest_floor" %in% df_sub$layer_type &&
        # Make an exception for some profiles
        ((survey_form == "so_som" &&
          !unique(df$unique_survey[vec]) %in% so_som_dont_move) ||
         (survey_form == "s1_som" &&
          !unique(df$unique_survey[vec]) %in% s1_som_dont_move))) {

      # Take the first forest floor layer only,
      # because there may be buried forest floor layers

      # assertthat::assert_that(
      #   df_sub$total_thickness[which(
      #     df_sub$layer_type == "forest_floor")][1] <= 40)

      if (df_sub$total_thickness[which(
        df_sub$layer_type == "forest_floor")][1] > 40) {

        # Then it should be considered as peat?
        # Manually confirmed for the profiles in s1_pfh

        if (abs(df_sub$layer_limit_superior[1]) < 40 &&
            (df_sub$layer_limit_inferior[1] > 0)) {

          # In this case, the profile shouldn't move, but the
          # below-ground layers should become peat

          vec_ff_to_peat <-
            vec_ff[which(
              (df$layer_limit_superior[vec_ff] >=
                 0) &
                df$layer_limit_inferior[vec_ff] <=
                df_sub$layer_limit_inferior[1])]

        } else {

          # Else, everything should become peat

          vec_ff_to_peat <-
            vec_ff[which(
              (df$layer_limit_superior[vec_ff] >=
                 df_sub$layer_limit_superior[1]) &
                df$layer_limit_inferior[vec_ff] <=
                df_sub$layer_limit_inferior[1])]
        }

        df$layer_type[vec_ff_to_peat] <- "peat"

        # vec_profiles_thick_ff <- c(vec_profiles_thick_ff,
        #                            unique(df$unique_survey_profile)[i])

      }
    }

    vec_ff <- vec[which(df$layer_type[vec] == "forest_floor")]

  }
}







# Step 3: Rank the layers (layer_number) from top to bottom ----
   # based on their layer limits

# Rank forest floors with any unknown layer limits ----

if (!identical(vec_ff, integer(0)) &&
   (any(is.na(df$layer_limit_superior[vec_ff])) ||
    any(is.na(df$layer_limit_inferior[vec_ff])))) {

  # Assert that there are maximum three forest floor layers

  assertthat::assert_that(length(vec_ff) <= 3,
                          msg = paste0("More than three forest floor layers ",
                                       "in profile ",
                                       pull(df[vec_ff[1], ],
                                            unique_survey_repetition)))

  ## If there is one forest floor layer ----

  if (length(vec_ff) == 1) {
    df$layer_number[vec_ff] <- 1
  }



  ## If there are two forest floor layers ----
  # Use the data frame "layer_number_two_forest_floor_layers"

  if (length(vec_ff) == 2) {

    layers <- df$code_layer[vec_ff]

    # Assert that the forest floor combination exists in
    # layer_number_two_forest_floor_layers

    # Generate all combinations of two unique values
    all_combinations <-
      apply(
        layer_number_two_forest_floor_layers[,
                   3:ncol(layer_number_two_forest_floor_layers)], 2,
        FUN = function(x) paste(x, collapse = "_"))

    # Combine the original combinations with their opposites
    all_combinations <-
      unique(c(all_combinations,
               unlist(map(all_combinations,
                          .f = function(x) paste(rev(strsplit(x, "_")[[1]]),
                                                 collapse = "_")))))

    assertthat::assert_that(
      paste(df$code_layer[vec_ff], collapse = "_") %in% all_combinations,
      msg = paste0("Unknown combination of two forest floor layers ('",
                   df$code_layer[vec_ff[1]], "' and '",
                   df$code_layer[vec_ff[2]],
                   "')."))


    # If the forest floor layers are called "O" and "O2",
    # the forest floor layer sequence depends on the partner code
    # (based on exploration of the % OC data)

    if (!is.na(match(layers[1],
                     layer_number_two_forest_floor_layers[, 3])) &&
        !is.na(match(layers[2],
                     layer_number_two_forest_floor_layers[, 3]))) {

      # If the profile has partner_code 3204
      if (df$partner_code[vec[1]] == 3204) {

        if (df$code_layer[vec_ff[1]] ==
            layer_number_two_forest_floor_layers[1, 4]) {

          df$layer_number[vec_ff[1]] <- 1
          df$layer_number[vec_ff[2]] <- 2

        } else {

          df$layer_number[vec_ff[1]] <- 2
          df$layer_number[vec_ff[2]] <- 1

        }
      } else {

        # If the profile does not have partner_code 3204
        if (df$code_layer[vec_ff[1]] ==
            layer_number_two_forest_floor_layers[1, 3]) {

          df$layer_number[vec_ff[1]] <- 1
          df$layer_number[vec_ff[2]] <- 2

        } else {
          df$layer_number[vec_ff[1]] <- 2
          df$layer_number[vec_ff[2]] <- 1

        }
      }
    } else {

      # If the forest floor layers are not called "O" and "O2"

      for (j in c(5:ncol(layer_number_two_forest_floor_layers))) {

        if (!is.na(match(layers[1],
                         layer_number_two_forest_floor_layers[, j])) &&
            !is.na(match(layers[2],
                         layer_number_two_forest_floor_layers[, j]))) {

          if (df$code_layer[vec_ff[1]] ==
              layer_number_two_forest_floor_layers[1, j]) {

            df$layer_number[vec_ff[1]] <- 1
            df$layer_number[vec_ff[2]] <- 2

          } else {

            df$layer_number[vec_ff[1]] <- 2
            df$layer_number[vec_ff[2]] <- 1

          }
        }
      }
    }
  } # End of "if two forest floor layers"


  ## If there are three forest floor layers ----
  # Use the data frame "layer_number_three_forest_floor_layers"

  if (length(vec_ff) == 3) {

    layers <- df$code_layer[vec_ff]

    # Assert that the combination of three forest floor layers is known

    # Test if all elements of layers are in the dataframe

    sorted_combinations <-
      apply(layer_number_three_forest_floor_layers[, -1], 2,
            function(x) paste(sort(x), collapse = "_"))

    assertthat::assert_that(
      paste(sort(layers), collapse = "_") %in% sorted_combinations,
      msg = paste0("Unknown combination of three forest floor layers ('",
                   df$code_layer[vec_ff[1]], "', '",
                   df$code_layer[vec_ff[2]], "' and '",
                   df$code_layer[vec_ff[3]],
                   "')."))

    col_table <-
      sorted_combinations[which(sorted_combinations ==
                                  paste(sort(layers), collapse = "_"))]

    col_table <-
      which(names(layer_number_three_forest_floor_layers) == names(col_table))


    df$layer_number[vec_ff[
      match(layer_number_three_forest_floor_layers[1, col_table],
            layers)]] <- 1
    df$layer_number[vec_ff[
      match(layer_number_three_forest_floor_layers[2, col_table],
            layers)]] <- 2
    df$layer_number[vec_ff[
      match(layer_number_three_forest_floor_layers[3, col_table],
            layers)]] <- 3

  } # End of "if three forest floor layers"



  # If any FF layer limits are known and any FF layer limits are unknown

  if (any(!is.na(df$layer_limit_superior[vec_ff])) &&
      any(!is.na(df$layer_limit_inferior[vec_ff]))) {

    # Assert that the sequence of the known layer limits is the same
    # like the sequence of the layer number

    assertthat::assert_that(identical(
      rank(df$layer_number[vec_ff[
        which(!is.na(df$layer_limit_superior[vec_ff]))]],
        na.last = "keep"),
      rank(df$layer_limit_superior[vec_ff[
        which(!is.na(df$layer_limit_superior[vec_ff]))]],
        na.last = "keep")))


    assertthat::assert_that(identical(
      rank(df$layer_number[vec_ff[
        which(!is.na(df$layer_limit_inferior[vec_ff]))]],
        na.last = "keep"),
      rank(df$layer_limit_inferior[vec_ff[
        which(!is.na(df$layer_limit_inferior[vec_ff]))]],
        na.last = "keep")))

  }


}  # End of "if forest floors with any unknown layer limits"


# Rank any remaining layers based on layer limits ----

if (!identical(which(is.na(df$layer_number[vec]) &
                    !is.na(df$layer_limit_superior[vec]) &
                    !is.na(df$layer_limit_inferior[vec])),
            integer(0))) {

  # Determine maximum layer number at the moment, if any

  if (all(is.na(df$layer_number[vec]))) {
    layer_number_max <- 0
  } else {
    layer_number_max <- as.numeric(max(df$layer_number[vec],
                                       na.rm = TRUE))
  }

  # Determine records with layer limits to be ranked

  vec_nonempty <- vec[which(is.na(df$layer_number[vec]) &
                              !is.na(df$layer_limit_superior[vec]) &
                              !is.na(df$layer_limit_inferior[vec]))]


  # Check if the ranking based on layer_limit_superior is the same
  # like the ranking based on layer_limit_inferior

  if (identical(rank(as.numeric(df$layer_limit_superior[vec_nonempty]),
                     na.last = "keep"),
                rank(as.numeric(df$layer_limit_inferior[vec_nonempty]),
                     na.last = "keep"))) {

    # Rank the layers based on layer_limit_superior

    df$layer_number[vec_nonempty] <-
      rank(as.numeric(df$layer_limit_superior[vec_nonempty]),
           na.last = "keep") +
      layer_number_max

  } else {

    # If the ranking based on layer_limit_superior is not the same
    # like the ranking based on layer_limit_inferior

    # Check for redundant layers

    source("./src/functions/get_redundant_layers.R")

    redundant_layers <- get_redundant_layers(
      layers = df$code_layer[vec_nonempty],
      superior_layer_limits = df$layer_limit_superior[vec_nonempty],
      inferior_layer_limits = df$layer_limit_inferior[vec_nonempty],
      df_sub = as.data.frame(df[vec_nonempty, ]))

    # There should be any redundant layers
    # E.g. both M01 and M05 + M51 are reported

    assertthat::assert_that(
      nrow(redundant_layers) > 0 &&
        nrow(filter(redundant_layers, redundancy_type == 0)) > 0)

    redundant_layers <- redundant_layers %>%
      filter(redundancy_type == 0)

    # Determine the non-redundant records with layer limits

    vec_non_redundant <-
      vec_nonempty[which(!df$code_layer[vec_nonempty] %in%
          c(redundant_layers$layer,
            redundant_layers$combined_layer))]

    # Assert that the ranking based on layer_limit_superior is the same
    # like the ranking based on layer_limit_inferior

    assertthat::assert_that(
      (identical(rank(as.numeric(df$layer_limit_superior[vec_non_redundant]),
                      na.last = "keep"),
                 rank(as.numeric(df$layer_limit_inferior[vec_non_redundant]),
                      na.last = "keep"))))

    # Rank the layers based on layer_limit_superior

    df$layer_number[vec_non_redundant] <-
      rank(as.numeric(df$layer_limit_superior[vec_non_redundant]),
           na.last = "keep") +
      layer_number_max


    }

} # End of "rank based on layer limits"



# Check if there are any redundant layers left ----
# as an intermediate layer that can be omitted,
# most likely in the forest floor

if (any(!is.na(df$layer_number[vec])) &&
    (any(!is.na(df$layer_limit_superior[vec])) &&
     any(!is.na(df$layer_limit_inferior[vec])))) {

  layers_redundant <- df[vec, ] %>%
    filter(!is.na(layer_number)) %>%
    arrange(layer_number) %>%
    mutate(
      prev_layer_limit_inferior = lag(.data$layer_limit_inferior),
      next_layer_limit_superior = lead(.data$layer_limit_superior)) %>%
    filter(!is.na(prev_layer_limit_inferior) &
             !is.na(next_layer_limit_superior) &
             (round(prev_layer_limit_inferior, 1) ==
                round(next_layer_limit_superior, 1)))

  assertthat::assert_that(all(pull(layers_redundant, layer_type) != "mineral"))

  assertthat::assert_that(
    all(is.na(pull(layers_redundant, layer_limit_superior))) &&
      all(is.na(pull(layers_redundant, layer_limit_inferior))),
    msg = paste0("Redundant layer in profile '",
                 unique(df$unique_survey_repetition[vec]),
                 "' with layer limits reported. Check."))

  # Evaluate each of the rows

  for (j in seq_along(pull(layers_redundant, code_line))) {

    layers_redundant_j <- layers_redundant %>%
      filter(code_line == pull(layers_redundant, code_line)[j])

    ind_j <- which(df$code_line == pull(layers_redundant, code_line)[j])

    # If the organic_layer_weight or the organic_carbon_content
    # are known:

    # if (!is.na(pull(layers_redundant_j, organic_layer_weight)) ||
    #     !is.na(pull(layers_redundant_j, organic_carbon_total))) {
    #
    #   # Place it on top of the profile
    #
    #   df$layer_number[ind_j] <- min(df$layer_number[vec], na.rm = TRUE) - 1
    #
    # } else {

      # If no organic_layer_weight and organic_carbon_total are known
      # Consider the layer redundant

      df$layer_number[ind_j] <- NA
   # }
  }

  # Rank the layer numbers again

  vec_nonempty <- vec[which(!is.na(df$layer_number[vec]))]

  df$layer_number[vec_nonempty] <-
    rank(as.numeric(df$layer_number[vec_nonempty]),
         na.last = "keep")

} # End of "if any additional redundant layers"








  # # Check if at least one of the layers has layer limit information
  #
  # if (any(!is.na(df$layer_limit_inferior[vec]) &
  #         !is.na(df$layer_limit_superior[vec]))) {
  #
  # # Which layers have layer limit information?
  #
  # # Rank forest floors without layer limit information
  #
  #   if (!identical(vec_ff, integer(0)) &&
  #       (all(is.na(df$layer_limit_inferior[vec_ff])) ||
  #       all(is.na(df$layer_limit_superior[vec_ff])))) {
  #
  #   # If there is one forest floor layer
  #
  #     if (length(vec_ff) == 1) {
  #       df$layer_number[vec_ff] <- 1
  #       } else
  #
  #    # If there are two forest floor layers
  #    # use the data frame "layer_number_two_forest_floor_layers"
  #
  #       if (length(vec_ff) == 2) {
  #
  #
  #         layers <- df$code_layer[vec_ff]
  #
  #       # Assert that the forest floor combination exists in
  #       # layer_number_two_forest_floor_layers
  #
  #         # Generate all combinations of two unique values
  #         all_combinations <-
  #           apply(
  #             layer_number_two_forest_floor_layers[,
  #                       3:ncol(layer_number_two_forest_floor_layers)], 2,
  #                 FUN = function(x) paste(x, collapse = "_"))
  #
  #         # Combine the original combinations with their opposites
  #         all_combinations <-
  #           unique(c(all_combinations,
  #                    unlist(map(all_combinations,
  #                        .f = function(x) paste(rev(strsplit(x, "_")[[1]]),
  #                                               collapse = "_")))))
  #
  #         assertthat::assert_that(
  #           paste(df$code_layer[vec_ff], collapse = "_") %in% all_combinations,
  #           msg = paste0("Unknown combination of two forest floor layers ('",
  #                        df$code_layer[vec_ff[1]], "' and '",
  #                        df$code_layer[vec_ff[2]],
  #                        "')."))
  #
  #
  #       # If the forest floor layers are called "O" and "O2",
  #       # the forest floor layer sequence depends on the partner code
  #       # (based on exploration of the % OC data)
  #
  #       if (!is.na(match(layers[1],
  #                        layer_number_two_forest_floor_layers[, 3])) &&
  #           !is.na(match(layers[2],
  #                        layer_number_two_forest_floor_layers[, 3]))) {
  #
  #       # If the profile has partner_code 3204
  #       if (df$partner_code[vec[1]] == 3204) {
  #
  #       if (df$code_layer[vec_ff[1]] ==
  #           layer_number_two_forest_floor_layers[1, 4]) {
  #
  #               df$layer_number[vec_ff[1]] <- 1
  #               df$layer_number[vec_ff[2]] <- 2
  #
  #               } else {
  #
  #               df$layer_number[vec_ff[1]] <- 2
  #               df$layer_number[vec_ff[2]] <- 1
  #
  #               }
  #         } else {
  #
  #       # If the profile does not have partner_code 3204
  #       if (df$code_layer[vec_ff[1]] ==
  #            layer_number_two_forest_floor_layers[1, 3]) {
  #
  #               df$layer_number[vec_ff[1]] <- 1
  #               df$layer_number[vec_ff[2]] <- 2
  #
  #               } else {
  #               df$layer_number[vec_ff[1]] <- 2
  #               df$layer_number[vec_ff[2]] <- 1
  #
  #               }
  #         }
  #         } else {
  #
  #       # If the forest floor layers are not called "O" and "O2"
  #
  #        for (j in c(5:ncol(layer_number_two_forest_floor_layers))) {
  #
  #               if (!is.na(match(layers[1],
  #                                layer_number_two_forest_floor_layers[, j])) &&
  #                    !is.na(match(layers[2],
  #                                 layer_number_two_forest_floor_layers[, j]))) {
  #
  #               if (df$code_layer[vec_ff[1]] ==
  #                   layer_number_two_forest_floor_layers[1, j]) {
  #
  #                   df$layer_number[vec_ff[1]] <- 1
  #                   df$layer_number[vec_ff[2]] <- 2
  #
  #                   } else {
  #                   df$layer_number[vec_ff[1]] <- 2
  #                   df$layer_number[vec_ff[2]] <- 1
  #
  #                   }
  #                 }
  #          }
  #       }
  #       } else
  #
  #
  #   # If there are three forest floor layers
  #   # use the data frame "layer_number_three_forest_floor_layers"
  #
  #       if (length(vec_ff) == 3) {
  #
  #       layers <- df$code_layer[vec_ff]
  #
  #       # Assert that the combination of three forest floor layers is known
  #
  #       # Test if all elements of layers are in either the
  #       # second or third column
  #
  #       assertthat::assert_that(
  #         all(layers %in% layer_number_three_forest_floor_layers[, 2]) ||
  #         all(layers %in% layer_number_three_forest_floor_layers[, 3]),
  #         msg = paste0("Unknown combination of three forest floor layers ('",
  #                      df$code_layer[vec_ff[1]], "', '",
  #                      df$code_layer[vec_ff[2]], "' and '",
  #                      df$code_layer[vec_ff[3]],
  #                      "')."))
  #
  #       col_table <- ifelse(
  #         all(layers %in% layer_number_three_forest_floor_layers[, 2]),
  #         2,
  #         ifelse(
  #           all(layers %in% layer_number_three_forest_floor_layers[, 3]),
  #           3,
  #           NA)) # In theory NA won't appear
  #
  #
  #       df$layer_number[vec_ff[
  #         match(layer_number_three_forest_floor_layers[1, col_table],
  #               layers)]] <- 1
  #       df$layer_number[vec_ff[
  #         match(layer_number_three_forest_floor_layers[2, col_table],
  #               layers)]] <- 2
  #       df$layer_number[vec_ff[
  #         match(layer_number_three_forest_floor_layers[3, col_table],
  #               layers)]] <- 3
  #
  #       }
  #     }
  #
  #
  #
  #
  #
  #
  #
  #
  #   # Rank forest floor layers with layer limit information
  #
  #   vec_ff_to_rank <- vec[which(df$layer_type[vec] == "forest_floor" &
  #                                 is.na(df$layer_number[vec]) &
  #                                 !is.na(df$layer_limit_superior[vec]) &
  #                                 !is.na(df$layer_limit_inferior[vec]) &
  #                                 df$layer_limit_superior[vec] <= 0 &
  #                                 df$layer_limit_inferior[vec] <= 0)]
  #
  #   if (!identical(vec_ff_to_rank,
  #                  integer(0))) {
  #
  #     if (all(is.na(df$layer_number[vec]))) {
  #       layer_number_max <- 0
  #     } else {
  #       layer_number_max <- as.numeric(max(df$layer_number[vec],
  #                                          na.rm = TRUE))
  #     }
  #
  #     # Rank the forest floor layers based on layer_limit_superior
  #
  #     df$layer_number[vec_ff_to_rank] <-
  #       rank(as.numeric(df$layer_limit_superior[
  #         vec_ff_to_rank]),
  #         na.last = "keep") +
  #       layer_number_max
  #
  #   }
  #
  #   # Rank the remaining layers that have layer limit information
  #
  #  # Check which layers have layer limit information but no layer number yet
  #
  #       # Check if there are any layers which do not have a layer number
  #       # but which do have known layer limits
  #
  #     if (!identical(which(is.na(df$layer_number[vec]) &
  #                          !is.na(df$layer_limit_superior[vec]) &
  #                          !is.na(df$layer_limit_inferior[vec])),
  #                    integer(0))) {
  #
  #     vec_nonempty <- vec[which(is.na(df$layer_number[vec]) &
  #                                !is.na(df$layer_limit_superior[vec]) &
  #                                !is.na(df$layer_limit_inferior[vec]))]
  #
  #   # Check if any layer_number(s) have already been assigned for forest
  #   # floor layers in the given profile by means of 'layer_number_max',
  #   # which equals the maximum existing layer number or 0
  #
  #         if (all(is.na(df$layer_number[vec]))) {
  #           layer_number_max <- 0
  #           } else {
  #
  #            # Maximum layer number that has already been assigned
  #
  #         layer_number_max <- as.numeric(max(df$layer_number[vec],
  #                                            na.rm = TRUE))
  #         }
  #
  #         vec_nonempty_remaining <- vec_nonempty
  #
  #
  #         # "M01" overlaps with "M05" and "M51"
  #         # If these three layers co-exist:
  #         # - In "df$layer_number", "M01" is hence considered "redundant",
  #         #   Because of which it doesn't get a layer number in
  #         #   "df$layer_number"
  #
  #         if (!identical(which(df$code_layer[vec_nonempty] == "M01"),
  #                        integer(0)) &&
  #             !identical(which(df$code_layer[vec_nonempty] == "M05"),
  #                        integer(0)) &&
  #             !identical(which(df$code_layer[vec_nonempty] == "M51"),
  #                        integer(0))) {
  #
  #         df$layer_number[vec_nonempty[
  #           which(df$code_layer[vec_nonempty] == "M05")]] <-
  #            1 + layer_number_max
  #
  #          df$layer_number[vec_nonempty[
  #            which(df$code_layer[vec_nonempty] == "M51")]] <-
  #            2 + layer_number_max
  #
  #
  #          # Update 'vec_nonempty_remaining' so that it no longer contains
  #          # the row indices of these layers
  #
  #          vec_nonempty_remaining <- vec_nonempty[
  #            which(df$code_layer[vec_nonempty] != "M05" &
  #                  df$code_layer[vec_nonempty] != "M51" &
  #                  df$code_layer[vec_nonempty] != "M01")]
  #          }
  #
  #         if (!identical(vec_nonempty_remaining, integer(0))) {
  #
  #          # Check if the ranking based on layer_limit_superior is the same
  #          # like the ranking based on layer_limit_inferior
  #
  #          if (identical(rank(as.numeric(df$layer_limit_superior[
  #                                        vec_nonempty_remaining]),
  #                             na.last = "keep"),
  #                        rank(as.numeric(df$layer_limit_inferior[
  #                                        vec_nonempty_remaining]),
  #                             na.last = "keep"))) {
  #
  #              # Update 'layer_number_max'
  #
  #              if (all(is.na(df$layer_number[vec]))) {
  #
  #                layer_number_max <- 0
  #
  #              } else {
  #
  #                layer_number_max <- as.numeric(max(df$layer_number[vec],
  #                                                   na.rm = TRUE))
  #                  }
  #
  #              # Rank the remaining layers based on layer_limit_superior
  #
  #               df$layer_number[vec_nonempty_remaining] <-
  #                 rank(as.numeric(df$layer_limit_superior[
  #                                       vec_nonempty_remaining]),
  #                      na.last = "keep") +
  #                 layer_number_max
  #
  #               } else {
  #
  #       # If there is a difference between ranking layer_limit_superior
  #       # vs ranking layer_limit_inferior:
  #
  #          source("./src/functions/get_redundant_layers.R")
  #
  #          redundant_layers <- get_redundant_layers(
  #                     layers =
  #                       df$code_layer[vec_nonempty_remaining],
  #                     superior_layer_limits =
  #                       df$layer_limit_superior[vec_nonempty_remaining],
  #                     inferior_layer_limits =
  #                       df$layer_limit_inferior[vec_nonempty_remaining],
  #                     df_sub =
  #                       as.data.frame(df[vec_nonempty_remaining, ])) %>%
  #            filter(redundancy_type == 0)
  #
  #          vec_non_redundant <-
  #            vec_nonempty_remaining[which(
  #              !df$code_layer[vec_nonempty_remaining] %in%
  #                c(redundant_layers$layer,
  #                  redundant_layers$combined_layer))]
  #
  #          # Rank the remaining non-redundant layers
  #          # based on layer_limit_superior
  #
  #          df$layer_number[vec_non_redundant] <-
  #            rank(as.numeric(df$layer_limit_superior[
  #                                   vec_non_redundant]),
  #                 na.last = "keep") +
  #            layer_number_max
  #               }
  #     }
  #     }
  #     }



## FSCC_10: Error reason: there is a gap between adjacent layers   ----

    # Check whether layers have layer limit information and a layer_number

vec_nonempty <- vec[which(!is.na(df$layer_limit_superior[vec]) &
                          !is.na(df$layer_limit_inferior[vec]) &
                          !is.na(df$layer_number[vec]))]

    # Order the row indices

vec_layer_number <- vec_nonempty[order(df$layer_number[vec_nonempty])]

    # Check if more than one layer has the required information

    if (length(vec_nonempty) > 1) {

    # Check for all layers (row indices)
    # whether the inferior layer limit does not equal the superior layer limit
    # of the next layer

    for (j in vec_layer_number[1:(length(vec_layer_number) - 1)]) {

          if (df$layer_limit_inferior[j] !=
               df$layer_limit_superior[vec_layer_number[
                 match(j, vec_layer_number) + 1]]) {

          j_next <- vec_layer_number[match(j, vec_layer_number) + 1]

    # Store information about the inconsistency in "list_layer_inconsistencies"

          # Inferior of current layer j

          # If the layer limit information has been updated in this script
          # i.e. if "layer_limit_inferior" does not equal
          # "layer_limit_inferior_orig":
          # Mention that this was based on a value "estimated by FSCC"

            if ((!is.na(df$layer_limit_inferior[j]) &&
                 is.na(df$layer_limit_inferior_orig[j])) ||
                (df$layer_limit_inferior[j] !=
                 df$layer_limit_inferior_orig[j])) {

               parameter_value_error <- paste0(df$layer_limit_inferior[j],
                                               " (estimated by FSCC)")

               } else {
                 parameter_value_error <- df$layer_limit_inferior_orig[j]
               }

          rule_id <- "FSCC_10"
          inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
            which(inconsistency_catalogue$rule_id == rule_id)]
          inconsistency_type <- inconsistency_catalogue$inconsistency_type[
            which(inconsistency_catalogue$rule_id == rule_id)]

          if ("code_layer_orig" %in% names(df)) {
            vec_code_layer <- as.character(df$code_layer_orig[j])
            } else {
              vec_code_layer <- as.character(df$code_layer[j])
            }

            list_layer_inconsistencies <- rbind(
               list_layer_inconsistencies,
               data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                          partner = df$partner[j],
                          partner_code = df$partner_code[j],
                          country = df$country[j],
                          code_country = df$code_country[j],
                          survey_year = df$survey_year[j],
                          code_plot = df$code_plot[j],
                          plot_id = df$plot_id[j],
                          code_layer_horizon_master = vec_code_layer,
                          repetition_profile_pit_id = df$repetition[j],
                          code_line = df$code_line[j],
                          parameter = as.factor(rep("layer_limit_inferior",
                                                    length(j))),
                          parameter_unit = as.factor(rep("cm", length(j))),
                          parameter_value = parameter_value_error,
                          inconsistency_reason = inconsistency_reason,
                          inconsistency_type = inconsistency_type,
                          rule_id = rule_id,
                          non_duplicated_error_type_per_record =
                            rep(TRUE, length(j)),
                          change_date = df$change_date[j],
                          download_date = rep(download_date_pir, length(j))))


            # Superior of next layer j_next

            # If the layer limit information has been updated in this script
            # i.e. if "layer_limit_superior" does not equal
            # "layer_limit_superior_orig":
            # Mention that this was based on a value "estimated by FSCC"

            if ((!is.na(df$layer_limit_superior[j_next]) &&
                 is.na(df$layer_limit_superior_orig[j_next])) ||
                (df$layer_limit_superior[j_next] !=
                 df$layer_limit_superior_orig[j_next])) {

               parameter_value_error <- paste0(df$layer_limit_superior[j_next],
                                               " (estimated by FSCC)")

               } else {
                 parameter_value_error <- df$layer_limit_superior_orig[j_next]
               }

            rule_id <- "FSCC_10"
            inconsistency_reason <-
              inconsistency_catalogue$inconsistency_reason[
                which(inconsistency_catalogue$rule_id == rule_id)]
            inconsistency_type <- inconsistency_catalogue$inconsistency_type[
              which(inconsistency_catalogue$rule_id == rule_id)]

            if ("code_layer_orig" %in% names(df)) {
              vec_code_layer <- as.character(df$code_layer_orig[j_next])
              } else {
                vec_code_layer <- as.character(df$code_layer[j_next])
              }

            list_layer_inconsistencies <- rbind(
               list_layer_inconsistencies,
               data.frame(survey_form = as.factor(rep(survey_form,
                                                      length(j_next))),
                          partner = df$partner[j_next],
                          partner_code = df$partner_code[j_next],
                          country = df$country[j_next],
                          code_country = df$code_country[j_next],
                          survey_year = df$survey_year[j_next],
                          code_plot = df$code_plot[j_next],
                          plot_id = df$plot_id[j_next],
                          code_layer_horizon_master = vec_code_layer,
                          repetition_profile_pit_id = df$repetition[j_next],
                          code_line = df$code_line[j_next],
                          parameter = as.factor(rep("layer_limit_superior",
                                                    length(j_next))),
                          parameter_unit = as.factor(rep("cm", length(j_next))),
                          parameter_value = parameter_value_error,
                          inconsistency_reason = inconsistency_reason,
                          inconsistency_type = inconsistency_type,
                          rule_id = rule_id,
                          non_duplicated_error_type_per_record =
                            rep(FALSE, length(j_next)),
                          change_date = df$change_date[j_next],
                          download_date = rep(download_date_pir,
                                              length(j_next))))

          }
    }
    }


# Is the zero line located between the forest floor vs mineral/peat? ----

#   The null line should be between
#   the forest floor (including those H layers just added;
#   negative layer limits) and the peat/mineral layers
#   (positive layer limits). Move null line in accordance if
#   necessary (by shifting the layers up or down and changing their
#   layer limits).

vec_non_redundant <- vec[which(!is.na(df$layer_number[vec]))]

source("./src/functions/summarise_profile_per_depth_layer_type.R")

if (!identical(vec_non_redundant, integer(0)) &&
    # Make an exception for some profiles
    ((survey_form == "so_som" &&
      !unique(df$unique_survey[vec]) %in% so_som_dont_move) ||
     (survey_form == "s1_som" &&
      !unique(df$unique_survey[vec]) %in% s1_som_dont_move))) {

df_sub <-
  summarise_profile_per_depth_layer_type(df_profile = df[vec_non_redundant, ])

diff_to_move <- 0

if (df_sub$layer_type[1] == "forest_floor" &&
    nrow(df_sub) > 1) {

  # If there is no gap between the forest floor and first below-ground layer
  # Ignore small gaps <= 0.1

  if (abs(df_sub$layer_limit_inferior[1] -
          df_sub$layer_limit_superior[2]) <= 0.1) {

    diff_to_move <- ifelse((df_sub$layer_limit_inferior[1] == 0 ||
                              df_sub$layer_limit_superior[2] == 0),
                           0,
                           mean(c(df_sub$layer_limit_inferior[1],
                                  df_sub$layer_limit_superior[2])))
  } else {

    # If there is a considerable gap, e.g. in "7_2007_10_2"

    # Consider the below-ground layer limit as reference
    # and move the forest floor layers to the below-ground

    vec_to_move <-
      vec_non_redundant[which(
        (df$layer_limit_superior[vec_non_redundant] >=
           df_sub$layer_limit_superior[1]) &
          df$layer_limit_inferior[vec_non_redundant] <=
          df_sub$layer_limit_inferior[1])]

    if (solve == TRUE) {

      # Regardless of whether it should be moved up or down
      df$layer_limit_inferior[vec_to_move] <-
        df$layer_limit_inferior[vec_to_move] - df_sub$layer_limit_inferior[1]
      df$layer_limit_superior[vec_to_move] <-
        df$layer_limit_superior[vec_to_move] - df_sub$layer_limit_inferior[1]

      diff_to_move <- df_sub$layer_limit_superior[2]

    }

  }

} else

  if (df_sub$layer_type[1] %in% c("peat", "mineral")) {

    diff_to_move <- df_sub$layer_limit_superior[1]

  }

if (diff_to_move != 0) {

  if (solve == TRUE) {

    # Regardless of whether it should be moved up or down
    df$layer_limit_inferior[vec] <- df$layer_limit_inferior[vec] - diff_to_move
    df$layer_limit_superior[vec] <- df$layer_limit_superior[vec] - diff_to_move

  }
}

}






 # Update the progress bar
if (!isTRUE(getOption("knitr.in.progress"))) {
setTxtProgressBar(progress_bar, i)
}
} # End of for loop over different profiles

if (!isTRUE(getOption("knitr.in.progress"))) {
close(progress_bar)
}

# Final dataset preparations ----

df <- df %>%
  # Sort in different levels
  arrange(country,
          code_plot,
          survey_year,
          repetition,
          layer_number) %>%
  # Add layer numbers for below-ground and above-ground only
  group_by(unique_survey_repetition) %>%
  mutate(
    layer_number_bg_only = ifelse(.data$layer_limit_superior >= 0 &
                                    .data$layer_limit_inferior >= 0,
                                  layer_number,
                                  NA),
    layer_number_bg_min = suppressWarnings(min(layer_number_bg_only,
                                               na.rm = TRUE)),
    layer_number_bg = layer_number_bg_only - (layer_number_bg_min - 1),
    layer_number_ff = ifelse(.data$layer_type %in% c("forest_floor") &
                               .data$layer_limit_superior <= 0 &
                               .data$layer_limit_inferior <= 0,
                             layer_number,
                             NA)) %>%
    select(-layer_number_bg_only,
           -layer_number_bg_min) %>%
  # Try to gap-fill layer limits based on the limits of adjacent layers
  # of the eff_soil_depth
  mutate(layer_number_deepest = suppressWarnings(max(.data$layer_number,
                                                     na.rm = TRUE)),
         depth_max = suppressWarnings(max(.data$layer_limit_inferior,
                                          na.rm = TRUE)),
         prev_layer_limit_inferior = lag(.data$layer_limit_inferior),
         next_layer_limit_superior = lead(.data$layer_limit_superior)) %>%
  ungroup() %>%
  left_join(prf_agg,
            by = "plot_id") %>%
  rowwise() %>%
  mutate(layer_limit_superior =
           # If the upper layer limit is unknown,
           # while the lower layer limit of the layer on top is known,
           # take this layer limit
           ifelse(is.na(.data$layer_limit_superior) &
                   !is.na(.data$prev_layer_limit_inferior) &
                   !is.na(.data$layer_number) &
                   (.data$layer_number != 1),
                 .data$prev_layer_limit_inferior,
                 # If the layers are overlapping:
                 # take average of the adjacent overlapping depth limits
                 ifelse(!is.na(.data$layer_limit_superior) &
                          !is.na(.data$prev_layer_limit_inferior) &
                          (.data$prev_layer_limit_inferior >
                             .data$layer_limit_superior) &
                          !is.na(.data$layer_number) &
                          (.data$layer_number != 1),
                        round(mean(c(.data$layer_limit_superior,
                                     .data$prev_layer_limit_inferior)), 1),
                        .data$layer_limit_superior)),
         layer_limit_inferior =
           # If the lower layer limit is unknown,
           # while the upper layer limit of the layer below is known,
           # take this layer limit
           ifelse(is.na(.data$layer_limit_inferior) &
                    !is.na(.data$next_layer_limit_superior) &
                    !is.na(.data$layer_number) &
                    (.data$layer_number != .data$layer_number_deepest),
                  .data$next_layer_limit_superior,
                  # If the layers are overlapping:
                  # take the average of the adjacent overlapping depth limits
                  ifelse(!is.na(.data$layer_limit_inferior) &
                           !is.na(.data$next_layer_limit_superior) &
                           (.data$layer_limit_inferior >
                              .data$next_layer_limit_superior) &
                           !is.na(.data$layer_number) &
                           (.data$layer_number != .data$layer_number_deepest),
                         round(mean(c(.data$layer_limit_inferior,
                                      .data$next_layer_limit_superior)), 1),
                         # If lower layer limit of lowest layer is unknown,
                         # use the eff_soil_depth
                         ifelse(is.na(.data$layer_limit_inferior) &
                                  (.data$layer_number ==
                                     .data$layer_number_deepest) &
                                  !is.na(.data$eff_soil_depth) &
                                  (.data$eff_soil_depth > .data$depth_max),
                                .data$eff_soil_depth,
                                .data$layer_limit_inferior)))) %>%
  ungroup() %>%
  mutate(layer_limit_superior = round(layer_limit_superior, 1),
         layer_limit_inferior = round(layer_limit_inferior, 1)) %>%
  select(-layer_number_deepest,
         -depth_max,
         -prev_layer_limit_inferior,
         -next_layer_limit_superior,
         -eff_soil_depth) %>%
  relocate(repetition, .after = code_plot) %>%
  relocate(code_layer, .after = repetition) %>%
  relocate(layer_type, .after = code_layer) %>%
  relocate(layer_number, .after = layer_type)


# Save the survey form and list_layer_inconsistencies for the given survey form
# to the global environment

if (save_to_env == TRUE) {
assign_env(survey_form, df)

assign_env(paste0("list_layer_inconsistencies_", survey_form),
           list_layer_inconsistencies)
} else {
  return(df)
}

} # End of "som" part




























































  # . ----
  # Part 2: processing "pfh" survey forms (with pedogenic layers) ----

  # Check if the given survey form is "pfh"

  if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

    # Retrieve the survey_form data

    if (is.null(data_frame)) {
      df <- get_env(survey_form)
    } else {
      df <- data_frame
    }

    # prf for eff_soil_depth

    if (unlist(strsplit(survey_form, "_"))[1] == "so") {

      assertthat::assert_that(file.exists(paste0("./data/additional_data/",
                                                 "SO_PRF_ADDS.xlsx")),
                              msg = paste0("'./data/additional_data/",
                                           "SO_PRF_ADDS.xlsx' ",
                                           "does not exist."))

      # This file was created by Nathalie on 17 Oct 2023
      prf <-
        openxlsx::read.xlsx(paste0("./data/additional_data/",
                                   "SO_PRF_ADDS.xlsx"),
                            sheet = 1) %>%
        rename(plot_id = PLOT_ID) %>%
        mutate(eff_soil_depth = as.numeric(DEPTHSTOCK))
    }

    if (unlist(strsplit(survey_form, "_"))[1] == "s1") {

      prf <- get_env(paste0(unlist(strsplit(survey_form, "_"))[1], "_prf")) %>%
        rowwise() %>%
        mutate(eff_soil_depth = ifelse(is.na(.data$eff_soil_depth) &
                                         any(!is.na(c(.data$rooting_depth,
                                                      .data$rock_depth,
                                                      .data$obstacle_depth))),
                                       # Maximum of these three depths
                                       max(c(.data$rooting_depth,
                                             .data$rock_depth,
                                             .data$obstacle_depth),
                                           na.rm = TRUE),
                                       .data$eff_soil_depth)) %>%
        ungroup()
    }

    prf_agg <- prf %>%
      group_by(plot_id) %>%
      # Sometimes there are different options,
      # No good way to solve this - we just have to pick one
      summarise(eff_soil_depth = median(eff_soil_depth, na.rm = TRUE))







    # The intention is to create a list_layer_inconsistencies of the following
    # format to store inconsistencies found in the survey_form data:

    list_layer_inconsistencies <-
      data.frame(survey_form = NULL,
                 partner = NULL,
                 partner_code = NULL,
                 country = NULL,
                 code_country = NULL,
                 survey_year = NULL,
                 code_plot = NULL,
                 plot_id = NULL,
                 code_layer_horizon_master = NULL,
                 repetition_profile_pit_id = NULL,
                 code_line = NULL,
                 parameter = NULL,
                 parameter_unit = NULL,
                 parameter_value = NULL,
                 inconsistency_reason = NULL,
                 inconsistency_type = NULL,
                 rule_id = NULL,
                 non_duplicated_error_type_per_record = NULL,
                 change_date = NULL,
                 download_date = NULL)

    # In addition, a list of redundant layers of the given survey form will
    # be created
    # (this is internal information for FSCC, so this is not included in PIR)
    # This "list_redundant_layers" has the following format:

    list_redundant_layers <-
      data.frame(survey_form = NULL,
                 partner = NULL,
                 partner_code = NULL,
                 country = NULL,
                 code_country = NULL,
                 survey_year = NULL,
                 code_plot = NULL,
                 plot_id = NULL,
                 horizon_master = NULL,
                 redundancy_type = NULL,
                 combined_redundant_horizon_master = NULL,
                 superior_layer_limit = NULL,
                 inferior_layer_limit = NULL,
                 horizon_number_original = NULL,
                 layer_number = NULL,
                 code_line = NULL,
                 inconsistency_reason = NULL,
                 rule_id = NULL,
                 change_date = NULL,
                 download_date = NULL)


    # Source the R code of the "get_redundant_layers()" function

    source("./src/functions/get_redundant_layers.R")


    # Before estimating layer limits when needed,
    # duplicate the columns with layer limits (to keep the original layer
    # limits)
    # Only the layer limit columns without "_orig" can be altered

    df$layer_number <- NA

    if (!"horizon_limit_up_orig" %in% names(df)) {
      df$horizon_limit_up_orig <- df$horizon_limit_up
    }

    if (!"horizon_limit_low_orig" %in% names(df)) {
      df$horizon_limit_low_orig <- df$horizon_limit_low
    }



    # Which extreme TOC values are confirmed as correct by partners?
    # The layer type of these don't have to be converted.
    # For example, in Wallonia, this is due to the presence of charcoal

    survey_form_input <- survey_form

    pir_toc_confirmed <- pir_checked %>%
      filter(survey_form == survey_form_input) %>%
      filter(parameter == "horizon_c_organic_total") %>%
      # Outside plausible range
      filter(rule_id == "FSCC_14") %>%
      # "The reported value is extreme but correct"
      filter(code_nfc_action_taken == 1) %>%
      pull(code_line)



    # This script is automatically moving the null line of
    # the profile in accordance with the location/thickness of
    # the forest floor, peat and mineral layers.

    # However, sometimes this causes a mismatch between the
    # null line location in "som" versus "pfh".
    # This can be due to tiny differences in the thickness
    # of the organic layer, differences in the TOC concentration...

    # Therefore, based on manual comparison,
    # it appears best and most logical to make an exception (in
    # moving the null line) for the following profiles:

    s1_pfh_dont_move <- c(
      # Poland
      "53_2007_281243",
      # Sweden
      "13_2005_845", "13_2006_944",
      # UK
      "6_2008_78", "6_2008_115", "6_2008_125", "6_2008_133",
      "14_2006_89", "14_2008_150", "14_2007_184", "14_2008_201",
      "1_1994_1744",
      "4_2007_1626", "4_2006_1689", "4_2006_1781", "4_2007_1877",
      "64_2007_306",
      "60_2006_587", "60_2006_4001")


    so_pfh_dont_move <- c(
      "67_2010_2",
      "60_2004_2")



    # Specific issues ----

    if (unlist(strsplit(survey_form, "_"))[1] == "s1") {

      # For one of the Hungarian profiles, all of the horizon_master
      # values are missing,
      # but it is clear that they should be mineral layers

      # layers_to_check <- df %>%
      #   filter(code_country == 51) %>%
      #   filter(code_plot == 1) %>%
      #   filter(horizon_master == "" |
      #            is.na(horizon_master)) %>%
      #   pull(code_line)
      #
      # if (!identical(layers_to_check, character(0))) {
      #
      #   df <- df %>%
      #     mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
      #                                "mineral",
      #                                .data$layer_type))
      #
      # }

      # Several of the Hungarian profiles also have just one horizon_master
      # but the horizon limits are usually known.
      # Assumption: if negative, layer type if forest floor,
      # else mineral

      # layers_to_check <- df %>%
      #   filter(code_country == 51) %>%
      #   filter(code_plot != 1) %>%
      #   filter(horizon_master == "" |
      #            is.na(horizon_master)) %>%
      #   pull(code_line)
      #
      # if (!identical(layers_to_check, character(0))) {
      #
      #   df <- df %>%
      #     mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
      #                                ifelse((.data$horizon_limit_up <= 0) &
      #                                         (.data$horizon_limit_low <= 0),
      #                                       "forest_floor",
      #                                       "mineral"),
      #                                .data$layer_type))
      #
      # }


      ## Manually correct switched horizon numbers ----

      # Swedish horizon numbers are switched:

      if (!"horizon_number_orig" %in% names(df)) {

        df <- df %>%
          mutate(horizon_number_orig = horizon_number)

      }

      df <- df %>%
        mutate(horizon_number =
                 ifelse(unique_survey_profile == "13_2006_546_1" &
                          horizon_limit_up == 8 &
                          horizon_number == 4,
                        3,
                        ifelse(unique_survey_profile == "13_2006_546_1" &
                                 horizon_limit_up == 13 &
                                 horizon_number == 3,
                               4,
                               .data$horizon_number))) %>%
        mutate(horizon_number =
                 ifelse(unique_survey_profile == "13_2006_1080_1" &
                          horizon_limit_up == 11 &
                          horizon_number == 3,
                        4,
                        ifelse(unique_survey_profile == "13_2006_1080_1" &
                                 horizon_limit_up == 6 &
                                 horizon_number == 4,
                               3,
                               .data$horizon_number))) %>%
        mutate(horizon_number =
                 ifelse(unique_survey_profile == "13_2006_2072_1" &
                          horizon_limit_up == -6 &
                          horizon_number == 3,
                        1,
                        ifelse(unique_survey_profile == "13_2006_2072_1" &
                                 horizon_limit_up == 0 &
                                 horizon_number == 1,
                               2,
                               ifelse(unique_survey_profile ==
                                        "13_2006_2072_1" &
                                        horizon_limit_up == 10 &
                                        horizon_number == 2,
                                      3,
                                      .data$horizon_number)))) %>%
        mutate(horizon_number =
                 ifelse(unique_survey_profile == "13_2006_2341_1" &
                          horizon_limit_up == 12 &
                          horizon_number == 3,
                        4,
                        ifelse(unique_survey_profile == "13_2006_2341_1" &
                                 horizon_limit_up == 7 &
                                 horizon_number == 4,
                               3,
                               .data$horizon_number))) %>%
        mutate(horizon_number =
                 ifelse(unique_survey_profile == "13_2006_231_1" &
                          horizon_limit_up == 14 &
                          horizon_number == 3,
                        4,
                        ifelse(unique_survey_profile == "13_2006_231_1" &
                                 horizon_limit_up == 10 &
                                 horizon_number == 4,
                               3,
                               .data$horizon_number))) %>%
        mutate(horizon_number =
                 ifelse(unique_survey_profile == "13_2006_1025_1" &
                          horizon_limit_up == 14 &
                          horizon_number == 3,
                        4,
                        ifelse(unique_survey_profile == "13_2006_1025_1" &
                                 horizon_limit_up == 9 &
                                 horizon_number == 4,
                               3,
                               .data$horizon_number)))  %>%
        mutate(horizon_number =
                 ifelse(unique_survey_profile == "13_2006_1360_1" &
                          horizon_limit_up == 65 &
                          horizon_number == 2,
                        3,
                        ifelse(unique_survey_profile == "13_2006_1360_1" &
                                 horizon_limit_up == 60 &
                                 horizon_number == 3,
                               2,
                               .data$horizon_number)))


    }


    ## Manually correct layer types based on horizon_c_organic_total ----

    #  This was manually determined by checking all different profile surveys
    #  within a certain plot_id, while harmonising the depth of the null line

    if (unlist(strsplit(survey_form, "_"))[1] == "s1") {

      # Irish plot 7_243
      # Based on the TOC content in "s1_som",
      # the layers below 10 cm should be mineral

      layers_to_check <- df %>%
        filter(plot_id == "7_243") %>%
        filter(horizon_limit_up_orig >= 10) %>%
        filter(layer_type != "mineral") %>%
        pull(code_line)

      if (!identical(layers_to_check, character(0))) {

        df <- df %>%
          mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
                                     "mineral",
                                     .data$layer_type))

      }

      # More layers that should become mineral

      layers_to_check <- c(
        # Ireland
        # 7_254
        # Based on the information that is filled in
        df %>%
          filter(plot_id == "7_254") %>%
          filter(horizon_master == "H") %>%
          filter(horizon_limit_up_orig >= 18,
                 horizon_limit_low_orig <= 41) %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # 7_278
        # Based on TOC "som"
        df %>%
          filter(plot_id == "7_278") %>%
          filter(horizon_master == "O") %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # 7_701
        # Based on TOC "som"
        df %>%
          filter(plot_id == "7_701") %>%
          filter(horizon_master == "O") %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # 7_708
        # Based on TOC "som"
        df %>%
          filter(plot_id == "7_708") %>%
          filter(horizon_master == "H") %>%
          filter(horizon_limit_up_orig >= 22,
                 horizon_limit_low_orig <= 31) %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # 7_712
        # Based on TOC "som"
        df %>%
          filter(plot_id == "7_712") %>%
          filter(horizon_master %in% c("H", "HA")) %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # Polish
        # 53_481683
        # Based on TOC
        df %>%
          filter(plot_id == "53_481683") %>%
          filter(horizon_master == "H") %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # UK
        # 6_103
        # Based on TOC
        df %>%
          filter(plot_id == "6_103") %>%
          filter(horizon_master == "H") %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # 6_113
        # Based on TOC
        df %>%
          filter(plot_id == "6_113") %>%
          filter(horizon_master == "H") %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # 6_120
        # Based on TOC
        df %>%
          filter(plot_id == "6_120") %>%
          filter(horizon_master == "H") %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # 6_104
        # Based on TOC
        df %>%
          filter(plot_id == "6_104") %>%
          filter(horizon_master == "O") %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # 6_114
        # Based on TOC
        df %>%
          filter(plot_id == "6_114") %>%
          filter(horizon_master == "H") %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # 4_8373
        # Based on TOC and s1_som
        df %>%
          filter(plot_id == "4_8373") %>%
          filter(survey_year == 2006) %>%
          filter(horizon_master == "OC") %>%
          filter(layer_type != "mineral") %>%
          pull(code_line),
        # 53_1161283
        # Based on TOC and s1_som
        df %>%
          filter(plot_id == "53_1161283") %>%
          filter(survey_year == 2007) %>%
          filter(horizon_master == "HC") %>%
          filter(layer_type != "mineral") %>%
          pull(code_line))

      if (!identical(layers_to_check, character(0))) {

        df <- df %>%
          mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
                                     "mineral",
                                     .data$layer_type))

      }

      # Layers that should become peat

      layers_to_check <- c(
        # Irish plot 7_208: layers to become peat
        # Based on the TOC content in "s1_som",
        df %>%
        filter(plot_id == "7_208") %>%
        filter(horizon_master %in% c("OA", "AE")) %>%
        filter(layer_type != "peat") %>%
        pull(code_line),
        # Slovenia
        # 60_83
        # Based on TOC
        df %>%
          filter(plot_id == "60_83") %>%
          filter(horizon_master == "COH") %>%
          filter(layer_type != "peat") %>%
          pull(code_line),
        # 60_195
        # Based on TOC
        df %>%
          filter(plot_id == "60_195") %>%
          filter(horizon_master %in% c("CR/OH", "ROH")) %>%
          filter(layer_type != "peat") %>%
          pull(code_line),
        # UK
        # 6_78
        # Based on TOC and sequence "som"
        df %>%
          filter(plot_id == "6_78") %>%
          filter(horizon_master == "OH") %>%
          filter(layer_type != "peat") %>%
          pull(code_line),
        # 6_115
        # Based on TOC and sequence "som"
        df %>%
          filter(plot_id == "6_115") %>%
          filter(horizon_master == "OH") %>%
          filter(layer_type != "peat") %>%
          pull(code_line),
        # 6_125
        # Based on TOC and sequence "som"
        df %>%
          filter(plot_id == "6_125") %>%
          filter(horizon_master == "OH") %>%
          filter(layer_type != "peat") %>%
          pull(code_line),
        # 6_133
        # Based on TOC and sequence "som"
        df %>%
          filter(plot_id == "6_133") %>%
          filter(horizon_master == "OH") %>%
          filter(layer_type != "peat") %>%
          pull(code_line),
        # Buried forest floor
        df %>%
          filter(layer_type == "forest_floor") %>%
          filter(!is.na(horizon_limit_up) &
                   horizon_limit_up >= 0) %>%
          filter(!is.na(horizon_limit_low) &
                   horizon_limit_low > 0) %>%
          filter(horizon_limit_low > horizon_limit_up) %>%
          pull(code_line)
        )

      if (!identical(layers_to_check, character(0))) {

        df <- df %>%
          mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
                                     "peat",
                                     .data$layer_type))

      }

      # Layers that should become forest_floor

      layers_to_check <- c(
        # UK
        # 6_130
        # Based on TOC and sequence "som"
        df %>%
          filter(plot_id == "6_130") %>%
          filter(horizon_master == "H") %>%
          filter(layer_type != "forest_floor") %>%
          pull(code_line),
        # Several Swedish H layers
        df %>%
          filter(code_country == 13) %>%
          filter(layer_type == "peat") %>%
          filter(startsWith(horizon_master, "H")) %>%
          filter(horizon_limit_up < 0 &
                   horizon_limit_low <= 0) %>%
          pull(code_line),
        # Other above-ground H layers
        df %>%
          filter(code_country != 13) %>%
          filter(plot_id != "6_130") %>%
          filter(layer_type == "peat") %>%
          filter(startsWith(horizon_master, "H")) %>%
          filter(is.na(horizon_limit_up) |
                   horizon_limit_up < 0) %>%
          pull(code_line),
        # Upper layer of 6_21
        # Based on location in original profile and "s1_som"
        df %>%
          filter(plot_id == "6_21") %>%
          filter(horizon_master == "BC" &
                   horizon_number == 1 &
                   horizon_limit_up == -2) %>%
          filter(layer_type == "mineral") %>%
          pull(code_line))


      if (!identical(layers_to_check, character(0))) {

        df <- df %>%
          mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
                                     "forest_floor",
                                     .data$layer_type))

      }


    } # End of "if s1_pfh"


    if (unlist(strsplit(survey_form, "_"))[1] == "so") {

      # Layers that should become forest_floor

      layers_to_check <- c(
        df %>%
          filter(layer_type == "peat") %>%
          filter(startsWith(horizon_master, "H")) %>%
          filter(is.na(horizon_limit_up) |
                   horizon_limit_up < 0) %>%
          pull(code_line))

      if (!identical(layers_to_check, character(0))) {

        df <- df %>%
          mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
                                     "forest_floor",
                                     .data$layer_type))

      }

      # Layers that should become peat

      layers_to_check <- c(
        # Buried forest floor
        df %>%
          filter(layer_type == "forest_floor") %>%
          filter(!is.na(horizon_limit_up) &
                   horizon_limit_up >= 0) %>%
          filter(!is.na(horizon_limit_low) &
                   horizon_limit_low > 0) %>%
          filter(horizon_limit_low > horizon_limit_up) %>%
          pull(code_line))

      if (!identical(layers_to_check, character(0))) {

        df <- df %>%
          mutate(layer_type = ifelse(.data$code_line %in% layers_to_check,
                                     "peat",
                                     .data$layer_type))

      }

    } # End of "if so_pfh"


    # Set up a progress bar to track processing

    if (!isTRUE(getOption("knitr.in.progress"))) {
    progress_bar <-
      txtProgressBar(min = 0,
                     max = length(unique(df$unique_survey_profile)),
                     style = 3)
    }




    # Evaluate soil profiles per unique_survey_profile ----

    for (i in seq_along(unique(df$unique_survey_profile))) {

    # Determine index of layers with the given unique_survey_profile in df

    vec <- which(unique(df$unique_survey_profile)[i] ==
                   df$unique_survey_profile)

    # index forest floor layers in df
    vec_ff <- vec[which(df$layer_type[vec] == "forest_floor")]

    # index below-ground layers in df
    vec_bg <- vec[which(df$layer_type[vec] == "mineral" |
                          df$layer_type[vec] == "peat")]


    # Norwegian profile without horizon_master and layer_type:

    # if (unique(df$unique_survey_profile)[i] == "55_1988_4_04a") {
    #   df$layer_type[which(is.na(df$layer_type))] <- "mineral"
    # }



    ## FSCC_2: Caution: the reported layer limits in above-ground ----
    # (negative) or below-ground (positive) layers have the wrong sign
    # (positive/negative).

    # above-ground

    # Check if layer limits of forest floor layers are known

    if (any(!is.na(df$horizon_limit_up_orig[vec_ff])) &&
        any(!is.na(df$horizon_limit_low_orig[vec_ff]))) {

      vec_ff_sup <- vec_ff[which(!is.na(df$horizon_limit_up_orig[vec_ff]))]
      vec_ff_inf <- vec_ff[which(!is.na(df$horizon_limit_low_orig[vec_ff]))]


      vec_ff_non_empty <-
        vec_ff[which((!is.na(df$horizon_limit_up_orig[vec_ff])) &
                       (!is.na(df$horizon_limit_low_orig[vec_ff])))]

      if (!identical(vec_bg, integer(0)) &&
          any(!is.na(df$horizon_limit_up_orig[vec_bg])) &&
          any(!is.na(df$horizon_limit_low_orig[vec_bg]))) {

        # Depth range forest floor

        depth_range_ff <- seq(round(max(c(df$horizon_limit_up[vec_ff],
                                          df$horizon_limit_low[vec_ff]),
                                        na.rm = TRUE)),
                              round(min(c(df$horizon_limit_up[vec_ff],
                                          df$horizon_limit_low[vec_ff]),
                                        na.rm = TRUE)),
                              by = -0.5)

        depth_range_ff <- depth_range_ff[-c(1, length(depth_range_ff))]

        # Depth range below-ground

        depth_range_bg <- seq(round(max(c(df$horizon_limit_up[vec_bg],
                                          df$horizon_limit_low[vec_bg]),
                                        na.rm = TRUE)),
                              round(min(c(df$horizon_limit_up[vec_bg],
                                          df$horizon_limit_low[vec_bg]),
                                        na.rm = TRUE)),
                              by = -0.5)

        depth_range_bg <- depth_range_bg[-c(1, length(depth_range_bg))]

      }


      # Check if forest floor layers have limits with wrong sign
      # (i.e. all >= 0 and at least once > 0 so not equal to 0)
      # Only for profiles without peat since peat soils usually have a
      # special sequence
      # There should be an overlap in depth ranges between above-ground
      # and below-ground and
      # the superior layer limit should be higher than the inferior layer limit
      # (in the assumption that the signs are switched)

      if (all(df$horizon_limit_low_orig[vec_ff_sup] >= 0) &&
          all(df$horizon_limit_up_orig[vec_ff_inf] >= 0) &&
          any(c(df$horizon_limit_low_orig[vec_ff_sup],
                df$horizon_limit_up_orig[vec_ff_inf]) > 0) &&
          (((!identical(vec_bg, integer(0)) &&
             any(!is.na(df$horizon_limit_up_orig[vec_bg])) &&
             any(!is.na(df$horizon_limit_low_orig[vec_bg]))) &&
            any(depth_range_ff %in% depth_range_bg)) ||
           identical(vec_bg, integer(0))) &&
          all(df$horizon_limit_up_orig[vec_ff_non_empty] >
              df$horizon_limit_low_orig[vec_ff_non_empty]) &&
          length(vec_ff) >= 1 &&
          all(df$layer_type[vec_bg] != "peat")) {


      # Check if forest floor layers have limits with wrong sign
      # (i.e. all >= 0 and at least once > 0 so not equal to 0)
      # Only for profiles without peat since peat soils usually have
      # a special sequence

      # if (all(df$horizon_limit_low_orig[vec_ff_sup] >= 0) &&
      #     all(df$horizon_limit_up_orig[vec_ff_inf] >= 0) &&
      #     any(c(df$horizon_limit_low_orig[vec_ff_sup],
      #           df$horizon_limit_up_orig[vec_ff_inf]) > 0) &&
      #     length(vec_ff) >= 1 &&
      #     all(df$layer_type[vec_bg] != "peat")) {

        # Store information about the inconsistency in
        # "list_layer_inconsistencies"

        # superior

        rule_id <- "FSCC_2"
        inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
          which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <- inconsistency_catalogue$inconsistency_type[
          which(inconsistency_catalogue$rule_id == rule_id)]

        list_layer_inconsistencies <- rbind(
          list_layer_inconsistencies,
          data.frame(survey_form = as.factor(rep(survey_form,
                                                 length(vec_ff_sup))),
                     partner = df$partner[vec_ff_sup],
                     partner_code = df$partner_code[vec_ff_sup],
                     country = df$country[vec_ff_sup],
                     code_country = df$code_country[vec_ff_sup],
                     survey_year = df$survey_year[vec_ff_sup],
                     code_plot = df$code_plot[vec_ff_sup],
                     plot_id = df$plot_id[vec_ff_sup],
                     code_layer_horizon_master = df$horizon_master[vec_ff_sup],
                     repetition_profile_pit_id = df$profile_pit_id[vec_ff_sup],
                     code_line = df$code_line[vec_ff_sup],
                     parameter = as.factor(rep("horizon_limit_up",
                                               length(vec_ff_sup))),
                     parameter_unit = as.factor(rep("cm", length(vec_ff_sup))),
                     parameter_value = df$horizon_limit_up_orig[vec_ff_sup],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       rep(TRUE, length(vec_ff_sup)),
                     change_date = df$change_date[vec_ff_sup],
                     download_date = rep(download_date_pir,
                                         length(vec_ff_sup))))

        # inferior

        rule_id <- "FSCC_2"
        inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
          which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <- inconsistency_catalogue$inconsistency_type[
          which(inconsistency_catalogue$rule_id == rule_id)]

        list_layer_inconsistencies <- rbind(
          list_layer_inconsistencies,
          data.frame(survey_form = as.factor(rep(survey_form,
                                                 length(vec_ff_inf))),
                     partner = df$partner[vec_ff_inf],
                     partner_code = df$partner_code[vec_ff_inf],
                     country = df$country[vec_ff_inf],
                     code_country = df$code_country[vec_ff_inf],
                     survey_year = df$survey_year[vec_ff_inf],
                     code_plot = df$code_plot[vec_ff_inf],
                     plot_id = df$plot_id[vec_ff_inf],
                     code_layer_horizon_master = df$horizon_master[vec_ff_inf],
                     repetition_profile_pit_id = df$profile_pit_id[vec_ff_inf],
                     code_line = df$code_line[vec_ff_inf],
                     parameter = as.factor(rep("horizon_limit_low",
                                               length(vec_ff_inf))),
                     parameter_unit = as.factor(rep("cm", length(vec_ff_inf))),
                     parameter_value = df$horizon_limit_low_orig[vec_ff_inf],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       rep(FALSE, length(vec_ff_inf)),
                     change_date = df$change_date[vec_ff_inf],
                     download_date = rep(download_date_pir,
                                         length(vec_ff_inf))))

        # If "solve" is TRUE, multiply the layer limits of the forest floor
        # layers with *(-1) so that they become negative

           if (solve == TRUE) {

          df$horizon_limit_low[vec_ff] <-
            (-1)*(df$horizon_limit_low_orig[vec_ff])
          df$horizon_limit_up[vec_ff] <-
            (-1)*(df$horizon_limit_up_orig[vec_ff])

           }
      }
      }



    # below-ground

    # Check if layer limits of below-ground layers are known

    if (any(!is.na(df$horizon_limit_up_orig[vec_bg])) &
        any(!is.na(df$horizon_limit_low_orig[vec_bg]))) {

      vec_bg_sup <- vec_bg[which(!is.na(df$horizon_limit_up_orig[vec_bg]))]
      vec_bg_inf <- vec_bg[which(!is.na(df$horizon_limit_low_orig[vec_bg]))]

      vec_bg_non_empty <-
        vec_bg[which((!is.na(df$horizon_limit_up_orig[vec_bg])) &
                       (!is.na(df$horizon_limit_low_orig[vec_bg])))]


      # Depth range forest floor

      if (!identical(vec_ff, integer(0)) &&
          (any(!is.na(df$horizon_limit_low_orig[vec_ff])) &&
           any(!is.na(df$horizon_limit_up_orig[vec_ff])))) {

        depth_range_ff <- seq(round(max(c(df$horizon_limit_up[vec_ff],
                                          df$horizon_limit_low[vec_ff]),
                                        na.rm = TRUE)),
                              round(min(c(df$horizon_limit_up[vec_ff],
                                          df$horizon_limit_low[vec_ff]),
                                        na.rm = TRUE)),
                              by = -0.5)

        depth_range_ff <- depth_range_ff[-c(1, length(depth_range_ff))]

        # Depth range below-ground

        depth_range_bg <- seq(round(max(c(df$horizon_limit_up[vec_bg],
                                          df$horizon_limit_low[vec_bg]),
                                        na.rm = TRUE)),
                              round(min(c(df$horizon_limit_up[vec_bg],
                                          df$horizon_limit_low[vec_bg]),
                                        na.rm = TRUE)),
                              by = -0.5)

        depth_range_bg <- depth_range_bg[-c(1, length(depth_range_bg))]

      }

      # Check if below-ground (mineral or peat layers) have limits with wrong sign
      # (i.e. all <= 0 and at least once < 0 so not equal to 0)

      if (all(df$horizon_limit_low_orig[vec_bg_inf] <= 0) &&
          all(df$horizon_limit_up_orig[vec_bg_sup] <= 0) &&
          any(c(df$horizon_limit_low_orig[vec_bg_inf],
                df$horizon_limit_up_orig[vec_bg_sup]) < 0) &&
          # Either (if there's a forest floor) there should be no overlap
          # or (if there is no forest floor), the superior layer limit should
          # be higher than the inferior layer limit (in the assumption
          # that the signs are switched)
          (((!identical(vec_ff, integer(0)) &&
             any(!is.na(df$horizon_limit_up_orig[vec_ff])) &&
             any(!is.na(df$horizon_limit_low_orig[vec_ff]))) &&
            (any(depth_range_bg %in% depth_range_ff))) ||
           ((identical(vec_ff, integer(0)) ||
             (all(is.na(df$horizon_limit_up_orig[vec_ff])) ||
              all(is.na(df$horizon_limit_low_orig[vec_ff])))) &&
            (all(df$horizon_limit_up_orig[vec_bg_non_empty] >
                 df$horizon_limit_low_orig[vec_bg_non_empty])))) &&
          length(vec_bg) >= 1) {


      # Check if below-ground layers have limits with wrong sign
      # (i.e. all <= 0 and at least once < 0 so not equal to 0)

      # if (all(df$horizon_limit_low_orig[vec_bg_inf] <= 0) &&
      #     all(df$horizon_limit_up_orig[vec_bg_sup] <= 0) &&
      #     any(c(df$horizon_limit_low_orig[vec_bg_inf],
      #           df$horizon_limit_up_orig[vec_bg_sup]) < 0) &&
      #     length(vec_bg) >= 1) {

        # Store information about the inconsistency in
        # "list_layer_inconsistencies"

        # superior

        rule_id <- "FSCC_2"
        inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
          which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <- inconsistency_catalogue$inconsistency_type[
          which(inconsistency_catalogue$rule_id == rule_id)]

        list_layer_inconsistencies <- rbind(
          list_layer_inconsistencies,
          data.frame(survey_form = as.factor(rep(survey_form,
                                                 length(vec_bg_sup))),
                     partner = df$partner[vec_bg_sup],
                     partner_code = df$partner_code[vec_bg_sup],
                     country = df$country[vec_bg_sup],
                     code_country = df$code_country[vec_bg_sup],
                     survey_year = df$survey_year[vec_bg_sup],
                     code_plot = df$code_plot[vec_bg_sup],
                     plot_id = df$plot_id[vec_bg_sup],
                     code_layer_horizon_master = df$horizon_master[vec_bg_sup],
                     repetition_profile_pit_id = df$profile_pit_id[vec_bg_sup],
                     code_line = df$code_line[vec_bg_sup],
                     parameter = as.factor(rep("horizon_limit_up",
                                               length(vec_bg_sup))),
                     parameter_unit = as.factor(rep("cm", length(vec_bg_sup))),
                     parameter_value = df$horizon_limit_up_orig[vec_bg_sup],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       rep(TRUE, length(vec_bg_sup)),
                     change_date = df$change_date[vec_bg_sup],
                     download_date = rep(download_date_pir,
                                         length(vec_bg_sup))))

        # inferior

        rule_id <- "FSCC_2"
        inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
          which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <- inconsistency_catalogue$inconsistency_type[
          which(inconsistency_catalogue$rule_id == rule_id)]

        list_layer_inconsistencies <- rbind(
          list_layer_inconsistencies,
          data.frame(survey_form = as.factor(rep(survey_form,
                                                 length(vec_bg_inf))),
                     partner = df$partner[vec_bg_inf],
                     partner_code = df$partner_code[vec_bg_inf],
                     country = df$country[vec_bg_inf],
                     code_country = df$code_country[vec_bg_inf],
                     survey_year = df$survey_year[vec_bg_inf],
                     code_plot = df$code_plot[vec_bg_inf],
                     plot_id = df$plot_id[vec_bg_inf],
                     code_layer_horizon_master = df$horizon_master[vec_bg_inf],
                     repetition_profile_pit_id = df$profile_pit_id[vec_bg_inf],
                     code_line = df$code_line[vec_bg_inf],
                     parameter = as.factor(rep("horizon_limit_low",
                                               length(vec_bg_inf))),
                     parameter_unit = as.factor(rep("cm", length(vec_bg_inf))),
                     parameter_value = df$horizon_limit_low_orig[vec_bg_inf],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       rep(FALSE, length(vec_bg_inf)),
                     change_date = df$change_date[vec_bg_inf],
                     download_date = rep(download_date_pir,
                                         length(vec_bg_inf))))


        # If "solve" is TRUE, multiply the layer limits of the forest floor
        # layers with *(-1) so that they become positive

          if (solve == TRUE) {

          df$horizon_limit_low[vec_bg] <-
            (-1)*(df$horizon_limit_low_orig[vec_bg])
          df$horizon_limit_up[vec_bg] <-
            (-1)*(df$horizon_limit_up_orig[vec_bg])

          }
      }
      }


    ## FSCC_2 Error reason: Typo observed ----

    # One value in the given profile seems to have the wrong sign

    if (unique(df$unique_survey_profile)[i] == "53_2008_281402_1") {

    if (df$horizon_limit_low_orig[vec[
      which(df$horizon_master[vec] == "OL")]] == 1) {

    ind_error <- vec[which(df$horizon_master[vec] == "OL")]

    rule_id <- "FSCC_2"
    inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
      which(inconsistency_catalogue$rule_id == rule_id)]
    inconsistency_type <- inconsistency_catalogue$inconsistency_type[
      which(inconsistency_catalogue$rule_id == rule_id)]

    list_layer_inconsistencies <- rbind(
      list_layer_inconsistencies,
      data.frame(survey_form = as.factor(rep(survey_form, length(ind_error))),
                 partner = df$partner[ind_error],
                 partner_code = df$partner_code[ind_error],
                 country = df$country[ind_error],
                 code_country = df$code_country[ind_error],
                 survey_year = df$survey_year[ind_error],
                 code_plot = df$code_plot[ind_error],
                 plot_id = df$plot_id[ind_error],
                 code_layer_horizon_master = df$horizon_master[ind_error],
                 repetition_profile_pit_id = df$profile_pit_id[ind_error],
                 code_line = df$code_line[ind_error],
                 parameter = as.factor(rep("horizon_limit_low",
                                           length(ind_error))),
                 parameter_unit = as.factor(rep("cm", length(ind_error))),
                 parameter_value = df$horizon_limit_low_orig[ind_error],
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(TRUE,
                                                            length(ind_error)),
                 change_date = df$change_date[ind_error],
                 download_date = rep(download_date_pir, length(ind_error))))

    if (solve == TRUE) {
      df$horizon_limit_low[ind_error] <- (-1)
      }
    }
    }

    # One value in the given profile seems to have the wrong sign

    if (unique(df$unique_survey_profile)[i] == "4_2007_684_684") {

    if (df$horizon_limit_low_orig[vec[
      which(df$horizon_master[vec] == "OF")]] == 2.4) {

    ind_error <- vec[which(df$horizon_master[vec] == "OF")]

    rule_id <- "FSCC_2"
    inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
      which(inconsistency_catalogue$rule_id == rule_id)]
    inconsistency_type <- inconsistency_catalogue$inconsistency_type[
      which(inconsistency_catalogue$rule_id == rule_id)]

    list_layer_inconsistencies <- rbind(
      list_layer_inconsistencies,
      data.frame(survey_form = as.factor(rep(survey_form, length(ind_error))),
                 partner = df$partner[ind_error],
                 partner_code = df$partner_code[ind_error],
                 country = df$country[ind_error],
                 code_country = df$code_country[ind_error],
                 survey_year = df$survey_year[ind_error],
                 code_plot = df$code_plot[ind_error],
                 plot_id = df$plot_id[ind_error],
                 code_layer_horizon_master = df$horizon_master[ind_error],
                 repetition_profile_pit_id = df$profile_pit_id[ind_error],
                 code_line = df$code_line[ind_error],
                 parameter = as.factor(rep("horizon_limit_low",
                                           length(ind_error))),
                 parameter_unit = as.factor(rep("cm", length(ind_error))),
                 parameter_value = df$horizon_limit_low_orig[ind_error],
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(TRUE,
                                                            length(ind_error)),
                 change_date = df$change_date[ind_error],
                 download_date = rep(download_date_pir, length(ind_error))))

    if (solve == TRUE) {
      df$horizon_limit_low[ind_error] <- (-2.4)
    }
    }
    }


    # One value in the given profile seems to have the wrong sign

    if (unique(df$unique_survey_profile)[i] == "67_2010_2_0001") {

      if (df$horizon_limit_up_orig[vec[
        which(df$horizon_number[vec] == 1)]] == 4) {

        ind_error <- vec[which(df$horizon_number[vec] == 1)]

        rule_id <- "FSCC_2"
        inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
          which(inconsistency_catalogue$rule_id == rule_id)]
        inconsistency_type <- inconsistency_catalogue$inconsistency_type[
          which(inconsistency_catalogue$rule_id == rule_id)]

        list_layer_inconsistencies <- rbind(
          list_layer_inconsistencies,
          data.frame(survey_form = as.factor(rep(survey_form,
                                                 length(ind_error))),
                     partner = df$partner[ind_error],
                     partner_code = df$partner_code[ind_error],
                     country = df$country[ind_error],
                     code_country = df$code_country[ind_error],
                     survey_year = df$survey_year[ind_error],
                     code_plot = df$code_plot[ind_error],
                     plot_id = df$plot_id[ind_error],
                     code_layer_horizon_master = df$horizon_master[ind_error],
                     repetition_profile_pit_id = df$profile_pit_id[ind_error],
                     code_line = df$code_line[ind_error],
                     parameter = as.factor(rep("horizon_limit_low",
                                               length(ind_error))),
                     parameter_unit = as.factor(rep("cm", length(ind_error))),
                     parameter_value = df$horizon_limit_low_orig[ind_error],
                     inconsistency_reason = inconsistency_reason,
                     inconsistency_type = inconsistency_type,
                     rule_id = rule_id,
                     non_duplicated_error_type_per_record =
                       rep(TRUE, length(ind_error)),
                     change_date = df$change_date[ind_error],
                     download_date = rep(download_date_pir, length(ind_error))))

        if (solve == TRUE) {
          df$horizon_limit_up[ind_error] <- (-4)
          df$layer_type[ind_error] <- "forest_floor"
        }
      }
    }



    # Typo observed: This layer limit seems wrong

    if (unique(df$unique_survey_profile)[i] == "10_2007_39_39") {

      ind_error <- vec[which(df$horizon_master[vec] == "OFH")]

      if (df$horizon_limit_low_orig[ind_error] == 0) {

        # If "solve" is set to TRUE, correct the layer limit

        if (solve == TRUE) {
          df$horizon_limit_low[ind_error] <- 4
        }
      }
    }





    ## FSCC_8: No layer limit information ----

    # Check if "horizon_limit_up" is missing anywhere

    if (any(is.na(df$horizon_limit_up_orig[vec]))) {

      vec_inconsistency <- vec[which(is.na(df$horizon_limit_up[vec]))]

      rule_id <- "FSCC_8"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      list_layer_inconsistencies <- rbind(
        list_layer_inconsistencies,
        data.frame(survey_form = as.factor(rep(survey_form,
                                               length(vec_inconsistency))),
                   partner = df$partner[vec_inconsistency],
                   partner_code = df$partner_code[vec_inconsistency],
                   country = df$country[vec_inconsistency],
                   code_country = df$code_country[vec_inconsistency],
                   survey_year = df$survey_year[vec_inconsistency],
                   code_plot = df$code_plot[vec_inconsistency],
                   plot_id = df$plot_id[vec_inconsistency],
                   code_layer_horizon_master =
                     df$horizon_master[vec_inconsistency],
                   repetition_profile_pit_id =
                     df$profile_pit_id[vec_inconsistency],
                   code_line = df$code_line[vec_inconsistency],
                   parameter = as.factor(rep("horizon_limit_up",
                                             length(vec_inconsistency))),
                   parameter_unit = as.factor(rep("cm",
                                                  length(vec_inconsistency))),
                   parameter_value =
                     df$horizon_limit_up_orig[vec_inconsistency],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record =
                     rep(TRUE, length(vec_inconsistency)),
                   change_date = df$change_date[vec_inconsistency],
                   download_date = rep(download_date_pir,
                                       length(vec_inconsistency))))
      }


    # Check if "horizon_limit_low" is missing anywhere

    if (any(is.na(df$horizon_limit_low_orig[vec]))) {
      vec_inconsistency <- vec[which(is.na(df$horizon_limit_low[vec]))]

      rule_id <- "FSCC_8"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      list_layer_inconsistencies <- rbind(
        list_layer_inconsistencies,
        data.frame(survey_form = as.factor(rep(survey_form,
                                               length(vec_inconsistency))),
                   partner = df$partner[vec_inconsistency],
                   partner_code = df$partner_code[vec_inconsistency],
                   country = df$country[vec_inconsistency],
                   code_country = df$code_country[vec_inconsistency],
                   survey_year = df$survey_year[vec_inconsistency],
                   code_plot = df$code_plot[vec_inconsistency],
                   plot_id = df$plot_id[vec_inconsistency],
                   code_layer_horizon_master =
                     df$horizon_master[vec_inconsistency],
                   repetition_profile_pit_id =
                     df$profile_pit_id[vec_inconsistency],
                   code_line = df$code_line[vec_inconsistency],
                   parameter = as.factor(rep("horizon_limit_low",
                                             length(vec_inconsistency))),
                   parameter_unit = as.factor(rep("cm",
                                                  length(vec_inconsistency))),
                   parameter_value =
                     df$horizon_limit_low_orig[vec_inconsistency],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record =
                     rep(TRUE, length(vec_inconsistency)),
                   change_date = df$change_date[vec_inconsistency],
                   download_date = rep(download_date_pir,
                                       length(vec_inconsistency))))
      }


    # FSCC_11 Error reason: horizon_limit_up == horizon_limit_low ----

    for (j in vec) {

      # Check if the superior and inferior layer limits are not empty
      # and equal to each other

    if (!is.na(df$horizon_limit_up[j]) && !is.na(df$horizon_limit_low[j])) {

    if ((df$horizon_limit_up[j] == df$horizon_limit_low[j])) {

    # superior

      rule_id <- "FSCC_11"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      list_layer_inconsistencies <- rbind(
        list_layer_inconsistencies,
        data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                   partner = df$partner[j],
                   partner_code = df$partner_code[j],
                   country = df$country[j],
                   code_country = df$code_country[j],
                   survey_year = df$survey_year[j],
                   code_plot = df$code_plot[j],
                   plot_id = df$plot_id[j],
                   code_layer_horizon_master = df$horizon_master[j],
                   repetition_profile_pit_id = df$profile_pit_id[j],
                   code_line = df$code_line[j],
                   parameter = as.factor(rep("horizon_limit_up", length(j))),
                   parameter_unit = as.factor(rep("cm", length(j))),
                   parameter_value = df$horizon_limit_up[j],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(TRUE, length(j)),
                   change_date = df$change_date[j],
                   download_date = rep(download_date_pir, length(j))))

      # inferior

      rule_id <- "FSCC_11"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      list_layer_inconsistencies <- rbind(
        list_layer_inconsistencies,
        data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                   partner = df$partner[j],
                   partner_code = df$partner_code[j],
                   country = df$country[j],
                   code_country = df$code_country[j],
                   survey_year = df$survey_year[j],
                   code_plot = df$code_plot[j],
                   plot_id = df$plot_id[j],
                   code_layer_horizon_master = df$horizon_master[j],
                   repetition_profile_pit_id = df$profile_pit_id[j],
                   code_line = df$code_line[j],
                   parameter = as.factor(rep("horizon_limit_low", length(j))),
                   parameter_unit = as.factor(rep("cm", length(j))),
                   parameter_value = df$horizon_limit_low[j],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record = rep(FALSE, length(j)),
                   change_date = df$change_date[j],
                   download_date = rep(download_date_pir, length(j))))
    }
    }
    }


    # FSCC_51: Layer limit equals -9999 ----

    # Superior

    vec_inconsistency <- vec[which(df$horizon_limit_up_orig[vec] < -500)]

    if (!identical(vec_inconsistency, integer(0))) {

      # Store information about the inconsistency in
      # "list_layer_inconsistencies"

      rule_id <- "FSCC_51"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      if ("code_layer_orig" %in% names(df)) {
        vec_code_layer <- as.character(df$code_layer_orig[j])
      } else {
        vec_code_layer <- as.character(df$code_layer[j])
      }

      list_layer_inconsistencies <- rbind(
        list_layer_inconsistencies,
        data.frame(survey_form = as.factor(rep(survey_form,
                                               length(vec_inconsistency))),
                   partner = df$partner[vec_inconsistency],
                   partner_code = df$partner_code[vec_inconsistency],
                   country = df$country[vec_inconsistency],
                   code_country = df$code_country[vec_inconsistency],
                   survey_year = df$survey_year[vec_inconsistency],
                   code_plot = df$code_plot[vec_inconsistency],
                   plot_id = df$plot_id[vec_inconsistency],
                   code_layer_horizon_master = vec_code_layer,
                   repetition_profile_pit_id = df$repetition[vec_inconsistency],
                   code_line = df$code_line[vec_inconsistency],
                   parameter = as.factor(rep("horizon_limit_up",
                                             length(vec_inconsistency))),
                   parameter_unit = as.factor(rep("cm",
                                                  length(vec_inconsistency))),
                   parameter_value =
                     df$horizon_limit_up_orig[vec_inconsistency],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record =
                     rep(TRUE, length(vec_inconsistency)),
                   change_date = df$change_date[vec_inconsistency],
                   download_date = rep(download_date_pir,
                                       length(vec_inconsistency))))

      # If "solve" is set to TRUE, set the layer limit to NA

      if (solve == TRUE) {
        df$horizon_limit_up[vec_inconsistency] <- NA
      }
    }


    # Inferior

    vec_inconsistency <- vec[which(df$horizon_limit_low_orig[vec] < -500)]

    if (!identical(vec_inconsistency, integer(0))) {

      # Store information about the inconsistency in
      # "list_layer_inconsistencies"

      rule_id <- "FSCC_51"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]

      if ("code_layer_orig" %in% names(df)) {
        vec_code_layer <- as.character(df$code_layer_orig[j])
      } else {
        vec_code_layer <- as.character(df$code_layer[j])
      }

      list_layer_inconsistencies <- rbind(
        list_layer_inconsistencies,
        data.frame(survey_form = as.factor(rep(survey_form,
                                               length(vec_inconsistency))),
                   partner = df$partner[vec_inconsistency],
                   partner_code = df$partner_code[vec_inconsistency],
                   country = df$country[vec_inconsistency],
                   code_country = df$code_country[vec_inconsistency],
                   survey_year = df$survey_year[vec_inconsistency],
                   code_plot = df$code_plot[vec_inconsistency],
                   plot_id = df$plot_id[vec_inconsistency],
                   code_layer_horizon_master = vec_code_layer,
                   repetition_profile_pit_id = df$repetition[vec_inconsistency],
                   code_line = df$code_line[vec_inconsistency],
                   parameter = as.factor(rep("horizon_limit_low",
                                             length(vec_inconsistency))),
                   parameter_unit = as.factor(rep("cm",
                                                  length(vec_inconsistency))),
                   parameter_value =
                     df$horizon_limit_low_orig[vec_inconsistency],
                   inconsistency_reason = inconsistency_reason,
                   inconsistency_type = inconsistency_type,
                   rule_id = rule_id,
                   non_duplicated_error_type_per_record =
                     rep(TRUE, length(vec_inconsistency)),
                   change_date = df$change_date[vec_inconsistency],
                   download_date = rep(download_date_pir,
                                       length(vec_inconsistency))))

      # If "solve" is set to TRUE, set the layer limit to NA

      if (solve == TRUE) {
        df$horizon_limit_low[vec_inconsistency] <- NA
      }
    }




    # Rank layers ----

    # If there are multiple horizons with the same horizon_master
    # in the given profile:
    # Add a different number to each of the horizons in "layers" and
    # "layers_vec" (i.e. internally) so that they become unique

    if (length(unique(df$horizon_master[vec])) <
        length(df$horizon_master[vec])) {

      layers <- paste0(df$horizon_master[vec],
                       1:length(vec))

    } else {
      layers <- df$horizon_master[vec]
    }

    # Rank forest floors with any unknown layer limits ----

    if (!identical(vec_ff, integer(0)) &&
        (any(is.na(df$horizon_limit_up[vec_ff])) ||
         any(is.na(df$horizon_limit_low[vec_ff])))) {

      # Assert that there are maximum three forest floor layers

      assertthat::assert_that(length(vec_ff) <= 3,
                              msg = paste0("More than three forest floor ",
                                           "layers in profile ",
                                           pull(df[vec_ff[1], ],
                                                unique_survey_profile)))

      ## If there is one forest floor layer ----

      if (length(vec_ff) == 1) {
        df$layer_number[vec_ff] <- 1
      }



      ## If there are two forest floor layers ----
      # Use the data frame "layer_number_two_forest_floor_layers"

      if (length(vec_ff) == 2) {

        layers_ff <- df$horizon_master[vec_ff]

        # Assert that the forest floor combination exists in
        # layer_number_two_forest_floor_layers

        # Generate all combinations of two unique values
        all_combinations <-
          apply(
            layer_number_two_forest_floor_layers[,
                 3:ncol(layer_number_two_forest_floor_layers)], 2,
            FUN = function(x) paste(x, collapse = "_"))

        # Combine the original combinations with their opposites
        all_combinations <-
          unique(c(all_combinations,
                   unlist(map(all_combinations,
                              .f = function(x) paste(rev(strsplit(x, "_")[[1]]),
                                                     collapse = "_")))))

        assertthat::assert_that(
          paste(df$horizon_master[vec_ff], collapse = "_") %in%
            all_combinations,
          msg = paste0("Unknown combination of two forest floor layers ('",
                       df$horizon_master[vec_ff[1]], "' and '",
                       df$horizon_master[vec_ff[2]],
                       "')."))

          # If the forest floor layers are not called "O" and "O2"

          for (j in c(3, 5:ncol(layer_number_two_forest_floor_layers))) {

            if (!is.na(match(layers_ff[1],
                             layer_number_two_forest_floor_layers[, j])) &&
                !is.na(match(layers_ff[2],
                             layer_number_two_forest_floor_layers[, j]))) {

              if (df$horizon_master[vec_ff[1]] ==
                  layer_number_two_forest_floor_layers[1, j]) {

                df$layer_number[vec_ff[1]] <- 1
                df$layer_number[vec_ff[2]] <- 2

              } else {

                df$layer_number[vec_ff[1]] <- 2
                df$layer_number[vec_ff[2]] <- 1

              }
            }
          }

      } # End of "if two forest floor layers"


      ## If there are three forest floor layers ----
      # Use the data frame "layer_number_three_forest_floor_layers"

      if (length(vec_ff) == 3) {

        layers_ff <- df$horizon_master[vec_ff]

        # Assert that the combination of three forest floor layers is known

        # Test if all elements of layers are in the dataframe

        sorted_combinations <-
          apply(layer_number_three_forest_floor_layers[, -1], 2,
                function(x) paste(sort(x), collapse = "_"))

        assertthat::assert_that(
          paste(sort(layers_ff), collapse = "_") %in% sorted_combinations,
          msg = paste0("Unknown combination of three forest floor layers ('",
                       df$horizon_master[vec_ff[1]], "', '",
                       df$horizon_master[vec_ff[2]], "' and '",
                       df$horizon_master[vec_ff[3]],
                       "')."))

        col_table <-
          sorted_combinations[which(sorted_combinations ==
                                      paste(sort(layers_ff), collapse = "_"))]

        col_table <-
          which(names(layer_number_three_forest_floor_layers) ==
                  names(col_table))


        df$layer_number[vec_ff[
          match(layer_number_three_forest_floor_layers[1, col_table],
                layers_ff)]] <- 1
        df$layer_number[vec_ff[
          match(layer_number_three_forest_floor_layers[2, col_table],
                layers_ff)]] <- 2
        df$layer_number[vec_ff[
          match(layer_number_three_forest_floor_layers[3, col_table],
                layers_ff)]] <- 3

      } # End of "if three forest floor layers"



      # If any FF layer limits are known and any FF layer limits are unknown

      if (any(!is.na(df$horizon_limit_up[vec_ff])) &&
          any(!is.na(df$horizon_limit_low[vec_ff]))) {

        # Assert that the sequence of the known layer limits is the same
        # like the sequence of the layer number

        assertthat::assert_that(identical(
          rank(df$layer_number[vec_ff[
            which(!is.na(df$horizon_limit_up[vec_ff]))]],
            na.last = "keep"),
          rank(df$horizon_limit_up[vec_ff[
            which(!is.na(df$horizon_limit_up[vec_ff]))]],
            na.last = "keep")))

        assertthat::assert_that(identical(
          rank(df$layer_number[vec_ff[
            which(!is.na(df$horizon_limit_low[vec_ff]))]],
            na.last = "keep"),
          rank(df$horizon_limit_low[vec_ff[
            which(!is.na(df$horizon_limit_low[vec_ff]))]],
            na.last = "keep")))

        # Assert that the sequence of the FF horizon_numbers is the same
        # like the sequence of the FF layer numbers

        assertthat::assert_that(identical(
          rank(df$layer_number[vec_ff], na.last = "keep"),
          rank(df$horizon_number[vec_ff], na.last = "keep")))

      }

    }  # End of "if forest floors with any unknown layer limits"


    # Else, rank layers based on horizon_number ----

    vec_horizon_number <- vec[which(is.na(df$layer_number[vec]) &
                                !is.na(df$horizon_number[vec]))]

    if (!identical(vec_horizon_number,
                   integer(0))) {

      # Determine maximum layer number at the moment, if any

      if (all(is.na(df$layer_number[vec]))) {
        layer_number_max <- 0
      } else {
        layer_number_max <- as.numeric(max(df$layer_number[vec],
                                           na.rm = TRUE))
      }

      # Determine records with layer limits to be ranked

      vec_nonempty <- vec_horizon_number[
        which(!is.na(df$horizon_limit_up[vec_horizon_number]) &
                          !is.na(df$horizon_limit_low[vec_horizon_number]))]


      # Check if the ranking based on horizon_limit_up is the same
      # like the ranking based on horizon_limit_low
      # AND
      # if the ranking based on horizon_limit_up is the same
      # like the ranking based on horizon_number
      # (only for records with layer limit information)

      if (identical(rank(as.numeric(df$horizon_limit_up[vec_nonempty]),
                         na.last = "keep"),
                    rank(as.numeric(df$horizon_limit_low[vec_nonempty]),
                         na.last = "keep")) &&
          identical(rank(as.numeric(df$horizon_limit_up[vec_nonempty]),
                         na.last = "keep"),
                    rank(as.numeric(df$horizon_number[vec_nonempty]),
                         na.last = "keep"))) {

        # Rank the layers based on horizon_number
        # (including layers without layer limit information)

        df$layer_number[vec_horizon_number] <-
          rank(as.numeric(df$horizon_number[vec_horizon_number]),
               na.last = "keep") +
          layer_number_max

        # Special case:
        # 52_2019_11_3
        # Contains two times the same record (i.e. same horizon_master and
        # layer limits, but one of the two records clearly has wrong data)


        if (survey_form == "so_pfh" &
            unique(df$unique_survey_profile[vec]) == "52_2019_11_3") {

          layer_to_check <- df[vec, ] %>%
            mutate(dupl = (duplicated(horizon_master) &
                     duplicated(horizon_limit_up) &
                     duplicated(horizon_limit_low)) |
                     (duplicated(horizon_master, fromLast = TRUE) &
                        duplicated(horizon_limit_up, fromLast = TRUE) &
                        duplicated(horizon_limit_low, fromLast = TRUE))) %>%
            filter(dupl == TRUE)

          if (nrow(layer_to_check) == 2) {

            layer_to_check <- layer_to_check %>%
              filter(horizon_clay > 40)

            df$layer_number[which(
              df$code_line == pull(layer_to_check, code_line))] <- NA

            df$layer_number[vec] <- rank(as.numeric(df$layer_number[vec]),
                                         na.last = "keep")
          }
        }


      } else {

        # If the ranking based on horizon_limit_up is not the same
        # like the ranking based on horizon_limit_low
        # or the one based on horizon_number

        # Check for redundant layers

        source("./src/functions/get_redundant_layers.R")

        redundant_layers <- get_redundant_layers(
          layers = layers[which(vec %in% vec_nonempty)],
          superior_layer_limits = df$horizon_limit_up[vec_nonempty],
          inferior_layer_limits = df$horizon_limit_low[vec_nonempty],
          df_sub = as.data.frame(df[vec_nonempty, ]))

        # If there are any redundant layers

        if (nrow(redundant_layers) > 0) {

        assertthat::assert_that(
          nrow(filter(redundant_layers, redundancy_type == 0)) > 0)

        redundant_layers <- redundant_layers %>%
          filter(redundancy_type == 0)

        # Determine the non-redundant records with layer limits

        vec_non_redundant <-
          vec_nonempty[which(!layers[which(vec %in% vec_nonempty)] %in%
                               c(redundant_layers$layer,
                                 redundant_layers$combined_layer))]


        } else {

          # If there are no redundant layers

          vec_non_redundant <- vec_nonempty
        }


        # Assert that the ranking based on horizon_limit_up is the same
        # like the ranking based on horizon_limit_low

        assertthat::assert_that(
          (identical(rank(as.numeric(df$horizon_limit_up[vec_non_redundant]),
                          na.last = "keep"),
                     rank(as.numeric(df$horizon_limit_low[vec_non_redundant]),
                          na.last = "keep"))))

        # Rank the layers based on horizon_limit_up

        df$layer_number[vec_non_redundant] <-
          rank(as.numeric(df$horizon_limit_up[vec_non_redundant]),
               na.last = "keep") +
          layer_number_max








        # Check if there is any non-redundant layer without layer limits
        # at the bottom of the profile

        vec_redundant <- vec[which(is.na(df$layer_number[vec]))]

        if (!identical(vec_redundant, integer(0))) {

          # Filter for rows with a higher horizon_number than the maximum
          # layer number and any unknown layer limits

          horizon_ranked <- rank(df$horizon_number[vec], na.last = "keep")

          vec_to_check <-
            vec_redundant[which(horizon_ranked[which(vec %in% vec_redundant)] >
                                  max(df$layer_number[vec], na.rm = TRUE) &
                                  (is.na(df$horizon_limit_up[vec_redundant]) |
                                     is.na(df$horizon_limit_low[
                                       vec_redundant])))]

          if (!identical(vec_to_check, integer(0))) {

            assertthat::assert_that(length(vec_to_check) == 1)

            # Check if the records contains any data

              data_to_check <- df[vec_to_check,
                                  which(names(df) == "colour_moist"):
                                    which(names(df) == "horizon_bulk_dens_est")]

            if (rowSums(!is.na(data_to_check)) > 0) {

              # Check if the profile depth is lower than the current lowest
              # depth

              eff_soil_depth_i <- prf_agg %>%
                filter(plot_id == unique(pull(df[vec, ], plot_id))) %>%
                pull(eff_soil_depth)

              max_depths_i <- df[vec, ] %>%
                filter(!is.na(layer_number)) %>%
                reframe(max_top = max(horizon_limit_up, na.rm = TRUE),
                        max_bottom = max(horizon_limit_low, na.rm = TRUE))

              if (eff_soil_depth_i > max(max_depths_i, na.rm = TRUE)) {

                df$layer_number[vec_to_check] <-
                  max(df$layer_number[vec], na.rm = TRUE) + 1
              }
            }
          }
        } # End of "if any redundant record at the bottom"

      } # End of "if the ranking of upper limits is not the same like
        # the ranking of lower limits

    } # End of "rank based on layer limits"


    # Check if any R horizon at the bottom ----

    vec_non_redundant <- vec[which(!is.na(df$layer_number[vec]))]

    if (!identical(vec_non_redundant, integer(0))) {

    vec_r <-
      vec_non_redundant[which(df$horizon_master[vec_non_redundant] == "R")]

    if (!identical(vec_r, integer(0)) &&
        df$layer_number[vec_r] == max(df$layer_number[vec_non_redundant])) {

      # Consider this layer redundant, since no soil in bedrock
      # (largely unweathered hard rock)

      df$layer_number[vec_r] <- NA

    }
    }

    # Check if any horizon containing R at the bottom

    vec_non_redundant <- vec[which(!is.na(df$layer_number[vec]))]

    if (!identical(vec_non_redundant, integer(0))) {

    vec_r <-
      vec_non_redundant[grepl("R",
                              df$horizon_master[vec_non_redundant],
                              ignore.case = TRUE)]

    # Start from the lowest record with R

    vec_r <- vec_r[order(df$layer_number[vec_r], decreasing = TRUE)]

    if (!identical(vec_r, integer(0))) {

      eff_soil_depth_i <- prf_agg %>%
        filter(plot_id == unique(pull(df[vec, ], plot_id))) %>%
        pull(eff_soil_depth)

      for (j in vec_r) {

        # If the record is the lowest record AND
        # (if the upper layer limit is >= the effective soil depth OR
        #  the coarse fragments content is not reported):

        # Reason to assume that the layer is redundant
        # (because if the upper layer limit is above the effective soil
        # depth and the coarse fragment content is known,
        # we can trust analyses, e.g. carbon density calculations, for
        # the given profile)

        if ((df$layer_number[j] == max(df$layer_number[vec_non_redundant],
                                           na.rm = TRUE)) &&
            (df$horizon_limit_up[j] >= eff_soil_depth_i ||
             (is.na(df$code_horizon_coarse_vol[j]) &
              is.na(df$horizon_coarse_weight[j])))) {

          # Consider redundant

          df$layer_number[j] <- NA

        }
    }
    }

    }


    # Check if there are any redundant layers left ----
    # as an intermediate layer that can be omitted,
    # most likely in the forest floor

    if (any(!is.na(df$layer_number[vec])) &&
        (any(!is.na(df$horizon_limit_up[vec])) &&
         any(!is.na(df$horizon_limit_low[vec])))) {

      layers_redundant <- df[vec, ] %>%
        filter(!is.na(layer_number)) %>%
        arrange(layer_number) %>%
        mutate(
          prev_horizon_limit_low = lag(.data$horizon_limit_low),
          next_horizon_limit_up = lead(.data$horizon_limit_up)) %>%
        filter(!is.na(prev_horizon_limit_low) &
                 !is.na(next_horizon_limit_up) &
                 (round(prev_horizon_limit_low, 1) ==
                    round(next_horizon_limit_up, 1)))

      # assertthat::assert_that(
      #   all(pull(layers_redundant, layer_type) != "mineral"))

      assertthat::assert_that(
        all(is.na(pull(layers_redundant, horizon_limit_up))) &&
          all(is.na(pull(layers_redundant, horizon_limit_low))))

      if (nrow(layers_redundant) > 0) {

      # Evaluate each of the rows

      for (j in seq_along(pull(layers_redundant, code_line))) {

        layers_redundant_j <- layers_redundant %>%
          filter(code_line == pull(layers_redundant, code_line)[j])

        ind_j <- which(df$code_line == pull(layers_redundant, code_line)[j])

        # If the organic_layer_weight or the organic_carbon_content
        # are known:

        if (!is.na(pull(layers_redundant_j, horizon_c_organic_total))) {

          # Place it on top of the profile

          df$layer_number[ind_j] <- min(df$layer_number[vec], na.rm = TRUE) - 1

        } else {

          # If no organic_layer_weight and organic_carbon_total are known
          # Consider the layer redundant

          df$layer_number[ind_j] <- NA
        }
      }

        # Rank the layer numbers again

        vec_nonempty <- vec[which(!is.na(df$layer_number[vec]))]

        df$layer_number[vec_nonempty] <-
          rank(as.numeric(df$layer_number[vec_nonempty]),
               na.last = "keep")


    }

    } # End of "if any additional redundant layers"






#
#     # Retrieve the row indices and horizon_master from layers with known layer
#     # limits
#
#     vec_nonempty <- vec[which(!is.na(df$horizon_limit_up[vec]) &
#                                 !is.na(df$horizon_limit_low[vec]))]
#     layers <- df$horizon_master[vec_nonempty]
#     layers_vec <- df$horizon_master[vec]
#
#     # If there are multiple horizons with the same horizon_master
#     # in the given profile:
#     # Add a different number to each of the horizons in "layers" and
#     # "layers_vec" (i.e. internally) so that they become unique
#
#     if (length(unique(df$horizon_master[vec_nonempty])) <
#         length(df$horizon_master[vec_nonempty])) {
#
#       layers <- paste0(df$horizon_master[vec_nonempty],
#                        1:length(layers))
#       layers_vec <- paste0(df$horizon_master[vec],
#                            1:length(layers_vec))
#       }
#
#     # Store the horizon limits and the relevant subset of "df"
#
#     superior_layer_limits <- df$horizon_limit_up[vec_nonempty]
#     inferior_layer_limits <- df$horizon_limit_low[vec_nonempty]
#     df_sub <- df[vec_nonempty, ]
#
#     # If there are more than two horizons:
#     # get the redundant layers via the function "get_redundant_layers()"
#     # and call it "redundant_layers"
#
#     if (length(vec_nonempty) >= 2) {
#
#       redundant_layers <-
#         get_redundant_layers(layers = layers,
#                              superior_layer_limits = superior_layer_limits,
#                              inferior_layer_limits = inferior_layer_limits,
#                              df_sub = df_sub)
#
#     } else {
#         redundant_layers <- integer(0)
#     }
#
# #     # Check if there are any redundant layers
#
#     if (!identical(redundant_layers, integer(0))) {
#
#     # Create a summary dataframe "redundant_layers_summary" with all the layers
#     # in the given unique profile
#     # to store information about the redundant layers
#
#       redundant_layers_summary <-
#         data.frame(layer = layers_vec,
#                    redundancy_type = rep(NA, length(layers_vec)),
#                    combined_layer = rep(NA, length(layers_vec)))
#
#     # Store the row indices of "redundant_layers_summary" that are mentioned
#     # in "redundant_layers", and evaluate for each of them:
#
#       # vec_redundant <- match(unique(redundant_layers$layer), layers_vec)
#       #
#       # for (j in vec_redundant) {
#       #
#       #   # Store the row indices of j in redundant_layers
#       #
#       #   vec_j <- which(layers_vec[j] == redundant_layers$layer)
#       #
#       #   # If there is any "redundancy_type" 0:
#       #   # Assign a "redundancy_type" 0
#       #
#       #   if (any(redundant_layers$redundancy_type[vec_j] == 0)) {
#       #
#       #     redundant_layers_summary$redundancy_type[j] <- 0
#       #     } else
#       #
#       #   # Else, if there is any "redundancy_type" 3:
#       #   # Assign a "redundancy_type" 3 + store "combined_layer"
#       #
#       #   if (any(redundant_layers$redundancy_type[vec_j] == 3)) {
#       #
#       #     vec_3 <- vec_j[which(redundant_layers$redundancy_type[vec_j] == 3)]
#       #     redundant_layers_summary$redundancy_type[j] <- 3
#       #     redundant_layers_summary$combined_layer[j] <-
#       #                           redundant_layers$combined_layer[vec_3]
#       #     } else
#       #
#       #   # Else, if there is any "redundancy_type" 2:
#       #   # Assign a "redundancy_type" 2 + store "combined_layer"
#       #
#       #   if (any(redundant_layers$redundancy_type[vec_j] == 2)) {
#       #     vec_2 <- vec_j[which(redundant_layers$redundancy_type[vec_j] == 2)]
#       #
#       #     if (length(vec_2) == 2) {
#       #
#       #       redundant_layers_summary$redundancy_type[j] <- 2
#       #       redundant_layers_summary$combined_layer[j] <-
#       #         paste0(redundant_layers$combined_layer[vec_2[1]], "_",
#       #              redundant_layers$combined_layer[vec_2[1]])
#       #       } else {
#       #
#       #       redundant_layers_summary$redundancy_type[j] <- 2
#       #       redundant_layers_summary$combined_layer[j] <-
#       #         redundant_layers$combined_layer[vec_2]
#       #       }
#       #
#       #    # Else (if there is any "redundancy_type" 1):
#       #    # Assign a "redundancy_type" 1
#       #
#       #     } else {
#       #     redundant_layers_summary$redundancy_type[j] <- 1
#       #     }
#       #   }
#
#
#       # Check if there are any redundant layers with redundancy type 0
#       # (i.e. to remove from the layer ranking)
#
#       if (!identical(which(redundant_layers$redundancy_type == 0),
#                      integer(0))) {
#
#         # Store these layers as "redundant_layers_to_remove"
#
#         redundant_layers_to_remove <-
#           redundant_layers$layer[which(redundant_layers$redundancy_type == 0)]
#
#         # Store the row indices in df of these layers
#
#         vec_redundant <- NULL
#
#         for (j in seq_along(redundant_layers_to_remove)) {
#           vec_redundant <- c(vec_redundant,
#                              vec_nonempty[
#                                which(layers == redundant_layers_to_remove[j])])
#           }
#
#         # "vec_unique" is the vector with row indices of the given profile
#         # from which the redundant layers are excluded
#
#         vec_unique <- vec[which((!vec %in% vec_redundant) &
#                                   (!is.na(df$horizon_limit_up[vec]) |
#                                      !is.na(df$horizon_limit_low[vec])))]
#         inferior_layer_limits_sub <- df$horizon_limit_low[vec_unique]
#         superior_layer_limits_sub <- df$horizon_limit_up[vec_unique]
#
#         # Rank the horizons of the given profile
#
#         # (excluding redundant layers)
#         # based on the inferior layer limits in the column
#         # "layer_number"
#
#         df$layer_number[vec_unique] <- rank(superior_layer_limits_sub,
#                                             na.last = "keep")
#
#
#         # Store information about the redundant_layers in
#         # "list_redundant_layers"
#         # (this is not reported in PIR)
#
#         inconsistency_reason <-
#           paste0("Caution: the given layer is redundant in the",
#                  " depth profile (i.e. a complete profile can be",
#                  " composed with the remaining layers).")
#         inconsistency_reason_vec <- rep(NA, length(vec))
#         inconsistency_reason_vec[which(!is.na(
#           match(layers_vec, redundant_layers_to_remove)))] <-
#           inconsistency_reason
#
#         rule_id <- rep(NA,length(vec))
#         rule_id[which(!is.na(match(layers_vec, redundant_layers_to_remove)))] <-
#               "[not in PIR]"
#
#         list_redundant_layers <- rbind(
#            list_redundant_layers,
#            data.frame(survey_form = as.factor(rep(survey_form, length(vec))),
#                       partner = df$partner[vec],
#                       partner_code = df$partner_code[vec],
#                       country = df$country[vec],
#                       code_country = df$code_country[vec],
#                       survey_year = df$survey_year[vec],
#                       code_plot = df$code_plot[vec],
#                       plot_id = df$plot_id[vec],
#                       horizon_master = df$horizon_master[vec],
#                       redundancy_type =
#                         redundant_layers_summary$redundancy_type,
#                       combined_redundant_horizon_master =
#                         redundant_layers_summary$combined_layer,
#                       superior_layer_limit = df$horizon_limit_up[vec],
#                       inferior_layer_limit = df$horizon_limit_low[vec],
#                       horizon_number_original = df$horizon_number[vec],
#                       layer_number = df$layer_number[vec],
#                       code_line = df$code_line[vec],
#                       inconsistency_reason = inconsistency_reason_vec,
#                       rule_id = rule_id,
#                       change_date = df$change_date[vec],
#                       download_date = rep(download_date_pir, length(vec))))
#       }
#       } # End of "if there are redundant layers"
#
# #
#       # If the column "layer_number" is still empty
#       # (i.e. if there are no redundant layers):
#
#      if (length(vec) >= 2) {
#
#       if (all(is.na(df$layer_number[vec]))) {
#
#       # If the ranking of the existing column "horizon_number"
#       # (which was submitted by the partners)
#       # equals the ranking based on the upper layer limits
#       # (which is not necessarily the case):
#
#       # (we use horizon_limit_up in "pfh" because the lower horizon limit
#       # is sometimes not known for the lowest pedogenetic horizon)
#
#       # Copy the column "horizon_number" to "layer_number"
#
#       # Else,
#       # Rank the horizons of the given profile
#       # based on the inferior layer limits in the column "layer_number"
#
#       if (all(identical(
#               rank(df$horizon_number[vec],
#                    na.last = "keep"),
#               rank(df$horizon_limit_up[vec],
#                    na.last = "keep"))) ||
#           all(rank(df$horizon_number[vec],
#                    na.last = "keep") ==
#               df$horizon_number[vec])) {
#       df$layer_number[vec] <- rank(df$horizon_number[vec],
#                                    na.last = "keep")
#       } else {
#       df$layer_number[vec] <-  rank(df$horizon_limit_up[vec],
#                                              na.last = "keep")
#       }
#       }
#
#     # If there is one intermediate peat/mineral row with unknown
#     # layer limits: remove from rank (redundant)
#
#     layer_number_max_i <- max(df$layer_number[vec], na.rm = TRUE)
#
#     layer_to_check <- df[vec, ] %>%
#       arrange(unique_survey_profile, layer_number) %>%
#       group_by(unique_survey_profile) %>%
#       # Add a new column displaying the horizon_limit_low value of the
#       # previous row for each group of unique_survey_profile
#       mutate(prev_horizon_limit_low = lag(horizon_limit_low)) %>%
#       # Add a new column displaying the horizon_limit_up value of the
#       # next row for each group of unique_survey_profile
#       mutate(next_horizon_limit_up = lead(horizon_limit_up)) %>%
#       filter(!is.na(layer_number)) %>% #
#       filter(layer_type %in% c("peat", "mineral")) %>%
#       filter(is.na(.data$horizon_limit_up) &
#                is.na(.data$horizon_limit_low)) %>%
#       # Requirement: horizon_limit_low above should be the same like the
#       # horizon_limit_up below
#       filter((prev_horizon_limit_low == next_horizon_limit_up) |
#                is.na(prev_horizon_limit_low) |
#                is.na(next_horizon_limit_up))
#
#     if (nrow(layer_to_check) <= 2) {
#       layer_to_check <- layer_to_check %>%
#         # Not the lowest layer, because that may still be informative
#         # even without layer limit info
#         filter(layer_number < layer_number_max_i)
#     }
#
#     layer_to_check <- layer_to_check %>%
#       pull(code_line)
#
#      if (!identical(layer_to_check, character(0))) {
#
#        # Replace this layer number by NA
#
#        df$layer_number[vec[which(df$code_line[vec] %in%
#                                    layer_to_check)]] <- NA
#
#        df$layer_number[vec] <- rank(df$layer_number[vec],
#                                              na.last = "keep")
#      }
#     }



    ## FSCC_10: Error reason: there is a gap between adjacent layers ----

    # Check whether layers have layer limit information and a horizon number

    vec_nonempty <- vec[which(!is.na(df$horizon_limit_up[vec]) &
                                !is.na(df$horizon_limit_low[vec]) &
                                !is.na(df$layer_number[vec]))]

    # Order the row indices

    vec_horizon_number <-
      vec_nonempty[order(df$layer_number[vec_nonempty])]

    # Check if more than one layer has the required information

    if (length(vec_nonempty) > 1) {

      # Check for all layers (row indices)
      # whether the inferior layer limit does not equal the superior layer limit
      # of the next layer

    for (j in vec_horizon_number[1:(length(vec_horizon_number) - 1)]) {

    if (df$horizon_limit_low[j] !=
         df$horizon_limit_up[vec_horizon_number[
           match(j, vec_horizon_number) + 1]]) {

    j_next <- vec_horizon_number[match(j, vec_horizon_number) + 1]

    # Store information about the inconsistency in "list_layer_inconsistencies"

    # Inferior of current layer j

    # If the layer limit information has been updated in this script
    # i.e. if "layer_limit_inferior" does not equal "layer_limit_inferior_orig:
    # Mention that this was based on a value "estimated by FSCC"

    if ((!is.na(df$horizon_limit_low[j]) &&
         is.na(df$horizon_limit_low_orig[j])) ||
        (df$horizon_limit_low[j] != df$horizon_limit_low_orig[j])) {
    parameter_value_error <- paste0(df$horizon_limit_low[j],
                                     " (estimated by FSCC)")
    } else {
      parameter_value_error <- df$horizon_limit_low_orig[j]
    }

    rule_id <- "FSCC_10"
    inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
      which(inconsistency_catalogue$rule_id == rule_id)]
    inconsistency_type <- inconsistency_catalogue$inconsistency_type[
      which(inconsistency_catalogue$rule_id == rule_id)]

    list_layer_inconsistencies <- rbind(
      list_layer_inconsistencies,
      data.frame(survey_form = as.factor(rep(survey_form, length(j))),
                 partner = df$partner[j],
                 partner_code = df$partner_code[j],
                 country = df$country[j],
                 code_country = df$code_country[j],
                 survey_year = df$survey_year[j],
                 code_plot = df$code_plot[j],
                 plot_id = df$plot_id[j],
                 code_layer_horizon_master = df$horizon_master[j],
                 repetition_profile_pit_id = df$profile_pit_id[j],
                 code_line = df$code_line[j],
                 parameter = as.factor(rep("horizon_limit_low", length(j))),
                 parameter_unit = as.factor(rep("cm", length(j))),
                 parameter_value = parameter_value_error,
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(TRUE, length(j)),
                 change_date = df$change_date[j],
                 download_date = rep(download_date_pir, length(j))))

    # Superior of next layer j_next

    # If the layer limit information has been updated in this script
    # i.e. if "layer_limit_superior" does not equal "layer_limit_superior_orig:
    # Mention that this was based on a value "estimated by FSCC"

    rule_id <- "FSCC_10"
    inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
      which(inconsistency_catalogue$rule_id == rule_id)]
    inconsistency_type <- inconsistency_catalogue$inconsistency_type[
      which(inconsistency_catalogue$rule_id == rule_id)]

    if ((!is.na(df$horizon_limit_up[j_next]) &&
         is.na(df$horizon_limit_up_orig[j_next])) ||
        (df$horizon_limit_up[j_next] != df$horizon_limit_up_orig[j_next])) {
    parameter_value_error <- paste0(df$horizon_limit_up[j_next],
                                     " (estimated by FSCC)")
    } else {
      parameter_value_error <- df$horizon_limit_up_orig[j_next]
    }

    list_layer_inconsistencies <- rbind(
      list_layer_inconsistencies,
      data.frame(survey_form = as.factor(rep(survey_form,
                                             length(j_next))),
                 partner = df$partner[j_next],
                 partner_code = df$partner_code[j_next],
                 country = df$country[j_next],
                 code_country = df$code_country[j_next],
                 survey_year = df$survey_year[j_next],
                 code_plot = df$code_plot[j_next],
                 plot_id = df$plot_id[j_next],
                 code_layer_horizon_master = df$horizon_master[j_next],
                 repetition_profile_pit_id = df$profile_pit_id[j_next],
                 code_line = df$code_line[j_next],
                 parameter = as.factor(rep("horizon_limit_up",
                                           length(j_next))),
                 parameter_unit = as.factor(rep("cm", length(j_next))),
                 parameter_value = parameter_value_error,
                 inconsistency_reason = inconsistency_reason,
                 inconsistency_type = inconsistency_type,
                 rule_id = rule_id,
                 non_duplicated_error_type_per_record = rep(FALSE,
                                                            length(j_next)),
                 change_date = df$change_date[j_next],
                 download_date = rep(download_date_pir, length(j_next))))
    }
    }
    }





    # Step intermediate - layer_type conversion for mineral to peat ----
    # This depends on the TOC content.

    # Exceptions:
    # - if some repetitions of the same plot_id and the same layer have a TOC
    #   within the mineral range (manually verified)
    # - if the TOC has been confirmed as extreme but correct in the PIRs

    # Note: We don't do the other way around: converting forest floor layers
    # with too low TOC to mineral.

    if (any(df$layer_type[vec] == "mineral")) {

      # organic_carbon_total - based on ranges_qaqc
      upper_limit_mineral <- 150
      upper_limit_mineral_top <- 200
      lower_limit_organic <- 93

      prof_i <- df[vec, ] %>%
        mutate(duplicated_horizon_master =
                 duplicated(horizon_master) |
                 duplicated(horizon_master, fromLast = TRUE)) %>%
        ungroup() %>%
        mutate(horizon_master = ifelse(.data$duplicated_horizon_master == TRUE,
                                       paste0(.data$horizon_master, "_",
                                              row_number()),
                                       .data$horizon_master)) %>%
        select(-duplicated_horizon_master) %>%
        filter(!is.na(layer_number)) %>%
        arrange(layer_number) %>%
        filter(!is.na(horizon_c_organic_total)) %>%
        select(horizon_master, layer_type, horizon_limit_up, horizon_limit_low,
               horizon_c_organic_total) %>%
        mutate(convert_layer_type = case_when(
          horizon_limit_up == 0 &
            layer_type == "mineral" &
            horizon_c_organic_total > upper_limit_mineral_top ~ "to_organic",
          horizon_limit_up > 0 &
            layer_type == "mineral" &
            horizon_c_organic_total > upper_limit_mineral_top ~ "to_organic",
          horizon_limit_up > 0 &
            layer_type == "mineral" &
            horizon_c_organic_total > upper_limit_mineral &
            (lag(layer_type != "mineral",
                  default = TRUE) |
               lag(horizon_c_organic_total > upper_limit_mineral_top,
                    default = TRUE)) ~ "to_organic",
          TRUE ~ NA_character_))

      if (nrow(prof_i) > 0 &&
          any(!is.na(pull(prof_i, convert_layer_type)))) {

        # Which layers may have to be converted?

        # horizon_master

        layers_to_check <-
          pull(filter(prof_i, !is.na(convert_layer_type)), horizon_master)

        # row indices

        vec_min_to_peat <- df[vec, ] %>%
          mutate(duplicated_horizon_master =
                   duplicated(horizon_master) |
                   duplicated(horizon_master, fromLast = TRUE)) %>%
          ungroup() %>%
          mutate(horizon_master = ifelse(.data$duplicated_horizon_master ==
                                           TRUE,
                                         paste0(.data$horizon_master, "_",
                                                row_number()),
                                         .data$horizon_master)) %>%
          select(-duplicated_horizon_master) %>%
          mutate(min_to_peat = (.data$horizon_master %in% layers_to_check)) %>%
          pull(min_to_peat) %>%
          which()

        vec_min_to_peat <- vec[vec_min_to_peat]


        # Exception: don't convert if not all depths of this plot_id
        # are in the organic range.

        # They should belong to those unique surveys (as manually evaluated):

        so_pfh_dont_convert <- c("67_2010_2", "52_2019_10")
        s1_pfh_dont_convert <- c("64_2007_723", "64_2008_753", "64_2008_997")

        # Retrieve those records of the same unique_survey

        lines_to_be_consistent <- NULL

        for (k in vec_min_to_peat) {

          depth_range_k <- df[k, ] %>%
            mutate(depth_sequence =
                     purrr::pmap(list(round(horizon_limit_up, 1),
                                      round(horizon_limit_low, 1)),
                                 seq, by = 0.1)) %>%
            pull(depth_sequence) %>%
            unlist %>%
            unique

          lines_to_be_consistent_k <- df %>%
            filter(unique_survey ==
                     unique(pull(df[vec_min_to_peat, ], unique_survey))) %>%
            rowwise() %>%
            filter(any(depth_range_k > .data$horizon_limit_up &
                         depth_range_k < .data$horizon_limit_low)) %>%
            ungroup() %>%
            group_by(unique_survey_profile) %>%
            arrange(-horizon_c_organic_total) %>%
            slice_head()

          lines_to_be_consistent <-
            bind_rows(lines_to_be_consistent,
                      lines_to_be_consistent_k)
        }





        if (all(!is.na(pull(lines_to_be_consistent,
                            horizon_c_organic_total))) &&
            any(pull(lines_to_be_consistent, horizon_c_organic_total) >
                upper_limit_mineral) &&
            any(pull(lines_to_be_consistent, horizon_c_organic_total) <
                upper_limit_mineral) &&
            ((survey_form == "s1_pfh" &&
              !unique(pull(lines_to_be_consistent, unique_survey)) %in%
              s1_pfh_dont_convert) ||
             (survey_form == "so_pfh" &&
              !unique(pull(lines_to_be_consistent, unique_survey)) %in%
              so_pfh_dont_convert))) {

          cat(paste0(unique(pull(lines_to_be_consistent, unique_survey)), "\n"))

          if (survey_form == "so_pfh") {

            # assertthat::assert_that(
            #   unique(pull(df[vec_min_to_peat, ], unique_survey)) %in%
            #     so_som_dont_convert)
          }

          if (survey_form == "s1_pfh") {

            # assertthat::assert_that(
            #   unique(pull(df[vec_min_to_peat, ], unique_survey)) %in%
            #     s1_som_dont_convert)
          }
        }

        # Convert the layer types and names if all conditions are met

        if ( # TOC is not confirmed as extreme by partners
          (!(any(pull(lines_to_be_consistent, code_line) %in%
                 pir_toc_confirmed))) &&
          # All TOC values in different repetitions should be higher than 150
          # and known
          (all(!is.na(pull(lines_to_be_consistent,
                           horizon_c_organic_total))) &&
            all(pull(lines_to_be_consistent, horizon_c_organic_total) >
               upper_limit_mineral)) &&
          # The unique_survey should not be manually listed as "dont convert"
          # (due to consistency reasons)
          ((survey_form == "so_pfh" &&
            !(any(unique(pull(lines_to_be_consistent, unique_survey)) %in%
                  so_pfh_dont_convert))) ||
           (survey_form == "s1_pfh" &&
            !(any(unique(pull(lines_to_be_consistent, unique_survey)) %in%
                  s1_pfh_dont_convert))))) {

          # Immediately convert all layers in lines_to_be_consistent

          df <- df %>%
            # mutate(across(c(code_layer,
            #                 unique_survey_layer,
            #                 unique_layer,
            #                 unique_layer_repetition),
            #               ~ ifelse(code_line %in% pull(lines_to_be_consistent,
            #                                            code_line),
            #                        str_replace_all(., "M", "H"),
            #                        .))) %>%
            mutate(layer_type = ifelse(.data$code_line %in%
                                         pull(lines_to_be_consistent,
                                              code_line),
                                       "peat",
                                       .data$layer_type))

        }
      }
    }












    # Layer_type conversion for peat layers to ff ----
    # If needed

    # Rule for organic H layers:
    # - if code_layer is H and no layer limits are provided,
    #   the layer should be in the forest floor. Change layer_type to
    #   forest_floor.
    # - if organic H layers are < 40 cm thick in total (and below any
    #   forest floor or above any mineral soil), this layer(s) should be
    #   considered as the forest floor. Change layer_type to forest_floor.
    # - if organic H layers are >= 40 cm thick in total, they can be
    #   considered as actual peat layers.

    # There is only one peat layer with unknown layer limits:
    # in unique_survey_profile: 55_1987_10_10a
    # The profile however looks complete, so no need to take it into account

    if (length(vec) >= 2) {

    vec_non_redundant <- vec[which(!is.na(df$layer_number[vec]))]

    if (any(df$layer_type[vec_non_redundant] == "peat") &&
        # Make an exception for some profiles
        ((survey_form == "so_pfh" &&
          !unique(df$unique_survey[vec_non_redundant]) %in% so_pfh_dont_move) ||
         (survey_form == "s1_pfh" &&
          !unique(df$unique_survey[vec_non_redundant]) %in%
                  c(s1_pfh_dont_move,
                    "60_2006_83")))) {

      vec_peat <-
        vec_non_redundant[which(df$layer_type[vec_non_redundant] == "peat")]

      source("./src/functions/summarise_profile_per_depth_layer_type.R")

      df_sub <-
        summarise_profile_per_depth_layer_type(df[vec_non_redundant, ])



      vec_df_sub_peat <- which(df_sub$layer_type == "peat")

      for (j in vec_df_sub_peat) {

        assertthat::assert_that(!is.na(df_sub$total_thickness[j]))

        # First row (i.e. on top)
        if (((j == 1 ||
              # Below forest_floor
              df_sub$layer_type[j - 1] == "forest_floor") &&
             # The so-called below-ground should be at least 40 cm deep,
             # otherwise we can't know for sure if the peat layer is thicker
             # than 40
             (sum(df_sub$total_thickness, na.rm = TRUE) >= 40)) &&
            # The total_thickness is less than 40
            df_sub$total_thickness[j] < 40) {

          # Then it should be considered as forest_floor

          vec_peat_to_ff <-
            vec_peat[which(
              (df$horizon_limit_up[vec_peat] >=
                 df_sub$horizon_limit_up[j]) &
                df$horizon_limit_low[vec_peat] <=
                df_sub$horizon_limit_low[j])]

          df$layer_type[vec_peat_to_ff] <- "forest_floor"
        }
      }
    }


    # Forest floors thicker than 40 cm ----

    if (any(df$layer_type[vec_non_redundant] == "forest_floor")) {

      vec_ff <- vec[which(df$layer_type[vec] == "forest_floor")]

      source("./src/functions/summarise_profile_per_depth_layer_type.R")

      df_sub <-
        summarise_profile_per_depth_layer_type(df[vec_non_redundant, ])

     if ("forest_floor" %in% df_sub$layer_type  &&
         # Make an exception for some profiles
         ((survey_form == "so_pfh" &&
           !unique(df$unique_survey[vec]) %in% so_pfh_dont_move) ||
          (survey_form == "s1_pfh" &&
           !unique(df$unique_survey[vec]) %in% s1_pfh_dont_move))) {

      # Take the first forest floor layer only,
      # because there may be buried forest floor layers

      # assertthat::assert_that(
      #   df_sub$total_thickness[which(
      #     df_sub$layer_type == "forest_floor")][1] <= 40)

       if (df_sub$total_thickness[which(
              df_sub$layer_type == "forest_floor")][1] > 40) {

           # Then it should be considered as peat?
           # Manually confirmed for the profiles in s1_pfh

         if (abs(df_sub$horizon_limit_up[1]) < 40 &&
             (df_sub$horizon_limit_low[1] > 0)) {

           # In this case, the profile shouldn't move, but the
           # below-ground layers should become peat

           vec_ff_to_peat <-
             vec_ff[which(
               (df$horizon_limit_up[vec_ff] >=
                  0) &
                 df$horizon_limit_low[vec_ff] <=
                 df_sub$horizon_limit_low[1])]

         } else {

           # Else, everything should become peat

           vec_ff_to_peat <-
             vec_ff[which(
               (df$horizon_limit_up[vec_ff] >=
                  df_sub$horizon_limit_up[1]) &
                 df$horizon_limit_low[vec_ff] <=
                 df_sub$horizon_limit_low[1])]
         }

           df$layer_type[vec_ff_to_peat] <- "peat"

         # vec_profiles_thick_ff <- c(vec_profiles_thick_ff,
         #                            unique(df$unique_survey_profile)[i])

       }
     }

      vec_ff <- vec[which(df$layer_type[vec] == "forest_floor")]

    }
    }








    # Is the zero line located between the forest floor vs mineral/peat? ----

    #   The null line should be between
    #   the forest floor (including those H layers just added;
    #   negative layer limits) and the peat/mineral layers
    #   (positive layer limits). Move null line in accordance if
    #   necessary (by shifting the layers up or down and changing their
    #   layer limits).

   if (length(vec) >= 2) {

    vec_non_redundant <- vec[which(!is.na(df$layer_number[vec]))]

    if (!identical(vec_non_redundant, integer(0))  &&
        # Make an exception for some profiles
        ((survey_form == "so_pfh" &&
          !unique(df$unique_survey[vec]) %in% so_pfh_dont_move) ||
         (survey_form == "s1_pfh" &&
          !unique(df$unique_survey[vec]) %in% s1_pfh_dont_move))) {

    source("./src/functions/summarise_profile_per_depth_layer_type.R")

    df_sub <-
      summarise_profile_per_depth_layer_type(df[vec_non_redundant, ])

    diff_to_move <- 0

    if (df_sub$layer_type[1] == "forest_floor" &&
        nrow(df_sub) > 1) {

      # If there is no gap between the forest floor and first below-ground layer
      # Ignore small gaps <= 0.1

      if (abs(df_sub$horizon_limit_low[1] -
              df_sub$horizon_limit_up[2]) <= 0.1) {

        diff_to_move <- ifelse((df_sub$horizon_limit_low[1] == 0 ||
                                  df_sub$horizon_limit_up[2] == 0),
                               0,
                               mean(c(df_sub$horizon_limit_low[1],
                                      df_sub$horizon_limit_up[2])))
      } else {

        # If there is a considerable gap, e.g. in "7_2007_10_2" (so_pfh)

        # Consider the below-ground layer limit as reference
        # and move the forest floor layers to the below-ground

        vec_to_move <-
          vec_non_redundant[which(
            (df$horizon_limit_up[vec_non_redundant] >=
               df_sub$horizon_limit_up[1]) &
              df$horizon_limit_low[vec_non_redundant] <=
              df_sub$horizon_limit_low[1])]

        if (solve == TRUE) {

          # Regardless of whether it should be moved up or down
          df$horizon_limit_low[vec_to_move] <-
            df$horizon_limit_low[vec_to_move] - df_sub$horizon_limit_low[1]
          df$horizon_limit_up[vec_to_move] <-
            df$horizon_limit_up[vec_to_move] - df_sub$horizon_limit_low[1]

          diff_to_move <- df_sub$horizon_limit_up[2]

        }

      }


      # Assure that the lower limit of this first layer equals
      # the upper limit of the second layer
      # Note: sometimes there is a small gap, e.g. 0.1 cm

      # assertthat::assert_that(
      #   abs(df_sub$horizon_limit_low[1] - df_sub$horizon_limit_up[2]) <= 0.5)
      #
      # diff_to_move <- ifelse((df_sub$horizon_limit_low[1] == 0 ||
      #                          df_sub$horizon_limit_up[2] == 0),
      #                        0,
      #                        mean(c(df_sub$horizon_limit_low[1],
      #                               df_sub$horizon_limit_up[2])))

    } else

    if (df_sub$layer_type[1] %in% c("peat", "mineral")) {

      diff_to_move <- df_sub$horizon_limit_up[1]

    }

    if (diff_to_move != 0) {

    if (solve == TRUE) {

      # Regardless of whether it should be moved up or down
      df$horizon_limit_low[vec] <- df$horizon_limit_low[vec] - diff_to_move
      df$horizon_limit_up[vec] <- df$horizon_limit_up[vec] - diff_to_move

    }
    }
    }
    }



    # Update the progress bar

    if (!isTRUE(getOption("knitr.in.progress"))) {
    setTxtProgressBar(progress_bar, i)
    }
    }

    # Final dataset preparations ----

    if (!"horizon_number_orig" %in% names(df)) {

      df <- df %>%
        rename(horizon_number_orig = horizon_number)
    }

    if (!"horizon_master_orig" %in% names(df)) {

      df <- df %>%
        mutate(horizon_master_orig = horizon_master)
    }

    df <- df %>%
      # Sort in different levels
      arrange(country,
              code_plot,
              survey_year,
              profile_pit_id,
              layer_number) %>%
      # mutate(horizon_master_orig =
      #          ifelse(exists("horizon_master_orig"),
      #                 horizon_master_orig,
      #                 horizon_master)) %>%
      group_by(unique_survey_profile) %>%
      # Some horizon_masters are not unique within the profile
      # Add the layer_number to the horizon_master name for those
      mutate(duplicated_horizon_master =
               duplicated(horizon_master) |
               duplicated(horizon_master, fromLast = TRUE)) %>%
      ungroup() %>%
      mutate(horizon_master = ifelse(.data$duplicated_horizon_master == TRUE,
                                        paste0(.data$horizon_master, "_",
                                               .data$layer_number),
                                        .data$horizon_master)) %>%
      select(-duplicated_horizon_master) %>%
      group_by(unique_survey_profile) %>%
      mutate(
        layer_number_bg_only = ifelse(.data$horizon_limit_up >= 0 &
                                        .data$horizon_limit_low >= 0,
                                      layer_number,
                                      NA),
        layer_number_bg_min = suppressWarnings(min(layer_number_bg_only,
                                                   na.rm = TRUE)),
        layer_number_bg = layer_number_bg_only - (layer_number_bg_min - 1),
        layer_number_ff = ifelse(layer_type %in% c("forest_floor") &
                                   .data$horizon_limit_up <= 0 &
                                   .data$horizon_limit_low <= 0,
                                 layer_number,
                                 NA)) %>%
      select(-layer_number_bg_only,
             -layer_number_bg_min) %>%
      # Try to gap-fill layer limits based on the limits of adjacent layers
      # of the eff_soil_depth
      mutate(layer_number_deepest = suppressWarnings(max(.data$layer_number,
                                                         na.rm = TRUE)),
             depth_max = suppressWarnings(max(.data$horizon_limit_low,
                                              na.rm = TRUE)),
             prev_horizon_limit_low = lag(.data$horizon_limit_low),
             next_horizon_limit_up = lead(.data$horizon_limit_up)) %>%
      ungroup() %>%
      left_join(prf_agg,
                by = "plot_id") %>%
      rowwise() %>%
      mutate(horizon_limit_up =
               # If the upper layer limit is unknown,
               # while the lower layer limit of the layer on top is known,
               # take this layer limit
               ifelse(is.na(.data$horizon_limit_up) &
                        !is.na(.data$prev_horizon_limit_low) &
                        !is.na(.data$layer_number) &
                        (.data$layer_number != 1),
                      .data$prev_horizon_limit_low,
                      # If the layers are overlapping:
                      # take average of the adjacent overlapping depth limits
                      ifelse(!is.na(.data$horizon_limit_up) &
                               !is.na(.data$prev_horizon_limit_low) &
                               (.data$prev_horizon_limit_low >
                                  .data$horizon_limit_up) &
                               !is.na(.data$layer_number) &
                               (.data$layer_number != 1),
                             round(mean(c(.data$horizon_limit_up,
                                          .data$prev_horizon_limit_low)), 1),
                             .data$horizon_limit_up)),
             horizon_limit_low =
               # If the lower layer limit is unknown,
               # while the upper layer limit of the layer below is known,
               # take this layer limit
               ifelse(is.na(.data$horizon_limit_low) &
                        !is.na(.data$next_horizon_limit_up) &
                        !is.na(.data$layer_number) &
                        (.data$layer_number != .data$layer_number_deepest),
                      .data$next_horizon_limit_up,
                      # If the layers are overlapping:
                      # take the average of the adjacent overlapping depth limits
                      ifelse(!is.na(.data$horizon_limit_low) &
                               !is.na(.data$next_horizon_limit_up) &
                               (.data$horizon_limit_low >
                                  .data$next_horizon_limit_up) &
                               !is.na(.data$layer_number) &
                               (.data$layer_number !=
                                  .data$layer_number_deepest),
                             round(mean(c(.data$horizon_limit_low,
                                          .data$next_horizon_limit_up)), 1),
                             # If lower layer limit of lowest layer is unknown,
                             # use the eff_soil_depth
                             ifelse(is.na(.data$horizon_limit_low) &
                                      (.data$layer_number ==
                                         .data$layer_number_deepest) &
                                      !is.na(.data$eff_soil_depth) &
                                      (.data$eff_soil_depth > .data$depth_max),
                                    .data$eff_soil_depth,
                                    .data$horizon_limit_low)))) %>%
      ungroup() %>%
      mutate(horizon_limit_up = round(horizon_limit_up, 1),
             horizon_limit_low = round(horizon_limit_low, 1)) %>%
      select(-layer_number_deepest,
             -depth_max,
             -prev_horizon_limit_low,
             -next_horizon_limit_up,
             -eff_soil_depth) %>%
      relocate(horizon_master, .after = profile_pit_id) %>%
      relocate(layer_type, .after = horizon_master) %>%
      relocate(layer_number, .after = layer_type) %>%
      relocate(horizon_limit_up, .after = layer_number) %>%
      relocate(horizon_limit_low, .after = horizon_limit_up)


    # Save the survey form; list_layer_inconsistencies for the given survey form
    # and "list_redundant_layers" for the given survey form
    # to the global environment

    if (save_to_env == TRUE) {
    assign_env(survey_form, df)

    assign_env(paste0("list_redundant_layers_", survey_form),
               list_redundant_layers)

    assign_env(paste0("list_layer_inconsistencies_", survey_form),
               list_layer_inconsistencies)

    } else {
      return(df)
    }

    if (!isTRUE(getOption("knitr.in.progress"))) {
    close(progress_bar)
    }
    } # End of "pfh" part



  # Return the duration of this function run

  duration_run_r <- Sys.time() - start_time_r
  cat(paste0("\n",
             duration_run_r))

}
