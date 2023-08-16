
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
#'    "horizon_number_unique" (in "pfh" survey forms) which represents the rank
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
#'      "horizon_number_unique" equals "horizon_number" if the ranking of
#'      "horizon_number" equals the ranking of the layer limits (which is not
#'      always the case); else "horizon_number_unique" is based on the ranking
#'      of the layer limits.
#'
#' WARNING - This function may not be optimally efficient and may ideally
#' require refactoring for better performance.
#'
#' @examples
#' get_layer_inconsistencies("so_som", solve = TRUE)
#'

get_layer_inconsistencies <- function(survey_form,
                                      solve = FALSE,
                                      save_to_env = FALSE) {

  source("./src/functions/get_env.R")
  source("./src/functions/assign_env.R")

  # Import the inconsistency catalogue ----
  
  assertthat::assert_that(
    file.exists("./data/additional_data/inconsistency_catalogue.csv"),
    msg = paste0("There is no 'inconsistency catalogue' in the",
                 " '.data/additional_data/' folder."))
  
  inconsistency_catalogue <-
    read.csv("./data/additional_data/inconsistency_catalogue.csv", sep = ";")

# Part 1: processing "som" survey forms (with fixed depths) ----

  # Check if the given survey form is "som"

if (unlist(strsplit(survey_form, "_"))[2] == "som") {

  # Retrieve the survey_form data and the "pfh" data (to estimate depth
  # layer limits) and to cross-check forest floor layers in
  # all survey_forms (FSCC_48)

df <- get_env(survey_form)
pfh <- get_env(paste0(unlist(strsplit(survey_form, "_"))[1], "_pfh"))

  # If the input survey form is "so_som" (Level II):
  # retrieve the "som" and "pfh" data from Level I ("s1_som" and "s1_pfh")
  # to cross-check forest floor layers in all survey_forms (FSCC_48)

if (unlist(strsplit(survey_form, "_"))[1] == "so") {

  survey_level <- "Level II"
  som_other <- get_env("s1_som")
  pfh_other <- get_env("s1_pfh")
  }


  # Set up a progress bar to track processing

if (!isTRUE(getOption("knitr.in.progress"))) {
progress_bar <-
  txtProgressBar(min = 0,
                 max = length(unique(df$unique_survey_repetition)),
                 style = 3)
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
             combi_8 = c("OLF", "OH"))

layer_number_three_forest_floor_layers <-
  data.frame(layer_number = c(1, 2, 3),
             combi_1 = c("OL", "OF", "OH"),
             combi_2 = c("O", "O3", "O2"))

  # This dataframe refers to "d_depth_level_soil" in the
  # adds/dictionaries folder of the raw data,
  # to which columns with the theoretical layer limits
  # ("layer_limit_superior" and "layer_limit_inferior") were added for
  # each code_layer

d_depth_level_soil <- read.csv("./data/additional_data/d_depth_level_soil.csv",
                               sep = ";")


  # Initialise variables to store layer numbers and limits

df$layer_number_superior <- NA # Layer numbers by ranking layer_limit_superior
df$layer_number_inferior <- NA # Layer numbers by ranking layer_limit_superior
df$layer_number <- NA # Final layer numbers
                      # If there are overlapping layers
                      # between 0 and 10 cm depth
                      # (i.e. M05 & M51 & M01),
                      # layer numbers are not given for the "redundant layer(s)"
                      # (in this case: least detailed redundant layer(s),
                      # i.e. M01)
df$layer_number_combined_layers <- NA # If there are overlapping layers
                                      # (e.g. M05 & M51 & M01),
                                      # layer numbers of the least detailed
                                      # profile (i.e. with M01)
                                      # are represented here

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

if (!"code_layer_original" %in% names(df)) {
df$code_layer_original <- df$code_layer
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







# Evaluate soil profiles per unique_survey_repetition

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
if (unique(df$unique_survey_repetition[vec]) == "64_2004_5_1") {
  if (any(is.na(df$code_layer[vec]))) {
    vec_ff <- vec[which(is.na(df$code_layer[vec]))]
  }
}


## FSCC_48: Inconsistent presence/absence forest floor layers ----

# This is only tested in "so_som"

if (survey_form == "so_som") {

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
vec_som_other <- which(som_other$plot_id == plot_id_i)
vec_pfh_other <- which(pfh_other$plot_id == plot_id_i)

# Determine which unique_survey_repetitions exist for the given plot_id
# in the three other survey_forms

survey_repetitions_pfh <-
  unique(pfh$unique_survey_profile[which(pfh$plot_id == plot_id_i)])
survey_repetitions_som_other <-
  unique(som_other$unique_survey_repetition[
    which(som_other$plot_id == plot_id_i)])
survey_repetitions_pfh_other <-
  unique(pfh_other$unique_survey_profile[
    which(pfh_other$plot_id == plot_id_i)])

# Create a list with layer names of the forest floor layers
# This step is not required to find out the inconsistencies

forest_floor_layers_som <- NULL
forest_floor_layers_pfh <- NULL
forest_floor_layers_som_other <- NULL
forest_floor_layers_pfh_other <- NULL

# Create a data frame with metadata of the repetitions that exist for the
# given plot_id

if (survey_level == "Level II") {
  survey_2 <- "so_pfh"
  survey_3 <- "s1_som"
  survey_4 <- "s1_pfh"
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

pfh_ff_i <- pfh[vec_pfh[
  which(!duplicated(pfh$unique_survey_profile[vec_pfh]))],
  which(names(pfh) %in% c("partner_code", "survey_year",
                          "code_plot", "plot_id",
                          "profile_pit_id", "unique_survey_profile"))]

names(pfh_ff_i)[which(names(pfh_ff_i) == "profile_pit_id")] <-
  "repetition_profile_pit_id"

names(pfh_ff_i)[which(names(pfh_ff_i) == "unique_survey_profile")] <-
  "unique_survey_repetition"


som_other_ff_i <- som_other[vec_som_other[
  which(!duplicated(som_other$unique_survey_repetition[vec_som_other]))],
        which(names(som_other) %in% c("partner_code", "survey_year",
                                      "code_plot", "plot_id",
                                      "repetition",
                                      "unique_survey_repetition"))]

names(som_other_ff_i)[
  which(names(som_other_ff_i) == "repetition")] <-
  "repetition_profile_pit_id"


pfh_other_ff_i <- pfh_other[vec_pfh_other[
  which(!duplicated(pfh_other$unique_survey_profile[vec_pfh_other]))],
          which(names(pfh_other) %in% c("partner_code", "survey_year",
                                        "code_plot", "plot_id",
                                        "profile_pit_id",
                                        "unique_survey_profile"))]

names(pfh_other_ff_i)[
  which(names(pfh_other_ff_i) == "profile_pit_id")] <-
  "repetition_profile_pit_id"

names(pfh_other_ff_i)[
  which(names(pfh_other_ff_i) == "unique_survey_profile")] <-
  "unique_survey_repetition"


# Merge these four dataframes to one data_frame "list_ff_meta_i"

list_ff_meta_i <-
   cbind(data.frame("survey_form" =
                    c(rep(survey_form, length(survey_repetitions_som)),
                      rep(survey_2, length(survey_repetitions_pfh)),
                      rep(survey_3, length(survey_repetitions_som_other)),
                      rep(survey_4, length(survey_repetitions_pfh_other))),
                   "survey_form_script" =
                     c(rep("df", length(survey_repetitions_som)),
                       rep("pfh", length(survey_repetitions_pfh)),
                       rep("som_other", length(survey_repetitions_som_other)),
                       rep("pfh_other", length(survey_repetitions_pfh_other)))),
        rbind(df_ff_i, pfh_ff_i, som_other_ff_i, pfh_other_ff_i),
        data.frame("number_ff_layers" =
                    rep(NA, (length(survey_repetitions_som) +
                             length(survey_repetitions_pfh) +
                             length(survey_repetitions_som_other) +
                             length(survey_repetitions_pfh_other)))))


# For each of these unique_survey_repetitions:
# Store in "list_ff_meta_i" how many forest floor layers exist
# in the given unique_survey_repetition

for (j in 1:(length(survey_repetitions_som) +
             length(survey_repetitions_pfh) +
             length(survey_repetitions_som_other) +
             length(survey_repetitions_pfh_other))) {

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

    } else

    # som_other

    if ((length(survey_repetitions_som_other) > 0) &&
        (j > (length(survey_repetitions_som) +
              length(survey_repetitions_pfh))) &&
        (j <= length(c(survey_repetitions_som, survey_repetitions_pfh,
                       survey_repetitions_som_other)))) {

      j_som_other <- (j - (length(survey_repetitions_som) +
                             length(survey_repetitions_pfh)))

      vec_som_other_j <- which(som_other$unique_survey_repetition ==
                                 survey_repetitions_som_other[j_som_other])

      forest_floor_j <- as.character(som_other$code_layer[vec_som_other_j[
                  which(som_other$layer_type[vec_som_other_j] ==
                          "forest_floor")]])

      list_ff_meta_i$number_ff_layers[j] <- length(forest_floor_j)

      forest_floor_layers_som_other <- append(forest_floor_layers_som_other,
                                        list(forest_floor_j))

      names(forest_floor_layers_som_other)[j_som_other] <-
                as.character(survey_repetitions_som_other[j_som_other])

    } else

    # pfh_other

    if ((length(survey_repetitions_pfh_other) > 0) &&
        (j > (length(survey_repetitions_som) + length(survey_repetitions_pfh) +
              length(survey_repetitions_som_other)))) {

      j_pfh_other <- (j - (length(survey_repetitions_som) +
                             length(survey_repetitions_pfh) +
                             length(survey_repetitions_som_other)))

      vec_pfh_other_j <- which(pfh_other$unique_survey_profile ==
                                 survey_repetitions_pfh_other[j_pfh_other])

      forest_floor_j <- as.character(pfh_other$horizon_master[vec_pfh_other_j[
                  which(pfh_other$layer_type[vec_pfh_other_j] ==
                          "forest_floor")]])

      list_ff_meta_i$number_ff_layers[j] <- length(forest_floor_j)

      forest_floor_layers_pfh_other <- append(forest_floor_layers_pfh_other,
                                        list(forest_floor_j))

      names(forest_floor_layers_pfh_other)[j_pfh_other] <-
                as.character(survey_repetitions_pfh_other[j_pfh_other])

    }
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
   vec_ff_som_other <-
     vec_som_other[which(som_other$layer_type[vec_som_other] == "forest_floor")]
   vec_ff_pfh_other <-
     vec_pfh_other[which(pfh_other$layer_type[vec_pfh_other] == "forest_floor")]

   # Store information about the inconsistency in "list_layer_inconsistencies"

   inconsistency_reason <- caution_ff

   # Report original code_layer if code_layer may have been changed
   # (possibly in "s1_som" due to FSCC_47)

   if ("code_layer_original" %in% names(som_other)) {
       vec_code_layer <-
         as.character(som_other$code_layer_original[vec_ff_som_other])
     } else {
       vec_code_layer <- as.character(som_other$code_layer[vec_ff_som_other])
     }

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

   if (!identical(vec_ff_som_other, integer(0))) {
   list_layer_inconsistencies <- rbind(
     list_layer_inconsistencies,
     data.frame(survey_form = (rep(survey_3, length(vec_ff_som_other))),
                partner = som_other$partner[vec_ff_som_other],
                partner_code = som_other$partner_code[vec_ff_som_other],
                country = som_other$country[vec_ff_som_other],
                code_country = som_other$code_country[vec_ff_som_other],
                survey_year = som_other$survey_year[vec_ff_som_other],
                code_plot = som_other$code_plot[vec_ff_som_other],
                plot_id = som_other$plot_id[vec_ff_som_other],
                code_layer_horizon_master = vec_code_layer,
                repetition_profile_pit_id =
                  som_other$repetition[vec_ff_som_other],
                code_line = som_other$code_line[vec_ff_som_other],
                parameter = (rep("code_layer", length(vec_ff_som_other))),
                parameter_unit = (rep(NA, length(vec_ff_som_other))),
                parameter_value =  vec_code_layer,
                inconsistency_reason = inconsistency_reason,
                inconsistency_type = inconsistency_type,
                rule_id = rule_id,
                non_duplicated_error_type_per_record =
                  rep(FALSE, length(vec_ff_som_other)),
                change_date = som_other$change_date[vec_ff_som_other],
                download_date = rep(download_date_pir,
                                    length(vec_ff_som_other))))
   }

   # "s1_pfh"

   if (!identical(vec_ff_pfh_other, integer(0))) {

   list_layer_inconsistencies <- rbind(
     list_layer_inconsistencies,
     data.frame(survey_form = (rep(survey_4, length(vec_ff_pfh_other))),
                partner = pfh_other$partner[vec_ff_pfh_other],
                partner_code = pfh_other$partner_code[vec_ff_pfh_other],
                country = pfh_other$country[vec_ff_pfh_other],
                code_country = pfh_other$code_country[vec_ff_pfh_other],
                survey_year = pfh_other$survey_year[vec_ff_pfh_other],
                code_plot = pfh_other$code_plot[vec_ff_pfh_other],
                plot_id = pfh_other$plot_id[vec_ff_pfh_other],
                code_layer_horizon_master =
                  pfh_other$horizon_master[vec_ff_pfh_other],
                repetition_profile_pit_id =
                  pfh_other$profile_pit_id[vec_ff_pfh_other],
                code_line = pfh_other$code_line[vec_ff_pfh_other],
                parameter = (rep("horizon_master", length(vec_ff_pfh_other))),
                parameter_unit = (rep(NA, length(vec_ff_pfh_other))),
                parameter_value =
                  as.character(pfh_other$horizon_master[vec_ff_pfh_other]),
                inconsistency_reason = inconsistency_reason,
                inconsistency_type = inconsistency_type,
                rule_id = rule_id,
                non_duplicated_error_type_per_record =
                  rep(FALSE, length(vec_ff_pfh_other)),
                change_date = pfh_other$change_date[vec_ff_pfh_other],
                download_date = rep(download_date_pir,
                                    length(vec_ff_pfh_other))))
   }

   # Change "non_duplicated_error_type_per_record" so that there is only one
   # unique inconsistency per plot_id

   length_inconsistency <-
     length(c(vec_ff_som, vec_ff_pfh, vec_ff_som_other, vec_ff_pfh_other))

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

  # Check if forest floor layers have limits with wrong sign
  # (i.e. all >= 0 and at least once > 0 so not equal to 0)
  # Only for profiles without peat since peat soils usually have a
  # special sequence

if (all(df$layer_limit_inferior_orig[vec_ff_sup] >= 0) &&
    all(df$layer_limit_superior_orig[vec_ff_inf] >= 0) &&
    any(c(df$layer_limit_inferior_orig[vec_ff_sup],
          df$layer_limit_superior_orig[vec_ff_inf]) > 0) &&
    length(vec_ff) >= 1 &&
    all(df$layer_type[vec_bg] != "peat")) {

  # Store information about the inconsistency in "list_layer_inconsistencies"

  # superior

  rule_id <- "FSCC_2"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  if ("code_layer_original" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_original[vec_ff_sup])
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

  if ("code_layer_original" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_original[vec_ff_inf])
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

  # Check if below-ground (mineral or peat layers) have limits with wrong sign
  # (i.e. all <= 0 and at least once < 0 so not equal to 0)

if (all(df$layer_limit_inferior_orig[vec_bg_inf] <= 0) &&
    all(df$layer_limit_superior_orig[vec_bg_sup] <= 0) &&
    any(c(df$layer_limit_inferior_orig[vec_bg_inf],
          df$layer_limit_superior_orig[vec_bg_sup]) < 0) &&
    length(vec_bg) >= 1) {

  # Store information about the inconsistency in "list_layer_inconsistencies"

  # Superior

  rule_id <- "FSCC_2"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]

  if ("code_layer_original" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_original[vec_bg_sup])
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

  if ("code_layer_original" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_original[vec_bg_inf])
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

  if ("code_layer_original" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_original[j])
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

     if ("code_layer_original" %in% names(df)) {
         vec_code_layer <- as.character(df$code_layer_original[j])
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

     if ("code_layer_original" %in% names(df)) {
         vec_code_layer <- as.character(df$code_layer_original[j])
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

  if ("code_layer_original" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_original[vec_inconsistency])
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

  if ("code_layer_original" %in% names(df)) {
      vec_code_layer <- as.character(df$code_layer_original[vec_inconsistency])
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

        if ("code_layer_original" %in% names(df)) {
          vec_code_layer <- as.character(df$code_layer_original[vec_error])
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

      if ("code_layer_original" %in% names(df)) {
          vec_code_layer <- as.character(df$code_layer_original[vec_error])
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

    if ("code_layer_original" %in% names(df)) {
        vec_code_layer <- as.character(df$code_layer_original[j])
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

vec_inconsistency <- vec[which(df$layer_limit_superior_orig[vec] < -150)]

if (!identical(vec_inconsistency, integer(0))) {

  # Store information about the inconsistency in "list_layer_inconsistencies"

  rule_id <- "FSCC_51"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]
  
  if ("code_layer_original" %in% names(df)) {
    vec_code_layer <- as.character(df$code_layer_original[j])
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

vec_inconsistency <- vec[which(df$layer_limit_inferior_orig[vec] < -150)]

if (!identical(vec_inconsistency, integer(0))) {
  
  # Store information about the inconsistency in "list_layer_inconsistencies"
  
  rule_id <- "FSCC_51"
  inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
    which(inconsistency_catalogue$rule_id == rule_id)]
  inconsistency_type <- inconsistency_catalogue$inconsistency_type[
    which(inconsistency_catalogue$rule_id == rule_id)]
  
  if ("code_layer_original" %in% names(df)) {
    vec_code_layer <- as.character(df$code_layer_original[j])
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





# Step 2: Try to gap-fill any empty layer_limit_superior and ----
#   layer_limit_inferior

  # If there is at least one layer without limit information

if ((any(is.na(df$layer_limit_inferior[vec])) ||
     any(is.na(df$layer_limit_superior[vec])))) {

  # Get the index of empty cells in layer_limit_superior and
  # layer_limit_inferior

vec_empty_layer_limit_superior <-
  vec[which(is.na(df$layer_limit_superior[vec]))]
vec_empty_layer_limit_inferior <-
  vec[which(is.na(df$layer_limit_inferior[vec]))]

# Option 2.1: Can we fill these layers based on pfh? ----
# (mainly for organic layers)

  # Get the index of the plot survey in pfh

vec_pfh <- which(df$plot_id[vec[1]] == pfh$plot_id) # In pfh

  # Fill empty cells in layer_limit_superior

if (!identical(vec_pfh, integer(0))) { # If this plot survey is in pfh

  # Check if there are empty cells in layer_limit_superior

if (!identical(vec_empty_layer_limit_superior, integer(0))) {

for (j in vec_empty_layer_limit_superior) {

  if ("code_layer_original" %in% names(df)) {
    vec_code_layer <- as.character(df$code_layer_original[j])
  } else {
    vec_code_layer <- as.character(df$code_layer[j])
  }

  # Check if code_layer is in pfh
  
  if (!is.na(match(vec_code_layer, pfh$horizon_master[vec_pfh]))) {
    df$layer_limit_superior[j] <-
      pfh$horizon_limit_up[vec_pfh[
        match(vec_code_layer, pfh$horizon_master[vec_pfh])]]

  } else

          # For OLF and OFH layers,
          # match the code_layer with OL-OF and OF-OH in pfh respectively

          if (vec_code_layer == "OLF" &&
              !is.na(match("OL", pfh$horizon_master[vec_pfh])) &&
              !is.na(match("OF", pfh$horizon_master[vec_pfh]))) {

              df$layer_limit_superior[j] <-
                pfh$horizon_limit_up[vec_pfh[
                  match("OL", pfh$horizon_master[vec_pfh])]]

              } else

          if (vec_code_layer == "OFH" &&
              !is.na(match("OF", pfh$horizon_master[vec_pfh])) &&
              !is.na(match("OH", pfh$horizon_master[vec_pfh]))) {

              df$layer_limit_superior[j] <-
                pfh$horizon_limit_up[vec_pfh[
                  match("OF", pfh$horizon_master[vec_pfh])]]

              }
    }
  }


  # Fill empty cells in layer_limit_inferior

  # Check if there are empty cells in layer_limit_inferior

 if (!identical(vec_empty_layer_limit_inferior, integer(0))) {

 for (j in vec_empty_layer_limit_inferior) {

  # If code_layer is in pfh

   if ("code_layer_original" %in% names(df)) {
     vec_code_layer <- as.character(df$code_layer_original[j])
   } else {
       vec_code_layer <- as.character(df$code_layer[j])
   }

    if (!is.na(match(vec_code_layer, pfh$horizon_master[vec_pfh]))) {
    df$layer_limit_inferior[j] <-
      pfh$horizon_limit_low[vec_pfh[match(vec_code_layer,
                                          pfh$horizon_master[vec_pfh])]]

    } else

          # For OLF and OFH layers,
          # match the code_layer with OL-OF and OF-OH in pfh respectively

          if (vec_code_layer == "OLF" &&
              !is.na(match("OL",pfh$horizon_master[vec_pfh])) &&
              !is.na(match("OF",pfh$horizon_master[vec_pfh]))) {

              df$layer_limit_inferior[j] <-
               pfh$horizon_limit_low[vec_pfh[
                 match("OF", pfh$horizon_master[vec_pfh])]]

              } else
          if (vec_code_layer == "OFH" &&
              !is.na(match("OF",pfh$horizon_master[vec_pfh])) &&
              !is.na(match("OH",pfh$horizon_master[vec_pfh]))) {

              df$layer_limit_inferior[j] <-
                pfh$horizon_limit_low[vec_pfh[
                  match("OH", pfh$horizon_master[vec_pfh])]]

              }
    }
   }
}



# Option 2.2: Can we fill these layers based on d_depth_level_soil? ----

  # Superior

  # Again select the indices of the empty superior layer limit and
  # inferior layer limit from vec

vec_empty_layer_limit_superior <-
  vec[which(is.na(df$layer_limit_superior[vec]))]
vec_empty_layer_limit_inferior <-
  vec[which(is.na(df$layer_limit_inferior[vec]))]

  # Check if vec_empty_layer_limit_superior is not empty
  # and fill the empty superior layer limit

if (!identical(vec_empty_layer_limit_superior, integer(0))) {

for (j in vec_empty_layer_limit_superior) {

        if ("code_layer_original" %in% names(df)) {
          vec_code_layer <- as.character(df$code_layer_original[j])
          } else {
            vec_code_layer <- as.character(df$code_layer[j])
          }

  if (!is.na(match(vec_code_layer, d_depth_level_soil$code))) {

    df$layer_limit_superior[j] <-
        d_depth_level_soil$layer_limit_superior[match(vec_code_layer,
                                                      d_depth_level_soil$code)]

  }
}
}


  # Inferior

  # Check if vec_empty_layer_limit_inferior is not empty
  # and fill the empty inferior layer limit

if (!identical(vec_empty_layer_limit_inferior, integer(0))) {

  for (j in vec_empty_layer_limit_inferior) {

        if ("code_layer_original" %in% names(df)) {
        vec_code_layer <- as.character(df$code_layer_original[j])
        } else {
          vec_code_layer <- as.character(df$code_layer[j])
        }

  if (!is.na(match(vec_code_layer, d_depth_level_soil$code))) {

      df$layer_limit_inferior[j] <-
        d_depth_level_soil$layer_limit_inferior[match(vec_code_layer,
                                                      d_depth_level_soil$code)]

  }
}
}
}



## FSCC_2 Error reason: Typo observed ----

  # One value in the given profile seems to have the wrong sign

if (unique(df$unique_survey_repetition)[i] == "3204_2009_503_1") {

  if (df$layer_limit_inferior_orig[vec[
    which(df$code_layer[vec] == "OF")]] == 3) {

  ind_error <- vec[which(df$code_layer[vec] == "OF")]

  # Store information about the inconsistency in "list_layer_inconsistencies"

rule_id <- "FSCC_2"
inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
  which(inconsistency_catalogue$rule_id == rule_id)]
inconsistency_type <- inconsistency_catalogue$inconsistency_type[
  which(inconsistency_catalogue$rule_id == rule_id)]

if ("code_layer_original" %in% names(df)) {
  vec_code_layer <- as.character(df$code_layer_original[ind_error])
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




## FSCC_7 Error reason: none of the layers has layer limit information ----

  # Check if none of the layers has superior layer limit information

if (all(is.na(df$layer_limit_superior[vec]))) {

  # Store information about the inconsistency in "list_layer_inconsistencies"

rule_id <- "FSCC_7"
inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
  which(inconsistency_catalogue$rule_id == rule_id)]
inconsistency_type <- inconsistency_catalogue$inconsistency_type[
  which(inconsistency_catalogue$rule_id == rule_id)]

if ("code_layer_original" %in% names(df)) {
  vec_code_layer <- as.character(df$code_layer_original[vec])
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

   if ("code_layer_original" %in% names(df)) {
     vec_code_layer <- as.character(df$code_layer_original[vec])
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




# Step 3: The layers are ordered (layer_number) from top to bottom ----
   # based on their layer limits


  # Check if at least one of the layers has layer limit information

  if (any(!is.na(df$layer_limit_inferior[vec]) &
          !is.na(df$layer_limit_superior[vec]))) {

  # Which layers have layer limit information?

  # If forest floor has no layer limit information ----

    if (!identical(vec_ff, integer(0)) &&
        (all(is.na(df$layer_limit_inferior[vec_ff])) ||
        all(is.na(df$layer_limit_superior[vec_ff])))) {

    # If there is one forest floor layer

      if (length(vec_ff) == 1) {
        df$layer_number[vec_ff] <- 1
        } else

     # If there are two forest floor layers
     # use the data frame "layer_number_two_forest_floor_layers"

        if (length(vec_ff) == 2) {

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

          if (unique(df$unique_survey_repetition[vec_ff]) == "64_2004_5_1") {
            if (all(is.na(df$code_layer[vec_ff]))) {

              ind_superior <-
                vec_ff[which(df$organic_layer_weight[vec_ff] ==
                               min(df$organic_layer_weight[vec_ff]))]
              ind_inferior <-
                vec_ff[which(df$organic_layer_weight[vec_ff] ==
                               max(df$organic_layer_weight[vec_ff]))]
              df$code_layer[ind_superior] <- "OL"
              df$code_layer[ind_inferior] <- "OFH"
              df$unique_survey_layer[vec_ff] <-
                paste0(df$partner_code[vec_ff], "_",
                       df$survey_year[vec_ff], "_",
                       df$code_plot[vec_ff], "_",
                       df$code_layer[vec_ff])
              df$unique_layer_repetition[vec_ff] <-
                paste0(df$partner_code[vec_ff], "_",
                       df$survey_year[vec_ff], "_",
                       df$code_plot[vec_ff], "_",
                       df$code_layer[vec_ff], "_",
                       df$repetition[vec_ff])
            }
          }

          layers <- df$code_layer[vec_ff]

        # Assert that the forest floor combination exists in
        # layer_number_two_forest_floor_layers

          # Generate all combinations of two unique values
          all_combinations <-
            apply(layer_number_two_forest_floor_layers[, 3:11], 2,
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

         for (j in c(5:11)) {

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
        } else


    # If there are three forest floor layers
    # use the data frame "layer_number_three_forest_floor_layers"

        if (length(vec_ff) == 3) {

        layers <- df$code_layer[vec_ff]
        
        # Assert that the combination of three forest floor layers is known
        
        # Test if all elements of layers are in either the second or third column
        
        assertthat::assert_that(
          all(layers %in% layer_number_three_forest_floor_layers[, 2]) ||
          all(layers %in% layer_number_three_forest_floor_layers[, 3]),
          msg = paste0("Unknown combination of three forest floor layers ('",
                       df$code_layer[vec_ff[1]], "', '",
                       df$code_layer[vec_ff[2]], "' and '",
                       df$code_layer[vec_ff[3]],
                       "')."))

        col_table <- ifelse(
          all(layers %in% layer_number_three_forest_floor_layers[, 2]),
          2,
          ifelse(
            all(layers %in% layer_number_three_forest_floor_layers[, 3]),
            3,
            NA)) # In theory NA won't appear
        

        df$layer_number[vec_ff[
          match(layer_number_three_forest_floor_layers[1, col_table],
                layers)]] <- 1
        df$layer_number[vec_ff[
          match(layer_number_three_forest_floor_layers[2, col_table],
                layers)]] <- 2
        df$layer_number[vec_ff[
          match(layer_number_three_forest_floor_layers[3, col_table],
                layers)]] <- 3

        }
      }

    # Copy these layer numbers to the other columns that represent layer numbers

        df$layer_number_inferior[vec] <- df$layer_number[vec]
        df$layer_number_superior[vec] <- df$layer_number[vec]
        df$layer_number_combined_layers[vec] <- df$layer_number[vec]


   # Rank the remaining layers that have layer limit information ----

   # Check which layers have layer limit information but no layer number yet

        # Check if there are any layers which do not have a layer number
        # but which do have known layer limits

      if (!identical(which(is.na(df$layer_number[vec]) &
                           !is.na(df$layer_limit_superior[vec]) &
                           !is.na(df$layer_limit_inferior[vec])),
                     integer(0))) {

      vec_nonempty <- vec[which(is.na(df$layer_number[vec]) &
                                 !is.na(df$layer_limit_superior[vec]) &
                                 !is.na(df$layer_limit_inferior[vec]))]

    # Check if any layer_number(s) have already been assigned for forest
    # floor layers in the given profile by means of 'layer_number_max',
    # which equals the maximum existing layer number or 0

          if (all(is.na(df$layer_number[vec]))) {
            layer_number_max <- 0
            } else {

             # Maximum layer number that has already been assigned

          layer_number_max <- as.numeric(max(df$layer_number[vec[
                                      which(!is.na(df$layer_number[vec]))]]))
          }

          vec_nonempty_remaining <- vec_nonempty


          # "M01" overlaps with "M05" and "M51"
          # If these three layers co-exist:
          # - In "df$layer_number", "M01" is hence considered "redundant",
          #   Because of which it doesn't get a layer number in
          #   "df$layer_number"
          # - In "df$layer_number_combined_layers", "M05" and "M51" are
          #   considered redundant, because of which they don't get a layer
          #   number in "df$layer_number_combined_layers"

          if (!identical(which(df$code_layer[vec_nonempty] == "M01"),
                         integer(0)) &&
              !identical(which(df$code_layer[vec_nonempty] == "M05"),
                         integer(0)) &&
              !identical(which(df$code_layer[vec_nonempty] == "M51"),
                         integer(0))) {

          df$layer_number[vec_nonempty[
            which(df$code_layer[vec_nonempty] == "M05")]] <-
             1 + layer_number_max

           df$layer_number[vec_nonempty[
             which(df$code_layer[vec_nonempty] == "M51")]] <-
             2 + layer_number_max

           df$layer_number_combined_layers[vec_nonempty[
             which(df$code_layer[vec_nonempty] == "M01")]] <-
             1 + layer_number_max

           # Update 'vec_nonempty_remaining' so that it no longer contains
           # the row indices of these layers

           vec_nonempty_remaining <- vec_nonempty[
             which(df$code_layer[vec_nonempty] != "M05" &
                   df$code_layer[vec_nonempty] != "M51" &
                   df$code_layer[vec_nonempty] != "M01")]
           }

           # Check if the ranking based on layer_limit_superior is the same
           # like the ranking based on layer_limit_inferior

           if (all(rank(as.numeric(df$layer_limit_superior[
             vec_nonempty_remaining])) ==
               rank(as.numeric(df$layer_limit_inferior[
                 vec_nonempty_remaining])))) {

               # Update 'layer_number_max'

               if (all(is.na(df$layer_number[vec]))) {

                 layer_number_max <- 0

               } else {

                 layer_number_max <- as.numeric(max(df$layer_number[vec[
                                          which(!is.na(df$layer_number[
                                            vec]))]]))
                   }

               # Rank the remaining layers based on layer_limit_superior

                df$layer_number[vec_nonempty_remaining] <-
                  rank(as.numeric(
                    df$layer_limit_superior[vec_nonempty_remaining])) +
                  layer_number_max


                # Also add a ranking to the column
                # "df$layer_number_combined_layers"
                # although this won't be saved at the end of the function

                if (all(is.na(df$layer_number_combined_layers[vec]))) {

                  layer_number_max_combined_layers <- 0

                  } else {

                layer_number_max_combined_layers <-
                     as.numeric(max(df$layer_number_combined_layers[vec[
                              which(!is.na(df$layer_number_combined_layers[
                                vec]))]]))}

                df$layer_number_combined_layers[vec_nonempty_remaining] <-
                      rank(as.numeric(df$layer_limit_superior[
                        vec_nonempty_remaining])) +
                      layer_number_max_combined_layers

                } else {

            # If there is a difference between ranking layer_limit_superior
            # vs ranking layer_limit_inferior:
            # Fill the columns "layer_number_superior" and
            # "layer_number_inferior"
            # Do not fill the "layer_number" and
            # "layer_number_combined_layers" columns

            # Note that only the column "layer_number" is saved at the end of
            # the script, so that this column will be empty in this case

           df$layer_number_superior[vec_nonempty_remaining] <-
             rank(as.numeric(df$layer_limit_superior[vec_nonempty_remaining])) +
             layer_number_max

           df$layer_number_inferior[vec_nonempty_remaining] <-
             rank(as.numeric(df$layer_limit_inferior[vec_nonempty_remaining])) +
             layer_number_max

           }
      }
      }



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

          if ("code_layer_original" %in% names(df)) {
            vec_code_layer <- as.character(df$code_layer_original[j])
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

            if ("code_layer_original" %in% names(df)) {
              vec_code_layer <- as.character(df$code_layer_original[j_next])
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

 # Update the progress bar
if (!isTRUE(getOption("knitr.in.progress"))) {
setTxtProgressBar(progress_bar, i)
}
}

if (!isTRUE(getOption("knitr.in.progress"))) {
close(progress_bar)
}

# Remove these columns:
# - layer_number_superior
# - layer_number_inferior
# - layer_number_combined_layers

df <- df[, -which(names(df) %in% c("layer_number_superior",
                                   "layer_number_inferior",
                                   "layer_number_combined_layers"))]

# Add columns which only give numbers to below-ground versus above-ground
# layers

df <- df %>%
  group_by(unique_survey_repetition) %>%
  mutate(
    layer_number_bg_only = ifelse(layer_type %in% c("mineral", "peat"),
                                  layer_number, NA),
    layer_number_bg_min = suppressWarnings(min(layer_number_bg_only,
                                               na.rm = TRUE)),
    layer_number_bg = layer_number_bg_only - (layer_number_bg_min - 1),
    layer_number_ff = ifelse(layer_type %in% c("forest_floor"),
                             layer_number, NA),) %>%
    select(-layer_number_bg_only, -layer_number_bg_min) %>%
    arrange(partner_code, unique_survey_repetition, layer_number)


# Save the survey form and list_layer_inconsistencies for the given survey form
# to the global environment

if (save_to_env == TRUE) {
assign_env(survey_form, df)

assign_env(paste0("list_layer_inconsistencies_", survey_form),
           list_layer_inconsistencies)
}

} # end of "som" part


















































  ### Part 2: processing "pfh" survey forms (with pedogenic layers) ----

  # Check if the given survey form is "pfh"

  if (unlist(strsplit(survey_form, "_"))[2] == "pfh") {

    # Retrieve the survey_form data

    df <- get_env(survey_form)

    # Convert missing horizon masters to ""
    # This is necessary for the "get_redundant_layers()" function

    df$horizon_master <- as.character(df$horizon_master)

    if (any(is.na(df$horizon_master))) {
      df$horizon_master[which(is.na(df$horizon_master))] <- ""
      }

    # Set up a progress bar to track processing

    if (!isTRUE(getOption("knitr.in.progress"))) {
    progress_bar <-
      txtProgressBar(min = 0,
                     max = length(unique(df$unique_survey_profile)),
                     style = 3)
    }


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
                  horizon_number_unique = NULL,
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

    df$horizon_number_unique <- NA

    if (!"horizon_limit_up_orig" %in% names(df)) {
      df$horizon_limit_up_orig <- df$horizon_limit_up
      }

    if (!"horizon_limit_low_orig" %in% names(df)) {
      df$horizon_limit_low_orig <- df$horizon_limit_low
      }


    # Evaluate soil profiles per unique_survey_profile

    for (i in seq_along(unique(df$unique_survey_profile))) {

    # Determine index of layers with the given unique_survey_profile in df

    vec <- which(unique(df$unique_survey_profile)[i] ==
                   df$unique_survey_profile)

    # index forest floor layers in df
    vec_ff <- vec[which(df$layer_type[vec] == "forest_floor")]

    # index below-ground layers in df
    vec_bg <- vec[which(df$layer_type[vec] == "mineral" |
                          df$layer_type[vec] == "peat")]


    ## FSCC_2: Caution: the reported layer limits in above-ground ----
    # (negative) or below-ground (positive) layers have the wrong sign
    # (positive/negative).

    # above-ground

    # Check if layer limits of forest floor layers are known

    if (any(!is.na(df$horizon_limit_up_orig[vec_ff])) &&
        any(!is.na(df$horizon_limit_low_orig[vec_ff]))) {

      vec_ff_sup <- vec_ff[which(!is.na(df$horizon_limit_up_orig[vec_ff]))]
      vec_ff_inf <- vec_ff[which(!is.na(df$horizon_limit_low_orig[vec_ff]))]


      # Check if forest floor layers have limits with wrong sign
      # (i.e. all >= 0 and at least once > 0 so not equal to 0)
      # Only for profiles without peat since peat soils usually have
      # a special sequence

      if (all(df$horizon_limit_low_orig[vec_ff_sup] >= 0) &&
          all(df$horizon_limit_up_orig[vec_ff_inf] >= 0) &&
          any(c(df$horizon_limit_low_orig[vec_ff_sup],
                df$horizon_limit_up_orig[vec_ff_inf]) > 0) &&
          length(vec_ff) >= 1 &&
          all(df$layer_type[vec_bg] != "peat")) {

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

      # Check if below-ground layers have limits with wrong sign
      # (i.e. all <= 0 and at least once < 0 so not equal to 0)

      if (all(df$horizon_limit_low_orig[vec_bg_inf] <= 0) &&
          all(df$horizon_limit_up_orig[vec_bg_sup] <= 0) &&
          any(c(df$horizon_limit_low_orig[vec_bg_inf],
                df$horizon_limit_up_orig[vec_bg_sup]) < 0) &&
          length(vec_bg) >= 1) {

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

    if (unique(df$unique_survey_profile)[i] == "3004_2007_684_684") {

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
    
    vec_inconsistency <- vec[which(df$horizon_limit_up_orig[vec] < -150)]
    
    if (!identical(vec_inconsistency, integer(0))) {
      
      # Store information about the inconsistency in
      # "list_layer_inconsistencies"
      
      rule_id <- "FSCC_51"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]
      
      if ("code_layer_original" %in% names(df)) {
        vec_code_layer <- as.character(df$code_layer_original[j])
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
    
    vec_inconsistency <- vec[which(df$horizon_limit_low_orig[vec] < -150)]
    
    if (!identical(vec_inconsistency, integer(0))) {
      
      # Store information about the inconsistency in
      # "list_layer_inconsistencies"
      
      rule_id <- "FSCC_51"
      inconsistency_reason <- inconsistency_catalogue$inconsistency_reason[
        which(inconsistency_catalogue$rule_id == rule_id)]
      inconsistency_type <- inconsistency_catalogue$inconsistency_type[
        which(inconsistency_catalogue$rule_id == rule_id)]
      
      if ("code_layer_original" %in% names(df)) {
        vec_code_layer <- as.character(df$code_layer_original[j])
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
    


    ## FSCC_10: Error reason: there is a gap between adjacent layers

    # Step 1: Check for redundant layers ----

    # Retrieve the row indices and horizon_master from layers with known layer
    # limits

    vec_nonempty <- vec[which(!is.na(df$horizon_limit_up[vec]) &
                                !is.na(df$horizon_limit_low[vec]))]
    layers <- df$horizon_master[vec_nonempty]
    layers_vec <- df$horizon_master[vec]

    # If there are multiple horizons with the same horizon_master
    # in the given profile:
    # Add a different number to each of the horizons in "layers" and
    # "layers_vec" (i.e. internally) so that they become unique

    if (length(unique(df$horizon_master[vec_nonempty])) <
        length(df$horizon_master[vec_nonempty])) {

      layers <- paste0(df$horizon_master[vec_nonempty],
                       1:length(layers))
      layers_vec <- paste0(df$horizon_master[vec],
                           1:length(layers_vec))
      }

    # Store the horizon limits and the relevant subset of "df"

    superior_layer_limits <- df$horizon_limit_up[vec_nonempty]
    inferior_layer_limits <- df$horizon_limit_low[vec_nonempty]
    df_sub <- df[vec_nonempty, ]

    # If there are more than two horizons:
    # get the redundant layers via the function "get_redundant_layers()"
    # and call it "redundant_layers"

    if (length(vec_nonempty) >= 2) {

      redundant_layers <-
        get_redundant_layers(layers = layers,
                             superior_layer_limits = superior_layer_limits,
                             inferior_layer_limits = inferior_layer_limits,
                             df_sub = df_sub)

    } else {
        redundant_layers <- integer(0)
    }

    # Check if there are any redundant layers

    if (!identical(redundant_layers, integer(0))) {

    # Create a summary dataframe "redundant_layers_summary" with all the layers
    # in the given unique profile
    # to store information about the redundant layers

      redundant_layers_summary <-
        data.frame(layer = layers_vec,
                   redundancy_type = rep(NA, length(layers_vec)),
                   combined_layer = rep(NA, length(layers_vec)))

    # Store the row indices of "redundant_layers_summary" that are mentioned
    # in "redundant_layers", and evaluate for each of them:

      vec_redundant <- match(unique(redundant_layers$layer), layers_vec)

      for (j in vec_redundant) {

        # Store the row indices of j in redundant_layers

        vec_j <- which(layers_vec[j] == redundant_layers$layer)

        # If there is any "redundancy_type" 0:
        # Assign a "redundancy_type" 0

        if (any(redundant_layers$redundancy_type[vec_j] == 0)) {

          redundant_layers_summary$redundancy_type[j] <- 0
          } else

        # Else, if there is any "redundancy_type" 3:
        # Assign a "redundancy_type" 3 + store "combined_layer"

        if (any(redundant_layers$redundancy_type[vec_j] == 3)) {

          vec_3 <- vec_j[which(redundant_layers$redundancy_type[vec_j] == 3)]
          redundant_layers_summary$redundancy_type[j] <- 3
          redundant_layers_summary$combined_layer[j] <-
                                redundant_layers$combined_layer[vec_3]
          } else

        # Else, if there is any "redundancy_type" 2:
        # Assign a "redundancy_type" 2 + store "combined_layer"

        if (any(redundant_layers$redundancy_type[vec_j] == 2)) {
          vec_2 <- vec_j[which(redundant_layers$redundancy_type[vec_j] == 2)]

          if (length(vec_2) == 2) {

            redundant_layers_summary$redundancy_type[j] <- 2
            redundant_layers_summary$combined_layer[j] <-
              paste0(redundant_layers$combined_layer[vec_2[1]], "_",
                   redundant_layers$combined_layer[vec_2[1]])
            } else {

            redundant_layers_summary$redundancy_type[j] <- 2
            redundant_layers_summary$combined_layer[j] <-
              redundant_layers$combined_layer[vec_2]
            }

         # Else (if there is any "redundancy_type" 1):
         # Assign a "redundancy_type" 1

          } else {
          redundant_layers_summary$redundancy_type[j] <- 1
          }
        }


      # Check if there are any redundant layers with redundancy type 0
      # (i.e. to remove from the layer ranking)

      if (!identical(which(redundant_layers$redundancy_type == 0),
                     integer(0))) {

        # Store these layers as "redundant_layers_to_remove"

        redundant_layers_to_remove <-
          redundant_layers$layer[which(redundant_layers$redundancy_type == 0)]

        # Store the row indices in df of these layers

        vec_redundant <- NULL

        for (j in seq_along(redundant_layers_to_remove)) {
          vec_redundant <- c(vec_redundant,
                             vec_nonempty[
                               which(layers == redundant_layers_to_remove[j])])
          }

        # "vec_unique" is the vector with row indices of the given profile
        # from which the redundant layers are excluded

        vec_unique <- vec_nonempty[!vec_nonempty %in% vec_redundant]
        inferior_layer_limits_sub <- df$horizon_limit_low[vec_unique]
        superior_layer_limits_sub <- df$horizon_limit_up[vec_unique]

        # Rank the horizons of the given profile (excluding redundant layers)
        # based on the inferior layer limits in the column
        # "horizon_number_unique"

        df$horizon_number_unique[vec_unique] <- rank(inferior_layer_limits_sub)


        # Store information about the redundant_layers in
        # "list_redundant_layers"
        # (this is not reported in PIR)

        inconsistency_reason <-
          paste0("Caution: the given layer is redundant in the",
                 " depth profile (i.e. a complete profile can be",
                 " composed with the remaining layers).")
        inconsistency_reason_vec <- rep(NA, length(vec))
        inconsistency_reason_vec[which(!is.na(
          match(layers_vec, redundant_layers_to_remove)))] <-
          inconsistency_reason

        rule_id <- rep(NA,length(vec))
        rule_id[which(!is.na(match(layers_vec, redundant_layers_to_remove)))] <-
              "[not in PIR]"

        list_redundant_layers <- rbind(
           list_redundant_layers,
           data.frame(survey_form = as.factor(rep(survey_form, length(vec))),
                      partner = df$partner[vec],
                      partner_code = df$partner_code[vec],
                      country = df$country[vec],
                      code_country = df$code_country[vec],
                      survey_year = df$survey_year[vec],
                      code_plot = df$code_plot[vec],
                      plot_id = df$plot_id[vec],
                      horizon_master = df$horizon_master[vec],
                      redundancy_type =
                        redundant_layers_summary$redundancy_type,
                      combined_redundant_horizon_master =
                        redundant_layers_summary$combined_layer,
                      superior_layer_limit = df$horizon_limit_up[vec],
                      inferior_layer_limit = df$horizon_limit_low[vec],
                      horizon_number_original = df$horizon_number[vec],
                      horizon_number_unique = df$horizon_number_unique[vec],
                      code_line = df$code_line[vec],
                      inconsistency_reason = inconsistency_reason_vec,
                      rule_id = rule_id,
                      change_date = df$change_date[vec],
                      download_date = rep(download_date_pir, length(vec))))
      }
      }

      # If the column "horizon_number_unique" is still empty
      # (i.e. if there are no redundant layers):

      if (all(is.na(df$horizon_number_unique[vec]))) {

      # If the ranking of the existing column "horizon_number"
      # (which was submitted by the partners)
      # equals the ranking based on the lower layer limits
      # (which is not necessarily the case):

      # Copy the column "horizon_number" to "horizon_number_unique"

      # Else,
      # Rank the horizons of the given profile
      # based on the inferior layer limits in the column "horizon_number_unique"

      if (all(rank(df$horizon_number[vec]) ==
              rank(df$horizon_limit_low[vec]))) {
      df$horizon_number_unique[vec] <- df$horizon_number[vec]
      } else {
      df$horizon_number_unique[vec] <-  rank(df$horizon_limit_low[vec])
      }
      }


    ## FSCC_10: Error reason: there is a gap between adjacent layers ----

    # Check whether layers have layer limit information and a horizon number

    vec_nonempty <- vec[which(!is.na(df$horizon_limit_up[vec]) &
                                !is.na(df$horizon_limit_low[vec]) &
                                !is.na(df$horizon_number_unique[vec]))]

    # Order the row indices

    vec_horizon_number <-
      vec_nonempty[order(df$horizon_number_unique[vec_nonempty])]

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

    # Update the progress bar

    if (!isTRUE(getOption("knitr.in.progress"))) {
    setTxtProgressBar(progress_bar, i)
    }
    }

    # Convert missing horizon masters back to NA

    if (any(df$horizon_master == "")) {
      df$horizon_master[which(df$horizon_master == "")] <- NA
    }

    df$horizon_master <- as.factor(df$horizon_master)

    
    # Add columns which only give numbers to below-ground versus above-ground
    # layers
    
    df <- df %>%
      group_by(unique_survey_profile) %>%
      mutate(
        horizon_number_bg_only = ifelse(layer_type %in% c("mineral", "peat"),
                                      horizon_number_unique, NA),
        horizon_number_bg_min = suppressWarnings(min(horizon_number_bg_only,
                                                   na.rm = TRUE)),
        horizon_number_bg =
          horizon_number_bg_only - (horizon_number_bg_min - 1),
        horizon_number_ff = ifelse(layer_type %in% c("forest_floor"),
                                 horizon_number_unique, NA),) %>%
      select(-horizon_number_bg_only, -horizon_number_bg_min) %>%
      arrange(partner_code, unique_survey_profile, horizon_number_unique)


    # Save the survey form; list_layer_inconsistencies for the given survey form
    # and "list_redundant_layers" for the given survey form
    # to the global environment

    if (save_to_env == TRUE) {
    assign_env(survey_form, df)

    assign_env(paste0("list_redundant_layers_", survey_form),
               list_redundant_layers)

    assign_env(paste0("list_layer_inconsistencies_", survey_form),
               list_layer_inconsistencies)
    }

    if (!isTRUE(getOption("knitr.in.progress"))) {
    close(progress_bar)
    }
    } # end of "pfh" part

}
