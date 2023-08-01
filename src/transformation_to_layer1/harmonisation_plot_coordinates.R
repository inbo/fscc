
# This script aims to explore how to harmonise plot_id's and their coordinates
# (known to be relevant for at least Poland and the UK)
# + Create harmonisation tables

# List required packages ----

stopifnot(require("sf"))
stopifnot(require("tidyverse"))
stopifnot(require("terra"))
stopifnot(require("maptiles"))
stopifnot(require("mapview"))
stopifnot(require("leaflet"))
stopifnot(require("htmltools"))
stopifnot(require("leafem"))
stopifnot(require("crosstalk"))
stopifnot(require("DT"))

# Source and create functions ----

source("./src/functions/get_plot_coord_colocation_table.R")

as_sf <- function(dataframe) {
  
  assertthat::assert_that("longitude_dec" %in% names(dataframe) &&
                          "latitude_dec" %in% names(dataframe),
                          msg = paste0("Columns 'longitude_dec' and/or ",
                                       "'latitude_dec' do not exist in the ",
                                       "given dataframe."))
  
  assertthat::assert_that(
    length(which(is.na(dataframe$longitude_dec))) == 0 &&
    length(which(is.na(dataframe$latitude_dec))) == 0,
    msg = paste0("Column(s) 'longitude_dec' and/or 'latitude_dec' contain(s) ",
                 "at least one NA."))
  
  dataframe_spat <- dataframe %>%
    mutate(longitude = longitude_dec) %>%
    mutate(latitude = latitude_dec) %>%
    st_as_sf(coords = c("longitude", "latitude"),
             crs = 4326) %>%
    st_transform(crs = 3035)
  
  return(dataframe_spat)

}


# Level II Poland ----

# Import coordinates of so_plot (Hamburg database) + table by Krzysztof

source("./src/functions/dec_coordinate.R")

extra_data_poland_LII <- 
  openxlsx::read.xlsx(paste0("./data/additional_data/partner_comm/",
                             "SO_PLOT_poland_LII_nathalie.xlsx"),
                      sheet = 1) %>%
  rename(code_country = CODE_COUNTRY) %>%
  rename(survey_year = YEAR) %>%
  rename(code_plot = CODE_PLOT) %>%
  filter(code_country == 53) %>%
  select(code_country, survey_year, code_plot, LATITUDE, LONGITUDE) %>%
  mutate(latitude_dec =
           sapply(LATITUDE, dec_coordinate, error_report = FALSE),
         longitude_dec =
           sapply(LONGITUDE, dec_coordinate, error_report = FALSE)) %>%
  select(-LONGITUDE, -LATITUDE) %>%
  mutate(plot_id = paste0("53_", code_plot))

assertthat::assert_that(
  identical(unique(extra_data_poland_LII$survey_year),
            c("1995", "1999")))

extra_so_plot_1995 <- extra_data_poland_LII %>%
  filter(survey_year == 1995)

extra_so_plot_1999 <- extra_data_poland_LII %>%
  filter(survey_year == 1999)

ks_data <- 
  openxlsx::read.xlsx(paste0("./data/additional_data/partner_comm/",
                             "all code plot, coordinations, PL KS - LI.xlsx"),
                      sheet = 1) %>%
  select(-"1995", -"1999", -"2003", -"2007", -"2017") %>%
  rename(longitude_dec = longitude) %>%
  rename(latitude_dec = latitude) %>%
  rename(plot_id = "Code.plot") %>%
  mutate(plot_id = paste0("53_", plot_id))

# Create plot coordinate co-location table

df_table_test <-
  get_plot_coord_colocation_table(code_survey = "so",
                                  partner_code = 53,
                                  extra_forms = c("extra_so_plot_1995",
                                                  "extra_so_plot_1999",
                                                  "ks_data"))

df_table_test_6000 <-
  get_plot_coord_colocation_table(code_survey = "so",
                                  partner_code = 53,
                                  extra_forms = c("extra_so_plot_1995",
                                                  "extra_so_plot_1999",
                                                  "ks_data"),
                                  dist_threshold = 6000)

# Sort the rows of this table

df_table <- df_table_test %>%
  mutate(arrange_col = as.numeric(gsub("^53_", "", so_prf_53_1999))) %>%
  mutate(arrange_col_2 = as.numeric(gsub("^53_", "", so_pls_53_1995))) %>%
  mutate(arrange_col_3 = as.numeric(gsub("^53_", "", so_pls_53_1999))) %>%
  mutate(arrange_col_4 = as.numeric(gsub("^53_", "", si_plt_53_2000))) %>%
  arrange(arrange_col, arrange_col_2, arrange_col_3,
          desc(all_equal), arrange_col_4) %>%
  select(-arrange_col, -arrange_col_2, -arrange_col_3, -arrange_col_4)

df_table$combined_coords <-
 apply(df_table[, which(names(df_table) %in% c("latitude_most_abundant",
                                               "longitude_most_abundant"))], 1,
       function(x) paste0(x, collapse = "_"))

df_table <- df_table %>%
  mutate(coords_duplicated = duplicated(combined_coords)) %>%
  select(-combined_coords) %>%
  relocate(coords_duplicated, .before = all_equal)

write.csv2(df_table,
           paste0("./output/spatial_plot_id_links/",
                  "links_plot_id_location_poland_LII_so_plot.csv"),
           row.names = FALSE,
           na = "")

df_table_spat_mapview <- df_table %>%
  mutate(across(where(is.character), ~ replace_na(.x, "-"))) %>%
  mutate(across(everything(), ~ ifelse(.x == "NA", "-", .x))) %>%
  mutate(longitude_dec = longitude_most_abundant) %>%
  mutate(latitude_dec = latitude_most_abundant) %>%
  select(-longitude_most_abundant, -latitude_most_abundant) %>%
  st_as_sf(coords = c("longitude_dec", "latitude_dec"),
           crs = 4326) %>%
  st_transform(crs = 3035)

mapView(df_table_spat_mapview,
        layer.name = "Spatial matches of Polish Level II plots",
        map.types = "OpenTopoMap",
        popup = TRUE)

mapshot(mapView(df_table_spat_mapview,
                layer.name = "Spatial matches of Polish Level II plots",
                map.types = "OpenTopoMap",
                popup = TRUE),
        url = paste0("./output/spatial_plot_id_links/",
                     "links_plot_id_location_poland_LII.html"))



### Analyse conversion needs ----

links_poland_LII_2000 <-
  read.csv2(paste0("./output/spatial_plot_id_links/",
                   "links_plot_id_location_poland_LII_so_plot.csv"),
            na.strings = "")

show_conversion_needs <- function(col) {
  
  print(names(links_poland_LII_2000)[col])
  
  print(sort(unique(na.omit(as.numeric(gsub("^53_", "",
                                            links_poland_LII_2000[, col]))))))
  print(all(
    links_poland_LII_2000[which(!is.na(links_poland_LII_2000[, col])), col] ==
        links_poland_LII_2000[which(!is.na(links_poland_LII_2000[, col])), 15]))
}

show_conversion_needs(2) # prf 1999
# To convert; 111, 150

show_conversion_needs(3) # prf 2009
# No conversion needed

show_conversion_needs(4) # prf 2017
# 816 should become 801, because both 801 (2009) and 816 (2017) from so_prf
# have the same coördinates.
# These coördinates are also the same like the coördinates of 801 in the
# file of Krzysztof (ks_data)

show_conversion_needs(5) # pls 1995
# To convert; 110, 111, 112

show_conversion_needs(6) # pls 1999
# To convert; 111, 150

show_conversion_needs(7) # pls 2009
# No conversion needed

show_conversion_needs(8) # pls 2017
# 816 should become 801, because both 801 (2009) and 816 (2017) from so_prf
# have the same coördinates.
# These coördinates are also the same like the coördinates of 801 in the
# file of Krzysztof (ks_data)

sort(as.numeric(extra_so_plot_1995$code_plot)) # 110, 111, 112
sort(as.numeric(extra_so_plot_1999$code_plot)) # 111, 150

so_som[(which(so_som$code_country == 53 & so_som$code_plot == 112)), ] %>% View


### Create harmonisation table ----

st_distance(extra_so_plot_1995 %>%
              filter(plot_id == "53_111") %>%
              as_sf,
            ks_data_spat[which(ks_data_spat$plot_id %in% 	
                                 c("53_801", "53_809", "53_810")), ])

st_distance(extra_so_plot_1995 %>%
              filter(plot_id == "53_89") %>%
              as_sf,
            ks_data_spat) %>% min


assertthat::assert_that(
  all(so_pls$plot_id[which(so_pls$partner_code == 53 &
                       so_pls$survey_year == 2017)] ==
  so_prf$plot_id[which(so_prf$partner_code == 53 &
                         so_prf$survey_year == 2017)]))


poland_LII_harmonisation_table <-
  rbind(extra_so_plot_1995,
        extra_so_plot_1999,
        select(so_pls[which(so_pls$partner_code == 53 &
                              so_pls$survey_year == 2017), ],
               code_country, survey_year, code_plot,
               latitude_dec, longitude_dec, plot_id)) %>%
  rename(plot_id_orig = plot_id) %>%
  as_sf



poland_LII_harmonisation_table <-
  distance_join(sf1 = poland_LII_harmonisation_table,
                sf2 = as_sf(ks_data),
                join_column = "plot_id",
                dist_threshold = 2000)

poland_LII_harmonisation_table <- poland_LII_harmonisation_table %>%
  st_drop_geometry() %>%
  rename(plot_id_2000 = plot_id) %>%
  as_sf

poland_LII_harmonisation_table <-
  distance_join(sf1 = poland_LII_harmonisation_table,
                sf2 = ks_data_spat,
                join_column = "plot_id",
                dist_threshold = 4000)

poland_LII_harmonisation_table <- poland_LII_harmonisation_table %>%
  st_drop_geometry() %>%
  rename(plot_id_4000 = plot_id) %>%
  as_sf

poland_LII_harmonisation_table <-
  distance_join(sf1 = poland_LII_harmonisation_table,
                sf2 = ks_data_spat,
                join_column = "plot_id",
                dist_threshold = 6000)

poland_LII_harmonisation_table_2 <- poland_LII_harmonisation_table %>%
  st_drop_geometry() %>%
  rename(plot_id_6000 = plot_id) %>%
  mutate(plot_id = ifelse((!is.na(plot_id_2000) |
                            plot_id_orig %in% c("53_112")),
                          plot_id_2000,
                          ifelse(!is.na(plot_id_4000),
                                 plot_id_4000,
                                 plot_id_6000))) %>%
  mutate(plot_id = as.character(plot_id)) %>%
  rename(latitude_so_plot = latitude_dec) %>%
  rename(longitude_so_plot = longitude_dec) %>%
  left_join(ks_data, by = "plot_id") %>%
  select(-plot_id_2000, -plot_id_4000, -plot_id_6000) %>%
  mutate(comb = paste0(.data$code_country, "_",
                       .data$plot_id_orig, "_",
                       .data$plot_id, "_",
                       .data$latitude_dec, "_",
                       .data$longitude_so_plot)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  arrange(as.numeric(code_plot)) %>%
  mutate(survey_year = ifelse(survey_year %in% c("1995", "1999"),
                              "1995_1999_2000",
                              survey_year)) %>%
  mutate(survey_code = ifelse(survey_year == "1995_1999_2000",
                              "si_so",
                              ifelse(survey_year == "2017",
                                     "so",
                                     NA))) %>%
  relocate(survey_code, .before = survey_year) %>%
  arrange(survey_year) %>%
  select(-comb) %>%
  rename(code_plot_orig = code_plot) %>%
  mutate(code_plot = gsub("^53_", "", .data$plot_id)) %>%
  relocate(code_plot, .after = plot_id) %>%
  mutate(latitude_so_plot = as.numeric(latitude_so_plot),
         longitude_so_plot = as.numeric(longitude_so_plot))
## TO DO: verify 816 versus 801 in 2017
## TO DO: give new name to 112


# The plot below only appears in "si" and would be co-located with plot 101
# (impossible), so this is actually not necessary for us
# Solution: just remove its coordinates
poland_LII_harmonisation_table_2 <-
  rbind(poland_LII_harmonisation_table_2,
        data.frame(code_country = 53,
                   survey_code = "si",
                   survey_year = NA,
                   code_plot_orig = 308,
                   latitude_so_plot = NA,
                   longitude_so_plot = NA,
                   plot_id_orig = "53_308",
                   plot_id = "53_308",
                   code_plot = 308,
                   latitude_dec = NA,
                   longitude_dec = NA))

  
write.csv2(poland_LII_harmonisation_table_2,
           paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                  "plot_coord_harmonisation_key_poland_so.csv"),
           row.names = FALSE,
           na = "")





# Level I Poland ----

# Import coordinates of Krzysztof


source("./src/functions/dec_coordinate.R")

ks_sheet_1 <- 
  openxlsx::read.xlsx(paste0("./data/additional_data/partner_comm/",
                             "all code plot, coordinations, PL KS - LI.xlsx"),
                      sheet = 1) %>%
  select(-"1995", -"1999", -"2003", -"2007", -"2017") %>%
  rename(longitude_dec = longitude) %>%
  rename(latitude_dec = latitude) %>%
  rename(plot_id = "Code.plot") %>%
  mutate(plot_id = paste0("53_", plot_id))

ks_sheet_2 <- 
  openxlsx::read.xlsx(paste0("./data/additional_data/partner_comm/",
                             "all code plot, coordinations, PL KS - LI.xlsx"),
                      sheet = 2) %>%
  mutate(longitude_ddmmss = gsub("[+\\.]", "", LONGITUDE)) %>%
  mutate(latitude_ddmmss = gsub("[+\\.]", "", LATITUDE)) %>%
  mutate(latitude_dec =
           sapply(latitude_ddmmss, dec_coordinate, error_report = FALSE),
         longitude_dec =
           sapply(longitude_ddmmss, dec_coordinate, error_report = FALSE)) %>%
  select(-LONGITUDE, -LATITUDE, -longitude_ddmmss, -latitude_ddmmss) %>%
  rename(plot_id = "CODE_PLOT") %>%
  mutate(plot_id = paste0("53_", plot_id))


# Create plot coordinate co-location table

df_table_test_2000 <-
  get_plot_coord_colocation_table(code_survey = "s1",
                                  partner_code = 53,
                                  extra_forms = c("ks_sheet_1",
                                                  "ks_sheet_2"))

df_table_test_2000 %>%
  summarise(across(everything(), ~ length(unique(.[!is.na(.)]))))


df_table_test_10 <-
  get_plot_coord_colocation_table(code_survey = "s1",
                                  partner_code = 53,
                                  extra_forms = c("ks_sheet_1",
                                                  "ks_sheet_2"),
                                  dist_threshold = 10)

df_table_test_10 %>%
  summarise(across(everything(), ~ length(unique(.[!is.na(.)]))))

df_table <- df_table_test_2000 %>%
  mutate(ks_sheet_2_bis = gsub("^53_0", "53_", ks_sheet_2)) %>%
  mutate(plot_id = ifelse((!is.na(all_equal_survey) &
                           !is.na(ks_sheet_2_bis) &
                           (all_equal_survey == ks_sheet_2_bis)),
                          ks_sheet_2,
                          NA)) %>%
  select(-ks_sheet_2_bis) %>%
  mutate(arrange_col = as.numeric(gsub("^53_", "", ks_sheet_2))) %>%
  mutate(arrange_col_2 = as.numeric(gsub("^53_", "", ks_sheet_1))) %>%
  arrange(arrange_col, arrange_col_2) %>%
  select(-arrange_col, -arrange_col_2)

write.csv2(df_table,
           paste0("./output/spatial_plot_id_links/",
                  "links_plot_id_location_poland_LI_2000_2.csv"),
           row.names = FALSE,
           na = "")


s1_som[which(s1_som$partner_code == 53),] %>%
  filter(as.numeric(code_plot) < 2000) %>%
  distinct(survey_year)

# Visualise geospatially in dynamic map

df_table_s1_spat_mapview <- df_table_s1_spat %>%
  #st_drop_geometry() %>%
  mutate(across(where(is.character), ~ replace_na(.x, "-"))) %>%
  mutate(across(everything(), ~ ifelse(.x == "NA", "-", .x))) %>%
  mutate(longitude_dec = longitude_most_abundant) %>%
  mutate(latitude_dec = latitude_most_abundant) %>%
  select(-longitude_most_abundant, -latitude_most_abundant) %>%
  st_as_sf(coords = c("longitude_dec", "latitude_dec"),
           crs = 4326) %>%
  st_transform(crs = 3035)

mapView(df_table_s1_spat_mapview, map.types = "OpenTopoMap")

mapshot(mapView(df_table_s1_spat_mapview,
                layer.name = "Spatial matches of Polish Level I plots",
                map.types = "OpenTopoMap",
                popup = TRUE),
        url = paste0("./output/spatial_plot_id_links/",
                     "links_plot_id_location_poland_LI.html"))





# Add plot_ids without coordinates

assertthat::assert_that(
  (length(unique(df_without_coords$unique_survey_year)) == 1 &&
     "s1_prf_53_2008" %in% unique(df_without_coords$unique_survey_year)),
  msg = paste0("Update script so that plot_ids without coordinates in other ",
               "unique_survey_years than s1_prf_53_2008 are also covered."))

unmatched_plot_ids <- NULL

col_ind <- col_indices$col_ind[which(
  col_indices$unique_survey_year == "s1_prf_53_2008")]

for (i in seq_along(df_without_coords$plot_id)) {
  
  row_indices <-
    lapply(df_table, function(col) which(col == df_without_coords$plot_id[i]))
  
  if (all(identical(row_indices, integer(0)))) {
    unmatched_plot_ids <- c(unmatched_plot_ids, df_without_coords$plot_id[i])
  }
  
  if (any(!identical(row_indices, integer(0)))) {
    
    col_matches <-
      names(row_indices)[sapply(row_indices, function(x) length(x) > 0)]
    
    for (j in seq_along(col_matches)) {
      
      row_ind <- row_indices[[which(names(row_indices) == col_matches[j])]]
      
      df_table[row_ind, col_ind] <- df_without_coords$plot_id[i]
      df_match_type[row_ind, col_ind] <- "plot_id"
    }
  }
}

if (!identical(unmatched_plot_ids, NULL)) {
df_table_j <- data.frame(matrix(NA,
                                nrow = length(unmatched_plot_ids),
                                ncol = length(unique_survey_years)))
names(df_table_j) <- unique_survey_years
df_table_j[, col_ind] <- unmatched_plot_ids
df_table <- rbind(df_table,
                  df_table_j)

df_match_type_j <- data.frame(matrix(NA,
                                nrow = length(unmatched_plot_ids),
                                ncol = length(unique_survey_years)))
names(df_match_type_j) <- unique_survey_years
df_match_type_j[, col_ind] <- "plot_id"
df_match_type <- rbind(df_match_type,
                       df_match_type_j)
}


# Are the plot_ids of s1 survey forms linked in a one-on-one way?
# Ignore system installment y1, where all the plot_ids seem different and
# where there are often multiple spatial matches with plot_ids from s1,
# so they are not linked in a one-on-one way.

combi <- paste0(df_table$s1_prf_53_1995, "_",
                df_table$s1_prf_53_2008, "_",
                df_table$s1_pls_53_1995, "_",
                df_table$s1_pls_53_2006, "_",
                df_table$s1_pls_53_2007)

length(unique(combi)) #642

length(unique(na.omit(df_table$s1_prf_53_1995))) #122
length(unique(combi[which(!is.na(df_table$s1_prf_53_1995))])) #122

length(unique(na.omit(df_table$s1_prf_53_2008))) # 524
length(unique(combi[which(!is.na(df_table$s1_prf_53_2008))])) #524

length(unique(na.omit(df_table$s1_pls_53_1995))) #122
length(unique(combi[which(!is.na(df_table$s1_pls_53_1995))])) #122

length(unique(na.omit(df_table$s1_pls_53_2006))) #102
length(unique(combi[which(!is.na(df_table$s1_pls_53_2006))])) #102

length(unique(na.omit(df_table$s1_pls_53_2007))) #422
length(unique(combi[which(!is.na(df_table$s1_pls_53_2007))])) #422

  # Answer: yes!

# Create a new table without y1

row_ind <- which((df_match_type$y1_pl1_53_NA != "reference") &
                   any(!is.na(df_table[, 1:5])))
df_table_s1 <-
  df_table[row_ind, ]
df_match_type_s1 <-
  df_match_type[row_ind, ]





# Level I UK ----

df_table_uk_s1 <-
  get_plot_coord_colocation_table(code_survey = "s1",
                                  partner_code = 6,
                                  dist_threshold = 2000)

write.csv2(df_table_uk_s1,
           paste0("./output/spatial_plot_id_links/",
                  "links_plot_id_location_uk_LI_2000.csv"),
           row.names = FALSE,
           na = "")

# Level II UK ----

df_table_uk_so <-
  get_plot_coord_colocation_table(code_survey = "so",
                                  partner_code = 6,
                                  dist_threshold = 10)

write.csv2(df_table_uk_so,
           paste0("./output/spatial_plot_id_links/",
                  "links_plot_id_location_uk_LII_10.csv"),
           row.names = FALSE,
           na = "")











#----------------------------

# Are the plot_ids of s1 survey forms linked in a one-on-one way? ----
# Ignore system installment y1, where all the plot_ids seem different and
# where there are often multiple spatial matches with plot_ids from s1,
# so they are not linked in a one-on-one way.

# vec_cols <- which(grepl(code_survey, names(df_table)))

# df_table <-
#   df_table[which(apply(df_table[, vec_cols], 1,
#                        function(x) any(!is.na(x)))), ]

# combi <-
#   apply(df_table[, vec_cols], 1, function(x) paste0(x, collapse = "_"))
# 
# length(unique(combi)) #642
# 
# for (i in seq_along(vec_cols)) {
#   assertthat::assert_that(
#     length(unique(na.omit(df_table[, vec_cols[i]]))) == 
#       length(unique(combi[which(!is.na(df_table[, vec_cols[i]]))])))
# }

df <- so_som %>%
  group_by(unique_survey_repetition) %>%
  mutate(
    layer_number_bg_only = ifelse(layer_type %in% c("mineral", "peat"), layer_number, NA),
    layer_number_bg_min = min(layer_number_bg_only, na.rm = TRUE),
    layer_number_bg = layer_number_bg_only - (layer_number_bg_min - 1)) %>%
  select(-layer_number_bg_only, -layer_number_bg_min)

