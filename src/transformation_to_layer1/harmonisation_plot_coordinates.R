
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
source("./src/functions/as_sf.R")

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
  rename(partner_code = code_country) %>%
  select(partner_code, survey_year, code_plot, LATITUDE, LONGITUDE) %>%
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
                                                  "ks_data"),
                                  dist_threshold = 10)

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

so_som[(which(so_som$partner_code == 53 & so_som$code_plot == 112)), ] %>% View


### Create harmonisation table ----

ks_data_spat <- as_sf(ks_data)

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
        select(so_pls[which(so_pls$plot_id == "53_816" &
                              so_pls$survey_year == 2017), ],
               partner_code, survey_year, code_plot,
               latitude_dec, longitude_dec, plot_id)) %>%
  rename(plot_id_orig = plot_id) %>%
  as_sf

poland_LII_harmonisation_table <-
  distance_join(sf1 = poland_LII_harmonisation_table,
                sf2 = as_sf(ks_data),
                join_column = "plot_id",
                summary_stat = "min_dist",
                dist_threshold = 6000)

poland_LII_harmonisation_table_2 <- poland_LII_harmonisation_table %>%
  st_drop_geometry() %>%
  # rename(latitude_so_plot = latitude_dec) %>%
  # rename(longitude_so_plot = longitude_dec) %>%
  select(-latitude_dec, -longitude_dec) %>%
  left_join(ks_data, by = "plot_id") %>%
  mutate(comb = paste0(.data$partner_code, "_",
                       .data$plot_id_orig, "_",
                       .data$plot_id, "_",
                       .data$latitude_dec, "_",
                       .data$longitude_dec)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  arrange(as.numeric(code_plot)) %>%
  mutate(survey_year = ifelse(survey_year %in% c("1995", "1999"),
                              "1995_1999_2000_1994_1996_1998",
                              survey_year)) %>%
  mutate(survey_code = ifelse(survey_year == "1995_1999_2000_1994_1996_1998",
                              "si_so",
                              ifelse(survey_year == "2017",
                                     "so",
                                     NA))) %>%
  relocate(survey_code, .before = survey_year) %>%
  arrange(survey_year) %>%
  select(-comb) %>%
  rename(code_plot_orig = code_plot) %>%
  mutate(code_plot = gsub("^53_", "", .data$plot_id)) %>%
  relocate(code_plot, .before = plot_id)

## TO DO: verify 816 versus 801 in 2017
## TO DO: give new name to 112


# The plot below only appears in "si" and would be co-located with plot 101
# (impossible), so this is actually not necessary for us
# Solution: just remove its coordinates
poland_LII_harmonisation_table_2 <-
  rbind(poland_LII_harmonisation_table_2,
        data.frame(partner_code = 53,
                   survey_code = "si",
                   survey_year = NA,
                   code_plot_orig = 308,
                   plot_id_orig = "53_308",
                   code_plot = 308,
                   plot_id = "53_308",
                   latitude_dec = NA,
                   longitude_dec = NA))

  
write.csv2(poland_LII_harmonisation_table_2,
           paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                  "LII_53_plot_coord_harmonisation_key_Poland.csv"),
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


### Analyse conversion needs ----

# 1.
# Data from plots which match with LII plots (i.e. plots lower than 2000)
# in the database are all from the year 1995.
# Do these data need to be moved to LII?

s1_som %>%
  filter(partner_code == 53) %>%
  filter(as.numeric(code_plot) < 2000) %>%
  distinct(survey_year)

s1_som %>%
  filter(partner_code == 53) %>%
  filter(as.numeric(code_plot) < 2000) %>%
  View

so_som %>%
  filter(partner_code == 53) %>%
  filter(survey_year == 1995) %>%
  View

list_1995_2 <-
  rbind(select(s1_prf[which(s1_prf$partner_code == 53 &
                            s1_prf$survey_year == 1995), ],
               plot_id,
               longitude_dec,
               latitude_dec),
        select(s1_pls[which(s1_pls$partner_code == 53 &
                            s1_pls$survey_year == 1995), ],
               plot_id,
               longitude_dec,
               latitude_dec)) %>%
  mutate(comb = paste0(.data$plot_id, "_",
                       .data$longitude_dec, "_",
                       .data$latitude_dec)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  rename(plot_id_orig = plot_id) %>%
  mutate(code_plot_orig = as.numeric(gsub("^53_", "", plot_id_orig))) %>%
  arrange(code_plot_orig) %>%
  as_sf

list_1995 <-
  distance_join(sf1 = list_1995_2,
                sf2 = as_sf(ks_data),
                join_column = "plot_id",
                summary_stat = "min_dist",
                dist_threshold = 2000) %>%
  st_drop_geometry()


s1_table <- s1_som %>%
  filter(partner_code == 53) %>%
  filter(survey_year == 1995) %>%
  rename(plot_id_orig = plot_id) %>%
  left_join(select(list_1995, plot_id_orig, plot_id),
            by = "plot_id_orig") %>%
  relocate(plot_id, .after = "code_plot")

plot_coord_harmonisation_key_poland_so <-
  read.csv2(paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                   "plot_coord_harmonisation_key_poland_so.csv"),
            na.strings = "") %>%
  filter(str_detect(survey_year, "1995"))

so_table <- so_som %>%
  filter(partner_code == 53) %>%
  filter(survey_year == 1995) %>%
  rename(plot_id_orig = plot_id) %>%
  left_join(select(plot_coord_harmonisation_key_poland_so,
                   plot_id_orig, plot_id),
            by = "plot_id_orig") %>%
  relocate(plot_id, .after = "code_plot")

s1_table %>%
  distinct(plot_id) %>%
  arrange(plot_id) %>% head

so_table %>%
  distinct(plot_id) %>%
  arrange(plot_id) %>% head

so_table %>%
  filter(plot_id == "53_701") %>%
  select(code_layer, organic_carbon_total, n_total)

# Create data frames with unique plot_id values from each table
s1_unique <- s1_table %>% 
  distinct(plot_id) %>% 
  filter(!is.na(plot_id) & (plot_id != "NA")) %>%
  mutate(s1_table = plot_id) %>% 
  select(plot_id, s1_table)

so_unique <- so_table %>% 
  distinct(plot_id) %>% 
  filter(!is.na(plot_id) & (plot_id != "NA")) %>%
  mutate(so_table = plot_id) %>% 
  select(plot_id, so_table)

# Merge the two data frames using left_join
merged_table <- full_join(s1_unique, so_unique, by = "plot_id")
merged_table[is.na(merged_table)] <- ""
names(merged_table) <- c("plot_id", "s1_som", "so_som")
merged_table <- full_join(merged_table,
                          select(list_1995, plot_id, plot_id_orig),
                          by = c("s1_som" = "plot_id")) %>%
  rename(s1_som_orig = plot_id_orig) %>%
  left_join(select(plot_coord_harmonisation_key_poland_so,
                   plot_id, plot_id_orig),
            by = c("so_som" = "plot_id")) %>%
  rename(so_som_orig = plot_id_orig)

s1_som %>%
  filter(plot_id == "53_701") %>%
  select(code_layer, organic_carbon_total, n_total)

write.csv2(merged_table,
           paste0("./output/spatial_plot_id_links/",
                  "links_plot_id_location_poland_s1_so_1995.csv"),
           row.names = FALSE,
           na = "")

# Conclusion:
# Data in s1_som from 1995 are quite different than those of so_som 1995,
# Even though they are roughly on the same locations.


# 2.
# plot_ids in system installment form y1_pl1 do not correspond at all
# with plot_ids from list Krzysztof

list_y1 <- y1_pl1 %>%
  filter(partner_code == 53) %>%
  select(plot_id, last_year, longitude_dec, latitude_dec) %>%
  rename(plot_id_orig = plot_id) %>%
  mutate(code_plot_orig = as.numeric(gsub("^53_", "", plot_id_orig))) %>%
  arrange(code_plot_orig) %>%
  as_sf

list_y1 <-
  distance_join(sf1 = list_y1,
                sf2 = as_sf(ks_sheet_2),
                join_column = "plot_id",
                summary_stat = "min_dist",
                dist_threshold = 2000) %>%
  st_drop_geometry()

list_y1 %>%
  filter(last_year == 2013) %>%
  filter(!is.na(plot_id)) %>%
  distinct(plot_id) %>%
  nrow

list_y1 %>%
  filter(last_year == 2013) %>%
  filter(!is.na(plot_id)) %>%
  select(plot_id) %>%
  nrow

list_y1 %>% filter(last_year == 2013) %>% filter(is.na(plot_id)) %>% nrow
list_y1 %>% filter(last_year == 2013) %>% nrow
# y1_pl1 2013: 5 out of 376 do not have a proximity match in Krzysztof's table

list_y1 %>% filter(last_year == 2018) %>% filter(is.na(plot_id)) %>% nrow
list_y1 %>% filter(last_year == 2018) %>% nrow
# y1_pl1 2018: 482 out of 616 do not have a proximity match in Krzysztof's table

ks_sheet_2$plot_id[which(!ks_sheet_2$plot_id %in%
                           (list_y1$plot_id[which(list_y1$last_year == 2013)]))]
# Out of 524 plots, 153 plots of the ks_sheet_2 list do not appear
# in y1_pl1 2013

# Create data frames with unique plot_id values from each table
y1_2013_unique <- y1_pl1 %>% 
  filter(last_year == 2013) %>%
  distinct(plot_id) %>% 
  filter(!is.na(plot_id) & (plot_id != "NA")) %>%
  mutate(plot_id_2013 = plot_id) %>% 
  select(plot_id, plot_id_2013)

y1_2018_unique <- y1_pl1 %>% 
  filter(last_year == 2018) %>%
  distinct(plot_id) %>% 
  filter(!is.na(plot_id) & (plot_id != "NA")) %>%
  mutate(plot_id_2018 = plot_id) %>% 
  select(plot_id, plot_id_2018)

# Merge the two data frames using left_join
merged_table <- full_join(y1_2013_unique, y1_2018_unique, by = "plot_id")
merged_table[is.na(merged_table)] <- ""
names(merged_table) <- c("plot_id", "plot_id_2013", "plot_id_2018")

merged_table %>%
  filter(!is.na(plot_id_2013) & (plot_id_2013 != "")) %>%
  filter(!is.na(plot_id_2018) & (plot_id_2018 != ""))
  # No common plot_ids between y1_pl1 2013 and 2018



y1_2013_unique <- y1_pl1 %>% 
  filter(last_year == 2013) %>%
  distinct(plot_id, .keep_all = TRUE) %>% 
  filter(!is.na(plot_id) & (plot_id != "NA")) %>%
  rename(plot_id_2013 = plot_id) %>% 
  select(plot_id_2013, latitude_dec, longitude_dec) %>%
  as_sf

y1_2018_unique <- y1_pl1 %>% 
  filter(last_year == 2018) %>%
  distinct(plot_id, .keep_all = TRUE) %>% 
  filter(!is.na(plot_id) & (plot_id != "NA")) %>%
  rename(plot_id_2018 = plot_id) %>% 
  select(plot_id_2018, latitude_dec, longitude_dec) %>%
  as_sf

y1_2018_2013 <- distance_join(sf1 = y1_2018_unique,
                              sf2 = y1_2013_unique,
                              join_column = "plot_id_2013",
                              summary_stat = "min_dist",
                              dist_threshold = 2000) %>%
  st_drop_geometry()

y1_2018_2013 %>%
  filter(!is.na(plot_id_2018)) %>%
  filter(!is.na(plot_id_2013)) %>%     # 89 co-located
  filter(plot_id_2013 == plot_id_2018) # of which 0 with same plot name


ks_sheet_2 %>% distinct() %>% nrow
s1_pls %>%
  filter(partner_code == 53) %>%
  filter(survey_year %in% c(2006, 2007, 2008)) %>%
  distinct(plot_id) %>%
  nrow
s1_prf %>%
  filter(partner_code == 53) %>%
  filter(survey_year %in% c(2006, 2007, 2008)) %>%
  distinct(plot_id) %>%
  nrow
s1_som %>%
  filter(partner_code == 53) %>%
  filter(survey_year %in% c(2006, 2007, 2008)) %>%
  distinct(plot_id) %>%
  nrow

# Conclusions:
# y1_pl1 2013 - spatial matches with ks_sheet_2 (except 5 plots)
#               Convert.
# y1_pl1 2018 - not really any spatial matches with anything
#               but the "s1" survey does not contain any data from 2018 anyway.
#               Ignore?
# s1_pls 1995 - most of them match with Level II plots Krzysztof but it doesn't
#               seem like the 1995 data of s1_som belong to so_som. What to do?
#               Krzysztof seems aware of the existence of 1995 data in PIR.
#               plot_ids would anyway have to be converted.
# s1_pls 2006 - 524 plots like in ks_sheet_2. Harmonise.
# s1_pls 2007 - 524 plots like in ks_sheet_2. Harmonise.
# s1_prf 1995 - most of them match with Level II plots Krzysztof but it doesn't
#               seem like the 1995 data of s1_som belong to so_som. What to do?
#               Krzysztof seems aware of the existence of 1995 data in PIR.
#               plot_ids would anyway have to be converted.
# s1_prf 2008 - No coordinates but 524 plots like in ks_sheet_2.
#               Add coordinates and harmonise.

### Create harmonisation table ----

# y1 2013
poland_LI_harmonisation_table <- list_y1 %>%
  rename(survey_year = last_year) %>%
  mutate(latitude_orig = as.numeric(latitude_dec)) %>%
  mutate(longitude_orig = as.numeric(longitude_dec)) %>%
  select(-longitude_dec, -latitude_dec) %>%
  filter(survey_year == 2013) %>%
  mutate(partner_code = 53,
         survey_code = "y1",
         code_plot = gsub("^53_", "", plot_id)) %>%
  left_join(ks_sheet_2,
            by = "plot_id") %>%
  select(partner_code, survey_code, survey_year, code_plot_orig,
         plot_id_orig, code_plot, plot_id, latitude_dec, longitude_dec) %>%
  # remove 5 plot_id_orig records for which no match was found in ks_sheet_2?
  filter(!is.na(plot_id))

# s1 2006, 2007, 2008
poland_LI_harmonisation_table_2 <-
  rbind(select(s1_pls[which(s1_pls$partner_code == 53 &
                              s1_pls$survey_year %in% c(2006, 2007, 2008)), ],
               plot_id, latitude_dec, longitude_dec),
        select(s1_prf[which(s1_prf$partner_code == 53 &
                              s1_prf$survey_year %in% c(2006, 2007, 2008)), ],
               plot_id, latitude_dec, longitude_dec)) %>%
  filter(!is.na(latitude_dec) & !is.na(longitude_dec)) %>%
  mutate(comb = paste0(.data$plot_id, "_",
                       .data$latitude_dec, "_",
                       .data$longitude_dec)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  rename(plot_id_orig = plot_id) %>%
  as_sf

poland_LI_harmonisation_table_2 <-
  distance_join(sf1 = poland_LI_harmonisation_table_2,
                sf2 = as_sf(ks_sheet_2),
                join_column = "plot_id",
                summary_stat = "min_dist",
                dist_threshold = 2000)

ks_sheet_2$plot_id[!ks_sheet_2$plot_id %in%
                     poland_LI_harmonisation_table_2$plot_id]

poland_LI_harmonisation_table_2_2 <-
  poland_LI_harmonisation_table_2 %>%
  st_drop_geometry() %>%
  select(-longitude_dec, -latitude_dec) %>%
  mutate(partner_code = 53,
         survey_code = "s1",
         code_plot = gsub("^53_", "", plot_id),
         code_plot_orig = gsub("^53_", "", plot_id_orig),
         survey_year = "2006_2007_2008") %>%
  left_join(ks_sheet_2,
            by = "plot_id") %>%
  select(partner_code, survey_code, survey_year, code_plot_orig,
         plot_id_orig, code_plot, plot_id, latitude_dec, longitude_dec)
  
poland_LI_harmonisation_table <-
  rbind(poland_LI_harmonisation_table,
        poland_LI_harmonisation_table_2_2)
  
write.csv2(poland_LI_harmonisation_table,
           paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                  "LI_53_plot_coord_harmonisation_key_Poland.csv"),
           row.names = FALSE,
           na = "")

# TO DO: check what to do with the 1995 data.


# Level I UK ----

df_table_uk_s1 <-
  get_plot_coord_colocation_table(code_survey = "s1",
                                  partner_code = 6,
                                  dist_threshold = 1)

write.csv2(df_table_uk_s1,
           paste0("./output/spatial_plot_id_links/",
                  "links_plot_id_location_uk_LI_1.csv"),
           row.names = FALSE,
           na = "")

### Analyse conversion needs ----

links_uk_LI_10 <-
  read.csv2(paste0("./output/spatial_plot_id_links/",
                   "links_plot_id_location_uk_LI_10.csv"),
            na.strings = "")

df_summary <- links_uk_LI_10 %>%
  mutate(code_plot = as.numeric(gsub("^6_", "", all_equal_survey))) %>%
  filter(code_plot <= 167) %>%
  group_by(all_equal_survey) %>%
  summarise(count = n()) %>%
  filter(count == 1) %>%
  mutate(code_plot = as.numeric(gsub("^6_", "", all_equal_survey))) %>%
  arrange(code_plot)

# y1_pl1: 940 from 2018 is co-located with 835 from 1994 (s1 & y1)

s1_som_summary <- s1_som %>%
  filter(partner_code == 6) %>%
  select(plot_id, unique_survey, survey_year) %>%
  distinct(unique_survey, .keep_all = TRUE) %>%
  group_by(plot_id) %>%
  summarise(survey_years_combined = paste0(survey_year, collapse = "_")) %>%
  mutate(code_plot = as.numeric(gsub("^6_", "", plot_id))) %>%
  arrange(code_plot)

y1_pl1 %>%
  filter(partner_code == 6) %>%
  mutate(unique_survey =
           paste0(partner_code, "_", last_year, "_", code_plot)) %>%
  select(plot_id, unique_survey, last_year) %>%
  distinct(unique_survey, .keep_all = TRUE) %>%
  group_by(plot_id) %>%
  summarise(survey_years_combined = paste0(last_year, collapse = "_")) %>%
  mutate(code_plot = as.numeric(gsub("^6_", "", plot_id))) %>%
  arrange(code_plot) %>% View


### Create harmonisation table ----

vec_moved_plots <- links_uk_LI_10 %>%
  mutate(code_plot = as.numeric(gsub("^6_", "", all_equal_survey))) %>%
  filter(code_plot <= 167) %>%
  group_by(all_equal_survey) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  mutate(code_plot = as.numeric(gsub("^6_", "", all_equal_survey))) %>%
  arrange(code_plot) %>%
  rename(plot_id = all_equal_survey) %>%
  select(plot_id) %>%
  as.vector() %>%
  unlist()


survey_years <- c(1994, 1995)

uk_LI_harmonisation_table_94 <-
  rbind(select(s1_pls[which(s1_pls$plot_id %in% vec_moved_plots &
                              s1_pls$survey_year %in% survey_years), ],
               plot_id, latitude_dec, longitude_dec, survey_year),
        select(s1_prf[which(s1_prf$plot_id %in% vec_moved_plots &
                              s1_prf$survey_year %in% survey_years), ],
               plot_id, latitude_dec, longitude_dec, survey_year)) %>%
  mutate(comb = paste0(plot_id, "_",
                       latitude_dec, "_",
                       longitude_dec, "_",
                       survey_year)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  bind_rows(select(mutate(s1_som[which(s1_som$plot_id %in% vec_moved_plots &
                                  s1_som$survey_year %in% survey_years), ],
                   latitude_dec = NA,
                   longitude_dec = NA),
            plot_id, latitude_dec, longitude_dec, survey_year)) %>%
  mutate(comb = paste0(plot_id, "_", survey_year)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  mutate(plot_id_orig = plot_id) %>%
  mutate(code_plot_orig = as.numeric(gsub("^6_", "", plot_id_orig))) %>%
  arrange(code_plot_orig) %>%
  mutate(year_suffix = substr(survey_year, 3, 4)) %>%
  mutate(code_plot = str_pad(as.character(code_plot_orig),
                             width = 4, pad = "0")) %>%
  mutate(code_plot = paste0(year_suffix, code_plot)) %>%
  select(-year_suffix) %>%
  mutate(partner_code = 6) %>%
  mutate(plot_id = paste0(partner_code, "_", code_plot)) %>%
  mutate(survey_code = "y1_s1") %>%
  mutate(survey_year = paste0(survey_years, collapse = "_")) %>%
  select(partner_code, survey_code, survey_year, code_plot_orig,
         plot_id_orig, code_plot, plot_id, latitude_dec, longitude_dec)

survey_years <- c(2005, 2006)

uk_LI_harmonisation_table_06 <-
  rbind(select(s1_pls[which(s1_pls$plot_id %in% vec_moved_plots &
                              s1_pls$survey_year %in% survey_years), ],
               plot_id, latitude_dec, longitude_dec, survey_year),
        select(s1_prf[which(s1_prf$plot_id %in% vec_moved_plots &
                              s1_prf$survey_year %in% survey_years), ],
               plot_id, latitude_dec, longitude_dec, survey_year)) %>%
  mutate(comb = paste0(plot_id, "_",
                       latitude_dec, "_",
                       longitude_dec, "_",
                       survey_year)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  mutate(plot_id_orig = plot_id) %>%
  mutate(code_plot_orig = as.numeric(gsub("^6_", "", plot_id_orig))) %>%
  arrange(code_plot_orig) %>%
  mutate(year_suffix = substr(survey_year, 3, 4)) %>%
  mutate(code_plot = str_pad(as.character(code_plot_orig),
                             width = 4, pad = "0")) %>%
  mutate(code_plot = paste0(year_suffix, code_plot)) %>%
  select(-year_suffix) %>%
  mutate(partner_code = 6) %>%
  mutate(plot_id = paste0(partner_code, "_", code_plot)) %>%
  mutate(survey_code = "y1_s1") %>%
  mutate(survey_year = paste0(survey_years, collapse = "_")) %>%
  select(partner_code, survey_code, survey_year, code_plot_orig,
         plot_id_orig, code_plot, plot_id, latitude_dec, longitude_dec)

survey_years <- c(2008)

uk_LI_harmonisation_table_08 <-
  rbind(select(s1_pls[which(s1_pls$plot_id %in% vec_moved_plots &
                              s1_pls$survey_year %in% survey_years), ],
               plot_id, latitude_dec, longitude_dec, survey_year),
        select(s1_prf[which(s1_prf$plot_id %in% vec_moved_plots &
                              s1_prf$survey_year %in% survey_years), ],
               plot_id, latitude_dec, longitude_dec, survey_year)) %>%
  mutate(comb = paste0(plot_id, "_",
                       latitude_dec, "_",
                       longitude_dec, "_",
                       survey_year)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  bind_rows(select(mutate(s1_som[which(s1_som$plot_id %in% vec_moved_plots &
                                         s1_som$survey_year %in% survey_years), ],
                          latitude_dec = NA,
                          longitude_dec = NA),
                   plot_id, latitude_dec, longitude_dec, survey_year)) %>%
  mutate(comb = paste0(plot_id, "_", survey_year)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  mutate(plot_id_orig = plot_id) %>%
  mutate(code_plot_orig = as.numeric(gsub("^6_", "", plot_id_orig))) %>%
  arrange(code_plot_orig) %>%
  mutate(year_suffix = substr(survey_year, 3, 4)) %>%
  mutate(code_plot = str_pad(as.character(code_plot_orig),
                             width = 4, pad = "0")) %>%
  mutate(code_plot = paste0(year_suffix, code_plot)) %>%
  select(-year_suffix) %>%
  mutate(partner_code = 6) %>%
  mutate(plot_id = paste0(partner_code, "_", code_plot)) %>%
  mutate(survey_code = "y1_s1") %>%
  mutate(survey_year = paste0(survey_years, collapse = "_")) %>%
  select(partner_code, survey_code, survey_year, code_plot_orig,
         plot_id_orig, code_plot, plot_id, latitude_dec, longitude_dec)

# plot 83 has incorrect coordinates in 2008
uk_LI_harmonisation_table_83 <-
  s1_pls %>%
  filter(plot_id == "6_83") %>%
  filter(survey_year == 2006) %>%
  select(survey_year, latitude_dec, longitude_dec) %>%
  mutate(partner_code = 6,
         survey_code = "y1_s1",
         survey_year = 2008,
         code_plot_orig = "83",
         plot_id_orig = "6_83",
         code_plot = code_plot_orig,
         plot_id = plot_id_orig) %>%
  relocate(latitude_dec, .after = last_col()) %>%
  relocate(longitude_dec, .after = last_col())

# plots 401 and 402 have incorrect degrees longitude in 
# Assumption (based on FSCDB.LI.1) that negative values are correct
# -> rely on s1_prf
# (Alternative: rely on y1_pl1 and s1_pls)

uk_LI_harmonisation_table_401402 <-
  s1_prf %>%
  filter(plot_id %in% c("6_401", "6_402")) %>%
  select(survey_year, latitude_dec, longitude_dec, plot_id, code_plot) %>%
  mutate(partner_code = 6,
         survey_code = "y1_s1",
         survey_year = 1994,
         code_plot_orig = code_plot,
         plot_id_orig = plot_id) %>%
  select(partner_code, survey_code, survey_year, code_plot_orig,
         plot_id_orig, code_plot, plot_id, latitude_dec, longitude_dec)

uk_LI_harmonisation_table <-
  rbind(uk_LI_harmonisation_table_94,
        uk_LI_harmonisation_table_06,
        uk_LI_harmonisation_table_08,
        uk_LI_harmonisation_table_83,
        uk_LI_harmonisation_table_401402) %>%
  mutate(latitude_dec = as.numeric(latitude_dec),
         longitude_dec = as.numeric(longitude_dec))

write.csv2(uk_LI_harmonisation_table,
           paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                  "LI_6_plot_coord_harmonisation_key_UK.csv"),
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


### Analyse conversion needs ----

links_uk_LII_10 <-
  read.csv2(paste0("./output/spatial_plot_id_links/",
                   "links_plot_id_location_uk_LII_10.csv"),
            na.strings = "")

df_summary <- links_uk_LII_10 %>%
  mutate(code_plot = as.numeric(gsub("^6_", "", all_equal_survey))) %>%
  filter(!is.na(code_plot)) %>%
  group_by(all_equal_survey) %>%
  summarise(count = n()) %>%
  filter(count == 2) %>%
  mutate(code_plot = as.numeric(gsub("^6_", "", all_equal_survey))) %>%
  arrange(code_plot)


# Moved: 512, 715, 716, 717, 922

# 512, 716, 717: 1995/1996 --> 2010
# 715, 922: 1995 and 2017 --> 2010

so_som_summary <- so_som %>%
  filter(partner_code == 6) %>%
  select(plot_id, unique_survey, survey_year) %>%
  distinct(unique_survey, .keep_all = TRUE) %>%
  group_by(plot_id) %>%
  summarise(survey_years_combined = paste0(survey_year, collapse = "_")) %>%
  mutate(code_plot = as.numeric(gsub("^6_", "", plot_id))) %>%
  arrange(code_plot)


### Create harmonisation table ----

vec_moved_plots <- links_uk_LII_10 %>%
  filter(!is.na(all_equal_survey)) %>%
  group_by(all_equal_survey) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  rename(plot_id = all_equal_survey) %>%
  select(plot_id) %>%
  as.vector() %>%
  unlist()


survey_years <- c(1995, 1996, 2017)

uk_LII_harmonisation_table_95 <-
  rbind(select(so_pls[which(so_pls$plot_id %in% vec_moved_plots &
                              so_pls$survey_year %in% survey_years), ],
               plot_id, latitude_dec, longitude_dec, survey_year),
        select(so_prf[which(so_prf$plot_id %in% vec_moved_plots &
                              so_prf$survey_year %in% survey_years), ],
               plot_id, latitude_dec, longitude_dec, survey_year)) %>%
  mutate(comb = paste0(plot_id, "_",
                       latitude_dec, "_",
                       longitude_dec, "_",
                       survey_year)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  bind_rows(select(mutate(so_som[which(so_som$plot_id %in% vec_moved_plots &
                                  so_som$survey_year %in% survey_years), ],
                          latitude_dec = NA,
                          longitude_dec = NA),
                   plot_id, latitude_dec, longitude_dec, survey_year)) %>%
  mutate(comb = paste0(plot_id, "_", survey_year)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  mutate(comb = paste0(plot_id, "_", latitude_dec, "_", longitude_dec)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  mutate(plot_id_orig = plot_id) %>%
  mutate(code_plot_orig = as.numeric(gsub("^6_", "", plot_id_orig))) %>%
  arrange(code_plot_orig) %>%
  mutate(year_suffix = substr(survey_year, 3, 4)) %>%
  mutate(code_plot = str_pad(as.character(code_plot_orig),
                             width = 3, pad = "0")) %>%
  mutate(code_plot = paste0(year_suffix, code_plot)) %>%
  select(-year_suffix) %>%
  mutate(partner_code = 6) %>%
  mutate(plot_id = paste0(partner_code, "_", code_plot)) %>%
  mutate(survey_code = "si_so") %>%
  mutate(survey_year = paste0(survey_years, collapse = "_")) %>%
  select(partner_code, survey_code, survey_year, code_plot_orig,
         plot_id_orig, code_plot, plot_id, latitude_dec, longitude_dec)



survey_years <- c(2010)

uk_LII_harmonisation_table_10 <-
  rbind(select(so_pls[which(so_pls$plot_id %in% vec_moved_plots &
                              so_pls$survey_year %in% survey_years), ],
               plot_id, latitude_dec, longitude_dec, survey_year),
        select(so_prf[which(so_prf$plot_id %in% vec_moved_plots &
                              so_prf$survey_year %in% survey_years), ],
               plot_id, latitude_dec, longitude_dec, survey_year)) %>%
  mutate(comb = paste0(plot_id, "_",
                       latitude_dec, "_",
                       longitude_dec, "_",
                       survey_year)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  bind_rows(select(mutate(so_som[which(so_som$plot_id %in% vec_moved_plots &
                                         so_som$survey_year %in% survey_years), ],
                          latitude_dec = NA,
                          longitude_dec = NA),
                   plot_id, latitude_dec, longitude_dec, survey_year)) %>%
  mutate(comb = paste0(plot_id, "_", survey_year)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  mutate(comb = paste0(plot_id, "_", latitude_dec, "_", longitude_dec)) %>%
  distinct(comb, .keep_all = TRUE) %>%
  select(-comb) %>%
  mutate(plot_id_orig = plot_id) %>%
  mutate(code_plot_orig = as.numeric(gsub("^6_", "", plot_id_orig))) %>%
  arrange(code_plot_orig) %>%
  mutate(year_suffix = substr(survey_year, 3, 4)) %>%
  mutate(code_plot = str_pad(as.character(code_plot_orig),
                             width = 3, pad = "0")) %>%
  mutate(code_plot = paste0(year_suffix, code_plot)) %>%
  select(-year_suffix) %>%
  mutate(partner_code = 6) %>%
  mutate(plot_id = paste0(partner_code, "_", code_plot)) %>%
  mutate(survey_code = "si_so") %>%
  mutate(survey_year = paste0(survey_years, collapse = "_")) %>%
  select(partner_code, survey_code, survey_year, code_plot_orig,
         plot_id_orig, code_plot, plot_id, latitude_dec, longitude_dec)

# Three plots in si_plt are co-located with so plots from 1995/1996,
# but have different names, i.e. 5, 11, 12

vec_other_name <- links_uk_LII_10 %>%
  filter(!is.na(all_equal_survey) &
           is.na(all_equal)) %>%
  rename(plot_id = si_plt_6_NA) %>%
  select(plot_id) %>%
  as.vector() %>%
  unlist()

matching_plots <- so_prf %>%
  filter(survey_year %in% c(1995, 1996) &
           plot_id %in% c("6_512", "6_516", "6_717")) %>%
  mutate(year_suffix = substr(survey_year, 3, 4)) %>%
  mutate(code_plot = str_pad(as.character(code_plot),
                             width = 3, pad = "0")) %>%
  mutate(code_plot = paste0(year_suffix, code_plot)) %>%
  mutate(plot_id = paste0(partner_code, "_", code_plot)) %>%
  select(plot_id, survey_year, latitude_dec, longitude_dec) %>%
  as_sf

sf1 <- si_plt %>%
  filter(plot_id %in% vec_other_name) %>%
  select(plot_id, last_year, latitude_dec, longitude_dec) %>%
  rename(plot_id_orig = plot_id) %>%
  as_sf

uk_LII_harmonisation_table_si <-
  distance_join(sf1 = sf1,
                sf2 = matching_plots,
                join_column = "plot_id",
                summary_stat = "min_dist",
                dist_threshold = 10)

uk_LII_harmonisation_table_si_2 <-
  uk_LII_harmonisation_table_si %>%
  st_drop_geometry() %>%
  mutate(partner_code = 6,
         survey_code = "si",
         survey_year = NA,
         code_plot_orig = as.numeric(gsub("^6_", "", plot_id_orig)),
         code_plot = as.numeric(gsub("^6_", "", plot_id))) %>%
  select(partner_code, survey_code, survey_year, code_plot_orig,
         plot_id_orig, code_plot, plot_id, latitude_dec, longitude_dec)
  

uk_LII_harmonisation_table <-
  rbind(uk_LII_harmonisation_table_95,
        uk_LII_harmonisation_table_10,
        uk_LII_harmonisation_table_si_2) %>%
  mutate(latitude_dec = as.numeric(latitude_dec),
         longitude_dec = as.numeric(longitude_dec))

write.csv2(uk_LII_harmonisation_table,
           paste0("./data/additional_data/plot_coord_harmonisation_keys/",
                  "LII_6_plot_coord_harmonisation_key_UK.csv"),
           row.names = FALSE,
           na = "")




# Other countries ----

# Create function which identifies row indices with plot_ids belonging to
# the solid soil surveys (s1 or so), so not only to system installment surveys

which_code_survey <- function(df, code_survey) {
  df_sub <- select(df, starts_with(paste0(code_survey, "_")))
  row_indices <- which(apply(!is.na(df_sub), 1, any))
  return(row_indices)
}

### Create plot coordinate co-location table ----
### and export as csv if there are any inconsistencies

# Level I

for (i in seq_along(unique(s1_som$partner_code))) {
  partner_code_i <- unique(s1_som$partner_code)[i]
  
  if (!partner_code_i %in% c(6, 53)) {
  partner_i <- d_partner$desc_short[which(d_partner$code == partner_code_i)]
  print(partner_i)

  df_table <-
    get_plot_coord_colocation_table(code_survey = "s1",
                                    partner_code = partner_code_i,
                                    dist_threshold = 10)

  df_table_sub <-
    df_table[which_code_survey(df_table, code_survey = "s1"), ]
  
  if (any(is.na(df_table_sub$all_equal_survey)) ||
      any(df_table_sub$plot_id_survey_duplicated == TRUE)) {
    
    # Save the table for the given partner
    
    write.csv2(df_table,
               paste0("./output/spatial_plot_id_links/",
                      "links_plot_id_location_",
                      partner_i,
                      "_LI_10.csv"),
               row.names = FALSE,
               na = "")
  }
}
}

# Level II

for (i in seq_along(unique(so_som$partner_code))) {
  partner_code_i <- unique(so_som$partner_code)[i]
  
  if (!partner_code_i %in% c(6, 53)) {
    partner_i <- d_partner$desc_short[which(d_partner$code == partner_code_i)]
    print(partner_i)
    
    df_table <-
      get_plot_coord_colocation_table(code_survey = "so",
                                      partner_code = partner_code_i,
                                      dist_threshold = 10)
    
    df_table_sub <-
      df_table[which_code_survey(df_table, code_survey = "so"), ]
    
    if (any(is.na(df_table_sub$all_equal_survey)) ||
        any(df_table_sub$plot_id_survey_duplicated == TRUE)) {
      
      # Save the table for the given partner
      
      write.csv2(df_table,
                 paste0("./output/spatial_plot_id_links/",
                        "links_plot_id_location_",
                        partner_i,
                        "_LII_10.csv"),
                 row.names = FALSE,
                 na = "")
    }
  }
}


get_plot_location_table <- function(code_survey,
                                    partner_code) {
  level <- ifelse(code_survey == "s1", "LI", "LII")
  partner_short <- d_partner$desc_short[which(d_partner$code == partner_code)]
  
  list_tables <- list.files("./output/spatial_plot_id_links/")
  list_tables <-
    list_tables[which(!list_tables %in%
                        c("links_plot_id_location_poland_s1_so_1995.csv"))]
  

  # Function to check if a string contains both terms (ignoring case)
  contains_terms <- function(string) {
    # Check for partner_short and either level or code_survey
    if (grepl(paste0(partner_short, "_"), string, ignore.case = TRUE)) {
      if (level == "LI") {
        # Check for "LI" and exclude strings containing "LII"
        grepl("LI", string, ignore.case = TRUE) &&
          !grepl("LII", string, ignore.case = TRUE)
      } else {
        # Check for level or code_survey
        grepl(level, string, ignore.case = TRUE) ||
          grepl(code_survey, string, ignore.case = TRUE)
      }
    } else {
      FALSE
    }
  }

  # Use the function to find matching elements
  file_name <- list_tables[sapply(list_tables, contains_terms)]

  if (!identical(file_name, character(0))) {

  if (length(file_name) > 1) {

  # Select the file that ends with "10" (not followed by any digits)
    file_name <- file_name[grep("10(?![0-9])", file_name, perl = TRUE)]

    assertthat::assert_that(length(file_name) == 1,
                            msg = paste0("More than one file for ",
                                         partner_short,
                                         " ",
                                         level, "."))
  }

  df_table <- 
    read.csv2(paste0("./output/spatial_plot_id_links/",
                     file_name),
            na.strings = "")

  return(df_table)
  
  } else {
    return(NA)
  }
}

### Add a column with the moving distance for exported csv's ----

for (i in seq_along(unique(s1_som$partner_code))) {

  partner_code_i <- unique(s1_som$partner_code)[i]
    partner_i <- d_partner$desc_short[which(d_partner$code == partner_code_i)]
    print(partner_i)
    
   df_table <- get_plot_location_table(partner_code = partner_code_i,
                                       code_survey = "s1")
   
   if (!is.data.frame(df_table)) {
     cat(paste0("No plot location table found for ",
                partner_i, " in '", code_survey,
                "'.\n"))
   } else {
     
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
     
     # Save the table for the given partner
     
     write.csv2(df_table,
                paste0("./output/spatial_plot_id_links/",
                       "links_plot_id_location_",
                       partner_i,
                       "_LI_10.csv"),
                row.names = FALSE,
                na = "")
     
     
   }
}


### Create overview tables with moving distances ----

df_move_distances_s1 <- data.frame(plot_id = NULL,
                                   dist_plot_move_meter = NULL)

for (i in seq_along(unique(s1_som$partner_code))) {
  
  partner_code_i <- unique(s1_som$partner_code)[i]
  partner_i <- d_partner$desc_short[which(d_partner$code == partner_code_i)]
  print(partner_i)
  
  df_table <- get_plot_location_table(partner_code = partner_code_i,
                                      code_survey = "s1")
  
  if (!is.data.frame(df_table)) {
    cat(paste0("No plot location table found for ",
               partner_i, " in '", code_survey,
               "'.\n"))
  } else {
    
    if (any(!is.na(df_table$dist_plot_move_meter))) {

      df_move_distances_s1 <-
        rbind(df_move_distances_s1,
              data.frame(plot_id =
                           df_table$all_equal_survey[
                             which(!is.na(df_table$dist_plot_move_meter))],
                         dist_plot_move_meter =
                           as.vector(na.omit(df_table$dist_plot_move_meter))))
    }
  }
}

write.csv2(df_move_distances_s1,
           paste0("./output/spatial_plot_id_links/",
                  "move_distances_s1.csv"),
           row.names = FALSE,
           na = "")

df_move_distances_so <- data.frame(plot_id = NULL,
                                   dist_plot_move_meter = NULL)

for (i in seq_along(unique(so_som$partner_code))) {
  
  partner_code_i <- unique(so_som$partner_code)[i]
  partner_i <- d_partner$desc_short[which(d_partner$code == partner_code_i)]
  print(partner_i)
  
  df_table <- get_plot_location_table(partner_code = partner_code_i,
                                      code_survey = "so")
  
  if (!is.data.frame(df_table)) {
    cat(paste0("No plot location table found for ",
               partner_i, " in '", code_survey,
               "'.\n"))
  } else {
    
    if (any(!is.na(df_table$dist_plot_move_meter))) {
      
      df_move_distances_so <-
        rbind(df_move_distances_so,
              data.frame(plot_id =
                           df_table$all_equal_survey[
                             which(!is.na(df_table$dist_plot_move_meter))],
                         dist_plot_move_meter =
                           as.vector(na.omit(df_table$dist_plot_move_meter))))
    }
  }
}

write.csv2(df_move_distances_so,
           paste0("./output/spatial_plot_id_links/",
                  "move_distances_so.csv"),
           row.names = FALSE,
           na = "")

summary(df_move_distances_s1$dist_plot_move_meter)
hist(filter(df_move_distances_so, dist_plot_move_meter < 2000)[, 2])

move_distances_s1 <-
  read.csv2(paste0("./output/spatial_plot_id_links/move_distances_s1.csv"))
move_distances_so <-
  read.csv2(paste0("./output/spatial_plot_id_links/move_distances_so.csv"))
dist_plot_move <-
  bind_rows(move_distances_s1, move_distances_so) %>%
  pull(dist_plot_move_meter)

quantile(dist_plot_move, 0.8)
hist(dist_plot_move[which(dist_plot_move <= 2000)],
     breaks = 20,
     xlab = "Distance plot move in 's1' and 'so' (in meter)",
     ylab = "Frequency",
     main = "Histogram of move distances\n326 obs between 2000 and 678382")







partner_code_i <- 53
partner_i <- d_partner$desc_short[which(d_partner$code == partner_code_i)]

df_table <- get_plot_location_table(partner_code = partner_code_i,
                                    code_survey = "so")

if (!is.data.frame(df_table)) {
  cat(paste0("No plot location table found for ",
             partner_i, " in '", code_survey,
             "'.\n"))
} else {
  
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
  
  # Save the table for the given partner
  
  write.csv2(df_table,
             paste0("./output/spatial_plot_id_links/",
                    "links_plot_id_location_",
                    partner_i,
                    "_LII_10.csv"),
             row.names = FALSE,
             na = "")
}


# Level II Sweden and Swethro ----

vec_sweden_sfa <-
  c(6106, 6506, 1114, 2270, 5203, 5303, 5503, 5506, 5605, 5705,
    5803, 5906, 6009, 6011, 6108, 6110, 6111, 6204, 6402, 6504,
    6508, 6602, 6603, 6704, 6706, 6707, 6902, 7003, 7004, 7005,
    7103, 7202, 7303, 7401, 7403, 7504, 6308, 6309) %>%
  as.character

vec_sweden_swethro <-
  c(1006, 1301, 1403, 5201, 5202, 5301, 5401, 5402, 5403, 5404,
    5501, 5502, 5601, 5602, 5603, 5604, 5701, 5702, 5703, 5704,
    5801, 5802, 5804, 6001, 6002, 6003, 6005, 6101, 6102, 6103,
    6109, 6201, 6203, 6301, 6302, 6303, 6305, 6307, 6401, 6501,
    6503, 6507, 6601, 6701, 6702, 6703, 6801, 6802, 6803, 6804,
    6901, 7001, 7002, 7101, 7106, 7201, 7301, 7302, 7402, 7404,
    7501, 7502) %>%
  as.character

table_sweden <-
  get_plot_coord_colocation_table(code_survey = "so",
                                  partner_code = 13,
                                  dist_threshold = 10)

table_swethro <-
  get_plot_coord_colocation_table(code_survey = "so",
                                  partner_code = 1301,
                                  dist_threshold = 10)

table_sweden_combined <-
  bind_rows(table_sweden %>%
              filter(!is.na(all_equal_survey)) %>%
              rename(latitude_dec = latitude_most_abundant) %>%
              rename(longitude_dec = longitude_most_abundant) %>%
              rename(plot_id = all_equal_survey) %>%
              select(plot_id, latitude_dec, longitude_dec),
            table_swethro %>%
              filter(!is.na(all_equal_survey)) %>%
              rename(latitude_dec = latitude_most_abundant) %>%
              rename(longitude_dec = longitude_most_abundant) %>%
              rename(plot_id = all_equal_survey) %>%
              select(plot_id, latitude_dec, longitude_dec)) %>%
  mutate(code_plot = sapply(strsplit(plot_id, "_"), function(x) x[2])) %>%
  mutate(source = ifelse(.data$code_plot %in% vec_sweden_sfa,
                         "sfa_sweden",
                         ifelse(.data$code_plot %in% vec_sweden_swethro,
                                "sfa_swethro",
                                "slu_sweden"))) %>%
  as_sf

mapView(table_sweden_combined,
        layer.name = "Swedish and SWETHRO Level II plots",
        zcol = "source",
        map.types = "OpenTopoMap",
        popup = TRUE)


mapshot(mapView(table_sweden_combined,
                layer.name = "Swedish and SWETHRO Level II plots",
                zcol = "source",
                map.types = "OpenTopoMap",
                popup = TRUE),
        url = paste0("./output/spatial_plot_id_links/",
                     "plots_sweden_swethro_LII.html"))

  # Suggestion: change partner_code 1301 (SWETHRO) to 13 (Sweden) for layer 1
  # As there doesn't seem to be any reason to keep them separated.



# Level I Germany ----

y1_pl1 %>%
  filter(code_country == 4) %>%
  distinct(unique_survey, .keep_all = TRUE) %>%
  mutate(partner_year = paste0(partner_code, "_", survey_year)) %>%
  group_by(partner_year) %>%
  summarise(count = n()) %>%
  View
  # So the LII partner codes stem from the BioSoil survey

s1_som %>%
  filter(code_country == 4) %>%
  distinct(plot_id, .keep_all = TRUE) %>%
  group_by(code_plot) %>%
  summarise(count = n()) %>%
  arrange(code_plot) %>%
  filter(count > 1) %>%
  View
  # All code_plots are unique within s1

data_availability_s1 %>%
  filter(code_country == 4) %>%
  distinct(plot_id, .keep_all = TRUE) %>%
  group_by(code_plot) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(code_plot) %>%
  filter(count > 1) %>%
  View
  # All code_plots are unique within s1

y1_pl1 %>%
  filter(code_country == 4) %>%
  distinct(plot_id, .keep_all = TRUE) %>%
  group_by(code_plot) %>%
  summarise(count = n()) %>%
  arrange(code_plot) %>%
  filter(count > 1) %>%
  View

vec_partner_codes_german_LI <-
  bind_rows(
    data_availability_s1 %>%
      filter(code_country == 4) %>%
      distinct(partner_code),
    data_availability_so %>%
      filter(code_country == 4) %>%
      distinct(partner_code),
    d_partner %>%
      filter(country_code == 4) %>%
      distinct(code) %>%
      rename(partner_code = code)) %>%
  distinct(partner_code) %>%
  filter(partner_code != 98) %>%
  pull(partner_code)


# Spain ----
# Potential coordinate issue Pyrennees?

table_spain_LI <-
  read.csv2(paste0("./output/spatial_plot_id_links/",
                   "links_plot_id_location_Spain_LI_10.csv"),
            na.strings = "")

table_spain_LII <-
  get_plot_coord_colocation_table(code_survey = "so",
                                  partner_code = 11,
                                  dist_threshold = 10)


table_spain <- bind_rows(table_spain_LI %>%
                filter(!is.na(all_equal_survey)) %>%
                filter(plot_id_survey_duplicated == FALSE) %>%
                rename(latitude_dec = latitude_most_abundant) %>%
                rename(longitude_dec = longitude_most_abundant) %>%
                rename(plot_id = all_equal_survey) %>%
                mutate(level = "Level I") %>%
                select(plot_id, latitude_dec, longitude_dec, level),
            table_spain_LII %>%
                filter(!is.na(all_equal_survey)) %>%
                filter(plot_id_survey_duplicated == FALSE) %>%
                rename(latitude_dec = latitude_most_abundant) %>%
                rename(longitude_dec = longitude_most_abundant) %>%
                rename(plot_id = all_equal_survey) %>%
                mutate(level = "Level II") %>%
                select(plot_id, latitude_dec, longitude_dec, level)) %>%
  as_sf

mapView(table_spain,
        layer.name = "Spanish plots",
        zcol = "level",
        map.types = "OpenTopoMap",
        popup = TRUE)

mapshot(mapView(table_spain,
          layer.name = "Spanish plots",
          zcol = "level",
          map.types = "OpenTopoMap",
          popup = TRUE),
        url = paste0("./output/spatial_plot_id_links/",
                     "plots_spain.html"))

  # Everything seems to be within the Spanish borders,
  # so probably no coordinates were moved

  # However, issues with coordinates are not related to moving of the plots
  # but to forgetting minus signs


# Russia ----
# Potential "so" plots that belong in "s1"?


table_russia_LI <-
  get_plot_coord_colocation_table(code_survey = "s1",
                                  partner_code = 62,
                                  dist_threshold = 10)

  # No Russian LII

table_russia <- table_russia_LI %>%
  filter(!is.na(all_equal_survey)) %>%
  rename(latitude_dec = latitude_most_abundant) %>%
  rename(longitude_dec = longitude_most_abundant) %>%
  rename(plot_id = all_equal_survey) %>%
  mutate(survey_form = ifelse(plot_id_survey_duplicated == FALSE,
                              "s1_prf",
                              "s1_pls")) %>%
  select(plot_id, latitude_dec, longitude_dec, survey_form) %>%
  as_sf

mapshot(mapView(table_russia,
        layer.name = "Russian (LI) plots",
        zcol = "survey_form",
        map.types = "OpenTopoMap",
        popup = TRUE),
        url = paste0("./output/spatial_plot_id_links/",
                     "plots_russia.html"))

  # No idea about any LII plots that would belong to LI
  # But there is a difference between coordinates in s1_prf and s1_pls:
  # coordinates of s1_pls (and y1) seem more systematically located on the grid
  # whereas coordinates in s1_prf seems more random.

# Lithuania ----


table_lithuania_LI <-
  get_plot_coord_colocation_table(code_survey = "s1",
                                  partner_code = 56,
                                  dist_threshold = 10)

table_lithuania_LII <-
  get_plot_coord_colocation_table(code_survey = "so",
                                  partner_code = 56,
                                  dist_threshold = 10)

# No Lithuanian LII

table_lithuania <- table_lithuania_LI %>%
  filter(!is.na(all_equal_survey)) %>%
  rename(latitude_dec = latitude_most_abundant) %>%
  rename(longitude_dec = longitude_most_abundant) %>%
  rename(plot_id = all_equal_survey) %>%
  mutate(survey_year = ifelse(plot_id_survey_duplicated == FALSE,
                              "1992",
                              "2000+")) %>%
  select(plot_id, latitude_dec, longitude_dec, survey_year) %>%
  as_sf

mapshot(mapView(table_lithuania,
                layer.name = "Lithuanian (LI) plots",
                zcol = "survey_year",
                map.types = "OpenTopoMap",
                popup = TRUE),
        url = paste0("./output/spatial_plot_id_links/",
                     "plots_lithuania.html"))

  # All plots have been moved for about 4 km into the southwestern direction
  # Linked to inaccuracies coordinate determinations during the '90s?



# Latvia ----


table_latvia_LI <-
  get_plot_coord_colocation_table(code_survey = "s1",
                                  partner_code = 64,
                                  dist_threshold = 10)

table_latvia_LII <-
  get_plot_coord_colocation_table(code_survey = "so",
                                  partner_code = 64,
                                  dist_threshold = 10)

  # Just one Latvian LII plot

table_latvia <- table_latvia_LI %>%
  filter(!is.na(all_equal_survey)) %>%
  rename(latitude_dec = latitude_most_abundant) %>%
  rename(longitude_dec = longitude_most_abundant) %>%
  rename(plot_id = all_equal_survey) %>%
  mutate(survey_year = ifelse(plot_id_survey_duplicated == FALSE,
                              "1991",
                              "2000+")) %>%
  select(plot_id, latitude_dec, longitude_dec, survey_year) %>%
  as_sf

mapshot(mapView(table_latvia,
                layer.name = "Latvian LI plots",
                zcol = "survey_year",
                map.types = "OpenTopoMap",
                popup = TRUE),
        url = paste0("./output/spatial_plot_id_links/",
                     "plots_latvia.html"))

  # Most plots have hardly moved, but some moved quite far


# Italy ----


table_italy_LI <-
  get_plot_coord_colocation_table(code_survey = "s1",
                                  partner_code = 5,
                                  dist_threshold = 10)

table_italy_LII <-
  get_plot_coord_colocation_table(code_survey = "so",
                                  partner_code = 5,
                                  dist_threshold = 10)

# Italian LII plots hardly moved (avg: 385 meter)

table_italy <- table_italy_LI %>%
  filter(!is.na(all_equal_survey)) %>%
  rename(latitude_dec = latitude_most_abundant) %>%
  rename(longitude_dec = longitude_most_abundant) %>%
  rename(plot_id = all_equal_survey) %>%
  mutate(survey = ifelse(plot_id_survey_duplicated == FALSE,
                              "s1_prf | 2006",
                              "s1_pls & 2007")) %>%
  select(plot_id, latitude_dec, longitude_dec, survey) %>%
  as_sf

mapshot(mapView(table_italy,
                layer.name = "Italian LI plots",
                zcol = "survey",
                map.types = "OpenTopoMap",
                popup = TRUE),
        url = paste0("./output/spatial_plot_id_links/",
                     "plots_italy.html"))

table_italy_far <- table_italy_LI %>%
  filter(!is.na(all_equal_survey)) %>%
  rename(latitude_dec = latitude_most_abundant) %>%
  rename(longitude_dec = longitude_most_abundant) %>%
  rename(plot_id = all_equal_survey) %>%
  filter(plot_id %in%
           table_italy_LI$all_equal_survey[
             which(table_italy_LI$dist_plot_move_meter > 1000)]) %>%
  mutate(survey = ifelse(plot_id_survey_duplicated == FALSE,
                         "s1_prf | 2006",
                         "s1_pls & 2007")) %>%
  select(plot_id, latitude_dec, longitude_dec, survey) %>%
  as_sf

mapshot(mapView(table_italy_far,
                layer.name = "Italian LI plots which moved more than 1000 meter",
                zcol = "survey",
                map.types = "OpenTopoMap",
                popup = TRUE),
        url = paste0("./output/spatial_plot_id_links/",
                     "plots_italy_far.html"))

  # Most plots have hardly moved, but some moved quite far


# France LI ----

# Issues with coordinates that moved far are not related to moving of the plots
# but to forgetting minus signs


# Estonia LI ----

table_estonia_LI <-
  read.csv2(paste0("./output/spatial_plot_id_links/",
                   "links_plot_id_location_Estonia_LI_10.csv"),
            na.strings = "")

table_estonia <- table_estonia_LI %>%
  filter(!is.na(all_equal_survey)) %>%
  rename(latitude_dec = latitude_most_abundant) %>%
  rename(longitude_dec = longitude_most_abundant) %>%
  rename(plot_id = all_equal_survey) %>%
  mutate(survey = ifelse(plot_id_survey_duplicated == FALSE,
                              "s1_prf & 1990s",
                              "s1_pls | 2000+")) %>%
  select(plot_id, latitude_dec, longitude_dec, survey) %>%
  as_sf

mapshot(mapView(table_estonia,
                layer.name = "Estonian LI plots",
                zcol = "survey",
                map.types = "OpenTopoMap",
                popup = TRUE),
        url = paste0("./output/spatial_plot_id_links/",
                     "plots_estonia.html"))

  # There is a difference between coordinates in s1_prf from the 90s
  # and the other coordinates from s1:
  # coordinates of s1_prf in the 90s seem more systematically located on the
  # grid whereas other coordinates seem more random.
  # The latter are probably the correct ones


# Croatia LI ----

table_croatia_LI <-
  read.csv2(paste0("./output/spatial_plot_id_links/",
                   "links_plot_id_location_Croatia_LI_10.csv"),
            na.strings = "")

table_croatia <- table_croatia_LI %>%
  filter(!is.na(all_equal_survey)) %>%
  rename(latitude_dec = latitude_most_abundant) %>%
  rename(longitude_dec = longitude_most_abundant) %>%
  rename(plot_id = all_equal_survey) %>%
  mutate(survey = ifelse(plot_id_survey_duplicated == FALSE,
                         "1990s",
                         "2017")) %>%
  select(plot_id, latitude_dec, longitude_dec, survey) %>%
  as_sf

mapshot(mapView(table_croatia,
                layer.name = "Croatian LI plots",
                zcol = "survey",
                map.types = "OpenTopoMap",
                popup = TRUE),
        url = paste0("./output/spatial_plot_id_links/",
                     "plots_croatia.html"))

  # There is a difference between coordinates from the 90s
  # and the coordinates from 2017:
  # coordinates of the 90s seem more systematically located on the
  # grid whereas other coordinates seem more random.
  # The latter are probably the correct ones


