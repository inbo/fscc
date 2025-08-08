

get_par_table <- function() {

  cation_data <- data.frame(
    par = c("free_h", "exch_k", "exch_ca",
            "exch_mg", "exch_na", "exch_al",
            "exch_fe", "exch_mn"),
    charge = c(1, 1, 2,
               2, 1, 3,
               2, 2),
    molar_mass = c(1.008, 39.098, 40.078,
                   24.305, 22.990, 26.982,
                   55.845, 54.938))

  parameter_table <- data.frame(
    som_parameter = c(
      "extrac_pb", "extrac_zn", "part_size_clay", "part_size_silt", "exch_mn",
      "carbonates", "extrac_al", "exch_mg", "exch_fe", "extrac_na",
      "exch_ca", "ph_cacl2", "n_total", "extrac_k", "exch_al",
      "tot_mg", "exch_k", "extrac_s", "organic_carbon_total", "exch_na",
      "extrac_cr", "extrac_fe", "tot_k", "ph_h2o", "part_size_sand",
      "extrac_cu", "free_h", "extrac_cd", "extrac_hg", "extrac_ca",
      "extrac_mg", "extrac_ni", "extrac_p", "extrac_al", "extrac_mn",
      "extrac_fe", "tot_ca", "p_ox", "exch_acidiy", "tot_na",
      "tot_mn", "tot_fe", "tot_al",
      "moisture_content", "bulk_density", "rea_al", "rea_fe",
      "organic_layer_weight"),
    pfh_parameter = c(
      NA, NA, "horizon_clay", "horizon_silt", NA,
      "horizon_caco3_total", NA, "horizon_exch_mg", NA, NA,
      "horizon_exch_ca", NA, "horizon_n_total", NA, NA,
      NA, "horizon_exch_k", NA, "horizon_c_organic_total", "horizon_exch_na",
      NA, NA, NA, "horizon_ph", "horizon_sand",
      NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA,
      NA, NA, NA,
      NA, "bulk_density", NA, NA,
      "organic_layer_weight"),
    unit = c(
      "mg kg-1", "mg kg-1", "%wt", "%wt", "cmol+ kg-1",
      "g kg-1", "mg kg-1", "cmol+ kg-1", "cmol+ kg-1", "mg kg-1",
      "cmol+ kg-1", "-", "g kg-1", "mg kg-1", "cmol+ kg-1",
      "mg kg-1", "cmol+ kg-1", "mg kg-1", "g kg-1", "cmol+ kg-1",
      "mg kg-1", "mg kg-1", "mg kg-1", "-", "%wt",
      "mg kg-1", "cmol+ kg-1", "mg kg-1", "mg kg-1", "mg kg-1",
      "mg kg-1", "mg kg-1", "mg kg-1", "mg kg-1", "mg kg-1",
      "mg kg-1", "mg kg-1", "-", "cmol+ kg-1", "mg kg-1",
      "mg kg-1", "mg kg-1", "mg kg-1",
      "%wt", "kg m-3", "mg kg-1", "mg kg-1",
      "kg m-2"),
    # Only for variables for which stocks can be calculated
    shorter_name = c(
      "extrac_pb", "extrac_zn", NA, NA, "exch_mn",
      "caco3", "extrac_al", "exch_mg", "exch_fe", "extrac_na",
      "exch_ca", NA, "n", "extrac_k", "exch_al",
      "tot_mg", "exch_k", "extrac_s", "oc", "exch_na",
      "extrac_cr", "extrac_fe", "tot_k", NA, NA,
      "extrac_cu", "free_h", "extrac_cd", "extrac_hg", "extrac_ca",
      "extrac_mg", "extrac_ni", "extrac_p", "extrac_al", "extrac_mn",
      "extrac_fe", "tot_ca", NA, NA, "tot_na",
      "tot_mn", "tot_fe", "tot_al",
      NA, NA, "rea_al", "rea_fe",
      NA
    ),
    # Shorter name capital (for in units)
    # Only for variables for which stocks can be calculated
    shorter_name_cap = c(
      "Pb", "Zn", NA, NA, "Mn",
      "CaCO3", "Al", "Mg", "Fe", "Na",
      "Ca", NA, "N", "K", "Al",
      "Mg", "K", "S", "C", "Na",
      "Cr", "Fe", "K", NA, NA,
      "Cu", "H", "Cd", "Hg", "Ca",
      "Mg", "Ni", "P", "Al", "Mn",
      "Fe", "Ca", NA, NA, "Na",
      "Mn", "Fe", "Al",
      NA, NA, "Al", "Fe",
      NA
    ),
    full_name = c(
      "Extractable lead", "Extractable zinc", NA, NA, "Exchangeable manganese",
      "Carbonates", "Extractable aluminium", "Exchangeable magnesium",
      "Exchangeable iron",  "Extractable sodium",
      "Exchangeable calcium", NA, "Nitrogen", "Extractable potassium",
      "Exchangeable aluminium",
      "Total magnesium", "Exchangeable potassium", "Extractable sulfur",
      "Organic carbon", "Exchangeable sodium",
      "Extractable chromium", "Extractable iron", "Total potassium", NA, NA,
      "Extractable cupper", "Free H+ acidity",
      "Exctractable cadmium", "Extractable mercury", "Extractable calcium",
      "Extractable magnesium", "Extractable nickel", "Extractable phosphorus",
      "Extractable aluminium", "Extractable manganese",
      "Extractable iron", "Total calcium", NA, NA, "Total sodium",
      "Total manganese", "Total iron", "Total aluminium",
      NA, NA, "Reactive aluminium", "Reactive iron",
      NA
    )) %>%
    mutate(
      unit_density_per_cm = case_when(
        # Only calculate for rows for which stocks can be calculated (i.e.
        # where shorter_var_name is not NA)
        !is.na(shorter_name) & unit == "mg kg-1" ~ "kg ha-1 cm-1",
        # Important: this needs to be converted!!!!! from "1E4 mol+ ha-1 cm-1"
        !is.na(shorter_name) & unit == "cmol+ kg-1" ~ "kg ha-1 cm-1",
        !is.na(shorter_name) & unit == "g kg-1" ~ "t ha-1 cm-1",
        TRUE ~ NA_character_
      )) %>%
    left_join(
      cation_data,
      by = join_by("som_parameter" == "par")) %>%
    mutate(
      density_convert_factor = ifelse(
        unit == "cmol+ kg-1",
        # Equivalent in mg kg-1 for 1 cmol+ kg-1:
        1 / # 1 cmol+ kg-1
          charge * # mol(+) mol-1 cations
          molar_mass * # g mol-1 cations
          1E1, # mg cg-1
        1)) %>%
    select(-charge, -molar_mass) %>%
    mutate(
      unit_markdown = gsub("1E4", "10<sup>4</sup>",
                           gsub("-1", "<sup>-1</sup>",
                                gsub("-2", "<sup>-2</sup>",
                                     gsub("-3", "<sup>-3</sup>", unit)))),
      unit_density_per_cm_markdown =
        gsub("1E4", "10<sup>4</sup>",
             gsub("-1", "<sup>-1</sup>",
                  gsub("-2", "<sup>-2</sup>",
                       gsub("-3", "<sup>-3</sup>", unit_density_per_cm)))),
      unit_stock_markdown =
        case_when(
          # Only calculate for rows for which stocks can be calculated (i.e.
          # where shorter_var_name is not NA)
          !is.na(shorter_name) & unit == "mg kg-1" ~
            paste0("kg ", shorter_name_cap, " ha<sup>-1</sup>"),
            #"kg ha-1",
          # Important: this needs to be converted!!!!!
          !is.na(shorter_name) & unit == "cmol+ kg-1" ~
            paste0("kg ", shorter_name_cap, " ha<sup>-1</sup>"),
           # "kg ha-1",
          !is.na(shorter_name) & unit == "g kg-1" ~
            paste0("t ", shorter_name_cap, " ha<sup>-1</sup>"),
           # "t ha-1",
          TRUE ~ NA_character_
        ))


  return(parameter_table)

}

