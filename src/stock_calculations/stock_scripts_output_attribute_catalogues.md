### Attribute catalogue of “so.som.CsPROFmps.csv”

-   partner_short: short name of data-submitting partner
-   PLOTID: combination of “partner_code” and “code_plot”
-   PROFID: combination of “survey_year”, “partner_code”, “code_plot”
    and “repetition”
-   survey_year: year of the data collection
-   partner_code: unique ID of data-submitting partner
-   country_code: unique ID of country
-   code_plot: observation plot number
-   repetition: number of replicate sample within layer (equivalent to a
    replicate profile within a plot in stock calculations)
-   OBSDEPTH: maximum depth of profile for which data are available,
    i.e. maximum inferior depth layer (“BOT”) within profile for which
    the carbon density (“CD”) is not an NA
-   SOILDEPTH: depth (in cm) until which the mass-preserving splines are
    fitted (to carbon density “CD”), i.e. 100 cm (constant)
-   nlay: number of layers in the profile - in this case: number of
    mineral layers only (disregarding any forest floor layers)
-   PCsmps10 to PCsmps100: integrated (i.e. summed up) carbon densities
    (carbon densities as returned from mass-preserving splines, for a
    depth of i cm, in ton C ha-1 cm-1) from 0 to 10 cm; 0 to 20 cm; …; 0
    to 100 cm depth; respectively
-   SOCs: sum of all carbon densities returned from mass-preserving
    splines (same as PCsmps100 since mass-preserving splines are fitted
    until a depth of 100 cm)

### Attribute catalogue of “SOILLAYCs.min.csv”

(attributes different than those above)

-   plot_ID: combination of “partner_code” and “code_plot”
-   code_layer: name of fixed-depth layer
-   layer_type: type of matrix (filtered for “mineral” in this file;
    alternatives are “forest_floor” and “peat”)
-   bulk_density: mean dry bulk density of the fine earth (in kg m-3)
-   organic_layer_weight: total dry weight of the organic layer (in kg
    m-2)
-   coarse_fragment_vol: volumetric fraction of coarse fragments (in
    %)
-   profile_ID: combination of “survey_year”, “partner_code”,
    “code_plot” and “repetition”
-   TOP: superior layer limit (in cm)
-   BOT: inferior layer limit (in cm)
-   AVD: average depth (in cm), calculated as the average between the
    superior and inferior layer limits
-   layer_number: sequential index assigned to soil layer within
    profile (the upper forest floor layer is designated as layer number
    1, and each successive layer below it is assigned an incrementally
    higher number, such as 2, 3, 4 etc)
-   organic_carbon_total: total organic carbon content (in g kg-1)
-   LAYTHICK: layer thickness (in cm), calculated as the absolute
    difference between the superior and inferior layer limits
-   VPCF: volume proportion of coarse fractions (relative, i.e. not in
    %), calculated as “coarse_fragment_vol” divided by 100
-   CD: mean carbon density per layer (ton C ha-1 cm-1), calculated as
    (“organic_carbon_total” * “bulk_density” * (1 - “VPCF”)) /
    10000
-   LAYSOCs: SOC stock for each layer (ton C ha-1), calculated as (“CD”
    * “LAYTHICK”)
-   av.THICK: availability of “LAYTHICK” for given layer, i.e. 0 if
    “LAYTHICK” is NA, else 1
-   av.TOC: availability of “organic_carbon_total” for given layer,
    i.e. 0 if “organic_carbon_total” is NA, else 1
-   av.BD: availability of “bulk_density” for given layer, i.e. 0 if
    “bulk_density” is NA, else 1
-   av.CF: availability of “coarse_fragment_vol” for given layer,
    i.e. 0 if “coarse_fragment_vol” is NA, else 1
