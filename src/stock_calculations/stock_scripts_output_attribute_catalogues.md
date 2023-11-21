
### Attribute catalogue of carbon stock .csv files

-   **avail_bd** - availability index of *bulk_density* for the given layer,
    i.e. 0 if no data were reported (NA), else 1
-   **avail_cf** - availability index of *coarse_fragment_vol_frac* for the
    given layer (before gap-filling NAs with 0), i.e. 0 if no data were
    reported (NA), else 1
-   **avail_org_layer_weight** - availability index of *organic_layer_weight* 
    for the given layer, i.e. 0 if no data were reported (NA), else 1
-   **avail_thick** - availability index of *layer_thickness* for the given
    layer (which indicates that layer limits are known), i.e. 0 if no data
    were reported (NA), else 1
-   **avail_toc** - availability index of *organic_carbon_total* for the
    given layer, i.e. 0 if no data were reported (NA), else 1
-   **bulk_density** - dry bulk density of the fine earth (in kg m-3)
-   **c_density** - mean carbon density for a given layer (ton C ha-1 cm-1),
    calculated as:
    (*organic_carbon_total* * *bulk_density* *
    (1 - *coarse_fragment_vol_frac*)) / 10000
-   **c_stock** - (soil) organic carbon stock of the complete forest soil
    (forest floor and below-ground; until *soil_depth*),
    obtained by summing up any *c_stock_below_ground* and
    *c_stock_forest_floor* values - either for a specific profile or the
    average across profiles within a plot survey
-   **c_stock_10** - below-ground soil organic carbon stock until 10 cm,
    obtained through depth integration (i.e. summing up) of carbon densities
    (as returned from mass-preserving splines for a depth of i cm
    in ton C ha-1 cm-1) from 0 to 10 cm, if the latter is above or equal to the
    *soil_depth* (else NA)
-   **c_stock_20** - below-ground soil organic carbon stock until 20 cm,
    obtained through depth integration (i.e. summing up) of carbon densities
    (as returned from mass-preserving splines for a depth of i cm
    in ton C ha-1 cm-1) from 0 to 20 cm, if the latter is above or equal to the
    *soil_depth* (else NA)
-   **c_stock_30** - below-ground soil organic carbon stock until 30 cm,
    obtained through depth integration (i.e. summing up) of carbon densities
    (as returned from mass-preserving splines for a depth of i cm
    in ton C ha-1 cm-1) from 0 to 30 cm, if the latter is above or equal to the
    *soil_depth* (else NA)
-   **c_stock_40** - below-ground soil organic carbon stock until 40 cm,
    obtained through depth integration (i.e. summing up) of carbon densities
    (as returned from mass-preserving splines for a depth of i cm
    in ton C ha-1 cm-1) from 0 to 40 cm, if the latter is above or equal to the
    *soil_depth* (else NA)
-   **c_stock_50** - below-ground soil organic carbon stock until 50 cm,
    obtained through depth integration (i.e. summing up) of carbon densities
    (as returned from mass-preserving splines for a depth of i cm
    in ton C ha-1 cm-1) from 0 to 50 cm, if the latter is above or equal to the
    *soil_depth* (else NA)
-   **c_stock_60** - below-ground soil organic carbon stock until 60 cm,
    obtained through depth integration (i.e. summing up) of carbon densities
    (as returned from mass-preserving splines for a depth of i cm
    in ton C ha-1 cm-1) from 0 to 60 cm, if the latter is above or equal to the
    *soil_depth* (else NA)
-   **c_stock_70** - below-ground soil organic carbon stock until 70 cm,
    obtained through depth integration (i.e. summing up) of carbon densities
    (as returned from mass-preserving splines for a depth of i cm
    in ton C ha-1 cm-1) from 0 to 70 cm, if the latter is above or equal to the
    *soil_depth* (else NA)
-   **c_stock_80** - below-ground soil organic carbon stock until 80 cm,
    obtained through depth integration (i.e. summing up) of carbon densities
    (as returned from mass-preserving splines for a depth of i cm
    in ton C ha-1 cm-1) from 0 to 80 cm, if the latter is above or equal to the
    *soil_depth* (else NA)
-   **c_stock_90** - below-ground soil organic carbon stock until 90 cm,
    obtained through depth integration (i.e. summing up) of carbon densities
    (as returned from mass-preserving splines for a depth of i cm
    in ton C ha-1 cm-1) from 0 to 90 cm, if the latter is above or equal to the
    *soil_depth* (else NA)
-   **c_stock_100** - below-ground soil organic carbon stock until 100 cm,
    obtained through depth integration (i.e. summing up) of carbon densities
    (as returned from mass-preserving splines for a depth of i cm
    in ton C ha-1 cm-1) from 0 to 100 cm, if the latter is above or equal to the
    *soil_depth* (else NA)
-   **c_stock_below_ground** - below-ground soil organic carbon stock,
    obtained through depth integration (i.e. summing up) of carbon
    densities (as returned from mass-preserving splines for a depth of i cm
    in ton C ha-1 cm-1) from 0 cm to *soil_depth* - either for a specific
    profile or the average across profiles within a plot survey
-   **c_stock_below_ground_stdev** - standard deviation of
    *c_stock_below_ground* values across profiles within a plot survey
-   **c_stock_forest_floor** - forest floor organic carbon stock,
    obtained through depth integration (i.e. summing up) of carbon
    densities (*c_stock_layer*) for all above-ground forest floor layers -
    either for a specific profile or the average across profiles within a plot
    survey
-   **c_stock_forest_floor_stdev** - standard deviation of
    *c_stock_forest_floor* values across profiles within a plot survey
-   **c_stock_layer** - soil organic carbon stock of a given layer 
    (ton C ha-1), calculated as (*c_density* * *layer_thickness*)
    or as
    (*organic_carbon_total* * *organic_layer_weight*) / 100
    (for organic layers)
-   **c_stock_ofh** - forest floor organic carbon stock for the "OFH", "OF"
    (fermentation) and/or "OH" (humified) layer, obtained through depth
    integration (i.e. summing up) of any "OFH", "OF" and "OH" layers
    (*c_stock_layer*) (NA if none of these layers has been reported)
-   **c_stock_ol** - forest floor organic carbon stock (*c_stock_layer*)
    for the "OL" (litter) layer (NA if no "OL" layer has been reported)
-   **c_stock_stdev** - standard deviation of
    *c_stock* values across profiles within a plot survey
-   **coarse_fragment_vol_frac** - volume proportion of coarse fractions
    (relative, i.e. not in %), gap-filled by assuming coarse fragments were
    absent (0) if nothing was reported (NA)
-   **code_country** - unique ID of country within ICP Forests
-   **code_layer** - code of the fixed-depth layer or pedogenic horizon
-   **code_plot** - observation plot number
-   **contains_peat** - logical indicating whether the below-ground part
    of the profile contains any peat layers
-   **depth_avg** - average layer depth (in cm), calculated as the average
    between "depth_top" and "depth_bottom"
-   **depth_bottom** - inferior layer limit (in cm)
-   **depth_top** - superior layer limit (in cm)
-   **forest_floor_layers** - string representing different layer codes
    (*code_layer*) of above-ground forest floor layers pasted together
    with underscores ("_"), in sequential order from top to bottom
-   **forest_floor_layers_unique** - unique value of *forest_floor_layers*
    if values of *forest_floor_layers* across different profiles within a plot 
    survey are the same, else NA
-   **forest_floor_thickness** - sum of layer thicknesses (*layer_thickness*)
    of forest floor layers for a given profile
-   **forest_floor_thickness_avg** - average of *forest_floor_thickness*
    values across profiles within a plot survey
-   **layer_number** - sequential index assigned to each soil layer within a
    profile ("profile_id"). The upper layer (often forest floor) is designated
    as layer number 1, and each successive layer below it is assigned
    an incrementally higher number, such as 2, 3, 4 etc.
-   **layer_thickness** - layer thickness (in cm), calculated as the absolute
    difference between "depth_top" and "depth_bottom"
-   **layer_type** - type of matrix - either "forest_floor", "peat" or
    "mineral"
-   **nlay** - number of layers in the whole profile
-   **nlay_below_ground** - number of below-ground layers in the profile
-   **nlay_below_ground_max** - maximum of *nlay_below_ground* values across
    profiles within a plot survey
-   **nlay_below_ground_min** - minimum of *nlay_below_ground* values across
    profiles within a plot survey
-   **nlay_forest_floor** - number of forest floor layers in the profile
-   **nlay_forest_floor_max** - maximum of *nlay_forest_floor* values across
    profiles within a plot survey
-   **nlay_forest_floor_min** - minimum of *nlay_forest_floor* values across
    profiles within a plot survey
-   **nlay_max** - maximum of *nlay* values across profiles within a plot survey
-   **nlay_min** - minimum of *nlay* values across profiles within a plot survey
-   **obs_depth** - maximum depth of profile for which data are available,
    i.e. maximum inferior depth layer (*depth_bottom*) within a profile for
    which the carbon density (*c_density*) is not an NA
-   **obs_depth_avg** - average of *obs_depth* values across profiles within
    a plot survey
-   **organic_carbon_total** - total organic carbon content (in g kg-1)
-   **organic_layer_weight** - total dry weight of an organic layer (in kg m-2)
-   **partner_code** - unique ID of data-submitting partner within ICP Forests
-   **partner_short** - short name of data-submitting partner
-   **plot_id** - combination of “code_country” and “code_plot”
-   **profile_id** - combination of “survey_year”, “code_country”, “code_plot”
    and “repetition”
-   **repetition** - unique number of the replicate sample/profile
    (within a layer) of a given plot survey
-   **rmse_mpspline** - root mean squared error from fitting the mass-preserving
    spline (measure of spline accuracy)
-   **rmse_mpspline_max** - maximum of *rmse_mpspline* values across profiles
    within a plot survey
-   **soil_depth** - effective depth (in cm) of the soil profile until the
    continuous rock of until 100 cm (if the soil profile extends deeper than
    100 cm). This is the depth until which depth-integrated below-ground
    carbon stocks are calculated
-   **soil_depth_avg** - average of *soil_depth* values across profiles within
    a plot survey
-   **survey_year** - year of the data collection (soil survey)


