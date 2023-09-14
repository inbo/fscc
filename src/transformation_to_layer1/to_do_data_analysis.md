
Please add any observed data issues to this file.

## TO DO - Transformation layer 0 –-> layer 1

-   Gap-filling:
    -   Folder with direct partner communication (AFSCDB.LII.2.1
        subfolder) - at least Austria, Spain, bulk density and coarse
        fragments from Sweden…
    -   Folder AFSCDB.LII.2.2
    -   Folder BIOSOIL.LII - at least ~~Spain,~~ Finland… (Note:
        missing Spanish data now in layer 0)
    -   Anything to gap-fill for LI? (e.g. folders BIOSOIL.LI,
        FSCDB.LI.1?) FSCDB.LI.1: check whether any “OPT” data are
        currently missing. Oldest survey from Italy seems to be missing,
        Latvia and Austria probably incomplete too?
    -   PIRs! (note separate e-mail Sture Wijk; Czech pH-H2O in “pfh”) +
        add column with validation code different parameters?
    -   Add column with data source for different variables
    -   Harmonise layers with custom depths (e.g. Mxx, Hxx, often in
        profiles with both peat and mineral) to theoretical fixed depths
        using C content and bulk density where needed, e.g. Estonia
    -   Check whether harmonisation of layer limits is needed,
        i.e. negative for forest floor layers (e.g. Estonia, where top
        of forest floor was designated as the 0-cm line)
    -   Note that some of the fixed-depth profiles do contain gaps, i.e.
        impossible to harmonise (except through mass-preserving
        splines?)
-   Check “other_obs” columns properly
-   ~~Harmonise plot_id’s and coordinates (e.g. Poland, UK)~~ (Note:
    completed)
-   Was there a systematic coordinate issue in the Pyrennees in Spain?
-   Correct coordinate sign mistakes (e.g. in Spain, where the minus
    sign was clearly sometimes forgotten, since it was there for other
    records of the same plot)
-   Harmonise “horizon_coarse_weight” if volumetric instead of wt%
    (e.g. Slovak Republic)
-   Harmonise soil textures where needed (e.g. Wallonia) to 63 µm limit
    using R soil texture wizard
-   “so_prf” (+ when possible: “s1_prf”): Left-join dataframe with
    harmonised WRB soil classification information by Nathalie --> one
    record per profile
    -   Include column with harmonisation method
    -   Include column with qualitative eutric/dystric factor
    -   Recode humus type (e.g. amphihumus) in accordance with survey
        year
    -   After joining: identify missing plots without soil
        classification (due to gap-filling) - also check in other survey
        forms.
    -   s1_prf: create machine-learning model to predict WRB soil
        classes in plots where this information is lacking?
-   Check Russian plots in “so” survey: some of them actually belong to
    “s1”. Move accordingly.
-   ~~Germany: harmonisation partner code across different survey
    forms?~~ (Note: completed after verifying that plot codes are
    unique across Germany - all German partner codes are now 98)
-   LOQ: harmonise and list assumptions
-   (Ring tests: no need to exclude any data on the basis of bad ring
    tests at this stage)
-   Gap-filling forest types and WRB LI and humus + confirmation by
    national experts
-   Remove incomplete unique profiles (e.g. profiles with only one
    forest floor layer)? Or do we keep it for plot-level integration (at
    least in “som” forms, i.e. with fixed depths)?

## TO DO - Transformation layer 1 –-> layer 2

-   Harmonisation of “som” survey forms to uniform layers: e.g. to
    (“OL”, “OFH”,) “M01”, “M12”, “M24”…
-   Summarise over different replicate profiles (“repetition” or
    “profile_pit_id”) per survey per plot (for each layer): average
    and standard deviation? (only possible for “som” survey forms)
-   “prf” and “pfh” files: retain one observation over time (data in
    these survey forms are assumed to be constant)
-   Selection of “useful” plots to be retained for further processing?

## TO DO - Stocks, indicators

-   Apply further gap-filling possibilities (e.g. constant bulk
    density) + list assumptions (see presentation Bruno Vienna)
-   Machine-learning prediction of lowest point splines (depth of
    100 cm) + Monte Carlo uncertainty assessment?
-   FSCDB.LI: Check whether indicator data in VWDD tables match the
    Vanmechelen report formulas
-   Check within-plot variability

## TO DO - Additional tests for potential future PIRs

-   Should an OL layer always be present in theory? Or depending on
    season, forest type?
-   When no O layers are reported: are they actually not present or have
    they just been ignored in the survey?
-   Check whether plots in other survey forms are also reported in “pls”
    (and system installment)
-   Check whether values below LOQ equal -1
-   Add a plausible range test for organic_layer_weight
-   Histosol: are H layers reported in “som”?
-   Are % OC values below 20 % in M layers and above 20 % in H layers?
-   When NA is reported for coarse fragments: is this actually not
    measured or does this mean that there are no stones?
-   Check whether code_plots are unique linked with coordinates.
