
Please add any observed data issues to this file
(e.g. by clicking on the pencil in GitHub > add information in [Markdown syntax](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax) > commit changes to main branch)

## TO DO - Transformation layer 0 –-> layer 1

* R script: `./src/transformation_to_layer1/solid_soil_data_transformation_to_layer1.R`
* Gap-filling from external data sources and internal gap-filling using assumptions:
  + Folder AFSCDB.LII.2.2: also check whether there are any plot surveys that do not appear in so_som at all. Use the original data forms (with different repetitions etc), not the aggregated version.
  + Anything to gap-fill for LI? (e.g. folders BIOSOIL.LI, FSCDB.LI.1?) FSCDB.LI.1: check whether any “OPT” data are currently missing. Oldest survey from Italy seems to be missing, Latvia and Austria probably incomplete too?
  + FSCDB.LI: add profiles to pfh and prf that are lacking (check if this is actually the case: there may be an issue with different former German code plots with the same plot_id that now look like the same plot (same partner_code))
  + Add column with data source for different variables (Note: partly completed)
* profiles with data until 20 cm or 40 cm: ~~(i) assume carbon density in subsoil does not change with time; (ii) take a fixed carbon density of 0.1 ton C cm-1 ha-1 between 80 and 100 cm; or~~ (iii) monte carlo machine learning prediction of carbon density at a depth of 100 cm (assessing confidence interval)
* considerable uncertainty in code_horizon_coarse_vol (in “pfh”) → first priority: coarse fragment fractions from other survey year?
* Harmonise “horizon_coarse_weight” if volumetric instead of wt% (e.g. Slovak Republic), as indicated by partners in PIRs.
* Harmonise soil textures where needed (e.g. Wallonia) to 63 µm limit using R soil texture wizard
* Check “other_obs” columns properly
* Are coordinates fine? Was there a systematic coordinate issue in the Pyrennees in Spain? Correct coordinate sign mistakes (e.g. in Spain, where the minus sign was clearly sometimes forgotten, since it was there for other records of the same plot)
* Update plot codes LI UK back to original for 1994
* Solve issue with non-unique German plot codes in LI
* survey_year in "som" and "pfh" does not always correspond with the actual sampling year (i.e. sometimes lab analysis year). Correct by means of survey_year in "prf" and "pls"
* Add potential sources of uncertainty, for example ring test standard deviations. In theory, this lab analytical uncertainty as well as sample pretreatment uncertainty should be somehow included along with spatial variation in the variation between plot repetitions. At this stage, we will just compare the order of magnitude of the ring test standard deviation with the standard deviation between plot repetitions. At this stage, no need to exclude any data on the basis of bad ring tests.
* Make a separate script for LI versus LII, in which you can choose the survey form (so_som, so_pfh, s1_som, s1_pfh) and the variable to calculate stocks from. Different methodological decisions (e.g. about internal gap-filling opportunities) should be changeable via function input variables. List these important methodological variables and their options. A file with this methodological information should be exported as metadata in the output.


* ~~in s1_som files, for all MINERAL horizons, set organic_layer_weight==NA if bulk density value is given.~~ (Note: completed)
  + ~~Folder with direct partner communication (AFSCDB.LII.2.1 subfolder) - at least Austria, Spain, bulk density and coarse fragments from Sweden… (Note: still necessary?)~~
  + ~~Folder BIOSOIL.LII - at least Spain, Finland… (Note: missing Spanish data now in layer 0)~~
  + ~~PIRs! (note separate e-mail Sture Wijk; Czech pH-H2O in “pfh”) + add column with validation code different parameters?~~
  + ~~Delete impossible values or codes in data? (e.g. forest type 32)~~
  + ~~Apply further internal gap-filling possibilities (e.g. constant bulk density) + list assumptions (see presentation Bruno Vienna)~~
  + ~~"som" survey forms gap-filling for C stocks:~~
    - ~~bulk_density: assumption constant over time; from "pfh" ("horizon_bulk_dens_measure", "horizon_bulk_dens_est"); "sw_swc" ("bulk_density");~~ pedotransfer functions/machine learning
    - ~~organic_carbon_total: from "pfh" ("horizon_c_organic_total")~~
    - ~~coarse_fragment_vol: assumption constant over time; from "pfh" ("horizon_coarse_weight", "code_horizon_coarse_vol");~~ machine learning?
    - ~~effective_soil_depth: assumption constant over time; from maximum "layer_limit_inferior" + machine learning? Or assumption: always deeper than 100 cm if we know it is deeper than 80 cm? This was manually harmonised by Nathalie (data form "./data/additional_data/SO_PRF_ADDS.csv")~~
    - ~~"pfh" survey forms gap filling for C stocks: add bulk densities forest floor from "som" after linking forest floor layers across "pfh" and "som" (same survey, i.e. maximum difference in survey years of 3 years)~~
* ~~Check whether vertical shifting of layer limits is needed, e.g. negative for forest floor layers (e.g. Estonia, where top of forest floor was designated as the 0-cm line). Rule for organic H layers:~~
  + ~~if code_layer is H and no layer limits, the layer should be in the forest floor. Change layer_type to forest_floor.~~
  + ~~if organic H layers are < 40 cm thick in total (and below any forest floor or above any mineral soil), this layer(s) should be considered as the forest floor. Change layer_type to forest_floor.~~
  + ~~if organic H layers are >= 40 cm thick in total, they can be considered as actual peat layers.~~
  ~~The null line should be between the forest floor (including those H layers just added; negative layer limits) and the peat/mineral layers (positive layer limits). Move null line in accordance if necessary (by shifting the layers up or down and changing their layer limits).~~
* ~~Harmonise plot_id’s and coordinates (e.g. Poland, UK)~~ (Note: completed)
* ~~“so_prf” (+ when possible: “s1_prf”): Left-join dataframe with harmonised WRB soil classification information by Nathalie --> one record per profile~~
  + ~~Include column with harmonisation method~~
  + ~~Include column with qualitative eutric/dystric factor~~
* ~~Check Russian plots in “so” survey: some of them actually belong to “s1”. Move accordingly.~~
* ~~Germany: harmonisation partner code across different survey forms?~~
* ~~Replace impossible data (e.g. bulk density above 2650 kg m-3) by NA~~
* ~~Regarding implausible values of "organic_layer_weight":~~
  + ~~Are Slovakian organic_layer_weight values for code_plot 211 and 212 reported in the wrong units?~~
    - ~~Other Slovakian plots: organic layer weights give a bulk density of 13.74 - 170 kg m-3 (95 % quantile)~~
    - ~~Plot 211: organic layer weights give a bulk density of 7056.38 - 47071.00 kg m-3 (95 % quantile), i.e. a factor 280 higher~~
    - ~~Plot 212: organic layer weights give a bulk density of 19212.08 - 88668.75 kg m-3 (95 % quantile), i.e. a factor 455 higher~~
    - ~~Conclusion there are two options: (i) either the data are wrongly reported in tonnes per ha (factor 100 higher); of (ii) the data are wrongly reported in g per m2 (factor 1000 higher). Assumption: they were reported in g per m2.~~
  + ~~Values of organic_layer_weight for which the derived bulk density is higher than 1400 kg m-3 (density of organic matter) are impossible and replaced by NA.~~
* ~~LOQ: harmonise and list assumptions~~
* ~~Gap-filling forest types and WRB LI and humus + confirmation by national experts~~
* ~~Remove incomplete unique profiles (e.g. profiles with only one forest floor layer)? Or do we keep it for plot-level integration (at least in “som” forms, i.e. with fixed depths)?~~
* ~~Add list with structure similar to PIRs, in which specific data updates by FSCC can be listed (along with their reason and a "change_date"). These can then be applied in a way similar to updated values in the checked PIRs.~~



~~## TO DO - Checked PIRs~~
* ~~Remove rows in pir where code_nfc_action_taken, nfc_remark and updated_value are empty~~
* ~~Ignore code_nfc_action_taken if updated_value is empty (we can't do much with it anyway for now). Not relevant to add "confirmations" (e.g. extreme but correct values; no data avaible...) to the layer 1 data for now. We'll have to use statistics and objective expert reasons to exclude values.~~
* ~~I'm sometimes not sure whether the column "parameter_value" was updated by the partner. Checking this is possibly by joining the checked pir with the shared empty pir and comparing these two columns. So possibly, "updated_value"" does not contain all newly delivered data from the pirs. The other way around is also possible: that I placed the "parameter_value" info to the "updated_value" column because I assumed the values looked updated. Also possible by comparing likewise.~~
* ~~(less urgent: add a column to the checked pirs which indicates whether inconsistencies were indeed correctly solved in layer 0, as promised in the pirs by partners)~~
* ~~inconsistencies with incorrect units (FSCC_22) don't need to be updated because this should have been done by the scripts. (Note: Decided to not take this into account, since automated correction units happens after this gap-filling)~~
* ~~if "updated_value" says "record_to_be_removed", partners indicated that this record should be removed~~
* ~~if "updated_value" says "data_to_be_removed", partners indicated that the value should be removed (i.e. replaced by NA)~~


## TO DO - Transformation layer 1 –-> layer 2

* "som" files: harmonisation of layers with custom depths (e.g. Mxx, Hxx, often in profiles with both peat and mineral) to theoretical fixed depths (e.g. to (“OL”, “OFH”,) “M01”, “M12”, “M24”…) using C content and bulk density where needed, e.g. Estonia. Note that some of the fixed-depth profiles do contain gaps, i.e. impossible to harmonise (except through mass-preserving splines?)
* Summarise over different replicate profiles (“repetition” or “profile_pit_id”) per survey per plot (for each layer): average and standard deviation/confidence interval? (only possible for “som” survey forms)
* Remove profiles without below-ground data? (i.e. only forest floor)
* "prf" and "pfh" files: retain one observation over time (data in these survey forms are assumed to be constant)
* "prf": retain one record per plot
* Selection of "useful" plots to be retained for further processing?
* Propagate any uncertainty correctly.

## TO DO - Stocks, indicators

* FSCDB.LI: Check whether indicator data in VWDD tables match the Vanmechelen report formulas
* Propagate any uncertainty correctly.
* Make functions to visualise output, e.g. violin plots per stratifier, overview graphs per plot_id, dynamic maps.
* Calculate change in carbon stock per year.
* Carbon stocks for carbon contents < LOQ?
* ~~Machine-learning prediction of lowest point splines (depth of 100 cm)~~ + Monte Carlo uncertainty assessment?
* ~~Convert scripts into a function.~~ Different methodological decisions should be changeable via function input variables. List these important methodological variables and their options. A file with this methodological information should be included as metadata in the output. Also include total uncertainty (including uncertainty from the spline fitting + spline extrapolation + propagated uncertainty from other sources) in the output.
* ~~Calculate stocks based on "pfh" survey forms too. Compare stocks based on fixed-depth layers with those based on pedogenetic horizons.~~
* ~~Also include stocks until 30 cm as output in plot-aggregated stock files.~~
* ~~Check within-plot variability~~


## TO DO - How to assess uncertainty?

* The process of deriving soil carbon stocks involves several steps, each of which is associated with specific uncertainties arising from various sources:
  + The data consists of spatial repetitions within a plot.
  + Samples were subjected to sample preparation and lab analyses, with separate analytical runs for each spatial repetition.
  + Data gaps and human errors: despite thorough data validation efforts, the presence of potential human errors and data gaps remains. The process of gap-filling entails uncertainty.
  + The calculation of carbon stocks involved depth integration cm-specific carbon densities (ton C cm-1 ha-1) after fitting mass-preserving splines to carbon densities of fixed-depth layers. Carbon densities per depth layer were calculated if the required parameters were present and not missing (NA). The main sources of uncertainty here are the fitting of a mass-preserving spline + the extrapolation to a depth of 100 cm (or the depth of the soil profile). Especially the extrapolation process introduces additional uncertainty when data is limited to shallow depths.
  + The mass-preserving spline fitting process provides an RMSE value as an indicator of the goodness of fit, which influences the subsequent extrapolation process for depths less than 100 cm.
* Dealing with uncertainty:
  + Determine if the lab analytical uncertainty (control charts) is distinct from spatial variations observed in the field (to avoid duplication of uncertainty sources).
  + Human errors are by definition assumed to be absent, since there is no way to quantify this or deal with this after data validation.
  + Data gaps were filled using methods like assuming constant bulk densities for a plot across multiple survey years. The uncertainty associated with these methodological choices could maybe be quantified by some kind of sensitivity analysis.
  + The mass-preserving spline fitting process provides an RMSE value as an indicator of the goodness of fit.
  + How to deal with uncertainty due to extrapolation to a depth of 100 cm?
  + Error propagation: describe in a transparent way how different uncertainties propagate through the analysis and calculations process to contribute to the overall uncertainty.


## TO DO - Additional tests for potential future PIRs

* Should an OL layer always be present in theory? Or depending on season, forest type?
* When no O layers are reported: are they actually not present or have they just been ignored in the survey?
* Check whether plots in other survey forms are also reported in “pls” (and system installment)
* Check whether values below LOQ equal -1
* Histosol: are H layers reported in “som”?
* Are % OC values below 20 % in M layers and above 20 % in H layers?
* When NA is reported for coarse fragments: is this actually not measured or does this mean that there are no stones?
* Check whether code_plots are uniquely linked with coordinates.
* Check whether H layers should be converted to forest floor and whether null line is on the correct location in the profile.
* ~~Add a plausible range test for organic_layer_weight~~

