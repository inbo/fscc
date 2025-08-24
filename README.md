<!-- badges: start -->
![GitHub](https://img.shields.io/github/license/inbo/fscc)
![GitHub Workflow Status](https://img.shields.io/github/workflow/status/inbo/fscc/check-project)
![GitHub repo size](https://img.shields.io/github/repo-size/inbo/fscc)
<!-- badges: end -->

# fscc
Forest Soil Coordinating Centre (FSCC) of ICP Forests

[Deroo, Heleen![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0003-4487-0262)[^aut][^cre][^RINF]
[Cools, Nathalie![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0002-7059-2318)[^aut][^cre][^RINF]
[De Vos, Bruno![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0001-9523-3453)[^aut][^cre][^RINF]
Research Institute for Nature and Forest (INBO)[^cph][^fnd]

[^cph]: copyright holder
[^fnd]: funder
[^aut]: author
[^cre]: contact person
[^RINF]: Research Institute for Nature and Forest

**keywords**: icp forests; forest soil coordinating centre; forest soils; european forests; reproducible data processing

<!-- community: inbo -->

<!-- description: start -->
Forest soils play a key role in a range of ecosystem services, including climate control, biodiversity and water quality, and are a primary driver of forest productivity.
However, insight in their response to ongoing environmental changes, such as nitrogen deposition, relies on the availability of harmonised, spatially and temporally representative data.
Since the early 1990s, the ICP Forests Programme runs a harmonised forest soil monitoring survey across Europe, including large-scale monitoring on a systematic 16 x 16-km grid (Level I: approximately 5000 plots) and intensive monitoring with a time interval of roughly 10 years (Level II: 250 plots).
Two surveys have been completed so far on both levels; a third survey (2020-2025) is ongoing in most countries.
A wide range of soil chemical (carbon, nitrogen, plant-available and semi-total elements, pH) and physical soil properties (soil texture, bulk density) have been analysed by national forest soil laboratories according to standardised protocols.
All data are centralised in the database of the Programme Coordinating Centre of ICP Forests.

In this R project, we aim to preprocess the raw solid soil data of ICP Forests to validated, gap-filled and harmonised versions (Layer 1), and process these data for a range of purposes, including the calculation of organic carbon and macronutrient (nitrogen, phosphorus, sulphur) stocks, stoichiometric nutrient ratios, and indicators for nutrient availability, heavy metal availability and acidification, in forest soils across Europe. 
Results are stratified by biogeographical region, European forest type and WRB Reference Soil Group.
<!-- description: end -->

***

### Repository overview  
This repository provides data-generating (i.e., preprocessing) and data-processing workflows, to support reproducible and transparent analyses on the solid soil data collected through the European ICP Forests monitoring network since the end of the 1980s. The aim of this R project is to correct and harmonise the raw data forms ("Layer 0") into validated data forms that are gap-filled with original measurements ("Layer 1"), and further gap-filled with additional estimates (in particular, pedotransfer function-estimated bulk densities; "Layer 1+"). These "layer 1+" data forms are then used for a range of other purposes, including the calculation of organic carbon stocks in European forest soils. The data preprocessing addresses various aspects, including error correction, incorporation of additional data, harmonisation of "wrong" data to the correct parameter (units), validation, harmonisation to theoretical fixed-depth layers (for "som" files), etc - all based on sound expert assumptions (in line with the ICP Forests manual). The ultimate aim is to achieve open and reproducible data workflows, as a requirement for qualifiable science and for collaboration.

### Project structure  
The repository is structured as follows:

* **src** folder: Contains all the R scripts for data processing and analysis. This includes subfolders:
  + **functions**: Custom-made R functions
  + **transformation_to_layer1**: R scripts to transform the data from "Layer 0" to "Layer 1" and further to "Layer 1+"
  + **pir_generation**: R scripts used to generate Partner Inconsistency Reports
  + **stock_calculations**: R scripts to calculate and visualise different kinds of stocks
  + **nutrient_avail**: R scripts to calculate and visualise nutrient availability and acidification metrics
  + **specific_esb_requests**: R scripts for specific data questions within the frame of the Ecological Studies book compilation
  + **sandbox**: R scripts for individual exploration and testing (ignored by Git) 
* **data** folder: Contains all versions of the data forms and additional data:
  + **raw_data**: Raw data forms ("layer 0") obtained from the central ICP Forests database
  + **layer1_data**: Processed, gap-filled, and harmonised data forms (actually "Layer 1+")
  + **additional_data**: Additional data forms needed for the transformation to Layer 1 and other purposes, e.g., data collected through direct communication with partners (mainly in response to the so-called "Partner inconsistency reports" (PIRs)); previous versions of the database (e.g. so-called "AFSCDB_LII_2_2"); harmonisation keys for plot codes; shapefiles...
  + **sensitive_metadata**: Sensitive information, e.g. the link to the private Google Drive folder where data are stored
* **output** folder: Stores outputs of this repository which do not fall under data forms, organised per output type:
  + **stocks**: Output regarding stocks of total soil organic carbon, nitrogen, semi-total macro-element pools (P, K, Ca, Mg, S), exchangeable element pools (K, Ca, Mg, Al, Fe, Mn), heavy metal stocks...
  + **pirs**: Newly generated partner inconsistency reports  
  + **gap_filling_details**: Details of the "decisions" made by the scripts during gap-filling, e.g. which of the "new" data in PIRs returned by countries needed to be inserted in the data forms
  + **spatial_plot_id_links**: Results of the spatial analysis of different surveys with coordinates - shows, for each partner, which plots across different surveys (survey form with coordinates x survey_year/last_year) are spatially co-located (based on the reported coordinates). This information was eventually used to solve plot_id/coordinate issues for Poland.
  + **specific_esb_requests**: Output for specific data questions (e.g. soil physical data for water budget modelling)


```
    .  
    ├── fscc.Rproj
    ├── README.md
    ├── .gitignore
    ├── .github
    ├── inst
    ├── src
    │   ├── functions                            <- Git
    │   ├── transformation_to_layer1             <- Git
    │   ├── pir_generation                       <- Git
    │   ├── stock_calculations                   <- Git
    │   ├── nutrient_avail                       <- Git
    │   ├── specific_esb_requests                <- Git
    │   ├── [...]                                <- Git
    │   └── sandbox                              <- GITIGNORE
    ├── output                                   <- GITIGNORE
    │   ├── stocks
    │   │   ├── oc
    │   │   │   ├── s1_plot_oc_stocks.csv
    │   │   │   ├── s1_profile_oc_stocks.csv
    │   │   │   ├── s1_layers.csv
    │   │   │   ├── so_plot_oc_stocks.csv
    │   │   │   ├── so_profile_oc_stocks.csv
    │   │   │   ├── so_layers.csv
    │   │   │   ├── plot_oc_stocks_attribute_catalogue.txt
    │   │   │   ├── so_oc_stock_potential_summary.csv
    │   │   │   ├── graphs
    │   │   │   ├── s1_som_splines_per_profile
    │   │   │   ├── s1_pfh_splines_per_profile
    │   │   │   ├── s1_splines_per_plot
    │   │   │   ├── so_som_splines_per_profile
    │   │   │   ├── so_pfh_splines_per_profile
    │   │   │   └── so_splines_per_plot
    │   │   └── […]
    │   ├── pirs
    │   ├── gap_filling_details
    │   ├── spatial_plot_id_links
    │   ├── specific_esb_requests
    │   └── [...] 
    └── data
        ├── sensitive_metadata                   <- GITIGNORE - Google Drive URL
        ├── raw_data                             <- GITIGNORE
        │   ├── s1
        │   ├── so
        │   └── […]
        ├── layer1_data                          <- GITIGNORE
        │   ├── s1
        │   │   ├── s1_som.csv
        │   │   ├── s1_pfh.csv
        │   │   ├── s1_prf.csv
        │   │   ├── s1_pls.csv
        │   │   ├── s1_lqa.csv
        │   │   ├── coordinates_s1.csv
        │   │   ├── data_availability_s1.csv
        │   │   ├── s1_strat.csv
        │   │   ├── s1_som_layer1_attribute_catalogue.txt
        │   │   ├── s1_pfh_layer1_attribute_catalogue.txt
        │   │   └── s1_strat_attribute_catalogue.txt
        │   ├── si
        │   ├── so
        │   ├── sw
        │   └── y1
        └── additional_data                      <- GITIGNORE (available upon request)
            ├── fscdb_LI
            │   └── original_access_versions
            │       ├── s1_fscdb_access_harmonised.csv
            │       └── [...]
            ├── afscdb_LII_2_2
            │   ├── repetitions
            │   │   ├── so_afscdb_harmonised_r.csv
            │   │   └── [...]
            │   └── plot-aggregated
            │       └── [...]
            ├── partner_comm
            │   ├── so_spain_correct
            │   ├── texture_data_collection_dec2024
            │   │   ├── so_som_texture.csv
            │   │   └── original
            │   │       └── [...]
            │   └── [...]
            ├── national_coauthor_carbon_stocks
            │   └── national_carbon_stocks_harmonised.r
            ├── shapefiles
            │   ├── European Soil Database
            │   ├── worldclim
            │   └── BiogeoRegions2016.shp
            ├── coordinates_plots
            │   └── [...]
            ├── plot_coord_harmonisation_keys
            │   └── [...]
            ├── preinternalgapfill
            │   └── [...]
            ├── 126_bd_20230405143047
            │   └── [...]
            ├── attribute_catalogue_dictionary.txt
            ├── 20230302_checked_pirs.xlsx
            ├── attribute_catalogue_pir.csv
            ├── inconsistency_catalogue.csv
            ├── parameters_mandatory.csv
            ├── d_depth_level_soil.csv
            ├── d_soil_coarse_fragments.csv
            ├── d_forest_type.csv
            ├── ranges_qaqc.csv
            ├── S1_PRF_ADDS.csv
            ├── SO_PRF_ADDS.xlsx
            ├── additional_manual_corrections_fscc.csv
            ├── s1_link_forest_floors.csv
            ├── so_link_forest_floors.csv
            ├── ICP-LII-plots_slope_aspec_COPDEM30.csv
            ├── France_PRF_1994-1995.xlsx
            ├── partner_boundaries.gpkg
            └── ICP_LOGO_transparent_background.png




```


### Workflow and collaboration  
#### R code version control
We use Git and GitHub to facilitate collaboration and version control. The main branch serves as the primary branch where versions of the R code - for step-wise transformation of data from "layer 0" to "layer 1" and to derived variables such as carbon stocks - are tracked through different commits. Only source code (in the `./src/` folder) is version-tracked, while data and output are stored in private Google Drive folders and the local project folder (ignored by Git). The latter includes additional data, which is available upon request.

#### Dataset management
All individual steps of the data (pre)processing are compiled, documented and applied using R scripts in this repository, e.g., `./src/transformation_to_layer1/transformation_to_layer1.R` for the transformation of "layer 0" to "layer 1".

In accordance with the ICP Forests policies, data are currently not open, and **storage** relies on a combination of:
* A private Google Drive folder with all versions
* Your local R project folder (ignored by Git) for the most recent versions


##### Preprocessing

R scripts:
* `./src/transformation_to_layer1/transformation_to_layer1.R`
* `./src/transformation_to_layer1/transformation_to_layer1+.R`

Whenever you create a new version of the (layer 1) data that you want to save, you are expected to save the data on Google Drive
using the `save_to_google_drive()` function. This way, all collaborators have access to the latest versions of the data. 

Whenever you start working on the project, it is recommended to synchronise your local data with the data on Google Drive using the `sync_local_data()` function, to make sure that you have the most up-to-date data available for your analysis in R.

Data folders in the local project folder have clean names (no version indication). On Google Drive, data are stored in subfolders of a specific folder, such as a subfolder of "layer1_data".
Such Google Drive subfolders have specific naming conventions to indicate their version/modification status:
* Raw data subfolders are named as "download_(download_date)", e.g., "download_20230605".
* Other subfolders are named as "(download_date)_(change_date)_(name_folder)", e.g., "20230605_20230628_layer1_data".


<img src="https://github.com/user-attachments/assets/332052f4-703f-4960-b170-5d8468f04b0a" width="80%">


##### Further processing (e.g., calculating SOC stocks)

R scripts:
* `./src/stock_calculations/stock_calculations.R`

At the moment, results are saved both to Google Drive using the `save_to_google_drive()` function and locally using `write.table()`. All versions of these datasets are saved on both locations, within subfolders indicating the version, according to the same pattern "(download_date)_(change_date)_(name_folder)", e.g., "20241213_20250807_oc_stocks".
The `sync_local_data()` function needs to be updated so that it can also synchronise locally saved versions of processed datasets (e.g., SOC stocks) with Google Drive, and rely on a single clean version (without version indication) in the local project folder (analogous to the layer 1 data).



#### Specific project coding conventions
* It is recommended to compile the code for larger manipulations and/or manipulations which are often repeated, into one function, which is placed in the `./src/functions/` folder. Such functions are sourced ad hoc using the `source()` function. Custom-made functions should be applicable to all data forms in Level I and Level II, and clearly documented using comments and Roxygen documentation (Code > Insert Roxygen Skeleton).
* All objects and files are by convention named using snake_case (lowercase separated by underscores)
* Use headers for long R scripts. You can show the document outline of an R script in RStudio via Ctrl + Shift + O (or the button with horizontal lines). Level-one headings should start with one hashtag (`#`) and have four trailing dashes (`----`); level-two headings should have two hashtags and four trailing dashes; etc.

### Contributing  
Please follow this workflow:

#### Getting started: first time
* Make sure you have Git and RStudio installed, and completed the [initial Git configuration in your RStudio](https://inbo.github.io/git-course/course_rstudio.html#22_Git_for_RStudio_configuration).
* **Clone** the repository to your local computer to work on the project.
* Review the README file for an overview of the project structure and collaboration instructions.
* Download the R script `./data/sensitive_metadata/google_drive_link.R`, which contains the URL of the private Google Drive folder which contains the data.
* Download data in the folder `./data/additional_data/`
* Download the raw and/or layer 1 data from the Google Drive folder to your local project folder using the `sync_local_data()` function.
* Import the layer 1 data from your local folder in R using the `read_processed()` function.



#### Getting started: next times
* Open the .Rproj file in RStudio. Ideally, your working directory is not dirty (any previous changes should have been committed before).
* **Pull** changes from the repository before starting your work to ensure you have the latest code and datasets from other team members.
* Synchronise your local data with the up-to-date Google Drive folder using the `sync_local_data()` function.
* Import the data from your local folder in R using the `read_processed()` function.


#### Downloading raw data (layer 0) from PCC database
* Download the zipped folders per survey
* On Google Drive: create a subfolder inside `./data/raw_data/` with the name "download_[download_date in YYYYMMDD]"
* Place the survey folders in this Google Drive folder (unzipping is not necessary but possible)
* Synchronise your local folders using the `sync_local_data()` function.
* Import the raw data from your local folder in R using the `read_raw()` function.


#### Adjust the data transformation code
* Remove any local branches which were merged with the main branch on GitHub.
* Checkout the correct branch or create a **new branch**. Every collaborator should have his/her own side branch.
* Open any R script and adjust the data transformation code as needed in a clear way.
* Save the updated data forms on Google Drive when needed.
* Regularly stage and **commit** changes to the correct branch, including the new or updated intermediate data forms, using clear and concise commit messages (to describe the modifications made to the code and datasets), and **push** them to the correct branch in the remote GitHub repository.
* When edits on the branch are ready to move to the stable "reference" main branch: create a **pull request** on GitHub to merge the branch with the main branch
* Communicate with collaborators to agree on the final stable version after any pull requests, and merge the adaptation into the main branch.


#### Refer to: 
* [Git(Hub) tutorial by INBO](https://inbo.github.io/tutorials/tutorials/git_introduction/)
* [Styleguide R code by INBO](https://inbo.github.io/tutorials/tutorials/styleguide_r_code/)  


### Acknowledgments  
This evaluation is based on data that was collected by partners of the official [UNECE ICP Forests Network](https://www.icp-forests.net/). Part of the data was co-financed by the European Commission. The Intellectual Property and Publication Policy (IPPP) covers rules, rights and obligations to be applied for data supply, request, and use from the official ICP Forests PCC Collaborative Database. 

### Contact  
For any questions or suggestions related to this project, please contact the Forest Soil Coordinating Centre (FSCC) (<fscc@inbo.be>) of [ICP Forests](http://icp-forests.net/) at the [Research Institute for Nature and Forest (INBO)](https://www.vlaanderen.be/inbo/en-gb/researchdomains/data-infrastructuur/international-cooperation-programmes/) in Flanders, Belgium:  

* Bruno De Vos: <bruno.devos@inbo.be>
* Nathalie Cools: <nathalie.cools@inbo.be>

The Programme Co-ordinating Centre (PCC) (<PCC-ICPForests@thuenen.de>) of ICP Forests at Thünen Institute of Forest Ecosystems is entrusted with a broad range of tasks in the fields of programme management, data processing, evaluations, and reporting.



