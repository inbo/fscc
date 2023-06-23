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

In this project, we aim to preprocess the raw solid soil data of ICP Forests to validated, gap-filled and harmonised versions (Layer 1) and further to aggregated versions (Layer 2), and process these data for a range of purposes, including the calculation of carbon and macronutrient stocks (nitrogen, phosphorus, sulphur), exchangeable element pools (calcium, magnesium, potassium, sodium, aluminium, iron, manganese), and indicators for nutrient availability, heavy metal availability and acidification, in both forest floors and mineral forest soils across Europe. 
Observed differences between the soil surveys before versus after the year 2000 are highlighted, and stratified by biogeographical region, forest and soil type, and their spatial autocorrelation with nitrogen deposition levels and link with data from other surveys (e.g. soil solution) is assessed.
<!-- description: end -->



### Repository overview  
This repository provides data-generating (i.e. preprocessing) and data-processing workflows, to support reproducible and transparent analyses on the solid soil data collected through the ICP Forests monitoring network. The aim of this R project is to convert, transform, and harmonise the raw data forms ("layer 0") into validated and gap-filled data forms ("layer 1"), and into aggregated data forms ("layer 2"). These "layer 1" and "layer 2" data forms are then used for a range of other purposes, including the calculation of soil carbon stocks in European forests. The data preprocessing addresses various aspects, including error correction, incorporation of additional data, harmonisation, validation, and aggregation (e.g. across repetitions). The ultimate aim is to achieve open and reproducible data workflows, as a requirement for qualifiable science and for collaboration.

### Project structure  
The repository is structured as follows:

* **src** folder: Contains all the R scripts for data processing and analysis. This includes subfolders:
  + **functions**: Custom-made R functions
  + **transformation_to_layer1**: R scripts to transform the data from "Layer 0" to "Layer 1"
  + **transformation_to_layer2**: R scripts to transform the data from "Layer 1" to "Layer 2"
  + **pir_generation**: R scripts used to generate Partner Inconsistency Reports
  + **stock_calculations**: R scripts to calculate and visualise different kinds of stocks
  + **pathfinder**: R scripts specific to the HorizonEU PathFinder project 
  + Other `src/` subfolders organised per output type
  + **sandbox**: R scripts for individual exploration and testing (ignored by Git) 
* **data** folder: Contains all versions of the data forms and additional data:
  + **raw_data**: Raw data forms ("layer 0") obtained from ICP Forests
  + **intermediate_data**: Intermediate versions of the data forms (between "layer 0" and "layer 1" + between "layer 1" and "layer 2"), organised through so-called "breakpoints".
  + **layer1_data**: Processed, gap-filled, and harmonised data forms
  + **layer2_data**: Aggregated data forms
  + **additional_data**: Additional data forms, e.g. data collected through direct communication with partners (mainly in response to the so-called "Partner inconsistency reports" (PIRs) - stored in subfolder **pir_checked**)
  + **sensitive_metadata**: Sensitive information, e.g. the link to the private Google Drive folder where data are stored
* **output** folder: Stores outputs of this repository which do not fall under data forms, organised per output type:
  + **stocks**: Output regarding stocks of total soil organic carbon, nitrogen, semi-total macro-element pools (P, K, Ca, Mg, S), exchangeable element pools (K, Ca, Mg, Al, Fe, Mn), heavy metal stocks...
  + **indices**: Output regarding derived indices/metrics which are not a part of the "layer 1" or "layer 2" data, such as nutrient availability/soil fertility indices, acidification indices, pollution indices
  + **physical_data**: Output regarding water availability and water budget modelling
  + **links_other_surveys**: Output regarding the links of the solid soil data with other surveys, such as "soil solution"
  + **links_n_deposition**: Output regarding the spatial autocorrelation with N deposition data"
  + **pirs**: Newly generated partner inconsistency reports  
  + **pathfinder**: Output related to the HorizonEU PathFinder project


```
    .  
    ├── fscc.Rproj  
    ├── README.md  
    ├── .gitignore  
    ├── src
    │   ├── functions                            <- Git
    │   ├── transformation_to_layer1             <- Git
    │   ├── transformation_to_layer2             <- Git
    │   ├── pir_generation                       <- Git
    │   ├── stock_calculations                   <- Git
    │   ├── pathfinder                           <- Git
    │   ├── [...]                                <- Git
    │   └── sandbox                              <- GITIGNORE
    ├── output                                   <- GITIGNORE
    │   ├── stocks  
    │   ├── indices
    │   ├── physical_data
    │   ├── links_other_surveys
    │   ├── links_n_deposition
    │   ├── pirs  
    │   ├── pathfinder  
    │   └── [...]  
    └── data  
        ├── sensitive_metadata                   <- GITIGNORE - Google Drive URL
        ├── raw_data                             <- GITIGNORE
        │   ├── 436_s1_20221116152213  
        │   ├── […]  
        │   └── 436_so_20221116152441  
        ├── intermediate_data                    <- GITIGNORE
        │   ├── 0_01_intermediate_data
        │   │   ├── 0_01_s1
        │   │   │   ├── s1_lqa.csv
        │   │   │   ├── s1_pfh.csv
        │   │   │   ├── s1_pls.csv
        │   │   │   ├── s1_prf.csv
        │   │   │   ├── s1_som.csv
        │   │   │   ├── coordinates_s1.csv
        │   │   │   └── data_availability_s1.csv
        │   │   ├── 0_01_si
        │   │   ├── 0_01_so
        │   │   ├── 0_01_sw
        │   │   └── 0_01_y1
        │   ├── 0_02_intermediate_data  
        │   ├── […]  
        │   ├── 1_01_intermediate_data
        │   ├── […]  
        │   └── additional_intermediate_data     <- GITIGNORE
        │       ├── d_country.csv
        │       └── d_partner.csv
        ├── layer1_data                          <- GITIGNORE
        ├── layer2_data                          <- GITIGNORE
        └── additional_data                      <- Git (?)
            ├── pir_checked
            └── shapefiles  
```


### Project output  
The expected output of the code in this repository is:

* "layer 1" versions of the data forms of the "s1" (Level I) and "so" (Level II) surveys
* "layer 2" versions of the data forms of the "s1" (Level I) and "so" (Level II) surveys
* Derived parameters, including stocks of total soil organic carbon, nitrogen, semi-total macro-element pools (P, K, Ca, Mg, S), exchangeable element pools (K, Ca, Mg, Al, Fe, Mn), heavy metal stocks, acidification indices, pollution indices - evaluate whether this applies to layer 1 data (e.g. stocks) or layer 2.
* Harmonised stratification data, including soil classifications (WRB 2014), humus forms, ecocluster classes and soil fertility indices
* Assessments of water availability + compilation of data for water budget modelling (LWF-Brook90)
* Link with other surveys (e.g. soil solution)
* Link with N deposition data
* Partner inconsistency reports (PIRs) with issues in the data (corrections/responses by the partners was requested in the period of March - May 2023)
* Results of (geo)statistical analyses
* Rmarkdown reports with results (graphs and interactive maps) per partner, to collect feedback on the plausibility of these results by the partners where needed (?)

### Workflow and collaboration  
#### R code version control
We use Git and GitHub to facilitate collaboration and version control. The main branch serves as the primary branch where versions of the R code - for step-wise transformation of data from "layer 0" to "layer 1" and subsequently to "layer 2" and to derived variables such as carbon stocks - are tracked through different commits. Only source code (in the `./src/` folder) and additional data (in the `./data/additional_data/` folder) are version-tracked, while data and output are stored in private Google Drive folders and the local project folder (ignored by Git).

#### Dataset management
All individual steps of the data (pre)processing for a certain output type are compiled, documented and applied using one Rmarkdown document per output type, such as `./src/solid_soil_data_transformation_to_layer1.Rmd` for the transformation of "layer 0" to "layer 1".

The two data preprocessing scripts (for the transformation to "layer 1" and for the transformation to "layer 2") contain several so-called **breakpoints**: these are important intermediate points in the Rmarkdown script in which intermediate versions of the data forms (processed with the steps preceding this breakpoint) are stored (in the folder `./output/intermediate_data/`). As such, these breakpoints allow to divide the data transformation process into manageable steps and save the data at specific points to avoid re-running the entire script from the beginning (this way saving processing time). Each breakpoint and the related intermediate data form versions are named as follows: *"[X]_[YY]_intermediate_data"* (e.g. "0_01_intermediate_data"), with:
* *[X]*: one number which refers to the preceding layer of the data, i.e. "0" for intermediate data between "layer 0" and "layer 1", and "1" for intermediate data between "layer 1" and "layer 2"
* *[YY]*: two numbers which refer to the breakpoint, e.g. "01" for the first breakpoint. Per "layer" (i.e. per unique *[X]*), numbers always start from 01 and go up.

If a collaborator wants to change the data transformation steps between breakpoints (x) and (x+1), please import the data forms from breakpoint (x), run the adjusted code starting from breakpoints (x), and (re)save the data forms at breakpoint (x+1) (and any further break points). Rule: only create a new breakpoint or update a breakpoint if it takes a considerable time to process data from the preceding breakpoint onwards, otherwise rerunning the code is preferable.
(idea: last breakpoint in YAML params?)

In accordance with the ICP Forests policies, data are currently not open, and **storage** relies on a combination of:
* A private Google Drive folder - advantages:
  + Sharing with collaborators, to ensure everyone is working with the same up-to-date data
  + Backup and recovery
  + Versioning: different versions of the datasets can be saved on Google Drive, including history
  + Reduced local storage
  + Access control
* Your local R project folder (ignored by Git) for the most recent versions - advantages:
  + Offline access and reduced dependency on internet
  + Flexibility (e.g. using local file paths)
  + Improved performance (e.g. faster read access)
  
Whenever you create a new version of the data that you want to save (e.g. update of an intermediate breakpoint, new breakpoint, layer 1, layer 2...), you are expected to save the data on Google Drive. This ensures that all collaborators have access to the latest versions of the data. 
Whenever you start working on the project, it is recommended to synchronise your local data with the data on Google Drive. The data on Google Drive serves as the central repository where all versions of the data are stored, while only the most recent versions of the data are stored locally in the R project folder. By synchronising, you ensure that you have the most up-to-date data available for your analysis or development in R.


<img src="https://github.com/inbo/fscc/assets/125061803/52e47167-21dc-421e-9f42-30e51702e8b1" width="80%">


#### Specific project coding conventions
It is recommended to compile the code for larger manipulations and/or manipulations which are often repeated, into one function, which is placed in the `./src/functions/` folder. Such functions are sourced ad hoc in code chunks using the `source()` function. Custom-made functions should be applicable to all data forms in Level I and Level II, and clearly documented using comments and Roxygen documentation (Code > Insert Roxygen Skeleton). In the global environment of R, all objects (data frames) referring to data forms between layer 0 and layer 2 are by convention named using lower case letters without any version number attached to it.

### Contributing  
Please follow this workflow:

#### Getting started: first time
* **Clone** the repository to your local computer to work on the project.
* Review the README file for an overview of the project structure and collaboration instructions.
* Create or paste an R script `./data/sensitive_metadata/google_drive_link.R`, which contains the URL of the private Google Drive folder which contains the data, as explained below.
* Download the data from the Google Drive folder using the `sync_local_data()` function.

Follow these steps to derive and save the URL of the Google Drive root folder:

* Go to the root folder on Google Drive, i.e. a folder with data which contains the following subfolders:

```
    .  
    ├── data
    │   ├── raw_data
    │   ├── intermediate_data
    │   ├── layer1_data
    │   └── layer2_data
    └── output
        ├── stocks
        └── [...]
```

* Right-click on the root folder and select "Copy link".
* Paste the link somewhere. It should look like  "https://drive.google.com/drive/folders/1Txxxxxxxe7?usp=drive_link". Copy the part you need, i.e. everything starting from two characters before the first x until two characters after the last x (i.e. before any question mark). The two characters before and after the x string are a random example.
* Create a new R script in the local folder "./data/sensitive_metadata/" and give it the following name: "google_drive_link.R".
* Paste the code below in this script:

```
# This is the URL of the Google Drive root folder which contains data related to
# the fscc project of ICP Forests. The data in this folder are sensitive and
# are only available for collaborators with access to the Google Drive folder.
root_drive <- "1Txxxxxxxe7"
```

* Replace the character string after `<-` with the actual code from the link as explained above.


#### Getting started: next times
* **Pull** changes from the repository before starting your work to ensure you have the latest code and datasets from other team members.
* Synchronise your local data with the up-to-date Google Drive folder using the `sync_local_data()` function.
* *Remove any local branches which were merged with the main branch on GitHub*

#### Adjust the data transformation code
* Open the main R Markdown script for the given output type (e.g. `./src/solid_soil_data_transformation_to_layer1.Rmd`) to access the data transformation workflow.
* Review the code and breakpoints in the R Markdown script to understand the different stages of the data transformation process.
* Determine the phase of the R markdown script in which your changes will be made.
* Import of the versions of the data forms from the breakpoint preceding the phase where you will adjust the data transformation code.
* *Create a new branch*
* Adjust the data transformation code as needed.
* If any next breakpoint(s) already exists: run the script from the last breakpoint onwards to update intermediate data forms.
* If no further breakpoint(s) exist while a new breakpoint would be meaningful: create and name a new breakpoint, and run the script up to this breakpoint.
* Save the updated intermediate data forms on Google Drive when needed.
* Regularly **commit** changes, including the new or updated intermediate data forms, using clear and concise commit messages (to describe the modifications made to the code and datasets), and **push** them to the remote GitHub repository.
* *When edits on the branch are ready: create a pull request on GitHub to merge the branch with the main branch*
* *Communicate with collaborators to inform them about any new breakpoint(s) and pull requests, and merge the adaptation into the main branch.*



#### Rules for collaboration (based on INBO)

* Commit often, make small commits
* Do not mix changes in one commit
* Think about your commit messages (try to avoid "Update..." etc)
* Always commit into a feature branch, never in the main branch.
* Always start features branches from the main branch.
* Only work in your own branches.
* Never merge someone else's pull request without their consent.
* Do not keep long-lived branches (form of technical debt)

#### Refer to: 
* [Git(Hub) tutorial by INBO](https://inbo.github.io/tutorials/tutorials/git_introduction/)
* [Styleguide R code by INBO](https://inbo.github.io/tutorials/tutorials/styleguide_r_code/)  


### Acknowledgments  
This evaluation is based on data that was collected by partners of the official [UNECE ICP Forests Network](http://icp-forests.net/contributors). Part of the data was co-financed by the European Commission. The Intellectual Property and Publication Policy (IPPP) covers rules, rights and obligations to be applied for data supply, request, and use from the official ICP Forests PCC Collaborative Database (ICP Forests DB). 

### Contact  
For any questions, suggestions, or inquiries related to this project, please contact the Forest Soil Coordinating Centre (FSCC) (<fscc@inbo.be>) of [ICP Forests](http://icp-forests.net/) at the [Research Institute for Nature and Forest (INBO)](https://www.vlaanderen.be/inbo/en-gb/researchdomains/data-infrastructuur/international-cooperation-programmes/) in Flanders, Belgium:  

* Bruno De Vos: <bruno.devos@inbo.be>
* Nathalie Cools: <nathalie.cools@inbo.be>
* Heleen Deroo: <heleen.deroo@inbo.be>  

The Programme Co-ordinating Centre (PCC) (<PCC-ICPForests@thuenen.de>) of ICP Forests at Thünen Institute of Forest Ecosystems is entrusted with a broad range of tasks in the fields of programme management, data processing, evaluations, and reporting.



