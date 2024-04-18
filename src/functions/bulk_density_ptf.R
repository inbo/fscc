
# Pedotransfer functions for bulk density estimates + uncertainty

# 1. Mineral layers ----

# Expected relationship (BD-PTF) with TOC:
# BD = 1511 – (81,1*sqrt(TOC))

# This is based on the analysis of layer 1 "s1" data (using different models
# and predictors) and validation using the layer 1 "so" data
# (in Feb/March 2024) - cfr. script [TO DO: ADD SCRIPT]

bd_ptf <- function(toc) {
  ifelse(is.na(toc),
         NA,
         ifelse(
           (round(1511 - (81.1 * sqrt(toc)))) < 6,
           6,
           round(1511 - (81.1 * sqrt(toc)))))
}

# Upper limit plausibility function ULPF = 1991 - (81,1*sqrt(TOC))
bd_upper <- function(toc) {
  ifelse(is.na(toc),
         1991,
         ifelse(
           (1991 - (81.1 * sqrt(toc))) < 6,
           6,
           1991 - (81.1 * sqrt(toc))))
}

# Lower limit plausibility function LLPF = 1031 - (81,1*sqrt(TOC))
# Use an ultimate minimum of 6
# (i.e. the lower 0.05 % quantile of all organic bulk density data)
# Else, some organic layers contain bulk densities of 1 or so,
# which is highly unrealistic
bd_lower <- function(toc) {
  ifelse(is.na(toc) | (1031 - (81.1 * sqrt(toc)) < 6),
         6,
         1031 - (81.1 * sqrt(toc)))
}

# Uncertainty in PTF
# (basically the same like bd_upper and bd_lower, respectively)

uncertainty_ptf <- 160 # kg m-3

bd_ptf_upper <- function(bd) {
  ifelse(is.na(bd),
         NA,
         ifelse((round(bd + uncertainty_ptf) > 2650),
                2650,
                round(bd + uncertainty_ptf)))
}

bd_ptf_lower <- function(bd) {
  ifelse(is.na(bd),
         NA,
         ifelse((round(bd - uncertainty_ptf) < 0),
                0,
                round(bd - uncertainty_ptf)))
}


# 2. Organic layers ----

# Mean bulk density for forest floor

source("./src/functions/get_parameter_stats.R")

bd_ff <- get_parameter_stats(parameter = "bulk_density",
                             mode = "stat",
                             layer_type = "forest_floor_excl_ol")

bd_ff <- bd_ff[which(names(bd_ff) == "Mean")]


# Mean bulk density for peat

source("./src/functions/get_parameter_stats.R")

bd_peat <- get_parameter_stats(parameter = "bulk_density",
                               mode = "stat",
                               layer_type = "peat")

bd_peat <- bd_peat[which(names(bd_peat) == "Mean")]







