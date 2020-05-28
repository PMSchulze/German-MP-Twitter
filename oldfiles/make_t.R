# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "stm", "stringi", "tidyverse", "tm"
)
not_installed <- packages_required[!packages_required %in%
                                     installed.packages()[, "Package"]]
if (length(not_installed) > 0) {
  lapply(
    not_installed,
    install.packages,
    repos = "http://cran.us.r-project.org",
    dependencies = TRUE,
    type = itype
  )
}
lapply(packages_required, library, character.only = TRUE)

# set working directory
# setwd('C:\\Users\\Simon\\OneDrive\\Uni\\LMU\\SS 2020\\Statistisches Consulting\\Bundestag-MP-Analyse')
setwd("/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse")

# ----------------------------------------------------------------------------------------------
# ---------------------------------------------- STM -------------------------------------------
# ----------------------------------------------------------------------------------------------

# load data
filepath <- "./data/preprocessed_cdu_monthly.rds"
data_preprocessed <- readRDS(filepath)

num_year <- (data_preprocessed$meta$Datum %>% substr(1,4) %>% as.numeric)%%2017
num_month <- (data_preprocessed$meta$Datum %>% substr(6,8) %>% as.numeric)
data_preprocessed$meta$t <- ((num_year*12)+num_month)-8

data_preprocessed$meta <- data_preprocessed$meta %>%
  select(Name, Twitter_Username, t, Datum, everything())

saveRDS(data_preprocessed, filepath)
