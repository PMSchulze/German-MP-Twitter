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

data <- readRDS("./data/searchK_large_data.rds")

plot(data$results$exclus,data$results$semcoh, type = "n", 
     xlab = "Exclusivity", ylab = "Semantic Coherence")
text(data$results$exclus[1],data$results$semcoh[1],label=5,col='blue')
text(data$results$exclus[2],data$results$semcoh[2],label=10,col='green')
text(data$results$exclus[3],data$results$semcoh[3],label=15,,col='red')
text(data$results$exclus[4],data$results$semcoh[4],label=20,,col='brown')
text(data$results$exclus[5],data$results$semcoh[5],label=30,,col='purple')

