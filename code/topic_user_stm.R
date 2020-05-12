# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "quanteda","stm", "stringi", "tidyverse", "tm"
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
setwd('C:\\Users\\Simon\\OneDrive\\Uni\\LMU\\SS 2020\\Statistisches Consulting\\Bundestag-MP-Analyse')

# ----------------------------------------------------------------------------------------------
# ---------------------- STM -----------------------
# ----------------------------------------------------------------------------------------------

# load data
topic_user_preprocessed <- readRDS("./data/topic_user_preprocessed.rds")
colnames_table <- read.csv(file = "./data/colnames_table.csv")

# extract documents, vocabulary and metadata
docs <- topic_user_preprocessed$documents
vocab <-  topic_user_preprocessed$vocab
meta <- topic_user_preprocessed$meta

# set CDU/CSU as reference category for variable "Partei"
meta$Partei <- meta$Partei %>%
  as.factor() %>%
  relevel(ref = 3)

# prevalence model
mod_prev <- stm::stm(
  documents = docs,
  vocab = vocab,
  data = meta,
  K = 7,
  prevalence =~ Partei + Bundesland + s(Geburtsjahr) + v_4 + v_22 + v_42,
  max.em.its = 50,
  init.type = "Spectral")

### top words per topic
labelTopics(mod_prev)

### summary visualization
plot(mod_prev, type = "summary", xlim = c(0, 0.3))

### relationship topic/vocabulary
plot(mod_prev, type = "perspectives", topics = c(3, 6))

### estimating metadata/topic relationship
meta$Bundesland <- as.factor(meta$Bundesland)
prep <- stm::estimateEffect(1:6 ~ Partei + Bundesland + s(Geburtsjahr) + v_4 + v_22 + v_42,
                            mod_prev,
                            metadata = meta,
                            uncertainty = "Global")
summary(prep, topics = 1)

### metadata/topic relationship visualization
plot(prep, "Partei", method = "difference", topics = c(1,2,3,4,5,6), cov.value1 = "CDU/CSU",
     cov.value2 = "SPD", model = mod_prev, xlab = "Partei")

plot(prep, "Bundesland", method = "pointestimate", topics = 6,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "Bundesland")

# monthseq <- seq(from = as.Date("2008-01-01"),
#                 to = as.Date("2008-12-01"), by = "month")
# monthnames <- months(monthseq)
# axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
#      labels = monthnames)

plot(prep, "v_42", method = "continuous", topics = 5,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "BIP pro Kopf")
# unemployment_range <- seq(from = 0,
#                 to = 0.45, by = 0.05)
# monthnames <- months(monthseq)
# axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
#                 labels = monthnames)

# prevalence model w/ interaction
mod_prev_int <- stm::stm(
  documents = docs,
  vocab = vocab,
  data = meta,
  K = 6,
  prevalence =~ Partei + Bundesland + s(Geburtsjahr) + v_4 + v_22 + v_42 + Partei*v_42,
  max.em.its = 50,
  init.type = "Spectral")


prep <- estimateEffect(c(3) ~ Partei * v_42, mod_prev_int,
                       metadata = meta, uncertainty = "None")
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "CDU/CSU", linecol = "black", ylim = c(0, 0.30),
     printlegend = FALSE)
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "SPD", linecol = "red", ylim = c(0, 0.30),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "Bündnis 90/Die Grünen", linecol = "green", ylim = c(0, 0.30),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "Die Linke", linecol = "purple", ylim = c(0, 0.30),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "FDP", linecol = "yellow", ylim = c(0, 0.30),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "AfD", linecol = "blue", ylim = c(0, 0.30),
     printlegend = FALSE, add = TRUE)
# legend(0, 0.06, c("CDU/CSU", "SPD"), lwd = 2,
#         col = c("black", "red"))


# topical content
mod_topic <- stm::stm(
  documents = docs,
  vocab = vocab, 
  data = meta,
  K = 5,
  prevalence =~ Partei + Bundesland + s(Geburtsjahr) + s(v_4) + s(v_22) + s(v_42),
  content =~ Partei,
  max.em.its = 75,
  init.type = "Spectral")

### word cloud
cloud(mod_topic, topic = 4, scale = c(2, 0.25))
