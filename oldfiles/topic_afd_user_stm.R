# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "huge", "stm", "stringi", "tidyverse", "tm"
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
# setwd("/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse")

# ----------------------------------------------------------------------------------------------
# ---------------------------------------------- STM -------------------------------------------
# ----------------------------------------------------------------------------------------------

# load data
data_train <- readRDS("./data/topic_afd_user_train_preprocessed_no#.rds")
data_test <- readRDS("./data/topic_afd_user_test_preprocessed_no#.rds")
colnames_table <- read.csv(file = "./data/topic_user_colnames.csv")

# extract documents, vocabulary and metadata
docs_train <- data_train$documents
vocab_train <-  data_train$vocab
meta_train <- data_train$meta

# search hyperparameter space for optimal K
hyperparameter_search <- searchK(documents = docs,
                                 vocab = vocab,
                                 K = c(5,6,7,8,9,10),
                                 init.type = "Spectral",
                                 proportion = 0.2, heldout.seed = 123, M = 10)

# set CDU/CSU as reference category for variable "Partei"
meta$Partei <- meta$Partei %>%
  as.factor() %>%
  relevel(ref = 3)

# topical prevalence
mod_prev <- stm::stm(
  documents = docs_train,
  vocab = vocab_train,
  data = meta_train,
  K = 6,
  prevalence =~ Bundesland,
  max.em.its = 75,
  init.type = "Spectral")

# align corpus
data_test <- alignCorpus(new = data_test, old.vocab = mod_prev$vocab, verbose = TRUE)

docs_test <- data_test$documents
vocab_test <-  data_test$vocab
meta_test <- data_test$meta

# fit new documents
test <- fitNewDocuments(model = mod_prev, documents = docs_test, newData = meta_test,
                origData = meta_train,
                prevalence =~ Bundesland,
                returnPosterior = FALSE,
                returnPriors = FALSE, designMatrix = NULL, test = TRUE,
                verbose = TRUE)


### top words per topic
labelTopics(mod_prev)

### all topics with overall proportions
plot(mod_prev, type = "summary", xlim = c(0, 0.3))

### manually label topics
mod_prev_labels <- c("miscellaneous",
                     "left/social",
                     "Europe",
                     "right/national",
                     "miscellaneous",
                     "green/climate")

### word clouds
cloud(mod_prev, topic = 6, scale = c(2, 0.25))

### vocabulary usage across two topics
plot(mod_prev, type = "perspectives", topics = c(2, 4))

### global topic correlation
mod_prev_corr <- topicCorr(mod_prev, method = "simple", cutoff = -0.25,
                           verbose = TRUE)
plot.topicCorr(mod_prev_corr)

## metadata/topic relationship
meta$Bundesland <- as.factor(meta$Bundesland)
prep <- stm::estimateEffect(1:6 ~ Partei + Bundesland + s(Struktur_4)
                            + s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
                            mod_prev,
                            metadata = meta,
                            uncertainty = "Global")
summary(prep, topics = 6)

### difference between two parties regarding topic prevalence
plot(prep, "Partei", method = "difference", topics = c(1,2,3,4,5,6), cov.value1 = "CDU/CSU",
     cov.value2 = "SPD", model = mod_prev, xlab = "more SPD          ...    more CDU/CSU",
     labeltype = "custom", custom.labels = mod_prev_labels)

### prevalence of topic 6 for all states
plot(prep, "Bundesland", method = "pointestimate", topics = 6,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "Bundesland")

### impact of immigrant share on prevalence of topic "left/social"
plot(prep, "Struktur_4", method = "continuous", topics = 2,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "% immigrants",
     ylim = c(0, 0.30))
axis(1, tick = TRUE)

### impact of immigrant share on prevalence of topic "right/national"
plot(prep, "Struktur_4", method = "continuous", topics = 4,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "% immigrants",
     ylim = c(0.05, 0.25))
axis(1, tick = TRUE)

### impact of immigrant share on prevalence of topic "green/climate"
plot(prep, "Struktur_4", method = "continuous", topics = 6,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "% immigrants",
     ylim = c(0, 0.15))
axis(1, tick = TRUE)

### impact of immigrant share on prevalence of topic "Europe"
plot(prep, "Struktur_4", method = "continuous", topics = 3,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "% immigrants",
     ylim = c(0.1, 0.25))
axis(1, tick = TRUE)

## prevalence model w/ interaction
mod_prev_int <- stm::stm(
  documents = docs,
  vocab = vocab,
  data = meta,
  K = 6,
  prevalence =~ Partei + Bundesland + Struktur_4
  + Struktur_22 + Struktur_42 + Struktur_54 + Partei*Struktur_42,
  max.em.its = 75,
  init.type = "Spectral")

### interaction party and unemployment for left/social topic
covariate <- "Struktur_42"
x_axis_label <- "Unemployment (in %)"
moderator <- "Partei"

prep <- estimateEffect(c(2) ~ Partei * Struktur_42, mod_prev_int,
                       metadata = meta, uncertainty = "None")

plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "CDU/CSU", linecol = "black", ylim = c(0, 0.70),
     printlegend = FALSE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "SPD", linecol = "red", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "Bündnis 90/Die Grünen", linecol = "green", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "Die Linke", linecol = "purple", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "FDP", linecol = "yellow", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "AfD", linecol = "blue", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)

### interaction party and GDP per capita for left/social topic
covariate <- "Struktur_22"
x_axis_label <- "GDP per capita"
moderator <- "Partei"

prep <- estimateEffect(c(2) ~ Partei * Struktur_22, mod_prev_int,
                       metadata = meta, uncertainty = "None")

plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "CDU/CSU", linecol = "black", ylim = c(0, 0.70),
     printlegend = FALSE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "SPD", linecol = "red", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "Bündnis 90/Die Grünen", linecol = "green", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "Die Linke", linecol = "purple", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "FDP", linecol = "yellow", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "AfD", linecol = "blue", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)

### interaction party and percentage of immigrants for left/social topic
covariate <- "Struktur_4"
x_axis_label <- "immigrant percentage"
moderator <- "Partei"

prep <- estimateEffect(c(3) ~ Partei * Struktur_4, mod_prev_int,
                       metadata = meta, uncertainty = "None")

plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "CDU/CSU", linecol = "black", ylim = c(0, 0.70),
     printlegend = FALSE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "SPD", linecol = "red", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "Bündnis 90/Die Grünen", linecol = "green", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "Die Linke", linecol = "purple", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "FDP", linecol = "yellow", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = covariate, model = mod_prev_int,
     method = "continuous", xlab = x_axis_label, moderator = moderator,
     moderator.value = "AfD", linecol = "blue", ylim = c(0, 0.70),
     printlegend = FALSE, add = TRUE)

# topical content
mod_topic <- stm::stm(
  documents = docs,
  vocab = vocab, 
  data = meta,
  K = 6,
  prevalence =~ Partei + Bundesland + s(Struktur_4)
  + s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  content =~ Partei,
  max.em.its = 75,
  init.type = "Spectral")

### top words per topic
labelTopics(mod_topic, n = 10)

### difference between two parties and one topic regarding vocabulary usage
plot(mod_topic, type = "perspectives", topics = 2)

### all topics with overall proportions
plot(mod_topic, type = "summary", xlim = c(0, 0.3))

### word clouds
cloud(mod_prev, topic = 6, scale = c(2, 0.25), labeltype = "prob")

