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
data <- readRDS("./data/preprocessed_monthly.rds")
data_corpus <- readRDS("./data/prep_monthly.rds")
# data_train <- readRDS("./data/preprocessed_monthly_train.rds")
# data_test <- readRDS("./data/preprocessed_monthly_test.rds")

n_topics <- 15

mod_prev <- stm::stm(
  documents = data$documents,
  vocab = data$vocab,
  data = data$meta,
  K = n_topics,
  prevalence =~ as.factor(Partei) + as.factor(Bundesland) + s(4) + s(22) + s(42) + s(54) + s(t),
  gamma.prior = 'L1',
  seed = 123,
  max.em.its = 100,
  init.type = "Spectral"
)

plot(mod_prev, type = "summary", xlim = c(0, 0.5), n=8)

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Labelling -------------------------------------------
# ----------------------------------------------------------------------------------------------

# labeling workflow (for each topic): 

## (1) inspect most frequent words per topic
## (2) evaluate most representative documents per topic
## (3) assign label

# ----------------------------------------------------------------------------------------------

# first, prepare objects/variables needed for labelling process

## table of MAP topic proportions per document (for all topics)
topic_props <- make.dt(
  mod_prev, 
  data$meta[c("Name", "Partei", "Bundesland")]) %>% 
  cbind(docname = names(data$documents), .)

## top words per topic (for all topics)
topic_words <- labelTopics(mod_prev, n = 20)

## number of top documents to be printed in step (2)
docs_number <- 5

## initialize list with empty labels
topic_labels <- list(
  Topic1 = NULL,
  Topic2 = NULL,
  Topic3 = NULL,
  Topic4 = NULL,
  Topic5 = NULL,
  Topic6 = NULL
)

# ----------------------------------------------------------------------------------------------

# specify topic that should be labelled
topic_number <- 2

# ----------------------------------------------------------------------------------------------

# actual labelling porcess

## (1) inspect most frequent words per topic
cloud(mod_prev, topic = topic_number, scale = c(2.5, 0.25)) # word cloud
topic_words$prob[topic_number,] # 20 most frequent words

## (2) evaluate most representative documents per topic
topic_name <- paste0("Topic", topic_number)
repr_docs <-  topic_props %>%
  arrange(desc(!!as.symbol(topic_name))) %>%
  .[1:docs_number, c("Name", "Partei", topic_name)] %>%
  left_join(data_corpus[,c("Tweets_Dokument", "Twitter_Username")], 
            by = "Twitter_Username") %>%
  mutate(Tweets_Dokument = substr(Tweets_Dokument, 50000, 52000))
repr_docs[1,] # view i-th most representative document

## (3) assign label
topic_labels[[topic_number]] <- "Green"

# ----------------------------------------------------------------------------------------------

# results

# topic_labels <- list(
#   Topic1 = "Miscellaneous",
#   Topic2 = "Work & Family",
#   Topic3 = "Left/Social",
#   Topic4 = "Green/Miscellaneous",
#   Topic5 = "Miscellaneous",
#   Topic6 = "Green"
# )

# ----------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------
# ----------------------------------- Estimate Effects -----------------------------------------
# ----------------------------------------------------------------------------------------------

prep <- stm::estimateEffect(
  1:6 ~ as.factor(Partei),
  mod_prev,
  documents = data$documents,
  metadata = data$meta,
  uncertainty = "Local"
)
summary(prep, topics = 1)

plot(prep, "Partei", method = "difference", topics = c(5,6), cov.value1 = "Bündnis 90/Die Grünen",
     cov.value2 = "AfD", model = mod_prev, xlab = "Partei")
