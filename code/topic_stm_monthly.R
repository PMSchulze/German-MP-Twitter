# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "huge", "reshape2", "stm", "stringi", "tidyverse", "tm"
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
# ---------------------------------- Hyperparameter Search -------------------------------------
# ----------------------------------------------------------------------------------------------

# load data
data <- readRDS("./data/preprocessed_monthly.rds")
colnames_table <- read.csv(file = "./data/topic_monthly_colnames.csv")
data_corpus <- readRDS("./data/prep_monthly.rds")

# extract documents, vocabulary and metadata
docs <- data$documents
vocab <-  data$vocab
meta <- data$meta

# search hyperparameter space for optimal K
hyperparameter_search <- searchK(
  documents = docs,
  vocab = vocab,
  data = meta,
  # K = c(5,6,7,8,9,10),
  K = c(5,10,15,20,25,30,35,40),
  prevalence =~ Partei,
  max.em.its = 150,
  init.type = "Spectral"
)

saveRDS(hyperparameter_search, "./data/searchK_large_data.rds")
searchK_data <- readRDS("./data/searchK_large_data.rds")
plot(searchK_data)

# ----------------------------------------------------------------------------------------------
# ---------------------------------- Prevalence Model Fitting ----------------------------------
# ----------------------------------------------------------------------------------------------

# choose covariates and number of topics
prev_var <- c("Partei", "Bundesland", "Datum", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54")
outcome <- ""
prevalence <- as.formula(paste(outcome, paste(prev_var, collapse = "+"), sep = "~")) 

K <- 15

# fit model
mod_prev <- stm::stm(
  documents = docs,
  vocab = vocab,
  data = meta,
  K = K,
  prevalence =~ Partei,
  gamma.prior = 'L1',
  seed = 123,
  max.em.its = 350,
  init.type = "Spectral")

# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Labelling -------------------------------------------
# ----------------------------------------------------------------------------------------------

# labeling workflow (for each topic): 

## (1) inspect most frequent words per topic (using different metrics as well as word cloud)
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
topic_words <- labelTopics(mod_prev, n = 8)

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
topic_number <- 6

# ----------------------------------------------------------------------------------------------

# actual labelling porcess

## (1) inspect most frequent words per topic
topic_words$prob[topic_number,] # 20 most frequent words

# logbeta_matrix <- mod_prev$beta$logbeta[[1]]
# mod_prev$vocab[which.max(logbeta_matrix[topic_number,])] # most frequent word directly from (log)beta vector

## (2) evaluate most representative documents per topic
topic_number_long <- paste0("Topic", topic_number)
data_corpus$docname <- paste0(data_corpus$Twitter_Username, "_", data_corpus$Jahr, "_", data_corpus$Monat)

repr_docs <-  topic_props %>%
  arrange(desc(!!as.symbol(topic_number_long))) %>%
  .[1:docs_number, c("Name", "docname", "Partei", topic_number_long)] %>%
  left_join(data_corpus[,c("Tweets_Dokument", "docname")], 
            by = "docname") %>%
  mutate(Tweets_Dokument = substr(Tweets_Dokument, 50000, 52000))
repr_docs[1,] # view i-th most representative document



topic_props %>%
  arrange(desc(Topic2)) %>%
  .[1:docs_number,c("Name", "docname", "Partei", paste0("Topic", topic_number))] %>%
  left_join(select(data_corpus, "Tweets_Dokument", "docname"), by = "docname") %>%
  select(c("Name", "docname", "Partei", paste0("Topic", topic_number), "Tweets_Dokument"))



## (3) assign label
topic_labels[[topic_number]] <- "Green"

## (3) assign label
topic_labels <- c("right/national",
                  "green/climate",
                  "future/digital",
                  "Europe",
                  "left/social",
                  "miscellaneous")


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
# -------------------------------- Global Topic Inspection -------------------------------------
# ----------------------------------------------------------------------------------------------

# all topics w/ global proportions (prints topic words with their corpus frequency)
plot(mod_prev, type = "summary", xlim = c(0, 0.3), custom.labels = topic_labels) # average thetas across all MPs 

doc_lengths <- lapply(docs[], length)
weights <- c()
i <- 1
while (i <= length(doc_lengths)) {
  weights[[i]] <- doc_lengths[[i]]/2
  i <- i + 1}
mean_weight <- mean(weights)
props_unweighted <- colMeans(mod_prev$theta[,1:K])
props_weighted <- colMeans((mod_prev$theta*weights/mean_weight)[, 1:K])

props_df <- data.frame(topic_labels, props_unweighted, props_weighted) %>% reshape2::melt(id = "topic_labels")
colnames(props_df) <- c("topic", "variable", "proportion")

ggplot(data = props_df, aes(x = topic, y = proportion, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("grey40","grey80")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

# vocabulary usage across two topics
plot(mod_prev, type = "perspectives", topics = c(3, 4), n = 30)

# global topic correlation
mod_prev_corr <- topicCorr(mod_prev, method = "simple", cutoff = -0.20,
                           verbose = TRUE) # based on correlations between mod_prev$theta
plot.topicCorr(mod_prev_corr)

cormat <- cor(mod_prev$theta)
cormat
