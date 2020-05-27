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
data_full <- readRDS("./data/topic_spd_user_preprocessed_no#.rds")
data_train <- readRDS("./data/topic_spd_user_train_preprocessed_no#.rds")
data_test <- readRDS("./data/topic_spd_user_test_preprocessed_no#.rds")
full_corpus <- readRDS("./data/topic_spd_user.rds")
colnames_table <- read.csv(file = "./data/topic_user_colnames.csv")

# search hyperparameter space for optimal K
hyperparameter_search <- searchK(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = c(5,6,7,8,9,10),
  prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
    s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  heldout.seed = 123,
  max.em.its = 50,
  init.type = "Spectral", 
  cores = 2
)

n_topics <- 6

# ----------------------------------------------------------------------------------------------
# topical prevalence
## try different covariate specifications

## start with model without prevalence covariates
mod_prev1 <- stm::stm(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = n_topics,
  seed = 123,
  max.em.its = 75,
  init.type = "Spectral"
)
## add prevalence covariates
mod_prev2 <- stm::stm(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = n_topics,
  prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
    s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  seed = 123,
  max.em.its = 75,
  init.type = "Spectral"
)
## modify prevalence prior
mod_prev3 <- stm::stm(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = n_topics,
  prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
    s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  seed = 123,
  max.em.its = 75,
  init.type = "Spectral",
  gamma.prior = "L1"
)
## modify prevalence prior
mod_prev4 <- stm::stm(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = n_topics,
  prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
    s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  seed = 123,
  max.em.its = 75,
  init.type = "Spectral",
  gamma.prior = "L1",
  control = list(gamma.enet = 0)
)
## modify prevalence prior
mod_prev5 <- stm::stm(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = n_topics,
  prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
    s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  seed = 123,
  max.em.its = 75,
  init.type = "Spectral",
  sigma.prior = 1
)

# evaluate
## semantic coherence & exclusivity
topicQuality(mod_prev1, documents = docs_train)
topicQuality(mod_prev2, documents = docs_train)
topicQuality(mod_prev3, documents = docs_train)
topicQuality(mod_prev4, documents = docs_train)
topicQuality(mod_prev5, documents = docs_train)

## top words per topic
labelTopics(mod_prev1)
labelTopics(mod_prev2)
labelTopics(mod_prev3)
labelTopics(mod_prev4)
labelTopics(mod_prev5)

## topic proportions and top words per topic
plot(mod_prev1, type = "summary", xlim = c(0, 0.5))
plot(mod_prev2, type = "summary", xlim = c(0, 0.5))
plot(mod_prev3, type = "summary", xlim = c(0, 0.5))
plot(mod_prev4, type = "summary", xlim = c(0, 0.5))
plot(mod_prev5, type = "summary", xlim = c(0, 0.5))

# ----------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------
# topical prevalence with heldout likelihood

# prepare object used for evaluating heldout likelihood
heldout <- make.heldout(docs_train, vocab_train, seed = 123)

## again, start with model without prevalence covariates
mod_prev_heldout1 <- stm::stm(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = n_topics,
  seed = 123,
  max.em.its = 75,
  init.type = "Spectral"
)

## again, add prevalence covariates
mod_prev_heldout2 <- stm::stm(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = n_topics,
  prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
    s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  seed = 123,
  max.em.its = 75,
  init.type = "Spectral"
)

## again, modify prevalence prior
mod_prev_heldout3 <- stm::stm(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = n_topics,
  prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
    s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  seed = 123,
  max.em.its = 75,
  init.type = "Spectral",
  gamma.prior = "L1"
)

## again, modify prevalence prior
mod_prev_heldout4 <- stm::stm(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = n_topics,
  prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
    s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  seed = 123,
  max.em.its = 75,
  init.type = "Spectral",
  gamma.prior = "L1",
  control = list(gamma.enet = 0)
)

## again, modify prevalence prior
mod_prev_heldout5 <- stm::stm(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = n_topics,
  prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
    s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  seed = 123,
  max.em.its = 75,
  init.type = "Spectral",
  sigma.prior = 1
)

# evaluate
eval.heldout(mod_prev_heldout1, heldout$missing)
eval.heldout(mod_prev_heldout2, heldout$missing)
eval.heldout(mod_prev_heldout3, heldout$missing)
eval.heldout(mod_prev_heldout4, heldout$missing)
eval.heldout(mod_prev_heldout5, heldout$missing)
topicQuality(mod_prev_heldout1, documents = heldout$documents)
topicQuality(mod_prev_heldout2, documents = heldout$documents)
topicQuality(mod_prev_heldout3, documents = heldout$documents)
topicQuality(mod_prev_heldout4, documents = heldout$documents)
topicQuality(mod_prev_heldout5, documents = heldout$documents)

## main result: prevalence specification does not affect evaluation metrics! due to regularizing prior?
# ----------------------------------------------------------------------------------------------

## main model for subsequent analyses
mod_prev <- stm::stm(
  documents = data_full$documents,
  vocab = data_full$vocab,
  data = data_full$meta,
  K = n_topics,
  prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
    s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  seed = 123,
  max.em.its = 75,
  init.type = "Spectral"
)

# ----------------------------------------------------------------------------------------------

# label topics 

## topic proportions and top words per topic
plot(mod_prev, type = "summary", xlim = c(0, 0.5))

# create most frequent words per topic (for all topics)
topic_words <- labelTopics(mod_prev, n = 20)

## table of MAP topic proportions per document (for all topics)
topic_props <- make.dt(
  mod_prev, 
  data_full$meta[
    c("Bundesland", "Anzahl_Follower", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54")]
) %>% cbind(Name = names(data_full$documents), .)


## labeling workflow (for each topic): 
### (1) inspect most frequent words per topic
### (2) evaluate most representative documents per topic
### (3) assign label

## (1) inspect most frequent words per topic
topic_words$prob[1,]

## (2) evaluate most representative documents per topic
topic_props %>%
  arrange(desc(Topic1)) %>%
  .[1:5,c("Name", "Topic1")] %>%
  left_join(full_corpus) %>%
  select(c("Name", "Topic1", "Tweets_Dokument"))

## (3) assign label

## choose labels
topic_labels <- list(
  Topic_1 = "Hamburg/Johannes Kahrs",
  Topic_2 = "nicht zuordbar",
  Topic_3 = "Berlin/Bundesregierung",
  Topic_4 = "Arbeit&Soziales",
  Topic_5 = "Europa",
  Topic_6 = "nicht zuordbar"
)

## relationship topic/vocabulary
plot(mod_prev, type = "perspectives", topics = c(4, 5))

# ----------------------------------------------------------------------------------------------
# align corpus
data_test <- alignCorpus(new = data_test, old.vocab = mod_prev$vocab, verbose = TRUE)

docs_test <- data_test$documents
vocab_test <-  data_test$vocab
meta_test <- data_test$meta

# fit new documents
test <- fitNewDocuments(
  model = mod_prev, 
  documents = docs_test, 
  newData = meta_test,
  origData = meta_train,
  prevalence =~ Bundesland,
  returnPosterior = FALSE,
  returnPriors = FALSE, 
  designMatrix = NULL, 
  test = TRUE,
  verbose = TRUE
)

# plots for test set
topic_props_test <- make.dt(
  test, meta_test[c("Bundesland", "Anzahl_Follower", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54")]
)

topic_props_test %>%
  ggplot(aes(Bundesland,Topic4)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  labs(y = "Themenanteil", 
       title = "'Arbeit & Soziales' - Themenanteil pro Bundesland")

topic_props_test %>%
  ggplot(aes(Struktur_22,Topic4)) +
  geom_smooth(method = "lm", formula = y ~ s(x)) +
  labs(x = "BIP pro Kopf", y = "Themenanteil", 
       title = "BIP vs. Thema 'Arbeit & Soziales'")

# same plots for training data
topic_props_train <- make.dt(
  mod_prev, 
  meta_train[c("Bundesland", "Anzahl_Follower", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54")]
)

topic_props_train %>%
  ggplot(aes(Bundesland,Topic4)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  labs(y = "Themenanteil", 
       title = "'Arbeit & Soziales' - Themenanteil pro Bundesland")

topic_props_train %>%
  ggplot(aes(Struktur_22,Topic4)) +
  geom_smooth(method = "lm", formula = y ~ s(x)) +
  labs(x = "BIP pro Kopf", y = "Themenanteil", 
       title = "BIP vs. Thema 'Arbeit & Soziales'")

topic_props_train %>%
  ggplot(aes(Struktur_4,Topic4)) +
  geom_smooth(method = "lm", formula = y ~ s(x)) +
  labs(x = "Ausländeranteil", y = "Themenanteil", 
       title = "BIP vs. Thema 'Arbeit & Soziales'")
# ----------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------

### estimating metadata/topic relationship
meta$Bundesland <- as.factor(meta$Bundesland)
prep <- stm::estimateEffect(
  1:6 ~ s(Struktur_4, df=5),
  mod_prev,
  documents = docs_train,
  metadata = meta_train,
  uncertainty = "Local"
)
summary(prep, topics = 1)

### metadata/topic relationship visualization
plot(prep, "Bundesland", method = "difference", topics = c(1,2,3,4,5,6,7), cov.value1 = "Bayern",
     cov.value2 = "Berlin", model = mod_prev, xlab = "Bundesland")
plot(prep, "Bundesland", method = "pointestimate", topics = 3,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "Bundesland")
plot(prep, "Anzahl_Follower", method = "continuous", topics = 3, ci.level = 0.8,
     model = mod_prev, printlegend = FALSE, ylim = c(0,1), main = "Arbeit & Soziales",
     xlab = "Anzahl Twitter-Follower", ylab = "Themenanteil", 
     yaxs="i", xaxs="i")
plot(prep, "Struktur_4", method = "continuous", topics = 3, ci.level = 0.8,
     model = mod_prev, printlegend = FALSE, ylim = c(0,1), main = "Arbeit & Soziales",
     xlab = "Ausländeranteil", ylab = "Themenanteil", 
     yaxs="i", xaxs="i")
plot(prep, "Struktur_22", method = "continuous", topics = 3, ci.level = 0.8,
     model = mod_prev, printlegend = FALSE, ylim = c(0,1), main = "Arbeit & Soziales",
     xlab = "BIP pro Kopf", ylab = "Themenanteil", 
     yaxs="i", xaxs="i")
plot(prep, "Struktur_42", method = "continuous", topics = 3, ci.level = 0.8,
     model = mod_prev, printlegend = FALSE, ylim = c(0,1),
     xlab = "Arbeitslosenquote", ylab = "Themenanteil", main = "Arbeit & Soziales",
     yaxs="i", xaxs="i")

# ----------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------
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
