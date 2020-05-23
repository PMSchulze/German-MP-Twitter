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
data_train <- readRDS("./data/topic_user_train_preprocessed_no#.rds")
data_test <- readRDS("./data/topic_user_test_preprocessed_no#.rds")
colnames_table <- read.csv(file = "./data/topic_user_colnames.csv")

# extract documents, vocabulary and metadata
docs_train <- data_train$documents
vocab_train <-  data_train$vocab
meta_train <- data_train$meta

# search hyperparameter space for optimal K
hyperparameter_search <- searchK(
  documents = docs_train,
  vocab = vocab_train,
  data = meta_train,
  K = c(5,6,7,8,9,10),
  prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
    s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  max.em.its = 50,
  init.type = "Spectral"
)

# topic prevalence --------------------------------------------------------

# choose covariates
prev_var <- c("Partei*Bundesland", "Partei*s(Anzahl_Follower)", "Partei*s(Struktur_4)",
                    "Partei*s(Struktur_22)", "Partei*s(Struktur_42)", "Partei*s(Struktur_54)")

outcome <- ""
prevalence <- as.formula(paste(outcome, paste(prev_var, collapse = "+"), sep = "~")) 

# fit model
mod_prev <- stm::stm(
  documents = docs_train,
  vocab = vocab_train,
  data = meta_train,
  K = 6,
  prevalence = prevalence,
  gamma.prior='L1',
  max.em.its = 300,
  init.type = "Spectral")

# mod_prev$mu # corpus mean of topic prevalence and coefficients
# mod_prev$beta # word probabilities for each topic
# mod_prev$sigma # covariance matrix
# mod_prev$theta # topic proportions

# top words per topic
labelTopics(mod_prev)

logbeta_matrix <- mod_prev$beta$logbeta[[1]]
mod_prev$vocab[which.max(logbeta_matrix[6,])] # highest prob (frequency) word for topic 6

# manually label topics
mod_prev_labels <- c("right/national",
                     "green/climate",
                     "future/digital",
                     "Europe",
                     "left/social",
                     "miscellaneous")

# align corpus for test data
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
  prevalence = prevalence,
  returnPosterior = FALSE,
  returnPriors = FALSE, 
  designMatrix = NULL, 
  test = TRUE,
  verbose = TRUE
)

# all topics w/ global proportions (prints topic words with their corpus frequency)
plot(mod_prev, type = "summary", xlim = c(0, 0.3)) # average thetas across all MPs 

### training data
doc_lengths <- lapply(docs_train[], length)
weights <- c()
i <- 1
while (i <= length(doc_lengths)) {
  weights[[i]] <- doc_lengths[[i]]/2
  i <- i + 1}
mean_weight <- mean(weights)
frequency <- colMeans(mod_prev$theta[,1:6])
weighted_frequency <- colMeans((mod_prev$theta*weights/mean_weight)[, 1:6])
frequency
weighted_frequency

### test data
doc_lengths <- lapply(docs_test[], length)
weights <- c()
i <- 1
while (i <= length(doc_lengths)) {
  weights[[i]] <- doc_lengths[[i]]/2
  i <- i + 1}
mean_weight <- mean(weights)
frequency <- colMeans(test$theta[,1:6])
weighted_frequency <- colMeans((test$theta*weights/mean_weight)[, 1:6])
frequency
weighted_frequency

# word clouds (for training data only)
cloud(mod_prev, topic = 6, scale = c(2, 0.25))

# vocabulary usage across two topics (for training data only)
plot(mod_prev, type = "perspectives", topics = c(2, 4))

# global topic correlation
mod_prev_corr <- topicCorr(mod_prev, method = "simple", cutoff = -0.20,
          verbose = TRUE) # based on correlations between mod_prev$theta
plot.topicCorr(mod_prev_corr)

### training data
cormat <- cor(mod_prev$theta)
cormat

### test data
cormat <- cor(test$theta)
cormat

# metadata/topic relationship ---------------------------------------------
covar <- c("Partei", "Bundesland", "Anzahl_Follower", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54")

# factorize categorical variables, set CDU/CSU as reference category for variable "Partei"
meta_train$Partei <- meta_train$Partei %>%
  as.factor() %>%
  relevel(ref = 3)
meta_train$Bundesland <- as.factor(meta_train$Bundesland)

meta_test$Partei <- meta_test$Partei %>%
  as.factor() %>%
  relevel(ref = 3)
meta_test$Bundesland <- as.factor(meta_test$Bundesland)

# training data
topic_props_train <- make.dt(
  mod_prev, 
  meta_train[covar])

topic_props_train %>%
  ggplot(aes(Partei, Topic6)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  labs(y = "Themenanteil", 
       title = "'right/national' - topic proportion by party")

topic_props_train %>%
  ggplot(aes(Struktur_22,Topic4)) +
  geom_smooth(method = "lm", formula = y ~ s(x)) +
  labs(x = "BIP pro Kopf", y = "Themenanteil", 
       title = "BIP vs. Thema 'Arbeit & Soziales'")

# test data
topic_props_test <- make.dt(
  test,
  meta_test[covar])

topic_props_test %>%
  ggplot(aes(Partei, Topic6)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point() +
  labs(y = "topic proportion", 
       title = "'right/national' - topic proportion by party")

topic_props_test %>%
  ggplot(aes(Struktur_22, Topic4)) +
  geom_smooth(method = "lm", formula = y ~ s(x)) +
  labs(x = "GDP p.c.", y = "topic proportion", 
       title = "GDP vs. Thema 'Arbeit & Soziales'")

## 
outcome <- "1:6"
prevalence <- as.formula(paste(outcome, paste(prev_var, collapse = "+"), sep = "~")) 

prep <- stm::estimateEffect(1:6 ~ Partei + Bundesland + s(Anzahl_Follower) + 
                              s(Struktur_4) + s(Struktur_22) + s(Struktur_42) + 
                              s(Struktur_54),
                            mod_prev,
                            metadata = meta_train,
                            uncertainty = "Global")
summary(prep, topics = 1)

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

# prevalence model w/ interaction -----------------------------------------

# fit model
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


# topical content -------------------------------------------------------

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

