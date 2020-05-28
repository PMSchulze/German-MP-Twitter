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
data <- readRDS("./data/preprocessed.rds")
colnames_table <- read.csv(file = "./data/topic_colnames.csv")
data_corpus <- readRDS("./data/prep.rds")

# extract documents, vocabulary and metadata
docs <- data$documents
vocab <-  data$vocab
meta <- data$meta

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

# choose covariates and number of topics
prev_var <- c("Partei", "Bundesland")
# prev_var <- c("Partei*Bundesland", "Partei*s(Anzahl_Follower)", "Partei*s(Struktur_4)",
#               "Partei*s(Struktur_22)", "Partei*s(Struktur_42)", "Partei*s(Struktur_54)")
outcome <- ""
prevalence <- as.formula(paste(outcome, paste(prev_var, collapse = "+"), sep = "~")) 

K <- 6

# fit model
mod_prev <- stm::stm(
  documents = docs,
  vocab = vocab,
  data = meta,
  K = K,
  prevalence = prevalence,
  # gamma.prior = 'L1',
  seed = 123,
  max.em.its = 200,
  init.type = "Spectral")

# top words per topic (all topics)
topic_words <- labelTopics(mod_prev)
topic_words

topic_number <- 1

logbeta_matrix <- mod_prev$beta$logbeta[[1]]
mod_prev$vocab[which.max(logbeta_matrix[topic_number,])] # highest prob (frequency) word for chosen topic_number

# word clouds
cloud(mod_prev, topic = topic_number, scale = c(2.5, 0.25))

# table of MAP topic proportions per document (for all topics)
topic_props <- make.dt(
  mod_prev, 
  data$meta[
    c("Name", "Partei", "Bundesland")]
) %>% cbind(docname = names(data$documents), .)

# labeling workflow (for each topic): 
## (1) inspect most frequent words per topic (already done above, see topic_words)
## (2) inspect word cloud
## (3) evaluate most representative documents per topic
## (4) assign label

## preparation
docs_number <- 5

# ## (1) inspect most frequent words per topic
# topic_words$prob[topic_number,]

## (2) evaluate most representative documents per topic
topic_props %>%
  arrange(desc(Topic1)) %>%
  .[1:docs_number,c("Name", "Partei", paste0("Topic", topic_number))] %>%
  left_join(select(data_corpus, "Tweets_Dokument", "docname"), by = "docname") %>%
  select(c("Name", "docname", "Partei", paste0("Topic", topic_number), "Tweets_Dokument"))

topic_props %>%
  arrange(desc(Topic1)) %>%
  .[1:docs_number,c("Name", "Partei", paste0("Topic", topic_number))] %>%
  left_join(select(data_corpus, "Tweets_Dokument")) %>%
  select(c("Name", "Partei", paste0("Topic", topic_number), "Tweets_Dokument"))

## (3) assign label
topic_labels <- c("Europe",
                  "women/social",
                  "left",
                  "right/national",
                  "miscellaneous",
                  "climate/digital")

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
  scale_fill_manual(values=c("grey50","grey80")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

# vocabulary usage comparison for two topics
plot(mod_prev, type = "perspectives", topics = c(2, 3), n = 30)

# global topic correlation
mod_prev_corr <- topicCorr(mod_prev, method = "simple", cutoff = -0.20,
                           verbose = TRUE) # based on correlations between mod_prev$theta
plot.topicCorr(mod_prev_corr)

cormat <- cor(mod_prev$theta)
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
  ggplot(aes(Partei, Topic1)) +
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
  ggplot(aes(Partei, Topic1)) +
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

