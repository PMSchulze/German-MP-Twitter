# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "betareg", "grid", "gridExtra", "huge", "mvtnorm", "quanteda", "reshape2", "scales", "stm", "stringi", "tidyverse", "tm"
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

# # search hyperparameter space for optimal K (grid: 5 thru 40, steps of size 5)
# hyperparameter_search <- searchK(
#   documents = data$documents,
#   vocab = data$vocab,
#   data = data$meta,
#   K = c(5,10,15,20,25,30,35,40),
#   prevalence =~ Partei,
#   max.em.its = 150,
#   init.type = "Spectral"
# )
# saveRDS(hyperparameter_search, "./data/searchK_large_data.rds")

searchK_data <- readRDS("./data/searchK_large_data.rds")
# plot(searchK_data)

# plot held-out likelihood, semantic coherence, exclusivity, and residuals for K grid
plot_heldout <- ggplot(data = searchK_data$results, aes(x = K, y = heldout)) +
  geom_line() +
  geom_point() +
  labs(y = "held-out likelihood") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

plot_semcoh <- ggplot(data = searchK_data$results, aes(x = K, y = semcoh)) + 
  geom_line() +
  geom_point() +
  labs(y = "semantic coherence") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

plot_exclus <- ggplot(data = searchK_data$results, aes(x = K, y = exclus)) +
  geom_line() +
  geom_point() +
  labs(y = "exclusivity") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

plot_residual <- ggplot(data = searchK_data$results, aes(x = K, y = residual)) + 
  geom_line() +
  geom_point() +
  labs(y = "residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

gridExtra::grid.arrange(plot_heldout, plot_semcoh, plot_exclus, plot_residual, ncol=2)

# decide on K, based on plots
K <- 15

# ----------------------------------------------------------------------------------------------
# ---------------------------------- Prevalence Model Fitting ----------------------------------
# ----------------------------------------------------------------------------------------------

# choose covariates and number of topics
covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
outcome <- ""
prevalence <- as.formula(paste(outcome, covar, sep = "~")) 

# # fit model
# mod_prev <- stm::stm(
#   documents = data$documents,
#   vocab = data$vocab,
#   data = data$meta,
#   K = K,
#   prevalence =~ prevalence,
#   gamma.prior = 'L1',
#   seed = 123,
#   max.em.its = 350,
#   init.type = "Spectral")

mod_prev <- readRDS("./data/mod_prev_monthly.rds")

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
  data$meta[c("Name", "Partei","Datum", "Bundesland")]) %>% 
  cbind(docname = names(data$documents), .)

## top words per topic (for all topics)
topic_words <- labelTopics(mod_prev, n = 8)

## topic to be evaluated
topic_number <- 1
topic_number_long <- paste0("Topic", topic_number)

## number of top documents to be printed in step (2)
docs_number <- 5

## initialize list with empty labels
## initialize list with empty labels
topic_labels <- list(
  Topic1 = NULL,
  Topic2 = NULL,
  Topic3 = NULL,
  Topic4 = NULL,
  Topic5 = NULL,
  Topic6 = NULL,
  Topic7 = NULL,
  Topic8 = NULL,
  Topic9 = NULL,
  Topic10 = NULL,
  Topic11 = NULL,
  Topic12 = NULL,
  Topic13 = NULL,
  Topic14 = NULL,
  Topic15 = NULL
)

# ----------------------------------------------------------------------------------------------

# actual labelling porcess

## (1) inspect most frequent words per topic
topic_words$prob[topic_number,] # 20 most frequent words

# logbeta_matrix <- mod_prev$beta$logbeta[[1]]
# mod_prev$vocab[which.max(logbeta_matrix[topic_number,])] # most frequent word directly from (log)beta vector

## (2) evaluate most representative documents per topic
data_corpus$docname <- paste0(data_corpus$Twitter_Username, "_", data_corpus$Jahr, "_", data_corpus$Monat)

repr_docs <-  topic_props %>%
  arrange(desc(!!as.symbol(topic_number_long))) %>%
  .[1:docs_number, c("Name", "docname", "Datum", "Partei", "Bundesland", topic_number_long)] %>%
  left_join(data_corpus[,c("Tweets_Dokument", "docname")], 
            by = "docname")

substr(repr_docs$Tweets_Dokument[1], 0, 256) # view most representative document
topic_number # topic
scales::percent(repr_docs[topic_number_long][1,1], accuracy = 0.01) # proportion
repr_docs$Name[1] # author/MP
repr_docs$Partei[1] # party
repr_docs$Bundesland[1] # state
repr_docs$Datum[1] # date

## (3) assign label
topic_labels[[topic_number]] <- "right/nationalist"

## (3) assign label
topic_labels <- list(
  Topic1 = "right/nationalist",
  Topic2 = "miscellaneous_1",
  Topic3 = "green/climate",
  Topic4 = "social/housing",
  Topic5 = "Europe_english",
  Topic6 = "mobility",
  Topic7 = "Europe",
  Topic8 = "corona",
  Topic9 = "left/anti-war",
  Topic10 = "Twitter/politics_1",
  Topic11 = "Twitter/politics_2",
  Topic12 = "miscellaneous_2",
  Topic13 = "Twitter/politics_3",
  Topic14 = "right-wing extremism",
  Topic15 = "social/health"
)

topic_labels %>%
  matrix(dimnames = list(names(topic_labels))) # print w/ topic number

# ----------------------------------------------------------------------------------------------
# -------------------------------- Global Topic Inspection -------------------------------------
# ----------------------------------------------------------------------------------------------

# all topics w/ global proportions (prints topic words with their corpus frequency)
plot(mod_prev, type = "summary", xlim = c(0, 0.3), custom.labels = topic_labels) # average thetas across all MPs 

doc_lengths <- lapply(data$documents[], length)
weights <- c()
i <- 1
while (i <= length(doc_lengths)) {
  weights[[i]] <- doc_lengths[[i]]/2
  i <- i + 1}
mean_weight <- mean(weights)
props_unweighted <- colMeans(mod_prev$theta[,1:K])
props_weighted <- colMeans((mod_prev$theta*weights/mean_weight)[, 1:K])

topic_labels_unlisted <- unlist(topic_labels, use.names = FALSE)

props_df <- data.frame(topic_labels_unlisted, props_unweighted, props_weighted) %>% reshape2::melt(id = "topic_labels_unlisted")
colnames(props_df) <- c("topic", "variable", "proportion")

ggplot(data = props_df, aes(x = topic, y = proportion, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("grey40","grey80")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

# vocabulary usage comparison for two topics
plot(mod_prev, type = "perspectives", topics = c(3, 6), n = 30)

# global topic correlation
mod_prev_corr <- topicCorr(mod_prev, method = "simple", cutoff = -0.20,
                           verbose = TRUE) # based on correlations between mod_prev$theta
plot.topicCorr(mod_prev_corr)

# global topic correlation
cormat <- cor(mod_prev$theta)
ggcorrplot::ggcorrplot(cormat) +
  scale_x_continuous(breaks = seq(1, 15, by = 1)) +
  scale_y_continuous(breaks = seq(1, 15, by = 1)) +
  labs(x = "topic number", y = "topic number") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

# ----------------------------------------------------------------------------------------------
# ----------------------------------- Estimate Effects -----------------------------------------
# ----------------------------------------------------------------------------------------------

# factorize categorical variables, set CDU/CSU as reference category for variable "Partei"
data$meta$Partei <- data$meta$Partei %>%
  as.factor() %>%
  relevel(ref = 3)
data$meta$Bundesland <- as.factor(data$meta$Bundesland)

# prep <- stm::estimateEffect(
#   1:15 ~ s(t, df=20),
#   mod_prev,
#   #documents = data$documents,
#   metadata = data$meta,
#   uncertainty = "Global"
# )
# summary(prep, topics = 1)
# 
# par(mfrow=c(3,3))
# for (i in 1:9){
#   plot(prep, "t", method = "continuous", topics = i, 
#        main = paste0(mod_labels[i,], collapse = ", "), 
#        printlegend = F, xlab = "t")
# }
# par(mfrow=c(3,3))
# for (i in 1:9){
#   plot(prep, "Partei", method = "pointestimate", topics = i, labeltype = "custom",
#        custom.labels = c("CDU/CSU", "FDP", "Die Linke", "SPD", "Bündnis 90/Die Grünen", "AfD"), 
#        main = paste0(mod_labels[i,], collapse = ", "), 
#        printlegend = F, xlab = "Expected Topic Proportion")
# }

# ----------------------------------------------------------------------------------------------

library("betareg")
library("mvtnorm")

# ------------------------------- Create helper functions --------------------------------------

sigmoid <- function(x) exp(x)/(1+exp(x))

majority <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

sample_normal <- function(mod) {
  mu <- mod$coefficients$mean
  var <- mod$vcov[1:length(mu), 1:length(mu)]
  mvtnorm::rmvnorm(1, mean = mu, sigma = var)
}

sample_coefs_beta <- function(stmobj, formula, metadata, nsims = 25, seed = NULL){
  topic_n <- as.numeric(as.character(formula)[2])
  topic_nam <- paste0("Topic", topic_n)
  set.seed(seed)
  theta_sim <- do.call(rbind, stm::thetaPosterior(stmobj, nsims = nsims, type = "Global"))[,topic_n]
  theta_sim <- lapply(split(1:(length(theta_sim)), 1:nsims), 
                      function(i) setNames(data.frame(theta_sim[i]), topic_nam))
  f <- paste(topic_nam, "~", as.character(formula)[3])
  est_beta <- lapply(theta_sim, 
                     function(x) betareg::betareg(as.formula(f), data = cbind(x, metadata)))
  res <- lapply(est_beta, sample_normal)
  return(res)
}

sample_all_betas <- function(covar, metadata, nsims, topics = K, seed = NULL) {
  res <- vector(mode = "list", length = topics)
  for (topic in 1:topics) {
    outcome <- topic
    formula <- as.formula(paste(outcome, covar, sep = "~"))
    res[[topic]] <- sample_coefs_beta(mod_prev, formula, metadata = metadata, nsims, seed = seed)
  }
  return(res)
}

predict_props_beta <- function(beta_coefs, est_var, formula, metadata){
  dat <- metadata[, -which(names(metadata) == est_var)]
  dat <- lapply(dat, function(x) if(is.numeric(x)) median(x) else majority(x))
  if (is.numeric(metadata[,est_var])) {
    dat_fit <- data.frame(
      dat, fitvar = seq(min(metadata[,est_var]), max(metadata[,est_var]), length.out = 500)
    )
  } else {
    dat_fit <- data.frame(dat, fitvar = unique(metadata[,est_var]))
  }
  names(dat_fit) <- c(names(dat),est_var)
  f <- paste("~",as.character(formula)[3])
  xmat <- stm::makeDesignMatrix(as.formula(f), metadata, dat_fit)
  fit_vals <- do.call(cbind, lapply(beta_coefs, function(x) sigmoid(xmat %*% t(x))))
  mu <- quanteda::rowMeans(fit_vals)
  ci <- apply(fit_vals, 1, function(x) quantile(x, probs = c(0.025, 0.975)))
  res <- data.frame(dat_fit[[est_var]], mu, ci[1,], ci[2,])
  names(res) <- c(est_var, "proportion", "ci_lower", "ci_upper")
  return(res)
}


# ----------------------------------------------------------------------------------------------

# ----------------------------- Actual Model Prediction ----------------------------------------

# covariates
covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
# formula always in form "i~var1+var2+...", where i = topic number
topic_number <- 4
formula <- as.formula(paste(topic_number, covar, sep = "~"))
# obtain list of nsims beta regression outputs
nsims <- 100

beta_coefs <- sample_coefs_beta(mod_prev, formula, data$meta, nsims = 25) # for topic = outcome only
 
start.time <- Sys.time()
set.seed(123)
all_betas <- sample_all_betas(covar = covar, metadata = data$meta, nsims = nsims, topics = K, seed = 123)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
saveRDS(all_betas, "./data/all_betas.rds")

all_betas <- readRDS("./data/all_betas.rds")

# estimate effect for variable while other variables held as median/majority value
preds <- predict_props_beta(beta_coefs, "t", formula, data$meta)

# example plots
par(mfrow=c(3,3))
for (v in c("t", "Partei", "Bundesland", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54")) {
  plot(predict_props_beta(all_betas[[topic_number]], v, formula, data$meta)[,1:2], type = "l", col = "blue")
}

# ----------------------------------------------------------------------------------------------

# -------------------------------------- Some plots with ggplot --------------------------------

library(grid)
library(gridExtra)
library(scales)

covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
varlist <- c(
  "t", "Partei", "Bundesland", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54"
)
varlist_fullnames <- c(
  "Time", "Party", "Federal State", "Immigrants (%)", "GDP per capita", 
  "Unemployement Rate (%)", "vote share (%)"
)

# ---------

## Topic 3: Green/Climate
topic_number <- 3
formula_3 <- as.formula(paste(topic_number, covar, sep = "~"))
beta_coefs_3 <- all_betas[[topic_number]]
preds_varlist_3 <- lapply(varlist, function(v) predict_props_beta(beta_coefs_3, v, formula_3, data$meta))
names(preds_varlist_3) <- varlist

### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_", v)
  assign(plot_nam, ggplot(preds_varlist_3[[v]], aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           xlab(varlist_fullnames[varlist==v]) +
           ylab("Expected Topic Proportion") +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma))
}
gridExtra::grid.arrange(
  plot_t, plot_Struktur_4, plot_Struktur_22, plot_Struktur_42, ncol=2, 
  top = grid::textGrob("Topic 3: Green/Climate", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

### Categorial Plots
(plot_party_3 <- ggplot(preds_varlist_3$Partei, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 3: Green/Climate")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# ---------

## Topic 4: Social/Housing
formula_4 <- 4~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)
beta_coefs_4 <- all_betas[[4]]
preds_varlist_4 <- lapply(varlist, 
                          function(v) predict_props_beta(beta_coefs_4, v, formula_4, data$meta))
names(preds_varlist_4) <- varlist

### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_", v)
  assign(plot_nam, ggplot(preds_varlist_4[[v]], aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma))
}
gridExtra::grid.arrange(
  plot_t, plot_Struktur_4, plot_Struktur_22, plot_Struktur_42, ncol=2, 
  top = grid::textGrob("Topic 4: Social/Housing", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

### Categorial Plots
(plot_party_4 <- ggplot(preds_varlist_4$Partei, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 4: Social/Housing")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# ---------

## Topic
topic_number <- 1
topic_title <- paste0("Topic ", topic_number, ": ", topic_labels[[topic_number]])
assign(paste0("topic_title_", topic_number), topic_title)
formula <- as.formula(paste(topic_number, covar, sep = "~"))
assign(paste0("formula_", topic_number), formula)
beta_coefs <- all_betas[[topic_number]]
assign(paste0("beta_coefs_", topic_number), beta_coefs)
preds_varlist <- lapply(varlist, function(v) predict_props_beta(beta_coefs, v, formula, data$meta))
assign(paste0("preds_varlist_1", topic_number), preds_varlist)
# names(preds_varlist) <- varlist
preds_varlist_nr <- paste0("preds_varlist_", topic_number)
names(!!as.symbol(preds_varlist_nr)) <- varlist

### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_", v)
  assign(plot_nam, ggplot(preds_varlist[[v]], aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma))
}
gridExtra::grid.arrange(
  plot_t, plot_Struktur_4, plot_Struktur_22, plot_Struktur_42, ncol=2, 
  top = grid::textGrob(topic_title, gp=grid::gpar(fontsize=16, fontface = "bold"))
)

### Categorial Plots
(plot_party <- ggplot(preds_varlist$Partei, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle(topic_title)+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# ---------

## Topic 1: Right/Nationalist
formula_1 <- 1~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)
beta_coefs_1 <- sample_coefs_beta(mod_prev, formula_1, data$meta, nsims = 25)
preds_varlist_1 <- lapply(varlist, 
                          function(v) predict_props_beta(beta_coefs_1, v, formula_1, data$meta))
names(preds_varlist_1) <- varlist

# ---------

gridExtra::grid.arrange(plot_party_3, plot_party_4)

preds_varlist_1$Partei$Topic <- "Right/Nationalist"
preds_varlist_3$Partei$Topic <- "Green/Climate"
preds_varlist_4$Partei$Topic <- "Social/Housing"
party_data <- rbind(preds_varlist_1$Partei, preds_varlist_3$Partei, preds_varlist_4$Partei)
(plot_party <- ggplot(party_data, aes(y=proportion, x = Partei, fill = Topic)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values=c("green", "blue", "red")) +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic Proportions per Party")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# ----------------------------------------------------------------------------------------------
# ---------------------------------- Content Model Fitting ----------------------------------
# ----------------------------------------------------------------------------------------------

# choose covariates and number of topics
covar <- "Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)"
content_var <- "Partei"
outcome <- ""

prevalence <- as.formula(paste(outcome, covar, sep = "~"))
content <- as.formula(paste(outcome, content_var, sep = "~"))

# fit model
mod_cont <- stm::stm(
  documents = data$documents,
  vocab = data$vocab,
  data = data$meta,
  K = K,
  prevalence = prevalence,
  content = content,
  gamma.prior = 'L1',
  seed = 123,
  max.em.its = 200,
  init.type = "Spectral")
saveRDS(mod_cont, "./data/mod_cont_monthly.rds")

mod_cont <- readRDS("./data/mod_cont_monthly.rds")

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
  mod_cont, 
  data$meta[c("Name", "Partei","Datum", "Bundesland")]) %>% 
  cbind(docname = names(data$documents), .)

## top words per topic (for all topics)
topic_words <- labelTopics(mod_cont, n = 20)

## topic to be evaluated
topic_number <- 2
topic_number_long <- paste0("Topic", topic_number)

## number of top documents to be printed in step (2)
docs_number <- 5

## initialize list with empty labels
## initialize list with empty labels
topic_cont_labels <- list(
  Topic1 = NULL,
  Topic2 = NULL,
  Topic3 = NULL,
  Topic4 = NULL,
  Topic5 = NULL,
  Topic6 = NULL,
  Topic7 = NULL,
  Topic8 = NULL,
  Topic9 = NULL,
  Topic10 = NULL,
  Topic11 = NULL,
  Topic12 = NULL,
  Topic13 = NULL,
  Topic14 = NULL,
  Topic15 = NULL
)

# ----------------------------------------------------------------------------------------------

# actual labelling porcess

## (1) inspect most frequent words per topic
topic_words$prob[topic_number,] # 20 most frequent words

# logbeta_matrix <- mod_prev$beta$logbeta[[1]]
# mod_prev$vocab[which.max(logbeta_matrix[topic_number,])] # most frequent word directly from (log)beta vector

## (2) evaluate most representative documents per topic
data_corpus$docname <- paste0(data_corpus$Twitter_Username, "_", data_corpus$Jahr, "_", data_corpus$Monat)

repr_docs <-  topic_props %>%
  arrange(desc(!!as.symbol(topic_number_long))) %>%
  .[1:docs_number, c("Name", "docname", "Datum", "Partei", "Bundesland", topic_number_long)] %>%
  left_join(data_corpus[,c("Tweets_Dokument", "docname")], 
            by = "docname")

substr(repr_docs$Tweets_Dokument[1], 0, 256) # view most representative document
topic_number # topic
scales::percent(repr_docs[topic_number_long][1,1], accuracy = 0.01) # proportion
repr_docs$Name[1] # author/MP
repr_docs$Partei[1] # party
repr_docs$Bundesland[1] # state
repr_docs$Datum[1] # date

## (3) assign label
topic_cont_labels[[topic_number]] <- "right/nationalist"

## (3) assign label
topic_cont_labels <- list(
  Topic1 = "right/nationalist_1",
  Topic2 = "miscellaneous_1",
  Topic3 = "left/humanitarian",
  Topic4 = "housing",
  Topic5 = "innovation",
  Topic6 = "green/energy",
  Topic7 = "miscellaneous_2",
  Topic8 = "corona",
  Topic9 = "foreign affairs",
  Topic10 = "election",
  Topic11 = "right/nationalist_2",
  Topic12 = "miscellaneous_3",
  Topic13 = "miscellaneous_4",
  Topic14 = "Twitter/politics",
  Topic15 = "miscellaneous_5"
)

topic_cont_labels %>%
  matrix(dimnames = list(names(topic_labels))) # print w/ topic number

# ----------------------------------------------------------------------------------------------
# -------------------------------- Global Topic Inspection -------------------------------------
# ----------------------------------------------------------------------------------------------

# all topics w/ global proportions (prints topic words with their corpus frequency)
plot(mod_cont, type = "summary", xlim = c(0, 0.3), custom.labels = topic_cont_labels) # average thetas across all MPs 

doc_lengths <- lapply(data$documents[], length)
weights <- c()
i <- 1
while (i <= length(doc_lengths)) {
  weights[[i]] <- doc_lengths[[i]]/2
  i <- i + 1}
mean_weight <- mean(weights)
props_unweighted <- colMeans(mod_prev$theta[,1:K])
props_weighted <- colMeans((mod_prev$theta*weights/mean_weight)[, 1:K])

topic_labels_unlisted <- unlist(topic_labels, use.names = FALSE)

props_df <- data.frame(topic_labels_unlisted, props_unweighted, props_weighted) %>% reshape2::melt(id = "topic_labels_unlisted")
colnames(props_df) <- c("topic", "variable", "proportion")

ggplot(data = props_df, aes(x = topic, y = proportion, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("grey40","grey80")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

# vocabulary usage comparison for two topics
plot(mod_cont, type = "perspectives", topics = c(3, 6), n = 30)

# global topic correlation
mod_cont_corr <- topicCorr(mod_cont, method = "simple", cutoff = -0.20,
                           verbose = TRUE) # based on correlations between mod_cont$theta
plot.topicCorr(mod_cont_corr)

# global topic correlation
cormat <- cor(mod_cont$theta)
ggcorrplot::ggcorrplot(cormat) +
  scale_x_continuous(breaks = seq(1, 15, by = 1)) +
  scale_y_continuous(breaks = seq(1, 15, by = 1)) +
  labs(x = "topic number", y = "topic number") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

