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
data_corpus <- readRDS("./data/prep_monthly.rds")
# data_train <- readRDS("./data/preprocessed_monthly_train.rds")
# data_test <- readRDS("./data/preprocessed_monthly_test.rds")

n_topics <- 15

mod_prev <- stm::stm(
  documents = data$documents,
  vocab = data$vocab,
  data = data$meta,
  K = n_topics,
  prevalence =~ as.factor(Partei) + as.factor(Bundesland) + 
    s(Struktur_4) + s(Struktur_22) + s(Struktur_42) + s(Struktur_54) + s(t),
  gamma.prior = 'L1',
  seed = 123,
  max.em.its = 100,
  init.type = "Spectral"
)

saveRDS(mod_prev, "./data/mod_prev_monthly.rds")

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
topic_number <- 3

# ----------------------------------------------------------------------------------------------

# actual labelling process

## (1) inspect most frequent words per topic
cloud(mod_prev, topic = topic_number, scale = c(2.5, 0.25)) # word cloud
topic_words$prob[topic_number,] # 20 most frequent words

## (2) evaluate most representative documents per topic
data_corpus$docname <- data_corpus[,c("Twitter_Username", "Jahr", "Monat")] %>% 
  unite("docname", sep = "_") %>% .$docname
repr_docs <-  topic_props %>%
  arrange(desc(!!as.symbol(paste0("Topic", topic_number)))) %>%
  .[1:docs_number, c("docname", "Name", "Partei", paste0("Topic", topic_number))] %>%
  left_join(data_corpus[,c("docname", "Tweets_Dokument")], by = "docname")
repr_docs[5,] # view i-th most representative document

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

# factorize categorical variables, set CDU/CSU as reference category for variable "Partei"
data$meta$Partei <- data$meta$Partei %>%
  as.factor() %>%
  relevel(ref = 3)
data$meta$Bundesland <- as.factor(data$meta$Bundesland)

prep <- stm::estimateEffect(
  1:15 ~ s(t, df=20),
  mod_prev,
  #documents = data$documents,
  metadata = data$meta,
  uncertainty = "Global"
)
summary(prep, topics = 1)

par(mfrow=c(3,3))
for (i in 1:9){
  plot(prep, "t", method = "continuous", topics = i, 
       main = paste0(mod_labels[i,], collapse = ", "), 
       printlegend = F, xlab = "t")
}
par(mfrow=c(3,3))
for (i in 1:9){
  plot(prep, "Partei", method = "pointestimate", topics = i, labeltype = "custom",
       custom.labels = c("CDU/CSU", "FDP", "Die Linke", "SPD", "Bündnis 90/Die Grünen", "AfD"), 
       main = paste0(mod_labels[i,], collapse = ", "), 
       printlegend = F, xlab = "Expected Topic Proportion")
}

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
  xmat <- stm::makeDesignMatrix(as.formula(f), data$meta, dat_fit)
  fit_vals <- do.call(cbind, lapply(beta_coefs, function(x) sigmoid(xmat %*% t(x))))
  mu <- quanteda::rowMeans(fit_vals)
  ci <- apply(fit_vals, 1, function(x) quantile(x, probs = c(0.025, 0.975)))
  res <- data.frame(dat_fit[[est_var]], mu, ci[1,], ci[2,])
  names(res) <- c(est_var, "proportion", "ci_lower", "ci_upper")
  return(res)
}

# ----------------------------------------------------------------------------------------------

# ----------------------------- Actual Model Prediction ----------------------------------------

# formula always in form "i~var1+var2+...", where i = topic number
formula <- 4~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)
# obtain list of nsims beta regression outputs (for respective topic)
beta_coefs <- sample_coefs_beta(mod_prev, formula, data$meta, nsims = 25)
# estimate effect for variable while other variables held as median/majority value
preds <- predict_props_beta(beta_coefs, "t", formula, data$meta)

# example plots
par(mfrow=c(3,3))
for (v in c("t", "Partei", "Bundesland", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54")) {
  plot(predict_props_beta(beta_coefs, v, formula, data$meta)[,1:2], type = "l", col = "blue")
}

# ----------------------------------------------------------------------------------------------

# -------------------------------------- Some plots with ggplot --------------------------------

library(grid)
library(gridExtra)
library(scales)

varlist <- c(
  "t", "Partei", "Bundesland", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54"
)
varlist_fullnames <- c(
  "Time", "Party", "Federal State", "Immigrants (%)", "GDP per capita", 
  "Unemployement Rate (%)", "Share of Votes (%)"
)

# ---------

## Topic 3: Green/Climate
formula_3 <- 3~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)
beta_coefs_3 <- sample_coefs_beta(mod_prev, formula_3, data$meta, nsims = 25)
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
beta_coefs_4 <- sample_coefs_beta(mod_prev, formula_4, data$meta, nsims = 25)
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
