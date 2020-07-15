library(ggplot2)
library(bayesplot)
library(rstanarm)
library(stm)
library(stmprevalence)
theme_set(bayesplot::theme_default())

# ----------------------------------------------------------------------------------------------

# set working directory
setwd("/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse")
# setwd('C:\\Users\\Simon\\OneDrive\\Uni\\LMU\\SS 2020\\Statistisches Consulting\\Bundestag-MP-Analyse')
# load data
data <- readRDS("./data/preprocessed_monthly.rds")
# load fitted topic model
mod_prev <- readRDS("./data/mod_prev_monthly.rds")
# load topic labels
topic_labels <- list(
  Topic1 = "Right/Nationalist",
  Topic2 = "Miscellaneous 1",
  Topic3 = "Climate Economics",
  Topic4 = "Social/Housing",
  Topic5 = "Digital/Future",
  Topic6 = "Climate Protection",
  Topic7 = "Europe",
  Topic8 = "Corona",
  Topic9 = "Left/Anti-war",
  Topic10 = "Twitter/Politics 1",
  Topic11 = "Twitter/Politics 2",
  Topic12 = "Miscellaneous 2",
  Topic13 = "Twitter/Politics 3",
  Topic14 = "Right-wing Extremism",
  Topic15 = "Society/Solidarity"
)
# load list of prevalence covariates
varlist <- c(
  "t", "Partei", "Bundesland", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54"
)
# load full names of prevalence covariates
varlist_fullnames <- c(
  "Time", "Party", "Federal State", "Immigrants (%)", "GDP per capita", 
  "Unemployement Rate (%)", "Vote share (%)"
)

#select topics 1,4,6 and covariates
formula <- c(1,4,6)~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)
metadata <- data$meta[varlist]
#factorize
metadata[sapply(metadata, is.character)] <- lapply(metadata[sapply(metadata, is.character)], 
                                       as.factor)
# ----------------------------------------------------------------------------------------------
est_betaregs <- function(stmobj, formula, metadata, nsims = 100, seed = 123) {
  response <- as.character(formula)[2]
  topic_n <- eval(parse(text=response))
  topic_nam <- paste0("Topic", topic_n)
  f <- paste(topic_nam, "~", as.character(formula)[3])
  res <- list()
  set.seed(seed)
  formals(glm)$family <- quasibinomial(link = "logit")
  for (k in 1:length(topic_n)) {
    theta_sim <- do.call(rbind,
                         stm::thetaPosterior(stmobj, nsims = nsims, type = "Global"))[,topic_n[k]]
    theta_sim <- lapply(split(1:(NROW(theta_sim)), 1:nsims),
                        function(i) setNames(data.frame(theta_sim[i]), topic_nam[k]))
    res[[k]] <- lapply(theta_sim, function(x) {
      rstanarm::stan_betareg(
        formula = as.formula(f[k]), 
        link = "logit", link.phi = "log",
        data = cbind(x, metadata), 
        algorithm = "optimizing", seed = seed
      )})
  }
  return(setNames(res, topic_nam))
}
# MAP estimates of 100 bayesian beta regressions
mod_betaregs<-est_betaregs(mod_prev, formula, metadata, nsims = 25)
# ----------------------------------------------------------------------------------------------

topic_preds <- list()
est_var <- "t"
for(est_var in varlist){
  if(is.numeric(metadata[,est_var])) {
    range_est_var <- seq(min(metadata[,est_var]), max(metadata[,est_var]), length.out = 500)
  } else {
    range_est_var <- unique(metadata[,est_var])
  }
  dat_new <- lapply(metadata[, -which(names(metadata) == est_var)], 
                    function(x) if(is.numeric(x)) median(x) else majority(x))
  xmat <- data.frame(dat_new, range_est_var)
  names(xmat) <- c(names(dat_new),est_var)
  levels(xmat$Partei) <- levels(metadata$Partei)
  levels(xmat$Bundesland) <- levels(metadata$Bundesland)
  # --------------------------------------------------------------------------------------------
  nm <- c(est_var, "proportion", "ci_lower", "ci_upper")
  
  preds_topic1 <- do.call(rbind, 
                          lapply(mod_betaregs$Topic1, function(x) posterior_predict(x, xmat, draws = 1000)))
  topic1_mean <- colMeans(preds_topic1)
  topic1_lower <- apply(preds_topic1, 2, quantile, 0.025)
  topic1_upper <- apply(preds_topic1, 2, quantile, 0.975)
  topic1_res <- setNames(data.frame(range_est_var, topic1_mean, topic1_lower, topic1_upper), nm)
  
  preds_topic4 <- do.call(rbind, 
                          lapply(mod_betaregs$Topic4, function(x) posterior_predict(x, xmat, draws = 1000)))
  topic4_mean <- colMeans(preds_topic4)
  topic4_lower <- apply(preds_topic4, 2, quantile, 0.025)
  topic4_upper <- apply(preds_topic4, 2, quantile, 0.975)
  topic4_res <- setNames(data.frame(range_est_var, topic4_mean, topic4_lower, topic4_upper), nm)
  
  preds_topic6 <- do.call(rbind, 
                          lapply(mod_betaregs$Topic6, function(x) posterior_predict(x, xmat, draws = 1000)))
  topic6_mean <- colMeans(preds_topic6)
  topic6_lower <- apply(preds_topic6, 2, quantile, 0.025)
  topic6_upper <- apply(preds_topic6, 2, quantile, 0.975)
  topic6_res <- setNames(data.frame(range_est_var, topic6_mean, topic6_lower, topic6_upper), nm)
  # ----------------------------------------------------------------------------------------------
  topic_preds[[est_var]] <- list(topic1_res, topic4_res, topic6_res)
  names(topic_preds[[est_var]]) <- c("Topic1", "Topic4", "Topic6")
}

str(topic_preds)

saveRDS(topic_preds, "./data/topic_preds.rds")