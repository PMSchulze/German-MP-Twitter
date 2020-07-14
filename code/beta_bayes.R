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
        data = cbind(x, metadata), 
        link = "logit", link.phi = "log",
        algorithm = "optimizing", seed = seed
      )})
  }
  return(setNames(res, topic_nam))
}
# MAP estimates of 100 bayesian beta regressions
mod_betaregs<-est_betaregs(mod_prev, formula, metadata, nsims = 15)
# ----------------------------------------------------------------------------------------------

est_var <- "t"
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
# ----------------------------------------------------------------------------------------------
preds_topic6 <- do.call(rbind, 
                        lapply(mod_betaregs$Topic6, function(x) posterior_predict(x, dat_fit, draws = 1000)))
topic6_mean <- colMeans(preds_topic6)
topic6_lower <- apply(preds_topic6, 2, quantile, 0.025)
topic6_upper <- apply(preds_topic6, 2, quantile, 0.975)
plot(xmat$t, topic6_mean, type = "l")
lines(xmat$t, topic6_lower, type = "l")
lines(xmat$t, topic6_upper, type = "l")
