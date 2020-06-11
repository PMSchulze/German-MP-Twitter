# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "betareg", "matrixStats", "mvtnorm", "quanteda", "stm"
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
setwd("/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse/code")

# ----------------------------------------------------------------------------------------------
# ------------------------------ Write general helper functions --------------------------------
# ----------------------------------------------------------------------------------------------

# sigmoid function; inverse of logit-link
sigmoid <- function(x) exp(x)/(1+exp(x))

# obtain majority vote for categorial variable
majority <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# create design matrix where all columns but est_var fixed as median/majority values
make_median_xmat <- function(est_var, formula, metadata, range_est_var) {
  dat <- metadata[, -which(names(metadata) == est_var)]
  dat_new <- lapply(dat, function(x) if(is.numeric(x)) median(x) else majority(x))
  dat_fit <- data.frame(dat_new, range_est_var)
  names(dat_fit) <- c(names(dat_new),est_var)
  f <- paste("~",as.character(formula)[3])
  return(stm::makeDesignMatrix(as.formula(f), metadata, dat_fit))
}

# helper function to sample from normal distribution of regression coefficients;
## allowed types are beta regression or quasibinomial glm
sample_normal <- function(mod, type) {
  mu <- var <- NULL
  if (type == "beta"){
    mu <- mod$coefficients$mean
    var <- mod$vcov[1:length(mu), 1:length(mu)]
  } else if (type == "quasibinomial") {
    mu <- mod$coefficients
    var <- vcov(mod)
  } else {
    stop("Error: Please set type='beta' or type='quasibinomial'")
  }
  return(mvtnorm::rmvnorm(1, mean = mu, sigma = var))
}

# helper function to simulate theta (either mean or quantile p) from LogisticNormal 
## for single document, given mean mu_d of gaussian
sim_theta_d <- function(mu_d, Sigma, nsims, p = "mean") {
  eta_sim <- cbind(mvtnorm::rmvnorm(nsims, mu_d, Sigma),0)
  theta_sim <- exp(eta_sim - matrixStats::rowLogSumExps(eta_sim))
  colnames(theta_sim) <- c()
  return(apply(theta_sim, 2, function(x) if(p=="mean") mean(x) else quantile(x, probs = p)))
}

# helper function to simulate mean as well as quantiles ci_lower, ci_upper of LogisticNormal,
## for all documents
sim_theta <- function(mu, Sigma, nsims, ci_lower, ci_upper) {
  topic_n <- ncol(mu)+1
  mean_emp <- apply(mu, 1, sim_theta_d, Sigma = Sigma, p = "mean", nsims = nsims)
  ci_lower <- apply(mu, 1, sim_theta_d, Sigma = Sigma, p = ci_lower, nsims = nsims)
  ci_upper <- apply(mu, 1, sim_theta_d, Sigma = Sigma, p = ci_upper, nsims = nsims)
  nm <- c("proportion", "ci_lower", "ci_upper")
  res <- lapply(1:topic_n, 
                function(i) setNames(data.frame(mean_emp[i,], ci_lower[i,], ci_upper[i,]), nm))
  return(setNames(res, paste0("Topic", 1:topic_n)))
}

# ----------------------------------------------------------------------------------------------
# --------------------------- Perform Method of Composition  -----------------------------------
# ----------------------------------------------------------------------------------------------

# implements method of composition:
## (1) perform nsim regressions (beta regression or quasibinomial glm, depending on specified type)
## (2) sample from resulting distributions of regression coefficents
sample_coefs <- function(stmobj, formula, type, metadata, nsims = 25, seed = NULL) {
  response <- as.character(formula)[2]
  topic_n <- eval(parse(text=response))
  topic_nam <- paste0("Topic", topic_n)
  f <- paste(topic_nam, "~", as.character(formula)[3])
  res <- list()
  set.seed(seed)
  formals(glm)$family <- quasibinomial(link = "logit")
  fit_reg <- if (type == "beta") betareg::betareg else if (type == "quasibinomial") glm
  for (k in 1:length(topic_n)) {
    theta_sim <- do.call(rbind, 
                         stm::thetaPosterior(stmobj, nsims = nsims, type = "Global"))[,topic_n[k]]
    theta_sim <- lapply(split(1:(NROW(theta_sim)), 1:nsims),
                        function(i) setNames(data.frame(theta_sim[i]), topic_nam[k]))
    res[[k]] <- lapply(theta_sim, function(x) {
      res_tmp <- fit_reg(formula = as.formula(f[k]), data = cbind(x, metadata))
      sample_normal(res_tmp, type)}
    )
  }
  return(setNames(res, topic_nam))
}

# obtain theta proportions and credible intervals (given set of previously sampled coefficients)
## for full range of variable est_var, holding all variables but est_var as median/majority
predict_props <- function(beta_coefs, est_var, formula, 
                          metadata, ci_lower = 0.025, ci_upper = 0.975) {
  response <- as.character(formula)[2]
  topic_n <- eval(parse(text=response))
  topic_nam <- paste0("Topic", topic_n)
  if(is.numeric(metadata[,est_var])) {
    range_est_var <- seq(min(metadata[,est_var]), max(metadata[,est_var]), length.out = 500)
  } else {
    range_est_var <- unique(metadata[,est_var])
  }
  xmat <- make_median_xmat(est_var, formula, metadata, range_est_var)
  res <- list()
  for (k in topic_nam){
    fit_vals <- do.call(cbind, lapply(beta_coefs[[k]], function(x) sigmoid(xmat %*% t(x))))
    mu <- quanteda::rowMeans(fit_vals)
    ci <- apply(fit_vals, 1, function(x) quantile(x, probs = c(ci_lower, ci_upper)))
    res[[k]] <- data.frame(range_est_var, mu, ci[1,], ci[2,])
    names(res[[k]]) <- c(est_var, "proportion", "ci_lower", "ci_upper")
  }
  return(setNames(res, topic_nam))
}

# ----------------------------------------------------------------------------------------------
# ------------------------------ Sample from LogisticNormal ------------------------------------
# ----------------------------------------------------------------------------------------------

# sample mean and credible interval from LogisticNormal for a given model (stmobj)
## and full observed range of variable (est_var), 
## holding all variables but est_var as median/majority
sample_props_logisticn <- function(stmobj, est_var, formula, metadata, nsims = 1000, 
                                 ci_lower = 0.025, ci_upper = 0.975, seed = NULL) {
  gamma <- stmobj$mu$gamma
  Sigma <- stmobj$sigma
  if(is.numeric(metadata[,est_var])) {
    range_est_var <- seq(min(metadata[,est_var]), max(metadata[,est_var]), length.out = 500)
  } else {
    range_est_var <- unique(metadata[,est_var])
  }
  xmat <- make_median_xmat(est_var, formula, metadata, range_est_var)
  mu <- xmat %*% gamma
  set.seed(seed)
  est <- sim_theta(mu, Sigma, nsims = nsims, ci_lower = ci_lower, ci_upper = ci_upper)
  res <- lapply(est, function(x) setNames(cbind(range_est_var, x), c(est_var, names(x))))
  return(res)
}