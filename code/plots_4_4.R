devtools::install_github("PMSchulze/stmprevalence")
library(grid)
library(gridExtra)
library(scales)
library(stmprevalence)
library(tidyverse)

# ----------------------------------------------------------------------------------------------

# set working directory
setwd("/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse")
# load data
data <- readRDS("./data/preprocessed_monthly.rds")
# load fitted topic model
mod_prev <- readRDS("./data/mod_prev_monthly.rds")
# load topic labels
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
# load list of prevalence covariates
varlist <- c(
  "t", "Partei", "Bundesland", "Struktur_4", "Struktur_22", "Struktur_42", "Struktur_54"
)
# load full names of prevalence covariates
varlist_fullnames <- c(
  "Time", "Party", "Federal State", "Immigrants (%)", "GDP per capita", 
  "Unemployement Rate (%)", "vote share (%)"
)

formula <- 1:15~Partei+ Bundesland + s(t, df = 5) + s(Struktur_4, df = 5) + 
  s(Struktur_22, df = 5) + s(Struktur_42, df = 5) + s(Struktur_54, df = 5)

# ----------------------------------------------------------------------------------------------
# ---------------------------------- Plots with estimateEffect ---------------------------------
# ----------------------------------------------------------------------------------------------

prep <- stm::estimateEffect(
  formula,
  mod_prev,
  metadata = data$meta,
  uncertainty = "Global"
)
plot(prep, "t", method = "continuous", topics = 1,
     main = "Topic 1: Right/Nationalist", printlegend = F, xlab = "t")
plot(prep, "t", method = "continuous", topics = 3,
     main = "Topic 3: Green/Climate", printlegend = F, xlab = "t")

# ----------------------------------------------------------------------------------------------
# ---------------------- Plots with stmprevalence: Method of Composition -----------------------
# ----------------------------------------------------------------------------------------------

# factorize categorical variables, set CDU/CSU as reference category for variable "Partei"
data$meta$Partei <- data$meta$Partei %>%
  as.factor() %>%
  relevel(ref = 3)
data$meta$Bundesland <- as.factor(data$meta$Bundesland)

# # estimate 100 beta regressions and sample from regressions coefficients
# all_betas <- sample_coefs(mod_prev, formula, type = "beta",
#                             data$meta, nsims = 100, seed = 123)
# # estimate 100 quasibinomial glms and sample from regressions coefficients
# all_quasibin <- sample_coefs(mod_prev, formula, type = "quasibinomial",
#                             data$meta, nsims = 100, seed = 123)
# # save results
# saveRDS(all_betas, "./data/all_betas.rds")
# saveRDS(all_quasibin, "./data/all_quasibin.rds")

# load previously computed results
all_betas <- readRDS("./data/all_betas.rds")
all_quasibin <- readRDS("./data/all_quasibin.rds")
# predict thetas using beta regression for all variables
preds_beta <- lapply(varlist, 
                     function(v) stmprevalence::predict_props(all_betas, v, formula, data$meta))
# predict thetas using quasibinomial glm for all variables
preds_quasibin <- lapply(varlist, 
                         function(v) stmprevalence::predict_props(all_quasibin, v, formula, data$meta))
names(preds_beta) <- names(preds_quasibin) <- varlist

# ----------------------------------------------------------------------------------------------

# First, we create all plots using the qusibinomial GLM; these plots are shown in main section

## Topic 3: Green/Climate -- Quasibinomial GLM
### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_quasibin_", v)
  assign(plot_nam, ggplot(preds_quasibin[[v]]$Topic3, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           xlab(varlist_fullnames[varlist==v]) +
           ylab("Expected Topic Proportion") +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma))
}
gridExtra::grid.arrange(
  plot_quasibin_t, plot_quasibin_Struktur_4, plot_quasibin_Struktur_22, plot_quasibin_Struktur_42, ncol=2, 
  top = grid::textGrob("Topic 3: Green/Climate", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

## Topic 3: Green/Climate -- Quasibinomial GLM
### Categorial Plots
(plot_party_3 <- ggplot(preds_quasibin$Partei$Topic3, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 3: Green/Climate") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

## Topic 4: Social/Housing -- Quasibinomial GLM
### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_quasibin_", v)
  assign(plot_nam, ggplot(preds_quasibin[[v]]$Topic4, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma))
}
gridExtra::grid.arrange(
  plot_quasibin_t, plot_quasibin_Struktur_4, plot_quasibin_Struktur_22, plot_quasibin_Struktur_42, 
  ncol=2, 
  top = grid::textGrob("Topic 4: Social/Housing", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

## Topic 4: Social/Housing -- Quasibinomial GLM
### Categorial Plots
### Categorial Plots
(plot_party_4 <- ggplot(preds_quasibin$Partei$Topic4, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 4: Social/Housing")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

## Topics 1,3, and 4 -- Quasibinomial GLM
### Categorial Plots
preds_quasibin$Partei$Topic1$Topic <- "Right/Nationalist"
preds_quasibin$Partei$Topic3$Topic <- "Green/Climate"
preds_quasibin$Partei$Topic4$Topic <- "Social/Housing"
party_data <- rbind(preds_quasibin$Partei$Topic1, preds_quasibin$Partei$Topic3, 
                    preds_quasibin$Partei$Topic4)
(plot_party <- ggplot(party_data, aes(y=proportion, x = Partei, fill = Topic)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values=c("#53A567FF", "#56A8CBFF", "#DA291CFF")) +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic Proportions by Party") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.text.x = element_text(angle = 45, hjust = 1)))

# ----------------------------------------------------------------------------------------------

# Then, we create all plots using the beta regression; these plots are shown in appendix

## Topic 3: Green/Climate -- Beta regression
### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_beta_", v)
  assign(plot_nam, ggplot(preds_beta[[v]]$Topic3, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           xlab(varlist_fullnames[varlist==v]) +
           ylab("Expected Topic Proportion") +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma))
}
gridExtra::grid.arrange(
  plot_beta_t, plot_beta_Struktur_4, plot_beta_Struktur_22, plot_beta_Struktur_42, ncol=2, 
  top = grid::textGrob("Topic 3: Green/Climate", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

## Topic 3: Green/Climate -- Beta regression
### Categorial Plots
(plot_party_3 <- ggplot(preds_beta$Partei$Topic3, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 3: Green/Climate") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

## Topic 4: Social/Housing -- Beta regression
### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_beta_", v)
  assign(plot_nam, ggplot(preds_beta[[v]]$Topic4, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma))
}
gridExtra::grid.arrange(
  plot_beta_t, plot_beta_Struktur_4, plot_beta_Struktur_22, plot_beta_Struktur_42, ncol=2, 
  top = grid::textGrob("Topic 4: Social/Housing", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

## Topic 4: Social/Housing -- Beta regression
(plot_party_4 <- ggplot(preds_beta$Partei$Topic4, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 4: Social/Housing")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

## Topics 1,3, and 4 -- Beta regression
### Categorial Plots
preds_beta$Partei$Topic1$Topic <- "Right/Nationalist"
preds_beta$Partei$Topic3$Topic <- "Green/Climate"
preds_beta$Partei$Topic4$Topic <- "Social/Housing"
party_data <- rbind(preds_beta$Partei$Topic1, preds_beta$Partei$Topic3, preds_beta$Partei$Topic4)
(plot_party <- ggplot(party_data, aes(y=proportion, x = Partei, fill = Topic)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values=c("#53A567FF", "#56A8CBFF", "#DA291CFF")) +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic Proportions by Party") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.text.x = element_text(angle = 45, hjust = 1)))

# ----------------------------------------------------------------------------------------------
# ------------------- Plots with stmprevalence: Direct assessment ------------------------------
# ----------------------------------------------------------------------------------------------

formula <- 1:15~Partei + Bundesland  + s(Struktur_4) + 
  s(Struktur_22) + s(Struktur_42) + s(Struktur_54) + s(t)

# # Sample from LogisticNormal and calculate mean and credible intervals; 
# # pre-calculated for speed reasons
# preds_logisticn <- lapply(varlist, function(v) sample_props_logisticn(mod_prev, v, formula, data$meta))
# names(preds_logisticn) <- varlist
# saveRDS(preds_logisticn, "./data/all_logisticn.rds")

# load previously calculated samples from LogisticNormal
preds_logisticn <- readRDS("./data/all_logisticn.rds")

## Topic 3: Green/Climate
### Continuous plot without credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_logisticn_", v)
  assign(plot_nam, ggplot(preds_logisticn[[v]]$Topic3, aes(x = !!as.symbol(v), y = proportion)) +
           geom_smooth(color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma))
}
gridExtra::grid.arrange(plot_logisticn_t, plot_logisticn_Struktur_4, 
                        plot_logisticn_Struktur_22, plot_logisticn_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 3: Green/Climate", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 3: Green/Climate
### Continuous plot with credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_logisticn_", v)
  plot_smoothed_ci <- ggplot(preds_logisticn[[v]]$Topic3) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_lower), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_upper), method = "loess", se = FALSE)
  smoothed_ci <- ggplot_build(plot_smoothed_ci)
  df_smoothed_ci <- data.frame(v = smoothed_ci$data[[1]]$x,
                               ci_lower = smoothed_ci$data[[1]]$y,
                               ci_upper = smoothed_ci$data[[2]]$y)
  assign(plot_nam, plot_smoothed_ci + 
           geom_ribbon(data = df_smoothed_ci, aes(x = v, ymin = ci_lower, ymax = ci_upper), 
                       fill = "grey70") +
           geom_smooth(data = preds_logisticn[[v]]$Topic3, aes(x = !!as.symbol(v), y = proportion),
                       color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma))
}
gridExtra::grid.arrange(plot_logisticn_t, plot_logisticn_Struktur_4, 
                        plot_logisticn_Struktur_22, plot_logisticn_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 3: Green/Climate", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 3: Green/Climate
### Categorial plot with credible intervals
(plot_logisticn_party_4 <- ggplot(preds_logisticn$Partei$Topic3, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 3: Green/Climate")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))


# For topic 4, we show our plots in appendix

## Topic 4: Social/Housing
### Continuous plot without credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_logisticn_", v)
  assign(plot_nam, ggplot(preds_logisticn[[v]]$Topic4, aes(x = !!as.symbol(v), y = proportion)) +
           geom_smooth(color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma))
}
gridExtra::grid.arrange(plot_logisticn_t, plot_logisticn_Struktur_4, 
                        plot_logisticn_Struktur_22, plot_logisticn_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 4: Social/Housing", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 4: Social/Housing
### Continuous plot with credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_logisticn_", v)
  plot_smoothed_ci <- ggplot(preds_logisticn[[v]]$Topic4) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_lower), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_upper), method = "loess", se = FALSE)
  smoothed_ci <- ggplot_build(plot_smoothed_ci)
  df_smoothed_ci <- data.frame(v = smoothed_ci$data[[1]]$x,
                               ci_lower = smoothed_ci$data[[1]]$y,
                               ci_upper = smoothed_ci$data[[2]]$y)
  assign(plot_nam, plot_smoothed_ci + 
           geom_ribbon(data = df_smoothed_ci, aes(x = v, ymin = ci_lower, ymax = ci_upper), 
                       fill = "grey70") +
           geom_smooth(data = preds_logisticn[[v]]$Topic4, aes(x = !!as.symbol(v), y = proportion),
                       color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma))
}
gridExtra::grid.arrange(plot_logisticn_t, plot_logisticn_Struktur_4, 
                        plot_logisticn_Struktur_22, plot_logisticn_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 4: Social/Housing", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 4: Social/Housing
### Categorial plot with credible intervals
(plot_logisticn_party_4 <- ggplot(preds_logisticn$Partei$Topic4, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 4: Social/Housing")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
