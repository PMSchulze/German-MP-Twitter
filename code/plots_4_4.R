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
plot(prep, "t", method = "continuous", topics = 6,
     main = "Topic 6: Climate Protection", printlegend = F, xlab = "t")

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
# all_quasibin <- stmprevalence::sample_coefs(mod_prev, formula, type = "quasibinomial",
#                            data$meta, nsims = 100, seed = 123)
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

## Topic 6: Climate Protection -- Quasibinomial GLM
### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_quasibin_", v)
  assign(plot_nam, ggplot(preds_quasibin[[v]]$Topic6, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           xlab(varlist_fullnames[varlist==v]) +
           ylab("Expected Topic Proportion") +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
}
gridExtra::grid.arrange(
  plot_quasibin_t, plot_quasibin_Struktur_4, plot_quasibin_Struktur_22, plot_quasibin_Struktur_42, ncol=2, 
  top = grid::textGrob("Topic 6: Climate Protection", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

## Topic 6: Climate Protection -- Quasibinomial GLM
### Categorial Plots
(plot_party_3 <- ggplot(preds_quasibin$Partei$Topic6, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 6: Climate Protection") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))

## Topic 4: Social/Housing -- Quasibinomial GLM
### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_quasibin_", v)
  assign(plot_nam, ggplot(preds_quasibin[[v]]$Topic4, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
           axis.title.x = element_text(size=16)))
}
gridExtra::grid.arrange(
  plot_quasibin_t, plot_quasibin_Struktur_4, plot_quasibin_Struktur_22, plot_quasibin_Struktur_42, 
  ncol=2, 
  top = grid::textGrob("Topic 4: Social/Housing", gp=grid::gpar(fontsize=16, fontface = "bold"))
)

## Topic 4: Social/Housing -- Quasibinomial GLM
### Categorial Plots
(plot_party_4 <- ggplot(preds_quasibin$Partei$Topic4, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 4: Social/Housing")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))

## Topics 1,3, and 4 -- Quasibinomial GLM
### Categorial Plots
preds_quasibin$Partei$Topic1$Topic <- "Right/Nationalist"
preds_quasibin$Partei$Topic6$Topic <- "Climate Protection"
preds_quasibin$Partei$Topic4$Topic <- "Social/Housing"
party_data <- rbind(preds_quasibin$Partei$Topic1, preds_quasibin$Partei$Topic6, 
                    preds_quasibin$Partei$Topic4)
(plot_party <- ggplot(party_data, aes(y=proportion, x = Partei, fill = Topic)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values=c("#53A567FF", "#56A8CBFF", "#DA291CFF")) +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic Proportions by Party") +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

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
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
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
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))

## Topic 4: Social/Housing -- Beta regression
### Continuous Plots
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_beta_", v)
  assign(plot_nam, ggplot(preds_beta[[v]]$Topic4, aes(!!as.symbol(v))) + 
           geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70") +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) +
           geom_line(aes(y = proportion)) +
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
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
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))

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
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# ----------------------------------------------------------------------------------------------
# ------------------- Plots with stmprevalence: Direct assessment ------------------------------
# ----------------------------------------------------------------------------------------------

# # Sample from LogisticNormal and calculate mean and credible intervals; 
# # pre-calculated for speed reasons
preds_logisticn <- lapply(varlist, function(v) sample_props_logisticn(mod_prev, v, formula, data$meta))
names(preds_logisticn) <- varlist
saveRDS(preds_logisticn, "./data/all_logisticn.rds")

# load previously calculated samples from LogisticNormal
preds_logisticn <- readRDS("./data/all_logisticn.rds")

## Topic 6: Climate Protection
### Continuous plot without credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_logisticn_", v)
  assign(plot_nam, ggplot(preds_logisticn[[v]]$Topic6, aes(x = !!as.symbol(v), y = proportion)) +
           geom_smooth(color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
}
gridExtra::grid.arrange(plot_logisticn_t, plot_logisticn_Struktur_4, 
                        plot_logisticn_Struktur_22, plot_logisticn_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 6: Climate Protection", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 6: Climate Protection
### Continuous plot with credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_logisticn_", v)
  plot_smoothed_ci <- ggplot(preds_logisticn[[v]]$Topic6) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_lower), method = "loess", se = FALSE) +
    stat_smooth(color=NA, aes(x = !!as.symbol(v), y = ci_upper), method = "loess", se = FALSE)
  smoothed_ci <- ggplot_build(plot_smoothed_ci)
  df_smoothed_ci <- data.frame(v = smoothed_ci$data[[1]]$x,
                               ci_lower = smoothed_ci$data[[1]]$y,
                               ci_upper = smoothed_ci$data[[2]]$y)
  assign(plot_nam, plot_smoothed_ci + 
           geom_ribbon(data = df_smoothed_ci, aes(x = v, ymin = ci_lower, ymax = ci_upper), 
                       fill = "grey70") +
           geom_smooth(data = preds_logisticn[[v]]$Topic6, aes(x = !!as.symbol(v), y = proportion),
                       color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
}
gridExtra::grid.arrange(plot_logisticn_t, plot_logisticn_Struktur_4, 
                        plot_logisticn_Struktur_22, plot_logisticn_Struktur_42, ncol=2, 
                        top = grid::textGrob("Topic 6: Climate Protection", 
                                             gp=grid::gpar(fontsize=16, fontface = "bold")))

## Topic 6: Climate Protection
### Categorial plot with credible intervals
(plot_logisticn_party_6 <- ggplot(preds_logisticn$Partei$Topic6, aes(y=proportion, x = Partei)) +
    geom_crossbar(aes(ymax = ci_upper, ymin = ci_lower), fill = "grey70") +
    xlab("Party") +
    ylab("Expected Topic Proportion") +
    ggtitle("Topic 6: Climate Protection")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))


# For topic 4, we show our plots in appendix

## Topic 4: Social/Housing
### Continuous plot without credible intervals
for(v in setdiff(varlist, c("Partei", "Bundesland"))){
  plot_nam <- paste0("plot_logisticn_", v)
  assign(plot_nam, ggplot(preds_logisticn[[v]]$Topic4, aes(x = !!as.symbol(v), y = proportion)) +
           geom_smooth(color = "black", method = "loess", se = FALSE, size = 0.8) +
           ylab("Expected Topic Proportion") +
           xlab(varlist_fullnames[varlist==v]) + 
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
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
           scale_x_continuous(labels = scales::comma) +
           theme(axis.text=element_text(size=12), 
                 axis.title.x = element_text(size=16)))
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
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
          axis.title.x = element_text(size=16)))
