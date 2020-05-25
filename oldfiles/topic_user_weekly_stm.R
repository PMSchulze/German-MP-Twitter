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
setwd('C:\\Users\\Simon\\OneDrive\\Uni\\LMU\\SS 2020\\Statistisches Consulting\\Bundestag-MP-Analyse')
# setwd("/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse")

# ----------------------------------------------------------------------------------------------
# ---------------------------------------------- STM -------------------------------------------
# ----------------------------------------------------------------------------------------------

# load data
data <- readRDS("./data/topic_user_weekly_preprocessed.rds")
colnames_table <- read.csv(file = "./data/topic_user_weekly_colnames.csv")

# extract documents, vocabulary and metadata
docs <- data$documents
vocab <-  data$vocab
meta <- data$meta

# --------------------------- Generate dummy variables for Ausschuss ---------------------------

# Bundestagsauschuesse
ausschuesse <- c(
  "Ausschuss für Arbeit und Soziales",
  "Auswärtiger Ausschuss",
  "Ausschuss für Bau, Wohnen, Stadtentwicklung und Kommunen",
  "Ausschuss für Bildung, Forschung und Technikfolgenabschätzung",
  "Ausschuss Digitale Agenda",
  "Ausschuss für Ernährung und Landwirtschaft",
  "Ausschuss für die Angelegenheiten der Europäischen Union",
  "Ausschuss für Familie, Senioren, Frauen und Jugend",
  "Finanzausschuss",
  "Ausschuss für Gesundheit",
  "Haushaltsausschuss",
  "Ausschuss für Inneres und Heimat",
  "Ausschuss für Kultur und Medien",
  "Ausschuss für Menschenrechte und humanitäre Hilfe",
  "Petitionsausschuss",
  "Ausschuss für Recht und Verbraucherschutz",
  "Sportausschuss",
  "Ausschuss für Tourismus",
  "Ausschuss für Umwelt, Naturschutz und nukleare Sicherheit",
  "Ausschuss für Verkehr und digitale Infrastruktur",
  "Verteidigungsausschuss",
  "Wahlprüfungsausschuss",
  "Ausschuss für Wahlprüfung, Immunität und Geschäftsordnung",
  "Ausschuss für Wirtschaft und Energie",
  "Ausschuss für wirtschaftliche Zusammenarbeit und Entwicklung"
)

# create a column for each Ausschuss and check membership (1 = yes, 0 = no)
ausschuesse_dummy <- purrr::map_dfc(ausschuesse, function(s) {
  as.numeric(stringi::stri_detect_fixed(meta$Ausschusspositionen, s))
}
)
colnames(ausschuesse_dummy) <- paste0("Ausschuss_", 1:25)

# add columns to the data frame (and delete inital Ausschuss variable)
meta <- meta %>% 
  add_column(ausschuesse_dummy, .after = "Ausschusspositionen") %>%
  select(-"Ausschusspositionen")

# ----------------------------------------------------------------------------------------------

# set CDU/CSU as reference category for variable "Partei"
meta$Partei <- meta$Partei %>%
  as.factor() %>%
  relevel(ref = 3)

# search hyperparameter space for optimal K
hyperparameter_search <- searchK(documents = docs,
        vocab = vocab,
        K = c(5,6,7,8,9,10),
        init.type = "Spectral",
        proportion = 0.2, heldout.seed = 123, M = 10)

# topical prevalence
mod_prev <- stm::stm(
  documents = docs,
  vocab = vocab,
  data = meta,
  K = 6,
  prevalence =~ Partei + Bundesland + s(Geburtsjahr) + s(Struktur_4)
  + s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
  max.em.its = 75,
  init.type = "Spectral")

### top words per topic
labelTopics(mod_prev)

### all topics with overall proportions
plot(mod_prev, type = "summary", xlim = c(0, 0.3))

### manually label topics
mod_prev_labels <- c("right/national",
                     "miscellaneous",
                     "left/social",
                     "green/climate",
                     "miscellaneous",
                     "Europe")

### word clouds
cloud(mod_prev, topic = 1, scale = c(2, 0.25))

### vocabulary usage across two topics
plot(mod_prev, type = "perspectives", topics = c(3, 6))

## metadata/topic relationship
meta$Bundesland <- as.factor(meta$Bundesland)
prep <- stm::estimateEffect(1:6 ~ Partei + Bundesland + s(Geburtsjahr) + s(Struktur_4)
                            + s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
                            mod_prev,
                            metadata = meta,
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
plot(prep, "Struktur_4", method = "continuous", topics = 3,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "% immigrants",
     ylim = c(-0.1, 0.15))
axis(1, tick = TRUE)

### impact of immigrant share on prevalence of topic "right/national"
plot(prep, "Struktur_4", method = "continuous", topics = 1,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "% immigrants",
     ylim = c(0.05, 0.25))
axis(1, tick = TRUE)

### impact of immigrant share on prevalence of topic "Europe"
plot(prep, "Struktur_4", method = "continuous", topics = 6,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "% immigrants",
     ylim = c(0.1, 0.25))
axis(1, tick = TRUE)

## prevalence model w/ interaction
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

prep <- estimateEffect(c(3) ~ Partei * Struktur_42, mod_prev_int,
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

prep <- estimateEffect(c(3) ~ Partei * Struktur_22, mod_prev_int,
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

# topical content
mod_topic <- stm::stm(
  documents = docs,
  vocab = vocab, 
  data = meta,
  K = 6,
  prevalence =~ Partei + Bundesland + s(Geburtsjahr) + s(Struktur_4)
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
cloud(mod_topic, topic = 6, scale = c(2, 0.25))

