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
topic_user_preprocessed <- readRDS("./data/topic_spd_user_preprocessed.rds")
colnames_table <- read.csv(file = "./data/topic_user_colnames.csv")

# extract documents, vocabulary and metadata
docs <- topic_user_preprocessed$documents
vocab <-  topic_user_preprocessed$vocab
meta <- topic_user_preprocessed$meta

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

search_mods <- stm::searchK(
    documents = docs,
    vocab = vocab,
    data = meta,
    K = c(5,6,7,8),
    prevalence =~ Bundesland + s(Anzahl_Follower) + s(Struktur_4) + 
      s(Struktur_22) + s(Struktur_42) + s(Struktur_54),
    max.em.its = 85,
    init.type = "Spectral"
)

plot(search_mods)

# set number of topics to 6
n_topics = 6

# prevalence model
mod_prev <- stm::stm(
  documents = docs,
  vocab = vocab,
  data = meta,
  K = n_topics,
  prevalence =~ Anzahl_Follower,
  max.em.its = 85,
  init.type = "Spectral")

### top words per topic
labelTopics(mod_prev)

### summary visualization
plot(mod_prev, type = "summary", xlim = c(0, 0.5))

### relationship topic/vocabulary
plot(mod_prev, type = "perspectives", topics = c(3, 6))

### estimating metadata/topic relationship
meta$Bundesland <- as.factor(meta$Bundesland)
prep <- stm::estimateEffect(1:6 ~ s(Struktur_4),
                            mod_prev,
                            # documents = docs,
                            metadata = meta,
                            uncertainty = "Global")
summary(prep, topics = 1)

### metadata/topic relationship visualization
plot(prep, "Bundesland", method = "difference", topics = c(1,2,3,4,5,6,7), cov.value1 = "Bayern",
     cov.value2 = "Berlin", model = mod_prev, xlab = "Bundesland")
plot(prep, "Bundesland", method = "pointestimate", topics = 3,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "Bundesland")
plot(prep, "Anzahl_Follower", method = "continuous", topics = 3, ci.level = 0.8,
     model = mod_prev, printlegend = FALSE, ylim = c(0,1), main = "Arbeit & Soziales",
     xlab = "Anzahl Twitter-Follower", ylab = "Themenanteil", 
     yaxs="i", xaxs="i")
plot(prep, "Struktur_4", method = "continuous", topics = 3, ci.level = 0.8,
     model = mod_prev, printlegend = FALSE, ylim = c(0,1), main = "Arbeit & Soziales",
     xlab = "Ausländeranteil", ylab = "Themenanteil", 
     yaxs="i", xaxs="i")
plot(prep, "Struktur_22", method = "continuous", topics = 3, ci.level = 0.8,
     model = mod_prev, printlegend = FALSE, ylim = c(0,1), main = "Arbeit & Soziales",
     xlab = "BIP pro Kopf", ylab = "Themenanteil", 
     yaxs="i", xaxs="i")
plot(prep, "Struktur_42", method = "continuous", topics = 3, ci.level = 0.8,
     model = mod_prev, printlegend = FALSE, ylim = c(0,1),
     xlab = "Arbeitslosenquote", ylab = "Themenanteil", main = "Arbeit & Soziales",
     yaxs="i", xaxs="i")

# ----------------------------------------------------------------------------------------------

# simulate topic proportions for each document (500 simulations/doc) and average
theta_sim <- thetaPosterior(mod_prev, nsims = 500, type = "Local", documents = docs) %>%
  lapply(colMeans) %>% 
  (function(s) do.call(rbind,s)) %>% 
  as_tibble() %>% 
  setNames(paste0("Topic", 1:n_topics))
# link topic proportions to names of parliamentarians
docs_topics <- tibble("Name" = names(docs), theta_sim)
# e.g. list parliamentarians with highest proportion of topic 3
arrange(docs_topics, desc(Topic3))

# ----------------------------------------------------------------------------------------------
# unemployment_range <- seq(from = 0,
#                 to = 0.45, by = 0.05)
# monthnames <- months(monthseq)
# axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
#                 labels = monthnames)

# prevalence model w/ interaction
mod_prev_int <- stm::stm(
  documents = docs,
  vocab = vocab,
  data = meta,
  K = 6,
  prevalence =~ Partei + Bundesland + s(Geburtsjahr) + v_4 + v_22 + v_42 + Partei*v_42,
  max.em.its = 50,
  init.type = "Spectral")


prep <- estimateEffect(c(3) ~ Partei * v_42, mod_prev_int,
                       metadata = meta, uncertainty = "None")
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "CDU/CSU", linecol = "black", ylim = c(0, 0.30),
     printlegend = FALSE)
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "SPD", linecol = "red", ylim = c(0, 0.30),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "Bündnis 90/Die Grünen", linecol = "green", ylim = c(0, 0.30),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "Die Linke", linecol = "purple", ylim = c(0, 0.30),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "FDP", linecol = "yellow", ylim = c(0, 0.30),
     printlegend = FALSE, add = TRUE)
plot(prep, covariate = "v_42", model = mod_prev_int,
     method = "continuous", xlab = "Unemployment", moderator = "Partei",
     moderator.value = "AfD", linecol = "blue", ylim = c(0, 0.30),
     printlegend = FALSE, add = TRUE)
# legend(0, 0.06, c("CDU/CSU", "SPD"), lwd = 2,
#         col = c("black", "red"))


# topical content
mod_topic <- stm::stm(
  documents = docs,
  vocab = vocab, 
  data = meta,
  K = 5,
  prevalence =~ Partei + Bundesland + s(Geburtsjahr) + s(v_4) + s(v_22) + s(v_42),
  content =~ Partei,
  max.em.its = 75,
  init.type = "Spectral")

### word cloud
cloud(mod_topic, topic = 4, scale = c(2, 0.25))
