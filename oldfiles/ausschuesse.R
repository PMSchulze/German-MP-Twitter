library(stringi)
library(tidyverse)
# -------------------------------------------------------------------------------------------------------------------------

# load data
topic_user_preprocessed <- readRDS("./data/topic_user_preprocessed.rds")
colnames_table <- read.csv(file = "./data/colnames_table.csv")

# extract documents, vocabulary and metadata
docs <- topic_user_preprocessed$documents
vocab <-  topic_user_preprocessed$vocab
meta <- topic_user_preprocessed$meta

# --------------------------- Generate dummy variables for Ausschuss -------------------------------------------------------

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
colnames(ausschuesse_dummy) <- paste0("Ausschuss_", 1:25, "_Bin")

# add columns to the data frame (and delete inital Ausschuss variable)
meta <- meta %>% 
  add_column(ausschuesse_dummy, .after = "Ausschusspositionen") %>%
  select(-"Ausschusspositionen")

# -------------------------------------------------------------------------------------------------------------------------

