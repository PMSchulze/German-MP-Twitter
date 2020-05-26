# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "quanteda", "stringi", "tidyverse"
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
setwd('/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse')

# ----------------------------------------------------------------------------------------------
# ------------------ Choose dataset for preprocessing ------------------------------------------
# ----------------------------------------------------------------------------------------------

# file <- "prep"
# file <- "prep_train"
# file <- "prep_test"
# file <- "prep_cdu"
# file <- "prep_cdu_train"
# file <- "prep_cdu_test"

# file <- "prep_monthly"
# file <- "prep_monthly_train"
file <- "prep_monthly_test"
# file <- "prep_cdu_monthly"
# file <- "prep_cdu_monthly_train"
# file <- "prep_cdu_monthly_test"

filepath <- paste0("./data/", file, ".rds")
data <- readRDS(filepath)

# ----------------------------------------------------------------------------------------------
# ------------------ Preprocessing data with the quanteda package ------------------------------
# ----------------------------------------------------------------------------------------------

if (grepl("monthly", file)) {
  data$docid <- paste0(data$Twitter_Username, "_", data$Jahr, "_", data$Monat)
  data$Datum <- with(data, sprintf("%d-%02d", Jahr, Monat))
  data <- data %>% 
    select(-c("Jahr", "Monat"))
  docid_field <- "docid"
} else {
  docid_field <- "Name"
}

# build corpus, which by default organizes documents into types, tokens and sentences
corp_topic <- quanteda::corpus(x = data, text_field = "Tweets_Dokument", 
                                  docid_field = docid_field)
# convert some special german characters and remove # infront of hashtags
corp_text_cleaned <- stringi::stri_replace_all_fixed(
  texts(corp_topic), 
  c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß","-"), 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss",""), 
  vectorize_all = FALSE
)
texts(corp_topic) <- corp_text_cleaned

# build document-feature matrix, where a feature corresponds to a word in our case
# stopwords, numbers and punctuation are removed and word stemming is performed
# each word is indexed and frequency per document assigned

# start with defining stopwords
stopwords_de <- read_lines(
  "https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt"
)
length(stopwords_de) # 620 stopwords
length(stopwords("de")) # 231 stopwords
# combine all stopwords (amp from '&' and innen from -innen)
stopwords_de_customized <- Reduce(union, list(stopwords_de, stopwords("de"), "amp", "innen"))
# convert special characters for stopwords (otherwise many stopwords are not detected!)
stopwords_de_customized <- stringi::stri_replace_all_fixed(
  stopwords_de_customized, 
  c("ä", "ö", "ü", "ß"), 
  c("ae", "oe", "ue", "ss"), 
  vectorize_all = FALSE
)

# build document-feature matrix
dfmatrix <- quanteda::dfm(
  corp_topic,
  remove = c(stopwords_de_customized,
             stopwords("en")),
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_url = TRUE,
  tolower = TRUE,
  verbose = FALSE
)

# check most frequent words
quanteda::topfeatures(dfmatrix, 20)

# manually remove specific tokens
dfmatrix_cleaned <- dfmatrix %>% 
  quanteda::dfm_remove(pattern = "#", valuetype = "regex") %>%  # hashtags
  quanteda::dfm_remove(pattern = "@", valuetype = "regex")  %>%   # @username
  quanteda::dfm_remove(pattern = "(^[0-9]+[,.]?[0-9]+)\\w{1,3}$",  # 10er, 14.00uhr etc.
                       valuetype = "regex") %>%
  quanteda::dfm_remove(pattern = "^[^a-zA-Z0-9]*$",  # non-alphanumerical 
                       valuetype = "regex") %>%  
  quanteda::dfm_remove(pattern = "^.*(aaa|aeae|fff|hhh|uuu|www).*$",  # aaaawww etc.
                       valuetype = "regex") %>%
  quanteda::dfm_remove(pattern = "^(polit|bundesregier|bundestag|deutsch|land|jaehrig|http)", # specific words
                       valuetype = "regex")

# perform word stemming and remove all words that appear rarely as well as words with <4 characters
dfmatrix_cleaned <- dfmatrix_cleaned %>% 
  quanteda::dfm_wordstem(language = "german") %>% 
  quanteda::dfm_remove(min_nchar = 4) %>%
  quanteda::dfm_trim(min_termfreq = 5, min_docfreq = 3)

# check most frequent words again
quanteda::topfeatures(dfmatrix_cleaned, 20)

# convert to stm object (this reduces memory use when fitting stm; see ?stm)
data_preprocessed <- quanteda::convert(dfmatrix_cleaned, to = "stm")

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
  as.numeric(stringi::stri_detect_fixed(data_preprocessed$meta$Ausschusspositionen, s))
  }
)
colnames(ausschuesse_dummy) <- paste0("Ausschuss_", 1:25)

# add columns to the data frame (and delete inital Ausschuss variable)
data_preprocessed$meta <- data_preprocessed$meta %>% 
  cbind(ausschuesse_dummy) %>%
  dplyr::select(-"Ausschusspositionen")

# ----------------------------------------------------------------------------------------------

# save
outpath <- stringi::stri_replace_all_fixed(filepath, "prep", "preprocessed")
saveRDS(data_preprocessed, outpath)

