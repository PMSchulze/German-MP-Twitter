# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "quanteda","stm", "stringi", "tidyverse", "tm"
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

# ----------------------------------------------------------------------------------------------
# ----------------------------------- Aggregate Twitter data ----------------------------------- 
# ----------------------------------------------------------------------------------------------

# load data
tweepy_df <- read_delim('./data/tweepy_df.csv', delim = ',')
# inspect parsing problems
problems(tweepy_df)
# remove rows where problems occur, drop retweets and users where download failed,
# keep only relevant columns and rename columns (capitalized + in german language)
topic <- tweepy_df %>% 
  filter(available == TRUE, is_retweet == 0) %>% 
  select(c('name','username', 'created_at', 'full_text','followers_count')) %>% 
  rename(
    Name = name, 
    Twitter_Username = username,
    Datum = created_at,
    Tweets = full_text, 
    Anzahl_Follower = followers_count
  ) %>%
  mutate(Datum = lubridate::date(Datum))
# save topic_df
saveRDS(topic, "./data/topic.rds")

# aggregate data on a per-user basis
topic_user <- topic %>% 
  group_by(Name) %>% 
  mutate(Tweets_Dokument = paste(Tweets, collapse = ' ')) %>%
  summarize(
    Twitter_Username = max(Twitter_Username), 
    Tweets_Dokument = max(Tweets_Dokument),
    Anzahl_Follower = max(Anzahl_Follower)
  )
# save
# saveRDS(topic_user, "./data/topic_user.rds")

# aggregate data on a per-week per-user basis
# topic_user_weekly <- topic %>% 
#   group_by(Name) %>% 
#   mutate(Tweets_Dokument = paste(Tweets, collapse = ' ')) %>%
#   summarize(
#     Twitter_Username = max(Twitter_Username), 
#     Tweets_Dokument = max(Tweets_Dokument),
#     Anzahl_Follower = max(Anzahl_Follower)
#   )
# save
# saveRDS(topic_user_weekly, "./data/topic_user_weekly.rds")

# ----------------------------------------------------------------------------------------------
# ---------------------- Load and merge abg_df and se_df to Twitter data -----------------------
# ----------------------------------------------------------------------------------------------

# load data
topic_user <- readRDS("./data/topic_user.rds")
abg_df <- read_delim("./data/abg_df.csv", delim = ",") %>%
  rename(Twitter_Username = Twitter, Wahlkreis_Nr = `Wahlkreis-Nr.`)
se_df <- read_delim("./data/se_df.csv", delim = ",") %>% 
  rename(Wahlkreis_Nr = `Wahlkreis-Nr.`, "AfD Anteil" = "AFD Anteil") %>% 
  select(-Bundesland)

# merge data
all_data <- topic_user %>% 
  inner_join(abg_df) %>% 
  inner_join(se_df, by = "Wahlkreis_Nr")

# drop variables that are completely expressed by other variables (e.g. Bevölkerung Deutsche 
# expressed by Bevölkerung mit Migratioshintergrund), and variables that are not easily 
# exploitable (e.g. Biografie) or uninformative (e.g. Fußnoten)
drop_vars <- c(
  "Ausschuesse",
  "Biografie",
  "Land",
  "Wahlkreis-Name",
  "Bevölkerung am 31.12.2015 - Deutsche (in 1000)", 
  "Zensus 2011, Bevölkerung nach Migrationshintergrund am 09.05.2011 - ohne Migrationshintergrund (%)",
  "Fußnoten", 
  "Bundesland-Nr.", 
  "CDU", 
  "SPD", 
  "Die Linke", 
  "Bündnis 90/Die Grünen",
  "CSU", 
  "FDP", 
  "AFD", 
  "CDU/CSU"
)
all_data <- all_data %>% 
  select(-drop_vars)

# drop rows where Partei==fraktionslos (only one row deleted)
all_data <- all_data %>% 
  filter(Partei != "fraktionslos")

# create variable "Wahlergebnis": voting share of party from a parlamentarian in his/her district
all_data$Wahlergebnis <- purrr::map2_dbl(1:nrow(all_data), paste(all_data$Partei, "Anteil"), 
                                         function(i,j) all_data[[i,j]])

# change colnames and store old names
colnames_table <- data.frame(oldnames = colnames(all_data), 
                             newnames = c(colnames(all_data)[1:11], paste0("v_", 1:54)))
colnames(all_data) <- colnames_table[["newnames"]]
write.csv(colnames_table, file = "./data/colnames_table.csv")

# ----------------------------------------------------------------------------------------------
# ------------------ Preprocessing of documents with the quanteda package ----------------------
# ----------------------------------------------------------------------------------------------

# build corpus, which by default organizes documents into types, tokens and sentences
corp_all_data <- quanteda::corpus(x = all_data, text_field = "Tweets_Dokument", 
                                  docid_field = "Name")
# convert some special german characters and remove # infront of hashtags
corp_text_cleaned <- stringi::stri_replace_all_fixed(
  texts(corp_all_data), 
  c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß", "#","-"), 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss", "",""), 
  vectorize_all = FALSE
)
texts(corp_all_data) <- corp_text_cleaned

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
  corp_all_data,
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

# manually remove specific tokens and all tokens with <4 characters
dfmatrix_cleaned <- dfmatrix %>% 
  quanteda::dfm_remove(pattern = "@", min_nchar = 4, valuetype = "regex")  %>%   # @username
  quanteda::dfm_remove(pattern = "(^[0-9]+[,.]?[0-9]+)\\w{1,3}$",  # 10er, 14.00uhr etc.
                       valuetype = "regex") %>%
  quanteda::dfm_remove(pattern = "^[^a-zA-Z0-9]*$",  # non-alphanumerical 
                       valuetype = "regex") %>%  
  quanteda::dfm_remove(pattern = "^.*(aaa|aeae|fff|hhh|uuu|www).*$",  # aaaawww etc.
                       valuetype = "regex") %>%
  quanteda::dfm_remove(pattern = "^(polit|bundesregier|bundestag|deutschland|jaehrig|http)", # specific words
                       valuetype = "regex")

# perform word stemming and remove all words that appear rarely
dfmatrix_cleaned <- dfmatrix_cleaned %>% 
  quanteda::dfm_wordstem(language = "german") %>% 
  quanteda::dfm_trim(min_termfreq = 5, min_docfreq = 3)

# check most frequent words again
quanteda::topfeatures(dfmatrix_cleaned, 20)

# convert to stm object (this reduces memory use when fitting stm; see ?stm)
topic_user_preprocessed <- quanteda::convert(dfmatrix_cleaned, to = "stm")

saveRDS(topic_user_preprocessed, "./data/topic_user_preprocessed.rds")

# ----------------------------------------------------------------------------------------------
# ---------------------- STM -----------------------
# ----------------------------------------------------------------------------------------------

# load data
topic_user_preprocessed <- readRDS("./data/topic_user_preprocessed.rds")
colnames_table <- read.csv(file = "./data/colnames_table.csv")

# extract documents, vocabulary and metadata
docs <- topic_user_preprocessed$documents
vocab <-  topic_user_preprocessed$vocab
meta <- topic_user_preprocessed$meta

# set CDU/CSU as reference category for variable "Partei"
meta$Partei <- meta$Partei %>%
  as.factor() %>%
  relevel(ref = 3)

# prevalence model
mod_prev <- stm::stm(
                  documents = docs,
                  vocab = vocab,
                  data = meta,
                  K = 7,
                  prevalence =~ Partei + Bundesland + s(Geburtsjahr) + v_4 + v_22 + v_42,
                  max.em.its = 50,
                  init.type = "Spectral")

### top words per topic
labelTopics(mod_prev)

### summary visualization
plot(mod_prev, type = "summary", xlim = c(0, 0.3))

### relationship topic/vocabulary
plot(mod_prev, type = "perspectives", topics = c(3, 6))

### estimating metadata/topic relationship
meta$Bundesland <- as.factor(meta$Bundesland)
prep <- stm::estimateEffect(1:6 ~ Partei + Bundesland + s(Geburtsjahr) + v_4 + v_22 + v_42,
                      mod_prev,
                      metadata = meta,
                      uncertainty = "Global")
summary(prep, topics = 1)

### metadata/topic relationship visualization
plot(prep, "Partei", method = "difference", topics = c(1,2,3,4,5,6), cov.value1 = "CDU/CSU",
     cov.value2 = "SPD", model = mod_prev, xlab = "Partei")

plot(prep, "Bundesland", method = "pointestimate", topics = 6,
     model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "Bundesland")

# monthseq <- seq(from = as.Date("2008-01-01"),
#                 to = as.Date("2008-12-01"), by = "month")
# monthnames <- months(monthseq)
# axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
#      labels = monthnames)

plot(prep, "v_42", method = "continuous", topics = 5,
    model = mod_prev, printlegend = FALSE, xaxt = "n", xlab = "BIP pro Kopf")
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
