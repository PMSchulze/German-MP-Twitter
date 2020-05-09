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
tweepy_df <- tweepy_df[-problems(tweepy_df)$row,] %>% 
  filter(available == TRUE, is_retweet == 0) %>% 
  select(c('name','username','full_text','followers_count')) %>% 
  rename(
    Name = name, 
    Twitter_Username = username, 
    Tweets = full_text, 
    Anzahl_Follower = followers_count
  )
# aggregate data, i.e. concatenate all tweets of each person
tweepy_docs_df <- tweepy_df %>% 
  group_by(Name) %>% 
  mutate(Tweets_Dokument = paste(Tweets, collapse = ' ')) %>%
  summarize(
    Twitter_Username = max(Twitter_Username), 
    Tweets_Dokument = max(Tweets_Dokument),
    Anzahl_Follower = max(Anzahl_Follower)
  )
# # save aggregated data
# saveRDS(tweepy_docs_df, "./data/tweepy_docs_df.rds")

# ----------------------------------------------------------------------------------------------
# ---------------------- Load and merge abg_df and se_df to Twitter data -----------------------
# ----------------------------------------------------------------------------------------------

# load data
tweepy_docs_df <- readRDS("./data/tweepy_docs_df.rds")
abg_df <- read_delim('./data/abg_df.csv', delim =',') %>%
  rename(Twitter_Username = Twitter, Wahlkreis_Nr = `Wahlkreis-Nr.`)
se_df <- read_delim('./data/se_df.csv', delim =',') %>% 
  rename(Wahlkreis_Nr = `Wahlkreis-Nr.`, "AfD Anteil" = "AFD Anteil") %>% 
  select(-Bundesland)
# merge data
all_data <- tweepy_docs_df %>% 
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

# drop rows where Partei==fraktionslos (1 row in total)
all_data <- all_data %>% 
  filter(Partei!="fraktionslos")

# create new variable: voting shares of party of respective parlamentarian in his/her district
select_party <- function(s){
  coln <- paste(s, "Anteil", sep = " ")
  return(coln)
}
for (i in 1:nrow(all_data)){
  all_data[i, "Wahlergebnis"] <- all_data[i, select_party(all_data[i, "Partei"])]
}

# change colnames and store old names
colnames_table <- data.frame(oldnames = colnames(all_data), 
                             newnames = c(colnames(all_data)[1:11], paste0("v_",1:54)))
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
length(stopwords('de')) # 231 stopwords
# combine all stopwords (amp from '&' and innen from -innen)
stopwords_de_customized <- Reduce(union, list(stopwords_de, stopwords("de"), "amp", "innen"))
# convert special characters for stopwords (otherwise many stopwords are not detected!)
stopwords_de_customized <- stringi::stri_replace_all_fixed(
  stopwords_de_customized, 
  c("ä", "ö", "ü", "Ä", "Ö", "Ü", "ß"), 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue", "ss"), 
  vectorize_all = FALSE
)

# build document-feature matrix
dfmatrix <- quanteda::dfm(
  corp_all_data,
  remove = c(stopwords_de_customized,
             stopwords('en')),
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
twitter_preprocessed <- quanteda::convert(dfmatrix_cleaned, to = "stm")

# save stm object as RDS file
saveRDS(twitter_preprocessed, "./data/twitter_preprocessed.rds")

# ----------------------------------------------------------------------------------------------
# ---------------------- STM -----------------------
# ----------------------------------------------------------------------------------------------

# load data
twitter_preprocessed <- readRDS("./data/twitter_preprocessed.rds")
colnames_table <- read.csv(file = "./data/colnames_table.csv")

# extract documents, vocabulary and metadata
docs <- twitter_preprocessed$documents
vocab <-  twitter_preprocessed$vocab
meta <- twitter_preprocessed$meta

# prevalence model
mod_prevalence <- stm::stm(
                  documents = docs,
                  vocab = vocab,
                  data = meta,
                  K = 10,
                  prevalence =~ Partei + Bundesland + Geburtsjahr,
                  max.em.its = 75,
                  init.type = "Spectral")

# topical content
mod_topic <- stm::stm(
                  documents = docs,
                  vocab = vocab, 
                  data = meta,
                  K = 10,
                  prevalence =~ Partei + Bundesland + Geburtsjahr,
                  content =~ Partei,
                  max.em.its = 75,
                  init.type = "Spectral")

# top words per topic
labelTopics(mod_prevalence, c(1, 2, 3))


thoughts3 <- stm::findThoughts(mod_prevalence, texts = texts(corp_all_data), n = 2,
                          topics = 3)$docs[[1]]
thoughts20 <- stm::findThoughts(mod_prevalence, texts = texts(corp_all_data), n = 2,
                           topics = 2)$docs[[1]]

par(mfrow = c(1, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts3, width = 30, main = "Topic 3")
plotQuote(thoughts20, width = 30, main = "Topic 2")

# estimating metadata/topic relationship
meta$Partei <- as.factor(meta$Partei)
meta$Bundesland <- as.factor(meta$Bundesland)
prep <- stm::estimateEffect(1:10 ~ Partei + Bundesland + Geburtsjahr,
                      mod_prevalence,
                      metadata = meta,
                      uncertainty = "Global")
summary(prep, topics = 3)

# summary visualization
plot(mod, type = "summary", xlim = c(0, 0.3))

# metadata/topic relationship visualization
plot(prep, "Partei", method = "pointestimate", topics = 6,
     model = mod_prevalence, printlegend = FALSE, xaxt = "n", xlab = "Partei")
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
     labels = monthnames)

# topical content
mod_topic <- stm::stm(documents = docs, vocab = vocab, K = 10,
                       prevalence =~ Partei, content =~ Partei,
                       max.em.its = 75, data = meta, init.type = "Spectral")

plot(mod_prevalence, type = "perspectives", topics = 8)
plot(mod, type = "perspectives", topics = c(8, 10))

# word cloud
cloud(mod_topic, topic = 3, scale = c(2, 0.25))


# # only for a binary covariate
# plot(prep, covariate = "rating", topics = c(6, 13, 18),
#      model = poliblogPrevFit, method = "difference", cov.value1 = "Liberal",
#      cov.value2 = "Conservative",
#      xlab = "More Conservative ... More Liberal",
#      main = "Effect of Liberal vs. Conservative", xlim = c(-0.1, 0.1),
#      labeltype = "custom", custom.labels = c("Obama/McCain", "Sarah Palin",
#                                              "Bush Presidency"))