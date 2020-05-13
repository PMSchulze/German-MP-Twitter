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
