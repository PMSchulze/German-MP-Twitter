# ----------------------------------------------------------------------------------------------
# ---------------------------------------- Preparation -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Install and load required packages
os <- Sys.info()[["sysname"]] # Get operating system information
itype <- ifelse(os == "Linux", "source", "binary") # Set corresponding installation type
packages_required <- c(
  "tidyverse"
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
# setwd('C:\\Users\\Simon\\Desktop\\Twitter')
# setwd('C:\\Users\\Simon\\OneDrive\\Uni\\LMU\\SS 2020\\Statistisches Consulting\\Bundestag-MP-Analyse')
setwd('/Users/patrickschulze/Desktop/Consulting/Bundestag-MP-Analyse')

# ----------------------------------------------------------------------------------------------
# ----------------------------------- Prepare Twitter data ------------------------------------- 
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

# plot histogram of tweets per year
hist(topic %>% mutate(Jahr = lubridate::year(Datum)) %>% pull(Jahr))

# drop all tweets prior to 24.09.2017 (date of the most recent Bundestagswahl)
topic <- topic %>% 
  filter(Datum >= '2017-09-24')

# save
saveRDS(topic, "./data/topic.rds")

# ----------------------------------------------------------------------------------------------
# ----------------------------------- Aggregate Twitter data ----------------------------------- 
# ----------------------------------------------------------------------------------------------

# load data
topic <- readRDS("./data/topic.rds")

# aggregate data on a per-user basis
topic_user <- topic %>% 
  group_by(Name) %>% 
  mutate(Tweets_Dokument = paste(Tweets, collapse = ' ')) %>%
  summarize(
    Twitter_Username = max(Twitter_Username), 
    Tweets_Dokument = max(Tweets_Dokument),
    Anzahl_Follower = max(Anzahl_Follower)
  ) %>%
  ungroup()

# aggregate data on a per-month per-user basis
topic_user_monthly <-  topic %>% 
  mutate(Jahr = lubridate::year(Datum), Monat = lubridate::month(Datum)) %>%
  group_by(Name, Jahr, Monat) %>% 
  mutate(Tweets_Dokument = paste(Tweets, collapse = ' ')) %>%
  summarize(
    Twitter_Username = max(Twitter_Username), 
    Tweets_Dokument = max(Tweets_Dokument),
    Anzahl_Follower = max(Anzahl_Follower)
  ) %>%
  ungroup() %>%
  arrange(Name, Jahr, Monat)

# ----------------------------------------------------------------------------------------------
# ---------------------- Load abg_df and se_df and merge with topic_user -----------------------
# ----------------------------------------------------------------------------------------------

# load data
abg_df <- read_delim("./data/abg_df.csv", delim = ",") %>%
  rename(Twitter_Username = Twitter, Wahlkreis_Nr = `Wahlkreis-Nr.`)
se_df <- read_delim("./data/se_df.csv", delim = ",") %>% 
  rename(Wahlkreis_Nr = `Wahlkreis-Nr.`, "AfD Anteil" = "AFD Anteil") %>% 
  select(-Bundesland)

# merge data
alldata_user <- topic_user %>% 
  inner_join(abg_df) %>% 
  inner_join(se_df, by = "Wahlkreis_Nr")

alldata_user_monthly <- topic_user_monthly %>% 
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
alldata_user <- alldata_user %>% 
  select(-drop_vars)
alldata_user_monthly <- alldata_user_monthly %>% 
  select(-drop_vars)

# drop rows where Partei==fraktionslos (only one row deleted)
alldata_user <- alldata_user %>% 
  filter(Partei != "fraktionslos")
alldata_user_monthly <- alldata_user_monthly %>% 
  filter(Partei != "fraktionslos")

# create variable "Wahlergebnis": voting share of party from a parlamentarian in his/her district
alldata_user$Wahlergebnis <- purrr::map2_dbl(
  1:nrow(alldata_user), paste(alldata_user$Partei, "Anteil"), 
  function(i,j) alldata_user[[i,j]]
)
alldata_user_monthly$Wahlergebnis <- purrr::map2_dbl(
  1:nrow(alldata_user_monthly), paste(alldata_user_monthly$Partei, "Anteil"), 
  function(i,j) alldata_user_monthly[[i,j]]
)

# change colnames and store old names
colnames_user <- data.frame(oldnames = colnames(alldata_user), 
                             newnames = c(colnames(alldata_user)[1:11], paste0("Struktur_", 1:54)))
colnames_user_monthly <- data.frame(oldnames = colnames(alldata_user_monthly), 
                                    newnames = c(colnames(alldata_user_monthly)[1:13], paste0("Struktur_", 1:54)))
colnames(alldata_user) <- colnames_user[["newnames"]]
colnames(alldata_user_monthly) <- colnames_user_monthly[["newnames"]]
write.csv(colnames_user, file = "./data/topic_colnames.csv")
write.csv(colnames_user_monthly, file = "./data/topic_monthly_colnames.csv")

# extract party specific dataset
alldata_cdu_user <- alldata_user %>% 
  filter(Partei == "CDU/CSU")
alldata_cdu_user_monthly <- alldata_user_monthly %>% 
  filter(Partei == "CDU/CSU")

# save
saveRDS(alldata_user, "./data/prep.rds")
saveRDS(alldata_cdu_user, "./data/prep_cdu.rds")
saveRDS(alldata_user_monthly, "./data/prep_monthly.rds")
saveRDS(alldata_cdu_user_monthly, "./data/prep_cdu_monthly.rds")


# reload and split
set.seed(123)
alldata_cdu_user <- readRDS("./data/prep_cdu.rds")
p <- 0.5
idx_train <- sample(1:nrow(alldata_cdu_user), p*nrow(alldata_cdu_user))
alldata_cdu_user_train <- alldata_cdu_user[idx_train,]
alldata_cdu_user_test <- alldata_cdu_user[-idx_train,]
saveRDS(alldata_cdu_user_train, "./data/prep_cdu_train.rds")
saveRDS(alldata_cdu_user_test, "./data/prep_cdu_test.rds")

set.seed(123)
alldata_user <- readRDS("./data/prep.rds")
p <- 0.5
idx_train <- sample(1:nrow(alldata_user), p*nrow(alldata_user))
alldata_user_train <- alldata_user[idx_train,]
alldata_user_test <- alldata_user[-idx_train,]
saveRDS(alldata_user_train, "./data/prep_train.rds")
saveRDS(alldata_user_test, "./data/prep_test.rds")


set.seed(123)
alldata_cdu_user_monthly <- readRDS("./data/prep_cdu_monthly.rds")
p <- 0.5
idx_train <- sample(1:nrow(alldata_cdu_user_monthly), p*nrow(alldata_cdu_user_monthly))
alldata_cdu_user_monthly_train <- alldata_cdu_user_monthly[idx_train,]
alldata_cdu_user_monthly_test <- alldata_cdu_user_monthly[-idx_train,]
saveRDS(alldata_cdu_user_monthly_train, "./data/prep_cdu_monthly_train.rds")
saveRDS(alldata_cdu_user_monthly_test, "./data/prep_cdu_monthly_test.rds")

set.seed(123)
alldata_user_monthly <- readRDS("./data/prep_monthly.rds")
p <- 0.5
idx_train <- sample(1:nrow(alldata_user_monthly), p*nrow(alldata_user_monthly))
alldata_user_monthly_train <- alldata_user_monthly[idx_train,]
alldata_user_monthly_test <- alldata_user_monthly[-idx_train,]
saveRDS(alldata_user_monthly_train, "./data/prep_monthly_train.rds")
saveRDS(alldata_user_monthly_test, "./data/prep_monthly_test.rds")
