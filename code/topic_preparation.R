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
# save
saveRDS(topic, "./data/topic.rds")

# ----------------------------------------------------------------------------------------------
# ----------------------------------- Aggregate Twitter data ----------------------------------- 
# ----------------------------------------------------------------------------------------------

# load data
topic <- readRDS("./data/topic.rds")

# plot histogram of tweets per year
hist(topic %>% mutate(Jahr = lubridate::year(Datum)) %>% pull(Jahr))

# drop all tweets prior to 24.09.2017 (date of the most recent Bundestagswahl)
topic <- topic %>% 
  filter(Datum >= '2017-09-24')

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

# aggregate data on a per-week per-user basis
topic_user_weekly <-  topic %>% 
  mutate(Jahr = lubridate::year(Datum), Woche = lubridate::isoweek(Datum)) %>%
  group_by(Name, Jahr, Woche) %>% 
  mutate(Tweets_Dokument = paste(Tweets, collapse = ' ')) %>%
  summarize(
    Twitter_Username = max(Twitter_Username), 
    Tweets_Dokument = max(Tweets_Dokument),
    Anzahl_Follower = max(Anzahl_Follower)
  ) %>%
  ungroup() %>%
  arrange(Name, Jahr, Woche)

hist(topic_user_weekly %>% pull(Jahr))
topic_user_weekly %>% filter(Twitter_Username == unique(topic_user_weekly$Twitter_Username)[10])

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
all_data_user <- topic_user %>% 
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
all_data_user <- all_data_user %>% 
  select(-drop_vars)

# drop rows where Partei==fraktionslos (only one row deleted)
all_data_user <- all_data_user %>% 
  filter(Partei != "fraktionslos")

# create variable "Wahlergebnis": voting share of party from a parlamentarian in his/her district
all_data_user$Wahlergebnis <- purrr::map2_dbl(1:nrow(all_data_user), paste(all_data_user$Partei, "Anteil"), 
                                         function(i,j) all_data_user[[i,j]])

# change colnames and store old names
colnames_table <- data.frame(oldnames = colnames(all_data_user), 
                             newnames = c(colnames(all_data_user)[1:11], paste0("v_", 1:54)))
colnames(all_data_user) <- colnames_table[["newnames"]]
write.csv(colnames_table, file = "./data/topic_user_colnames.csv")

# save
saveRDS(all_data_user, "./data/topic_user.rds")