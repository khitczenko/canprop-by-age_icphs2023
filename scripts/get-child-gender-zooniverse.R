# get sex and age by child

library(dplyr)

# Read in annotations / private_metadata (raw files from babblecorpus)
df <- read.csv("../data/chunks_maj_judgments_all_sources.csv")
# Some rows have a diagnosis of NA -- fill in the right diagnosis (these are always tsimane)
df <- subset(df, Diagnosis != 'AngelmanSyndrome') # that leaves us with 63 low-risk kids 

## Make data frame that has ChildID + gender for each language
# deal with french
french <- subset(df, language == 'french')
phonses_children <- read.csv('../data/phonSES/metadata/children.csv')
phonses_recordings <- read.csv('../data/phonSES/metadata/recordings.csv')
phonses <- merge(phonses_recordings, phonses_children, by = "child_id")
phonses$recording_filename <- gsub('lena/', '', gsub('.wav', '', phonses$recording_filename))
# note that date_iso is WRONG in phonses gin - UPDATE (TODO)
phonses_ids <- distinct(data.frame(ChildID = french$ChildID, ITS = french$ITS))
phonses <- merge(phonses_ids, phonses, by.x="ITS", by.y="recording_filename")
phonses <- data.frame(ChildID = phonses$ChildID, Sex = phonses$child_sex)
phonses$Corpus <- 'phonses'

# deal with english
english <- read.csv('../data/demo-data.tsv', sep = '\t')
english <- subset(english, Diagnosis == 'Low-RiskControl')
english$Diagnosis <- NULL
english_age <- data.frame(ChildID = english$ChildID, Age = english$Age)
english$Age <- NULL
english$Corpus <- 'purdue'

# deal with yeli
yeli <- subset(df, language == 'yeli')
yeli_children <- read.csv('../data/png2019/metadata/children.csv')
yeli_recordings <- read.csv('../data/png2019/metadata/recordings.csv')
png2019 <- merge(yeli_recordings, yeli_children, by = "child_id")
png2019$recording_filename <- gsub('.wav', '', png2019$recording_filename)
png2019_ids <- distinct(data.frame(ChildID = yeli$ChildID, ITS = yeli$ITS))
png2019 <- merge(png2019_ids, png2019, by.x="ITS", by.y="recording_filename")
png2019 <- data.frame(ChildID = png2019$ChildID, Sex = toupper(png2019$child_sex))
png2019$Corpus <- 'png2019'


# deal with tsimane
tsimane <- read.csv('../data/bsl_metadata_tsi_full.csv', sep=';')
tsimane_sex <- data.frame(ChildID = tsimane$child_ID, Sex = tsimane$sex)
tsimane_age <- data.frame(ChildID = tsimane$child_ID, Age = tsimane$age)
tsimane_c2017_sex <- read.csv('../data/tsimane2017/metadata/children.csv')
tsimane_c2017_sex$child_id <- paste0('2017_', tsimane_c2017_sex$child_id)
c2017_ids <- unique(subset(df, language == 'tsimane')$ChildID)
tsimane_c2017_sex <- subset(tsimane_c2017_sex, child_id %in% c2017_ids)
tsimane_sex <- rbind(tsimane_sex, data.frame(ChildID = tsimane_c2017_sex$child_id, Sex = tsimane_c2017_sex$child_sex))
tsimane_sex$Corpus <- 'tsimane'

sex_by_childid <- rbind(phonses, english, png2019, tsimane_sex)

write.csv(sex_by_childid, '../data/child-gender-zooniverse.csv', row.names = FALSE, quote = FALSE)
