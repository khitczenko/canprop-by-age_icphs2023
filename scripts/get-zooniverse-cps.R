# Process babblecorpus data
# setwd('/Users/kasia/Documents/Research/canonical-proportions-age-bucld/scripts')

library(dplyr) 

# Read in annotations / private_metadata (raw files from babblecorpus)
df <- read.csv("../data/chunks_maj_judgments_all_sources.csv", stringsAsFactors=FALSE)
# Some rows have a diagnosis of NA -- fill in the right diagnosis (these are always tsimane)
df <- subset(df, Diagnosis != 'AngelmanSyndrome') # that leaves us with 63 low-risk kids
df$ITS[df$language == 'english'] <- df$ChildID[df$language == 'english'] # this is so that we can loop over ITS further down (english doesnt have ITS as this point)

# Just check what we're working with data-wise 
# english <- subset(df, language == 'english') # length(unique(english$ChildID)) = 10
# yeli <- subset(df, language == 'yeli') # length(unique(yeli$ChildID)) = 5
# tsimane <- subset(df, language == 'tsimane') # length(unique(tsimane$ChildID)) = 35
# french <- subset(df, language == 'french') # length(unique(french$ChildID)) = 13

# Make output data frame -- NOTE: we are making one row per recording (and will exclude later)
children <- unique(df$ITS)
out <- data.frame(child_ID=rep(NA, length(children)), 
                  ITS=rep(NA,length(children)),
                  age_mo_round=rep(NA, length(children)),
                  child_gender=rep(NA, length(children)),
                  corpus=rep(NA, length(children)),
                  cp=rep(NA, length(children)),
                  n_can=rep(NA, length(children)),
                  n_speechlike=rep(NA, length(children)),
                  n_total=rep(NA, length(children)),
                  n_total_inc_no_majority=rep(NA, length(children)),
                 stringsAsFactors=FALSE)          

sex <- read.csv('../data/child-gender-zooniverse.csv')

# Calculate canonical proportion per child
for (i in 1:length(children)) {
  tmp <- subset(df, ITS == children[i])
  can <- nrow(subset(tmp, Answer == 'Canonical'))
  ncan <- nrow(subset(tmp, Answer == 'Non-Canonical'))
  cp <- can / (can + ncan)
  
  out[i,] <- list(tmp$ChildID[1], 
                  tmp$ITS[1], 
                  round(tmp$Age[1]), 
                  subset(sex, ChildID == tmp$ChildID[1])$Sex[1], 
                  tmp$language[1], 
                  cp, 
                  can, 
                  ncan+can, 
                  nrow(subset(tmp, Answer != "")), 
                  nrow(tmp))
}

# Exclude kids that have NA for age -> N = 5 kids, but 6 lines (b/c one has two)
# So now we have 58 kids
out <- subset(out, !is.na(age_mo_round))

# Make sure one age per child
children <- unique(out$child_ID)
out$include <- 1
for (i in 1:length(children)) {
  tmp <- subset(out, child_ID == children[i])
  if (nrow(tmp) > 1) {
    max_clips <- max(tmp$n_total)
    out$include[out$child_ID == children[i] & out$n_total < max_clips] <- 0
  }
}

# Get rid of extra recordings - still have 58 kids and also 58 lines
out <- subset(out, include == 1)
out$include <- NULL

# Change 'english' to 'purdue'
out$corpus[out$corpus == 'english'] <- 'purdue'

# Output table with per-child information
write.csv(out, '../data/zooniverse_cp.csv', row.names = FALSE, quote = FALSE)
