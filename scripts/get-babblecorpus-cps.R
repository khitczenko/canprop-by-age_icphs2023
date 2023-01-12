# Process babblecorpus data
# setwd('/Users/kasia/Documents/Research/canonical-proportions-age-bucld/scripts')

# Read in annotations / private_metadata (raw files from babblecorpus)
df <- read.csv("../data/uniqchild.csv")

# Subset to only those datasets we will be working with
df <- subset(df, corpus %in% c('Quechua', 'English-Bergelson', 'Tseltal', 'Yélî'))

# Make output data frame
children <- unique(df$unq_childID)
out <- data.frame(child_ID=rep(NA, length(children)), 
                  age_mo_round=rep(NA, length(children)),
                  child_gender=rep(NA, length(children)),
                  corpus=rep(NA, length(children)),
                  cp=rep(NA, length(children)),
                  n_can=rep(NA, length(children)),
                  n_speechlike=rep(NA, length(children)),
                  n_total=rep(NA, length(children)),
                 stringsAsFactors=FALSE)          

# Calculate canonical proportion per child
for (i in 1:length(children)) {
  tmp <- subset(df, unq_childID == children[i])
  out[i,] <- list(tmp$unq_childID[1], 
                  tmp$age_mo_round[1], 
                  substr(tmp$child_gender[1], 1, 1), 
                  tmp$corpus[1], 
                  tmp$Ratio[1], 
                  tmp$Canonical[1],
                  tmp$Total[1],
                  NA)
  
}

# Output table with per-child information
write.csv(out, '../data/babblecorpus_cp.csv', row.names = FALSE, quote = FALSE)