# setwd('/Users/kasia/Documents/Research/canonical-proportions-age-bucld/scripts')
library(dplyr)

annotator = file = n_canonical = n_noncanonical = n_keychild <- c()

dirs <- list.files(path = "../data/solomon/annotations/solis/")
for (i in 1:length(dirs)) {
	print(dirs[i])
	p <- paste0("../data/solomon/annotations/solis/", dirs[i], "/converted/")
	files <- list.files(path = p)
	for (j in 1:length(files)) {
		# Open the file
		current <- read.csv(paste0(p,files[j]),stringsAsFactors=FALSE)
		# Subset the file to just look at key child (CHI)
		keychild <- subset(current, speaker_type == 'CHI')
		# Get total number of rows
		n_rows <- nrow(keychild)
		# Get total number of C
		n_can <- nrow(subset(keychild, vcm_type == 'C'))
		# Get total number of N
		n_noncan <- nrow(subset(keychild, vcm_type == 'N'))
		annotator = c(annotator, dirs[i])
		file = c(as.character(file), files[j])
		n_canonical = c(n_canonical, n_can)
		n_noncanonical = c(n_noncanonical, n_noncan)
		n_keychild = c(n_keychild, n_rows)
	}
}

out <- data.frame(annotator, file, n_canonical, n_noncanonical, n_keychild, stringsAsFactors=FALSE)

# Make sure that all files have the same structure
for (i in 1:nrow(out)) {  
  out[i,'filestructure'] <- length(strsplit(out$file[i], '_')[[1]])
}
exceptions <- nrow(subset(out, filestructure != 8)) # Make sure exceptions = 0 (it does!)
out$filestructure <- NULL

# Get file name that each row comes from
for (i in 1:nrow(out)) {
  out[i, 'recording_filename'] <- paste(strsplit(out$file[i], '_')[[1]][1:6], collapse='_')
}


# Get rid of files where there's no key child -- this brings total files down to 142
out <- subset(out, n_keychild > 0)
# length(unique(out$file)) -- 109 unique files, meaning some are repeated
# To deal with these, we will randomly sample one of the annotations to use
allfiles <- unique(out$file)
shuffled_out = out[sample(1:nrow(out)), ]
shuffled_out$include <- 1
for (i in 1:length(allfiles)) {
  nrepeats <- nrow(subset(shuffled_out, file == allfiles[i]))
  if (nrepeats > 1) {
    shuffled_out$include[shuffled_out$file == allfiles[i]][2:nrepeats] <- 0
  }
}
out <- subset(shuffled_out, include == 1) #nrow(out) = 109
out$include <- NULL

summary <- out %>% group_by(recording_filename) %>% summarize(n_keychild = sum(n_keychild), n_noncanonical = sum(n_noncanonical), n_canonical = sum(n_canonical))

# Combine summary information (about counts per file) with demographic information
children <- read.csv('../data/solomon/metadata/children.csv')
recordings <- read.csv('../data/solomon/metadata/recordings.csv')
recordings$recording_filename <- gsub('.WAV', '', recordings$recording_filename)
recordings_by_child <- data.frame(child_id = recordings$child_id, date_iso = recordings$date_iso, recording_filename = recordings$recording_filename)
summary <- merge(summary, recordings_by_child, by = 'recording_filename')
rel_child_data <- data.frame(child_id = children$child_id, child_dob = children$child_dob, child_sex = toupper(children$child_sex))
summary <- merge(summary, rel_child_data, by = 'child_id')
summary$age_mo_round <- round(as.numeric(difftime(summary$date_iso, summary$child_dob, units = 'days'))/30.4)
summary$cp <- summary$n_canonical / (summary$n_canonical + summary$n_noncanonical)

out <- data.frame(child_ID = summary$child_id, 
                  age_mo_round = summary$age_mo_round, 
                  child_gender = summary$child_sex, 
                  corpus = rep('solomon', nrow(summary)),
                  cp = summary$cp,
                  n_can = summary$n_canonical,
                  n_speechlike = summary$n_canonical + summary$n_noncanonical,
                  n_total = summary$n_keychild)

write.csv(out, '../data/solomon_cp.csv', row.names = FALSE, quote = FALSE)



