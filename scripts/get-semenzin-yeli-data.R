# Process LangComplexity file to get yeli cp

yeli <- read.csv('../data/CR_by_child-updated_21_01.csv', sep = ';')
colnames(yeli)[1] <- 'ChildID'
yeli <- subset(yeli, corpus == 'Yélî')
yeli$age_mo_round = round(as.numeric(gsub(',', '.', yeli$Age.in.months)))
yeli$cp = gsub(',', '.', yeli$CR)
out <- data.frame(child_ID = yeli$ChildID,
                  age_mo_round = yeli$age_mo_round,
                  child_gender = yeli$Gender,
                  corpus = rep('yeli-semenzin', nrow(yeli)),
                  cp = yeli$cp)

write.csv(out, '../data/yeli-semenzin_cp.csv', row.names = FALSE, quote = FALSE)