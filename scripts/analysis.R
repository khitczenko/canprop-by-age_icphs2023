# setwd('/Users/kasia/Documents/Research/canonical-proportions-age-bucld/scripts')

### PART 0: Set up
library(plyr)
library(dplyr)
library(lme4)
library(influence.ME)
library(car)
library(ggplot2)
library(ggpubr)
library("RColorBrewer")
library("plotrix")

## These commented-out lines merge data sources and create combined-data-for-analysis.R,
## which gets read in below.
# # read in data from 4 sources and merge
# bc <- read.csv('../data/babblecorpus_cp.csv')
# zo <- read.csv('../data/zooniverse_cp.csv')
# so <- read.csv('../data/solomon_cp.csv')
# ye <- read.csv('../data/yeli-semenzin_cp.csv')
# df <- rbind.fill(bc, zo, so, ye)
# nrow(df) # 137
# 
# # because of overlaps, we only want to use yeli children from yeli-semenzin 
# # & babblecorpus -> get rid of 5 kids from zooniverse_cp.
# df <- subset(df, corpus != 'yeli')
# # nrow(df) # 132
# 
# # exclude 3 french phonses kids that didn't give consent for scientific archiving
# # they are the only ones who don't have gender info
# df <- subset(df, !(is.na(child_gender)))
# # nrow(df) # 129 children in our sample
# 
# # standardize corpus names
# df$corpus[df$corpus == 'Yélî'] <- 'Yélî Dnye (N=41)'
# df$corpus[df$corpus == 'yeli-semenzin'] <- 'Yélî Dnye (N=41)'
# df$corpus[df$corpus == 'English-Bergelson'] <- 'English-Bergelson (N=10)'
# df$corpus[df$corpus == 'purdue'] <- 'English-Seidl (N=10)'
# df$corpus[df$corpus == 'french'] <- 'French (N=10)'
# df$corpus[df$corpus == 'solomon'] <- 'Solomon (N=15)'
# df$corpus[df$corpus == 'tsimane'] <- 'Tsimane\' (N=30)'
# df$corpus[df$corpus == 'Quechua'] <- 'Quechua (N=3)'
# df$corpus[df$corpus == 'Tseltal'] <- 'Tseltal (N=10)'

# write.csv(df,"../data/combined-data-for-analysis.csv", row.names = FALSE, quote = FALSE)

# Read in combined data (created by commented-out code above)
df <- read.csv("../data/combined-data-for-analysis.csv", stringsAsFactors = FALSE)



### PART 1: Plots for BUCLD abstract & talk

# 1. Canonical proportion by age, colored by corpus
p_corpus <- ggplot(df, aes(x = age_mo_round, y = cp, color = corpus)) + geom_point() + 
  xlab("Age (months)") + ylab("Canonical proportion") + 
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 4)) + 
  theme(legend.title=element_blank()) + 
  geom_point(size=3)

# 2. Separate out each corpus and plot linear vs. quadratic lines
p_sep_by_corpus <- ggplot(df, aes(x = age_mo_round, y = cp)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 0.5, se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ x, linewidth = 0.5, se = FALSE, linetype = 'dashed') +
  facet_wrap(~ corpus, ncol = 2) +
  xlab("Age (months)") + ylab("Canonical Proportion") + 
  scale_color_manual(name='Regression Model', breaks=c('Linear', 'Quadratic'), values=c('Quadratic'='blue', 'Linear'='purple'))

# 3. Canonical proportion by age, colored by child sex
p_sex <- ggplot(df, aes(x = age_mo_round, y = cp, color = child_gender)) + geom_point() + 
  xlab("Age (months)") + ylab("Canonical proportion") + 
  theme(legend.position="right") +
  guides(colour = guide_legend(nrow = 4)) + 
  theme(legend.title=element_blank()) + 
  geom_point(size=3)





### PART 2: Plots for ICPhS paper

# 1. Canonical proportion by age, colored by corpus (updated from BUCLD version for increased readability)
p_corpus <- ggplot(df, aes(x = age_mo_round, y = cp, color = corpus, shape = corpus)) + geom_point() + 
  xlab("Age (months)") + ylab("Canonical proportion") + 
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 3)) + 
  theme(legend.title=element_blank()) + 
  geom_point(size=3) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values=c(16,16,20,20,18,15,16,17))

# 2. Canonical proportion by age, colored by syllabic complexity of the language
# Get syllabic complexity
langinfo <- read.csv("../data/LAAC_Internship2020_Languages.xlsx - Usable as csv.csv")
df$syllcomplexity[df$corpus == 'Yélî Dnye (N=41)'] <- langinfo$Maddieson_sylcomp[langinfo$Language == 'Yélî']
df$syllcomplexity[df$corpus == 'Quechua (N=3)'] <- langinfo$Maddieson_sylcomp[langinfo$Language == 'Quechua']
df$syllcomplexity[df$corpus == 'English-Bergelson (N=10)' | df$corpus == 'English-Seidl (N=10)'] <- langinfo$Maddieson_sylcomp[langinfo$Language == 'English']
df$syllcomplexity[df$corpus == 'Tseltal (N=10)'] <- langinfo$Maddieson_sylcomp[langinfo$Language == 'Tseltal']
df$syllcomplexity[df$corpus == 'French (N=10)'] <- langinfo$Maddieson_sylcomp[langinfo$Language == 'French']
df$syllcomplexity[df$corpus == 'Tsimane\' (N=30)'] <- langinfo$Maddieson_sylcomp[langinfo$Language == 'Tsimane']
df$syllcomplexity[df$corpus == 'Solomon (N=15)'] <- langinfo$Maddieson_sylcomp[langinfo$Language == 'Roviana']

# Prepare data for plotting (reorder factors, subset data to overlapping region)
df$syllcomplexity <- factor(df$syllcomplexity, levels = c("Low", "Moderate", "High"))
sub_for_comp <- subset(df, age_mo_round <= 40)
# To see full age range plotted, uncomment the following line:
# sub_for_comp <- df

# a. Scatterplot for syllabic complexity
p_syllcomp <- ggplot(sub_for_comp, aes(x = age_mo_round, y = cp, fill = syllcomplexity, shape = syllcomplexity)) + geom_point() + 
  xlab("Age (months)") + ylab("Canonical proportion") + 
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  theme(legend.title=element_blank()) + 
  geom_point(size=3) + 
  scale_shape_manual(values=c(25, 23, 24))

# b. Barplot for syllabic complexity
sub_for_comp.summary <- sub_for_comp %>%
  group_by(syllcomplexity) %>%
  summarise(mean = mean(cp), sd = sd(cp), n = n(), se = sd / sqrt(n))

barcomp <-ggplot(data=sub_for_comp, aes(x=syllcomplexity, y=cp, fill=syllcomplexity)) +
  geom_bar(fun = "mean", stat = "summary") +
  theme(axis.title.y=element_blank(),legend.position="none") +
  geom_jitter(data=sub_for_comp, width=0.15, size = 0.25, colour = 'grey40') + 
  xlab("Syllabic Complexity") + 
  geom_errorbar(data=sub_for_comp.summary, aes(x=syllcomplexity,y=mean,ymin=mean-se,ymax=mean+se), width=.2, size = 0.6) 


# 3. Canonical proportion by age, colored by rural vs. urban
# Get rural vs. urban
df$urbrur[df$corpus == 'Yélî Dnye (N=41)'] <- 'Rural'
df$urbrur[df$corpus == 'Quechua (N=3)'] <- 'Rural'
df$urbrur[df$corpus == 'English-Bergelson (N=10)' | df$corpus == 'English-Seidl (N=10)'] <- 'Urban'
df$urbrur[df$corpus == 'Tseltal (N=10)'] <- 'Rural'
df$urbrur[df$corpus == 'French (N=10)'] <- 'Urban'
df$urbrur[df$corpus == 'Tsimane\' (N=30)'] <- 'Rural'
df$urbrur[df$corpus == 'Solomon (N=15)'] <- 'Rural'

# Prepare data for plotting
sub_for_urbrur <- subset(df, age_mo_round <= 20)
# To see full age range plotted, uncomment the following line:
# sub_for_urbrur <- df

# a. Scatterplot for rural vs. urban
p_urbrur <- ggplot(sub_for_urbrur, aes(x = age_mo_round, y = cp, color = urbrur)) + geom_point() + 
  xlab("Age (months)") + ylab("Canonical proportion") + 
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  theme(legend.title=element_blank()) + 
  geom_point(size=3) + 
  scale_color_manual(values = c('grey70', 'grey30'))

# b. Barplot for rural vs. urban
sub_for_urbrur.summary <- sub_for_urbrur %>%
  group_by(urbrur) %>%
  summarise(mean = mean(cp), sd = sd(cp), n = n(), se = sd / sqrt(n))

barurbrur <-ggplot(data=sub_for_urbrur, aes(x=urbrur, y=cp, fill=urbrur)) +
  geom_bar(fun = "mean", stat = "summary") +
  theme(axis.title.y=element_blank(),legend.position="none") +
  geom_jitter(width=0.15, size = 0.25, colour = 'grey50') +
  xlab("Environment") + 
  scale_fill_manual(values = c('grey70', 'grey30')) +
  geom_errorbar(data=sub_for_urbrur.summary, aes(x=urbrur,y=mean,ymin=mean-se,ymax=mean+se), width=.2, size = 0.6) 


# 4. Make final combined plot for ICPhs paper
p_combined <- ggarrange(p_corpus,
ggarrange(p_syllcomp, barcomp, ncol=2, widths=c(1.7,1), common.legend = TRUE, legend="bottom", align="h"),
ggarrange(p_urbrur, barurbrur, ncol=2, widths=c(1.3,2), common.legend = TRUE, legend="bottom", align="h"),
nrow=3, heights = c(10,6,6)
)
ggsave("../results/combined-plot.pdf", width = 4.5, height = 9.12, p_combined)






### PART 3: Statistical analyses

# 1. Regression look at relationship between age and canonical proportion
# calculate zscored age + zscored age^2 for regression
df$age_mo_round_z <- (df$age_mo_round - mean(df$age_mo_round))/sd(df$age_mo_round)
df$age_mo_round_sq <- df$age_mo_round_z**2
df$age_mo_round_sq_z <- (df$age_mo_round_sq - mean(df$age_mo_round_sq))/sd(df$age_mo_round_sq)

## fit initial model using all data
## running the full models (with slopes + intercepts) results in singular fit
## so we step back to just slopes; we compare a linear model to a quadratic model
# slopes & intercepts: age.model <- glmer(cp ~ age_mo_round_z + child_gender + (1+age_mo_round_z|corpus), data = df, family = binomial(), weights = n_speechlike)
age.model <- glmer(cp ~ age_mo_round_z + child_gender + (0+age_mo_round_z|corpus), data = df, family = binomial(), weights = n_speechlike)
# slopes & intercepts: agesqz.model <- glmer(cp ~ age_mo_round_sq_z + age_mo_round_z + child_gender + (1 + age_mo_round_z + age_mo_round_sq_z|corpus), data = df, family = binomial(), weights = n_speechlike)
agesqz.model <- glmer(cp ~ age_mo_round_sq_z + age_mo_round_z + child_gender + (0 + age_mo_round_z + age_mo_round_sq_z|corpus), data = df, family = binomial(), weights = n_speechlike)

summary(age.model)
# Random effects:
# Groups Name           Variance Std.Dev.
# corpus age_mo_round_z 0.4085   0.6391  
# Number of obs: 98, groups:  corpus, 8

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    -0.40846    0.02215 -18.438  < 2e-16 ***
#   age_mo_round_z  1.10607    0.24475   4.519 6.21e-06 ***
#   child_genderM  -0.03428    0.02607  -1.315    0.189  

summary(agesqz.model)
# Random effects:
#   Groups Name              Variance Std.Dev. Corr
# corpus age_mo_round_z    0.1804   0.4247       
# age_mo_round_sq_z 0.3445   0.5870   0.61
# Number of obs: 98, groups:  corpus, 8

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -0.51161    0.02669 -19.168  < 2e-16 ***
#   age_mo_round_sq_z -0.19238    0.21971  -0.876   0.3812    
# age_mo_round_z     1.18361    0.16352   7.238 4.54e-13 ***
#   child_genderM     -0.04453    0.02655  -1.677   0.0935 .  

# Comparison between two models: the quadratic model is a better fit
anova(age.model, agesqz.model)
# Data: df
# Models:
#   age.model: cp ~ age_mo_round_z + child_gender + (0 + age_mo_round_z | corpus)
# agesqz.model: cp ~ age_mo_round_sq_z + age_mo_round_z + child_gender + (0 + age_mo_round_z + age_mo_round_sq_z | corpus)
# npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)    
# age.model       4 2024.3 2034.6 -1008.14   2016.3                         
# agesqz.model    7 1813.7 1831.8  -899.83   1799.7 216.62  3  < 2.2e-16 ***

# 2. Welch's T-Tests comparing syllabic complexities
df_syllcomp <- subset(df, age_mo_round >= 1 & age_mo_round <= 40)
# Compare low vs. high
df_syllcomp_lowhigh <- subset(df_syllcomp, syllcomplexity != "Moderate")
t.test(cp~syllcomplexity, data = df_syllcomp_lowhigh)
# Compare low vs. moderate
df_syllcomp_lowmod <- subset(df_syllcomp, syllcomplexity != "High")
t.test(cp~syllcomplexity, data = df_syllcomp_lowmod)
# Compare moderate vs. high
df_syllcomp_modhigh <- subset(df_syllcomp, syllcomplexity != "Low")
t.test(cp~syllcomplexity, data = df_syllcomp_modhigh)



# 3. Welch's T-Test comparing urban vs. rural in 6-20mo subsample (i.e., ages where they overlap)
df_urbrur <- subset(df, age_mo_round >= 6 & age_mo_round <= 20)
t.test(cp~urbrur, data = df_urbrur)
# Welch Two Sample t-test
# 
# data:  cp by urbrur
# t = 4.1822, df = 58.814, p-value = 9.744e-05
# alternative hypothesis: true difference in means between group Rural and group Urban is not equal to 0
# 95 percent confidence interval:
#   0.06946314 0.19692864
# sample estimates:
#   mean in group Rural mean in group Urban 
# 0.3235304           0.1903346 
