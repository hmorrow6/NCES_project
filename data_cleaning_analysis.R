#This script assumes the user was able to get data from https://nces.ed.gov/surveys/els2002/
#There are multiple datasets for that project, but the one used here is (a copied version) of the file labeled: els_02_12_byf3pststu_v1_0.csv

##### SET UP AND DATA CLEANING

#Set up clean environment, load necessary libraries
rm(list=ls())

library(tidyverse)
library(naniar)
library(visdat)
library(lme4)
library(corrplot)

#change quoted text below to reflect the directory the files are saved in
setwd("Naturally_Occurring_Data/NCES data/Project/")

#read in the file (make sure the name matches)
els_full <- read.csv(file = 'els_02_12_byf3pststu_v1_0_copy.csv')

#pull only the variables of interest
els <- select(els_full, c(STU_ID, BYTXMSTD, BYTXRSTD, BYS41A, BYS41B, BYS44F, BYSES1QU, BYACTCTL, BYSCSAF2))

#quickly check the first rows to make sure it loaded properly
head(els)

#Let's rename the variables to make this data easier to understand
els<- els %>% 
  rename(
    math = BYTXMSTD,
    reading = BYTXRSTD,
    music = BYS41A,
    theatre = BYS41B,
    creative_time = BYS44F,
    SES = BYSES1QU,
    effort = BYACTCTL,
    safety = BYSCSAF2
  )

#save as csv for shorter run time
write.csv(els, "els_project.csv")
#to read that back
els <- read.csv(file = 'els_project.csv')


summary(els)

#replace all negative values with NA
#can't include -1 because some values are standardized with a mean of 0 and std dev of 1. For music, -1 means "don't know"
els_test <- els %>% replace_with_na_all(condition = ~.x %in% c(-4, -6, -7, -8, -9))
summary(els_test)

els <- els_test
summary(els)

#to use the mcar_test() function in naniar, need to do this (guessing that that function is in beta?)
install.packages("remotes")
remotes::install_github("njtierney/naniar")

devtools::source_url("https://github.com/njtierney/naniar/blob/master/R/mcar-test.R?raw=TRUE")

#mcar test
mcar_test(els)

#so our data is not MCAR (missing completely at random). However, we have some students who don't have a standardized math or reading score. Maybe these students are missing all of the variables?


vis_dat(els)
vis_miss(els)

ggplot(els, 
       aes(x = math, 
           y = reading)) + 
  geom_miss_point()

els_shadow <- bind_shadow(els)
glimpse(els_shadow)


## for looking at the 305 students who don't have a reading or math value to see if they're missing everything else
els %>%
  bind_shadow() %>%
  group_by(math_NA) %>%
  summarise_at(.vars = "creative_time",
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)

# they are, so let's make them into their own dataset
els_miss1 <- subset(els, is.na(reading) & is.na(math))

# created a dataset without those 305 students
els2 <- subset(els, !is.na(reading))

summary (els2)
mcar_test(els2)

## added this because I didn't want the PCA variables, or the weird X id variable

els2 <- subset(els2, select=-c(X, effort, safety))

# create dummy variable to see how many variables each student is missing

els2$na_count <- apply(is.na(els2), 1, sum)

## list clearly the mean and sd for groups with missing data in a table. 
#create datasets for each missing variable
els2_miss_music <- subset(els2, is.na(music))
els2_miss_theatre <- subset(els2, is.na(theatre))
els2_miss_creative <- subset(els2, is.na(creative_time))
els2_miss_SES <- subset(els2, is.na(SES))
#els2_miss_effort <- subset(els2, is.na(effort))
#els2_miss_safety <- subset(els2, is.na(safety))

#get summary results for both reading and math scores
els2_miss_music %>%
  summarise(mean_r = mean(reading, na.rm=TRUE),
            median_r = median(reading, na.rm=TRUE),
            sd_r = sd(reading, na.rm=TRUE),
            mean_m = mean(math, na.rm=TRUE),
            median_m = median(math, na.rm=TRUE),
            sd_m = sd(math, na.rm=TRUE))

els2_miss_theatre %>%
  summarise(mean_r = mean(reading, na.rm=TRUE),
            median_r = median(reading, na.rm=TRUE),
            sd_r = sd(reading, na.rm=TRUE),
            mean_m = mean(math, na.rm=TRUE),
            median_m = median(math, na.rm=TRUE),
            sd_m = sd(math, na.rm=TRUE))

els2_miss_creative %>%
  summarise(mean_r = mean(reading, na.rm=TRUE),
            median_r = median(reading, na.rm=TRUE),
            sd_r = sd(reading, na.rm=TRUE),
            mean_m = mean(math, na.rm=TRUE),
            median_m = median(math, na.rm=TRUE),
            sd_m = sd(math, na.rm=TRUE))

els2_miss_SES %>%
  summarise(mean_r = mean(reading, na.rm=TRUE),
            median_r = median(reading, na.rm=TRUE),
            sd_r = sd(reading, na.rm=TRUE),
            mean_m = mean(math, na.rm=TRUE),
            median_m = median(math, na.rm=TRUE),
            sd_m = sd(math, na.rm=TRUE))


#redo na_count
els2$na_count <- apply(is.na(els2), 1, sum)


#Further, what are the summary stats if you group by how many variables are missing?

els2 %>%
  group_by(na_count) %>%
  summarise(mean_r = mean(reading, na.rm=TRUE),
            median_r = median(reading, na.rm=TRUE),
            sd_r = sd(reading, na.rm=TRUE),
            mean_m = mean(math, na.rm=TRUE),
            median_m = median(math, na.rm=TRUE),
            sd_m = sd(math, na.rm=TRUE))

# amount of students with missing variables
els2 %>%
  group_by(na_count) %>%
  tally()

#create dataset with no NAs
els3 <- na.omit(els2)
summary(els3)

#Change variables to factor types to allow for calculations
as.factor(els3$music)
as.factor(els3$theatre)
as.factor(els3$creative_time)
as.factor(els3$SES)

#Testing assumptions: collinearity -- passed

els3_corr <- round(cor(els3), 2)
p.mat <- cor.mtest(els3, conf.level = .95)
corrplot(els3_corr, addCoef.col = "black", p.mat = p.mat$p, sig.level = 0.05)

#testing assumptions: linear relationships -- passed

plot(els3$math, els3$reading, main="Reading and Math",
     xlab="Math Score", ylab="Reading Score ", pch=19)
abline(lm(els3$reading~els3$math), col="red") # regression line (y~x)

#testing normal distribution

hist(els$reading)
hist(els$math)

##### MODELING 

## Linear regression models, only for students who have no missing data
#Changed the models to always include SES first, upon discovering how strong of a predictor it is.

#Models for reading scores, which add a predictor variable each time
#what order should predictors be in?
rmod_1 <- lm(reading ~ SES, data = els3)
summary (rmod_1)
rmod_2 <- lm(reading ~ SES + creative_time, data = els3)
summary(rmod_2)
anova(rmod_1, rmod_2)
rmod_3 <- lm(reading ~ SES + creative_time + music, data = els3)
summary(rmod_3)
anova(rmod_2, rmod_3)
rmod_4 <- lm(reading ~ SES + creative_time + music + theatre, data = els3)
summary(rmod_4)
anova(rmod_3, rmod_4)
AIC(rmod_1)
AIC(rmod_2)
AIC(rmod_3)
AIC(rmod_4)

#Models for math scores, adding a predictor variable each time
mmod_1 <- lm(math ~ SES, data = els3)
summary (mmod_1)
mmod_2 <- lm(math ~ SES + creative_time, data = els3)
summary(mmod_2)
anova(mmod_1, mmod_2)
mmod_3 <- lm(math ~ SES + creative_time + music, data = els3)
summary(mmod_3)
anova(mmod_2, mmod_3)
mmod_4 <- lm(math ~ SES + creative_time + music + theatre, data = els3)
summary(mmod_4)
anova(mmod_3, mmod_4)
AIC(mmod_1)
AIC(mmod_2)
AIC(mmod_3)
AIC(mmod_4)

mmod_5 <- lm(math ~ SES + creative_time + theatre, data = els3)
summary(mmod_5)
anova(mmod_5, mmod_4)
