library(tidyverse)
library(stringr)
library(glmnet)
library(caret)
library(car)
library(margins)
library(purrr)
library(coefplot)
library(mice)
library(coefplot)

setwd("../../Build/Code")

source("../../File_Opener.r")

####################
# Feature Cleaning #
####################
select <- dplyr::select
# change some factor variables to dummies
d1 <- d1 %>% 
  mutate(decision_pros_dummy = ifelse(decision1 == "Prosecuted", 1, 0),
         decision_div_dummy = ifelse(decision1 == "Diverted", 1, 0),
         decision_dis_dummy = ifelse(decision1 == "Dismissed", 1, 0),
         decision_UTD_dummy = ifelse(decision1 == "UTD", 1, 0),
         CHARGESEVWEIGHT2 = CHARGESEVWEIGHT^2,
         ethnicity.yth = relevel(ethnicity.yth, 'Wh'),
         nprior.onehot = ifelse(nprior == 0, 0, 1))

d1$nprior.cat <- cut(d1$nprior, breaks = c(-1,0,1,2,3,100), labels = c("0","1", "2", "3", "4 or more"))

# Change all logicals to dummies
cols <- sapply(d1, is.logical)
d1[,cols] <- lapply(d1[,cols], as.numeric)

# Parse chargedescript variable
table(d1$CHARGEDESCRIPT)
d1 <- d1 %>%
  separate(col = CHARGEDESCRIPT, into = c("Charge_Type", "Charge_Descript"), sep = "(:| / ){1}")

# filter out low freq categories to reduce overfitting
charge_type_summary <-as.data.frame(table(d1$Charge_Type))
charge_type_summary$Var1 <- as.character(charge_type_summary$Var1)
charge_type_filter <- charge_type_summary$Var1[charge_type_summary$Freq > 100]
d1_filtered <- d1 %>%
  filter(Charge_Type %in% charge_type_filter)

# select non-chisq variables
d1_filtered <- d1_filtered %>%
  select(-contains("Chisq"))

# SpecNeeds cleaning

specNeeds1 <- as.data.frame(table(d1_filtered$SPECNEEDS1))
specNeeds1$Var1 <- as.character(specNeeds1$Var1)
d1_filtered[specNeeds1$Var1[!(specNeeds1$Var1 %in% colnames(d1_filtered))]] = 0

specNeeds2 <- as.data.frame(table(d1_filtered$SPECNEEDS2))
specNeeds2$Var1 <- as.character(specNeeds2$Var1)
d1_filtered[specNeeds2$Var1[!(specNeeds2$Var1 %in% colnames(d1_filtered))]] = 0

specNeeds3 <- as.data.frame(table(d1_filtered$SPECNEEDS3))
specNeeds3$Var1 <- as.character(specNeeds3$Var1)
d1_filtered[specNeeds3$Var1[!(specNeeds3$Var1 %in% colnames(d1_filtered))]] = 0

specNeeds4 <- as.data.frame(table(d1_filtered$SPECNEEDS4))
specNeeds4$Var1 <- as.character(specNeeds4$Var1)
d1_filtered[specNeeds4$Var1[!(specNeeds4$Var1 %in% colnames(d1_filtered))]] = 0

specNeeds5 <- as.data.frame(table(d1_filtered$SPECNEEDS5))
specNeeds5$Var1 <- as.character(specNeeds5$Var1)
d1_filtered[specNeeds5$Var1[!(specNeeds5$Var1 %in% colnames(d1_filtered))]] = 0

specNeeds6 <- as.data.frame(table(d1_filtered$SPECNEEDS6))
specNeeds6$Var1 <- as.character(specNeeds6$Var1)
d1_filtered[specNeeds6$Var1[!(specNeeds6$Var1 %in% colnames(d1_filtered))]] = 0

for(i in colnames(d1_filtered[187:226])){
  d1_filtered[[i]][d1_filtered$SPECNEEDS1 == i | d1_filtered$SPECNEEDS2 == i | d1_filtered$SPECNEEDS3 == i |
                     d1_filtered$SPECNEEDS4 == i | d1_filtered$SPECNEEDS5 == i | d1_filtered$SPECNEEDS6 == i] <- 1
}

# remove all specneeds columns bc they're no longer necessary
d1_filtered <- d1_filtered %>%
  select(-contains("SPECNEEDS"))

# remove nprior that have sev weight indicator
d1_filtered <- d1_filtered %>%
  select(-contains("nprior.sevgt"))


# remove columns not important for analysis (either because they dont mean anything or are directly contradictory/redundant with other variables)
d1_filtered <- d1_filtered %>%
  select(-pid, -recid, -chdesc1, -fips1, -REFERRALSOURCE, -refsrc2, -de1link, -fips.ju, -refdate, -offdate, -refsfy.prev, -refsfy, -CHARGECODE, -CHARGESEVWEIGHT, -CHARGESEVWEIGHT2)


# create missing variable columns
d1_filtered$missing.data.edu <- ifelse(is.na(d1_filtered$HOMELESSINDICATOR),1, 0)
d1_filtered$missing.data.absences <- ifelse(is.na(d1_filtered$ABSENCES),1, 0)
d1_filtered$missing.data.mth <- ifelse(is.na(d1_filtered$ethnicity.mth) |is.na(d1_filtered$birthage.mth) | is.na(d1_filtered$offage.mth) |
                                       is.na(d1_filtered$refage.mth) |is.na(d1_filtered$prevlivebirths.mth) |is.na(d1_filtered$birtheduc.mth)
                                       ,1,0)


# remove NA columns
colSums(is.na(d1_filtered))
d1_filtered <- d1_filtered[, which(colMeans(!is.na(d1_filtered)) == 1)]

#names(d1_filtered)<-make.names(names(d1_filtered),unique = TRUE)
#d1_filtered <- mice(d1_filtered, m= 5, maxit = 50, method = "pmm", seed = 123)





# apply complete cases
d1_filtered <- d1_filtered[complete.cases(d1_filtered),]

d1_filtered[sapply(d1_filtered, is.character)] <- lapply(d1_filtered[sapply(d1_filtered, is.character)], 
                                       as.factor)

rm(charge_type_summary, specNeeds1, specNeeds2, specNeeds3, specNeeds4, specNeeds5, specNeeds6, d1, cols, i, charge_type_filter)
