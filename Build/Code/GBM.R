library(gbm)

#################
### GBM Runs ####
#################

set.seed(123)

# pros GBM
train.data <- d1_filtered %>%
  select(-county.exposure, -decision1, -decision_div_dummy, -decision_dis_dummy, -decision_UTD_dummy) 

gbm1 <- gbm(decision_pros_dummy ~ . + .*. + ethnicity.yth*refsrc*nprior + ethnicity.yth*refsrc*nprior.cat + ethnicity.yth*refsrc*nprior.onehot , data = train.data, var.monotone = NULL, distribution = "bernoulli", n.trees = 100, shrinkage = 0.1,
            interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE,verbose = FALSE, n.cores = 6)

plot(gbm1)
print(gbm1)
gbm1.summary <- summary(gbm1)
gbm_pros_coefs <- as.data.frame(gbm1.summary$var[gbm1.summary$rel.inf != 0])
addition <- data.frame("coefs" = c("exposure", "ethnicity.yth"))
names(gbm_pros_coefs) <- "coefs"
gbm_pros_coefs <- rbind(addition, gbm_pros_coefs)
gbm_pros_coefs$coefs <- factor(gbm_pros_coefs$coefs)

write_csv(gbm_pros_coefs, "../Output/gbm_coefs_pros.csv")

# div GBM
set.seed(123)
train.data <- d1_filtered %>%
  select(-county.exposure, -decision1, -decision_pros_dummy, -decision_dis_dummy, -decision_UTD_dummy) 

gbm2 <- gbm(decision_div_dummy ~ . + .*.+ ethnicity.yth*refsrc*nprior + ethnicity.yth*refsrc*nprior.cat + ethnicity.yth*refsrc*nprior.onehot, data = train.data, var.monotone = NULL, distribution = "bernoulli", n.trees = 100, shrinkage = 0.1,
            interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE,verbose = FALSE, n.cores = 6)
plot(gbm2)
print(gbm2)
gbm2.summary <- summary(gbm2)
gbm_div_coefs <- as.data.frame(gbm2.summary$var[gbm2.summary$rel.inf != 0])
names(gbm_div_coefs) <- "coefs"
gbm_div_coefs <- rbind(addition, gbm_div_coefs)
gbm_div_coefs$coefs <- factor(gbm_div_coefs$coefs)

write_csv(gbm_div_coefs, "../Output/gbm_coefs_div.csv")

# dis GBM
set.seed(123)
train.data <- d1_filtered %>%
  select(-county.exposure, -decision1, -decision_pros_dummy, -decision_div_dummy, -decision_UTD_dummy) 

gbm3 <- gbm(decision_dis_dummy ~ . + .*. + ethnicity.yth*refsrc*nprior + ethnicity.yth*refsrc*nprior.cat + ethnicity.yth*refsrc*nprior.onehot, data = train.data, var.monotone = NULL, distribution = "bernoulli", n.trees = 100, shrinkage = 0.1,
            interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE,verbose = FALSE, n.cores = 6)
plot(gbm3)
print(gbm3)
gbm3.summary <- summary(gbm3)
gbm_dis_coefs <- as.data.frame(gbm3.summary$var[gbm3.summary$rel.inf != 0])
names(gbm_dis_coefs) <- "coefs"
gbm_dis_coefs <- rbind(addition, gbm_dis_coefs)
gbm_dis_coefs$coefs <- factor(gbm_dis_coefs$coefs)

write_csv(gbm_dis_coefs, "../Output/gbm_coefs_dis.csv")
