
####################
# Lasso Regression #
####################

# data frame for lasso treatment effects
LASSO_Effects <- data.frame("Model" = as.character(),
                            "y1" = as.numeric(),
                            "y0"= as.numeric(),
                            "eff" = as.numeric(),
                            "var" = as.character())


# Find the best lambda using cross-validation
set.seed(123)

train.data <- d1_filtered %>%
  select(-county.exposure, -decision1, -decision_div_dummy, -decision_dis_dummy, -decision_UTD_dummy) 

f <- as.formula(decision_pros_dummy ~ .)
x <- model.matrix(f, train.data)[,-1]
y <- as.matrix(train.data$decision_pros_dummy)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
model.1.1se <-glmnet(x, y, alpha = 1, family = "binomial",
                     lambda = cv.lasso$lambda.1se)

# Display regression coefficients
coef(model)
coef(model.1.1se)

# lambda min version
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_pros_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model, newx=nx, s=cv.lasso$lambda.min, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model, newx=nx, s=cv.lasso$lambda.min, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c("Lasso (No Int. Terms, Lambda.min")
y1 <- c(as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(as.numeric(mean(predictedvalues2$`1`)))
eff <- c(as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c("exposure")

# lambda 1se version
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_pros_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model.1.1se, newx=nx, s=cv.lasso$lambda.1se, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model.1.1se, newx=nx, s=cv.lasso$lambda.1se, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c(modelnames, "Lasso (No Int. Terms, Lambda.1se)")
y1 <- c(y1, as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(y0, as.numeric(mean(predictedvalues2$`1`)))
eff <- c(eff, as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c(var, "exposure")


# Find the effect of race (specifically being AA vs not) lambda min
model.coef <- coef(model)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means["ethnicity.ythAA","mean"] <- 0
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

coefs_means$mean[coefs_means$coefs == "ethnicity.ythAA"] <- 1
coefs_means$mean[coefs_means$coefs %in% c("ethnicity.ythAI", "ethnicity.ythAP", "ethnicity.ythOt")] <- 0

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (No Int. Terms, Lambda.min)")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")


# Find the effect of race (specifically being AA vs not) lambda 1se
model.coef <- coef(model.1.1se)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means["ethnicity.ythAA","mean"] <- 0
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

coefs_means$mean[coefs_means$coefs == "ethnicity.ythAA"] <- 1
coefs_means$mean[coefs_means$coefs %in% c("ethnicity.ythAI", "ethnicity.ythOt")] <- 0

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (No Int. Terms, Lambda.1se")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")

# Pros Rate Lasso w/ Int terms from ethnicity
train.data <- d1_filtered %>%
  select(-county.exposure, -decision1, -decision_div_dummy, -decision_dis_dummy, -decision_UTD_dummy)

f <- as.formula(decision_pros_dummy ~ . + ethnicity.yth*sex + ethnicity.yth*refsrc + ethnicity.yth*nprior + ethnicity.yth*SNAP.cumulative + ethnicity.yth*TANF.cumulative)
x <- model.matrix(f, train.data)[,-1]
y <- as.matrix(train.data$decision_pros_dummy)

cv.lasso.2 <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model2 <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso.2$lambda.min)
model.2.1se <-glmnet(x, y, alpha = 1, family = "binomial",
                     lambda = cv.lasso.2$lambda.1se)

# Display regression coefficients
coef(model2)
coef(model.2.1se)

# lambda min, with int terms
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_pros_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model2, newx=nx, s=cv.lasso.2$lambda.min, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model2, newx=nx, s=cv.lasso.2$lambda.min, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c(modelnames, "Lasso (with Int. Terms, Lambda.min)")
y1 <- c(y1, as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(y0, as.numeric(mean(predictedvalues2$`1`)))
eff <- c(eff, as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c(var, "exposure")

# lambda 1se, int terms
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_pros_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model.2.1se, newx=nx, s=cv.lasso.2$lambda.1se, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model.2.1se, newx=nx, s=cv.lasso.2$lambda.1se, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c(modelnames, "Lasso (with Int. Terms, Lambda.1se)")
y1 <- c(y1, as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(y0, as.numeric(mean(predictedvalues2$`1`)))
eff <- c(eff, as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c(var, "exposure")


# Find the effect of race (specifically being AA vs not) lambda min
model.coef <- coef(model2)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- train.data
nx$ethnicity.yth[nx$ethnicity.yth=="AA"] <- "Wh"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

nx<- train.data
nx$ethnicity.yth[nx$ethnicity.yth %in% c("Wh", "AI","AP","Ot","Hi")] <- "AA"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (with Int. Terms, Lambda.min)")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")


# Find the effect of race (specifically being AA vs not) lambda 1se
model.coef <- coef(model.2.1se)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- train.data
nx$ethnicity.yth[nx$ethnicity.yth=="AA"] <- "Wh"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

nx<- train.data
nx$ethnicity.yth[nx$ethnicity.yth %in% c("Wh", "AI","AP","Ot","Hi")] <- "AA"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (with Int. Terms, Lambda.1se)")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")

# Save results to a dataframe

LASSO_Effects <- data.frame("Model" = modelnames,
                            "y1" = y1,
                            "y0"= y0,
                            "eff" =eff,
                            "var" = var)

# write it to csv
write_csv(LASSO_Effects, "../Output/Lasso_effects.csv")

# get nonzero coefs from lasso
model.2.1se.Coef <- coef(model.2.1se)
coefs_important <- data.frame("coefs" = model.2.1se.Coef@Dimnames[[1]][which(model.2.1se.Coef != 0 )])
coefs_important$coefs  <- as.character(coefs_important$coefs)
coefs_important <- filter(coefs_important, !grepl("Intercept",coefs))
coefs_important$coefs[2:36] <- "Charge_Type"
coefs_important <- coefs_important %>%
  distinct()
coefs_important$coefs[1] <- "exposure"
coefs_important$coefs[3] <- "sex"
coefs_important$coefs[4:6] <- "ethnicity.yth"
coefs_important$coefs[7:11] <- "refsrc"
coefs_important <- coefs_important %>%
  distinct()
coefs_important <- coefs_important$coef[1:76]
int_terms <- c("ethnicity.yth*sex","ethnicity.yth*refsrc", "ethnicity.yth*nprior", "ethnicity.yth*SNAP.cumulative", "ethnicity.yth*TANF.cumulative")
coefs_important <- c(coefs_important, int_terms)

Important_Coefs <- data.frame("coefs" = coefs_important)
Important_Coefs$coefs <- as.character(Important_Coefs$coefs)
write_csv(Important_Coefs, "../Output/Important_Coefs.csv")






                                      ###################
####################################### Dismissal Lasso #######################################\
                                      ###################

# data frame for lasso treatment effects
LASSO_Effects_dis <- data.frame("Model" = as.character(),
                            "y1" = as.numeric(),
                            "y0"= as.numeric(),
                            "eff" = as.numeric(),
                            "var" = as.character())

train.data <- d1_filtered %>%
  select(-county.exposure, -decision1, -decision_div_dummy, -decision_pros_dummy, -decision_UTD_dummy)

f <- as.formula(decision_dis_dummy ~ . + ethnicity.yth*sex + ethnicity.yth*refsrc + ethnicity.yth*nprior + ethnicity.yth*SNAP.cumulative + ethnicity.yth*TANF.cumulative)
x <- model.matrix(f, train.data)[,-1]
y <- as.matrix(train.data$decision_dis_dummy)

cv.lasso.3 <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model3 <- glmnet(x, y, alpha = 1, family = "binomial",
                 lambda = cv.lasso.3$lambda.min)
model.3.1se <-glmnet(x, y, alpha = 1, family = "binomial",
                     lambda = cv.lasso.3$lambda.1se)

# Display regression coefficients
coef(model3)
coef(model.3.1se)

# lambda min, with int. terms 
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_dis_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model3, newx=nx, s=cv.lasso.3$lambda.min, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model3, newx=nx, s=cv.lasso.3$lambda.min, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c("Lasso (with Int. Terms, Lambda.min)")
y1 <- c(as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(as.numeric(mean(predictedvalues2$`1`)))
eff <- c(as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c("exposure")

# lambda 1se with int terms
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_dis_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model.3.1se, newx=nx, s=cv.lasso.3$lambda.1se, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model.3.1se, newx=nx, s=cv.lasso.3$lambda.1se, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c(modelnames, "Lasso (with Int. Terms, Lambda.1se)")
y1 <- c(y1, as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(y0, as.numeric(mean(predictedvalues2$`1`)))
eff <- c(eff, as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c(var, "exposure")

# Find the effect of race (specifically being AA vs not) lambda min
model.coef <- coef(model3)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- train.data
nx$ethnicity.yth[nx$ethnicity.yth=="AA"] <- "Wh"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

nx<- train.data
nx$ethnicity.yth[nx$ethnicity.yth %in% c("Wh", "AI","AP","Ot","Hi")] <- "AA"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (with Int. Terms, Lambda.min)")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")


# Find the effect of race (specifically being AA vs not) lambda 1se
model.coef <- coef(model.3.1se)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- train.data
nx$ethnicity.yth[nx$ethnicity.yth=="AA"] <- "Wh"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

nx<- train.data
nx$ethnicity.yth[nx$ethnicity.yth %in% c("Wh", "AI","AP","Ot","Hi")] <- "AA"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (with Int. Terms, Lambda.1se)")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")


#### NO Interaction terms
f <- as.formula(decision_dis_dummy ~ .)
x <- model.matrix(f, train.data)[,-1]
y <- as.matrix(train.data$decision_dis_dummy)

cv.lasso.4 <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model4 <- glmnet(x, y, alpha = 1, family = "binomial",
                 lambda = cv.lasso.4$lambda.min)
model.4.1se <-glmnet(x, y, alpha = 1, family = "binomial",
                     lambda = cv.lasso.4$lambda.1se)

# Display regression coefficients
coef(model4)
coef(model.4.1se)

# lambda min, with int. terms 
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_dis_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model4, newx=nx, s=cv.lasso.4$lambda.min, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model4, newx=nx, s=cv.lasso.4$lambda.min, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c(modelnames, "Lasso (no Int. Terms, Lambda.min)")
y1 <- c(y1, as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(y0, as.numeric(mean(predictedvalues2$`1`)))
eff <- c(eff, as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c(var, "exposure")

# lambda 1se with int terms
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_dis_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model.4.1se, newx=nx, s=cv.lasso.4$lambda.1se, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model.4.1se, newx=nx, s=cv.lasso.4$lambda.1se, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c(modelnames, "Lasso (no Int. Terms, Lambda.1se)")
y1 <- c(y1, as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(y0, as.numeric(mean(predictedvalues2$`1`)))
eff <- c(eff, as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c(var, "exposure")

# Find the effect of race (specifically being AA vs not) lambda min
model.coef <- coef(model4)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- train.data
nx$ethnicity.yth[nx$ethnicity.yth=="AA"] <- "Wh"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

nx<- train.data
nx$ethnicity.yth[nx$ethnicity.yth %in% c("Wh", "AI","AP","Ot","Hi")] <- "AA"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (no Int. Terms, Lambda.min)")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")


# Find the effect of race (specifically being AA vs not) lambda 1se
model.coef <- coef(model.4.1se)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- train.data
nx$ethnicity.yth[nx$ethnicity.yth=="AA"] <- "Wh"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

nx<- train.data
nx$ethnicity.yth[nx$ethnicity.yth %in% c("Wh", "AI","AP","Ot","Hi")] <- "AA"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (no Int. Terms, Lambda.1se)")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")

# Save results to a dataframe

LASSO_Effects_dis <- data.frame("Model" = modelnames,
                            "y1" = y1,
                            "y0"= y0,
                            "eff" =eff,
                            "var" = var)

write_csv(LASSO_Effects_dis, "../Output/Lasso_effects_dis.csv")


rm(list = ls()[grep("model|lasso", ls())])

                                      ###################
####################################### Diversion Lasso #######################################\
                                      ###################

# data frame for lasso treatment effects
LASSO_Effects_div <- data.frame("Model" = as.character(),
                                "y1" = as.numeric(),
                                "y0"= as.numeric(),
                                "eff" = as.numeric(),
                                "var" = as.character())

train.data <- d1_filtered %>%
  select(-county.exposure, -decision1, -decision_dis_dummy, -decision_pros_dummy, -decision_UTD_dummy)

f <- as.formula(decision_div_dummy ~ . + ethnicity.yth*sex + ethnicity.yth*refsrc + ethnicity.yth*nprior + ethnicity.yth*SNAP.cumulative + ethnicity.yth*TANF.cumulative)
x <- model.matrix(f, train.data)[,-1]
y <- as.matrix(train.data$decision_div_dummy)

cv.lasso.5 <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model5 <- glmnet(x, y, alpha = 1, family = "binomial",
                 lambda = cv.lasso.5$lambda.min)
model.5.1se <-glmnet(x, y, alpha = 1, family = "binomial",
                     lambda = cv.lasso.5$lambda.1se)

# Display regression coefficients
coef(model5)
coef(model.5.1se)

# lambda min, with int. terms 
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_div_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model5, newx=nx, s=cv.lasso.5$lambda.min, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model5, newx=nx, s=cv.lasso.5$lambda.min, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c("Lasso (with Int. Terms, Lambda.min)")
y1 <- c(as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(as.numeric(mean(predictedvalues2$`1`)))
eff <- c(as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c("exposure")

# lambda 1se with int terms
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_div_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model.5.1se, newx=nx, s=cv.lasso.5$lambda.1se, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model.5.1se, newx=nx, s=cv.lasso.5$lambda.1se, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c(modelnames, "Lasso (with Int. Terms, Lambda.1se)")
y1 <- c(y1, as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(y0, as.numeric(mean(predictedvalues2$`1`)))
eff <- c(eff, as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c(var, "exposure")

# Find the effect of race (specifically being AA vs not) lambda min
model.coef <- coef(model5)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- train.data
nx$ethnicity.yth[nx$ethnicity.yth=="AA"] <- "Wh"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

nx<- train.data
nx$ethnicity.yth[nx$ethnicity.yth %in% c("Wh", "AI","AP","Ot","Hi")] <- "AA"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (with Int. Terms, Lambda.min)")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")


# Find the effect of race (specifically being AA vs not) lambda 1se
model.coef <- coef(model.5.1se)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- train.data
nx$ethnicity.yth[nx$ethnicity.yth=="AA"] <- "Wh"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

nx<- train.data
nx$ethnicity.yth[nx$ethnicity.yth %in% c("Wh", "AI","AP","Ot","Hi")] <- "AA"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (with Int. Terms, Lambda.1se)")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")


#### NO Interaction terms
f <- as.formula(decision_div_dummy ~ .)
x <- model.matrix(f, train.data)[,-1]
y <- as.matrix(train.data$decision_div_dummy)

cv.lasso.6 <- cv.glmnet(x, y, alpha = 1, family = "binomial")
model6 <- glmnet(x, y, alpha = 1, family = "binomial",
                 lambda = cv.lasso.6$lambda.min)
model.6.1se <-glmnet(x, y, alpha = 1, family = "binomial",
                     lambda = cv.lasso.6$lambda.1se)

# Display regression coefficients
coef(model6)
coef(model.6.1se)

# lambda min, with int. terms 
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_div_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model6, newx=nx, s=cv.lasso.6$lambda.min, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model6, newx=nx, s=cv.lasso.6$lambda.min, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c(modelnames, "Lasso (no Int. Terms, Lambda.min)")
y1 <- c(y1, as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(y0, as.numeric(mean(predictedvalues2$`1`)))
eff <- c(eff, as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c(var, "exposure")

# lambda 1se with int terms
nx <- model.matrix(f, train.data)[,-1]
nx <- as.data.frame(nx)
nx[,1] <- 1
ny <- as.matrix(train.data$decision_div_dummy)
nx <- as.matrix(nx)
predictedvalues1 <- as.data.frame(predict(model.6.1se, newx=nx, s=cv.lasso.6$lambda.1se, type="response" ))
hist(predictedvalues1$`1`)
nx[,1] <-0 
predictedvalues2 <- as.data.frame(predict(model.6.1se, newx=nx, s=cv.lasso.6$lambda.1se, type="response" ))
hist(predictedvalues2$`1`)
mean(predictedvalues1$`1`)
mean(predictedvalues2$`1`)
mean(predictedvalues1$`1`-predictedvalues2$`1`)

modelnames <- c(modelnames, "Lasso (no Int. Terms, Lambda.1se)")
y1 <- c(y1, as.numeric(mean(predictedvalues1$`1`)))
y0 <- c(y0, as.numeric(mean(predictedvalues2$`1`)))
eff <- c(eff, as.numeric(mean(predictedvalues1$`1`-predictedvalues2$`1`)))
var <- c(var, "exposure")

# Find the effect of race (specifically being AA vs not) lambda min
model.coef <- coef(model6)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- train.data
nx$ethnicity.yth[nx$ethnicity.yth=="AA"] <- "Wh"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

nx<- train.data
nx$ethnicity.yth[nx$ethnicity.yth %in% c("Wh", "AI","AP","Ot","Hi")] <- "AA"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (no Int. Terms, Lambda.min)")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")


# Find the effect of race (specifically being AA vs not) lambda 1se
model.coef <- coef(model.6.1se)
model.coef_names <- data.frame("coefs" = model.coef@Dimnames[[1]][which(model.coef != 0 )], "values" = model.coef@x)
model.coef_names <- model.coef_names[-1,]

nx <- train.data
nx$ethnicity.yth[nx$ethnicity.yth=="AA"] <- "Wh"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_1 <- pnorm(sum(coefs_means$values*coefs_means$mean))

nx<- train.data
nx$ethnicity.yth[nx$ethnicity.yth %in% c("Wh", "AI","AP","Ot","Hi")] <- "AA"
nx <- model.matrix(f, nx)[,-1]
nx <- as.data.frame(nx)
nx_sample_means <- data.frame( "mean" = colMeans(nx))
nx_sample_means$coefs <- rownames(nx_sample_means)
coefs_means <- merge(model.coef_names, nx_sample_means)

race_prob_2 <- pnorm(sum(coefs_means$values*coefs_means$mean))
effect_of_race <- race_prob_2 - race_prob_1
effect_of_race

modelnames <- c(modelnames, "Lasso (no Int. Terms, Lambda.1se)")
y1 <- c(y1, as.numeric(race_prob_2))
y0 <- c(y0, as.numeric(race_prob_1))
eff <- c(eff, race_prob_2-race_prob_1)
var <- c(var, "race")

# Save results to a dataframe

LASSO_Effects_div <- data.frame("Model" = modelnames,
                                "y1" = y1,
                                "y0"= y0,
                                "eff" =eff,
                                "var" = var)

write_csv(LASSO_Effects_div, "../Output/Lasso_effects_div.csv")

rm(list = ls()[grep("model|lasso", ls())])
