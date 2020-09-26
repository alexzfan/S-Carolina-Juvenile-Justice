library(broom)
library(GGally)
library(dotwhisker)
library(MatchIt)
library(mfx) #run after unless you want to mess up select functionality from tidy

# Full Model from Lasso
Important_Coefs<-read.csv("../../Build/Output/Important_Coefs.csv")
#Important_Coefs <- read_csv("Matched_Important_Coefs.csv")
d1_filtered$exposure <- ifelse(d1_filtered$exposure == "Prosecutor", 1, 0)

match_f <- as.formula(paste("exposure ~ ", paste(Important_Coefs$coefs[2:18], collapse= "+"), " + ", paste(Important_Coefs$coefs[50:81], collapse= "+")))

mod_match <- matchit(match_f, method= "nearest", data = d1_filtered)
summary(mod_match)

dta_m <- match.data(mod_match)
dim(dta_m)

test.1 <- glm(decision_pros_dummy ~ exposure, family = binomial(link = "probit"), data = dta_m)
summary(test.1)
probitmfx(formula = decision_pros_dummy~exposure, data = dta_m)

select <- dplyr::select
#Race Subgroups
subsampleAA2<-dta_m%>%
  filter(ethnicity.yth=='AA')%>%
  select(-ethnicity.yth)
subsampleWh2<-dta_m%>%
  filter(ethnicity.yth=='Wh')%>%
  select(-ethnicity.yth)
subsampleAI2<-dta_m%>%
  filter(ethnicity.yth=='AI')%>%
  select(-ethnicity.yth)
subsampleAP2<-dta_m%>%
  filter(ethnicity.yth=='AP')%>%
  select(-ethnicity.yth)
subsampleHi2<-dta_m%>%
  filter(ethnicity.yth=='Hi')%>%
  select(-ethnicity.yth)
subsampleOt2<-dta_m%>%
  filter(ethnicity.yth=='Ot')%>%
  select(-ethnicity.yth)



#African American sub-sample 
match_probit1<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleAA2)

term<-('Prosecutor System')
estimate<-(match_probit1$mfxest[1])
std.error<-(match_probit1$mfxest[2])
statistic<-match_probit1$mfxest[3]
p.value<-match_probit1$mfxest[4]
match_probit1.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t1<-as_tibble(match_probit1.data)%>%mutate(model="African American subsample")

#White subsample
match_probit2<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleWh2)

term<-('Prosecutor System')
estimate<-(match_probit2$mfxest[1])
std.error<-(match_probit2$mfxest[2])
statistic<-match_probit2$mfxest[3]
p.value<-match_probit2$mfxest[4]
match_probit2.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t2<-as_tibble(match_probit2.data)%>%mutate(model="White subsample")

#AI subsample 
match_probit3<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleAI2)

term<-('Prosecutor System')
estimate<-(match_probit3$mfxest[1])
std.error<-(match_probit3$mfxest[2])
statistic<-match_probit3$mfxest[3]
p.value<-match_probit3$mfxest[4]
match_probit3.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t3<-as_tibble(match_probit3.data)%>%mutate(model="American Indian subsample")

#AP subsample
match_probit4<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleAP2)

term<-('Prosecutor System')
estimate<-(match_probit4$mfxest[1])
std.error<-(match_probit4$mfxest[2])
statistic<-match_probit4$mfxest[3]
p.value<-match_probit4$mfxest[4]
match_probit4.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t4<-as_tibble(match_probit4.data)%>%mutate(model="Asian Pacific subsample")

#Hispanic subsample
match_probit5<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleHi2)

term<-('Prosecutor System')
estimate<-(match_probit5$mfxest[1])
std.error<-(match_probit5$mfxest[2])
statistic<-match_probit5$mfxest[3]
p.value<-match_probit5$mfxest[4]
match_probit5.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t5<-as_tibble(match_probit5.data)%>%mutate(model="Hispanic subsample")

#Other
match_probit6<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleOt2)
term<-('Prosecutor System')
estimate<-(match_probit6$mfxest[1])
std.error<-(match_probit6$mfxest[2])
statistic<-match_probit6$mfxest[3]
p.value<-match_probit6$mfxest[4]
match_probit6.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t6<-as_tibble(match_probit6.data)%>%mutate(model="Other_race subsample")

match_race_allModels_Prosecuted<-rbind(match_t1,match_t2,match_t3,match_t4,match_t5,match_t6)

match_race_allModels_Prosecuted$model <- factor(match_race_allModels_Prosecuted$model, 
                                          levels = unique(match_race_allModels_Prosecuted$model),
                                          labels = unique(match_race_allModels_Prosecuted$model))

match_race_allModels_Prosecuted %>%
  arrange(desc(model)) %>%
  dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("Matched (Prosecuted) Marginal Effects for Treatment_All Ethnicity Groups")+
  scale_color_brewer(palette="Dark2",
                     breaks=match_race_allModels_Prosecuted$model)


#African American sub-sample 
match_probit21<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleAA2)

term<-('Prosecutor System')
estimate<-(match_probit21$mfxest[1])
std.error<-(match_probit21$mfxest[2])
statistic<-match_probit21$mfxest[3]
p.value<-match_probit21$mfxest[4]
match_probit21.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t21<-as_tibble(match_probit21.data)%>%mutate(model="African American subsample")

#White subsample
match_probit22<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleWh2)

term<-('Prosecutor System')
estimate<-(match_probit22$mfxest[1])
std.error<-(match_probit22$mfxest[2])
statistic<-match_probit22$mfxest[3]
p.value<-match_probit22$mfxest[4]
match_probit22.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t22<-as_tibble(match_probit22.data)%>%mutate(model="White subsample")

#AI subsample 
match_probit23<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleAI2)

term<-('Prosecutor System')
estimate<-(match_probit23$mfxest[1])
std.error<-(match_probit23$mfxest[2])
statistic<-match_probit23$mfxest[3]
p.value<-match_probit23$mfxest[4]
match_probit23.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t23<-as_tibble(match_probit23.data)%>%mutate(model="American Indian subsample")

#AP subsample
match_probit24<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleAP2)

term<-('Prosecutor System')
estimate<-(match_probit24$mfxest[1])
std.error<-(match_probit24$mfxest[2])
statistic<-match_probit24$mfxest[3]
p.value<-match_probit24$mfxest[4]
match_probit24.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t24<-as_tibble(match_probit24.data)%>%mutate(model="Asian Pacific subsample")

#Hispanic subsample
match_probit25<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleHi2)

term<-('Prosecutor System')
estimate<-(match_probit25$mfxest[1])
std.error<-(match_probit25$mfxest[2])
statistic<-match_probit25$mfxest[3]
p.value<-match_probit25$mfxest[4]
match_probit25.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t25<-as_tibble(match_probit25.data)%>%mutate(model="Hispanic subsample")

#Other
match_probit26<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleOt2)

term<-('Prosecutor System')
estimate<-(match_probit26$mfxest[1])
std.error<-(match_probit26$mfxest[2])
statistic<-match_probit26$mfxest[3]
p.value<-match_probit26$mfxest[4]
match_probit26.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t26<-as_tibble(match_probit26.data)%>%mutate(model="Other_race subsample")

match_race_allModels_Dismissal<-rbind(match_t21,match_t22,match_t23,match_t24,match_t25,match_t26)

match_race_allModels_Dismissal$model <- factor(match_race_allModels_Dismissal$model, 
                                                levels = unique(match_race_allModels_Dismissal$model),
                                                labels = unique(match_race_allModels_Dismissal$model))

match_race_allModels_Dismissal %>%
  arrange(desc(model)) %>%
  dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("Matched (Dismissal) Marginal Effects for Treatment_All Ethnicity Groups")+
  scale_color_brewer(palette="Dark2",
                     breaks=match_race_allModels_Dismissal$model)


#African American sub-sample 
match_probit31<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleAA2)

term<-('Prosecutor System')
estimate<-(match_probit31$mfxest[1])
std.error<-(match_probit31$mfxest[2])
statistic<-match_probit31$mfxest[3]
p.value<-match_probit31$mfxest[4]
match_probit31.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t31<-as_tibble(match_probit31.data)%>%mutate(model="African American subsample")

#White subsample
match_probit32<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleWh2)

term<-('Prosecutor System')
estimate<-(match_probit32$mfxest[1])
std.error<-(match_probit32$mfxest[2])
statistic<-match_probit32$mfxest[3]
p.value<-match_probit32$mfxest[4]
match_probit32.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t32<-as_tibble(match_probit32.data)%>%mutate(model="White subsample")

#AI subsample 
match_probit33<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleAI2)

term<-('Prosecutor System')
estimate<-(match_probit33$mfxest[1])
std.error<-(match_probit33$mfxest[2])
statistic<-match_probit33$mfxest[3]
p.value<-match_probit33$mfxest[4]
match_probit33.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t33<-as_tibble(match_probit33.data)%>%mutate(model="American Indian subsample")

#AP subsample
match_probit34<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleAP2)

term<-('Prosecutor System')
estimate<-(match_probit34$mfxest[1])
std.error<-(match_probit34$mfxest[2])
statistic<-match_probit34$mfxest[3]
p.value<-match_probit34$mfxest[4]
match_probit34.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t34<-as_tibble(match_probit34.data)%>%mutate(model="Asian Pacific subsample")

#Hispanic subsample
match_probit35<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleHi2)

term<-('Prosecutor System')
estimate<-(match_probit35$mfxest[1])
std.error<-(match_probit35$mfxest[2])
statistic<-match_probit35$mfxest[3]
p.value<-match_probit35$mfxest[4]
match_probit35.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t35<-as_tibble(match_probit35.data)%>%mutate(model="Hispanic subsample")

#Other
match_probit36<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleOt2)

term<-('Prosecutor System')
estimate<-(match_probit36$mfxest[1])
std.error<-(match_probit36$mfxest[2])
statistic<-match_probit36$mfxest[3]
p.value<-match_probit36$mfxest[4]
match_probit36.data<-data.frame(term,estimate,std.error,statistic,p.value)
match_t36<-as_tibble(match_probit36.data)%>%mutate(model="Other_race subsample")

## I put the matched sample in, but cant get a logo

match_race_allModels_Diversion<-rbind(match_t31,match_t32,match_t33,match_t34,match_t35,match_t36)


match_race_allModels_Diversion$model <- factor(match_race_allModels_Diversion$model, 
                                                levels = unique(match_race_allModels_Diversion$model),
                                                labels = unique(match_race_allModels_Diversion$model))

match_race_allModels_Diversion %>%
  arrange(desc(model)) %>%
  dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("Matched (Diversion) Marginal Effects for Treatment_All Ethnicity Groups")+
  scale_color_brewer(palette="Dark2",
                     breaks=match_race_allModels_Diversion$model)



###Lasso
##Prosecuted _Naive Models (Lasso and Probits)
lasso_pros_effect<-read.csv("../../Build/Output/Lasso_effects_pros.csv")

#lambda min no interaction
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_pros_effect$eff[1], lasso_pros_effect$eff[3])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso1.data<-data.frame(term,estimate,std.error,statistic,p.value)
l1<-as_tibble(lasso1.data)%>%mutate(model="Lasso_lambda.min_No Interaction Terms")

#lambda 1se no interaction 
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_pros_effect$eff[2], lasso_pros_effect$eff[5])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso2.data<-data.frame(term,estimate,std.error,statistic,p.value)
l2<-as_tibble(lasso2.data)%>%mutate(model="Lasso_lambda.1se_No Interaction Terms")

# lambda min with interaction
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_pros_effect$eff[5], lasso_pros_effect$eff[7])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso5.data<-data.frame(term,estimate,std.error,statistic,p.value)
l5<-as_tibble(lasso5.data)%>%mutate(model="Lasso_lambda.min_With Interaction Terms")

# lambda 1se with interaction -exposure
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_pros_effect$eff[6], lasso_pros_effect$eff[8])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso6.data<-data.frame(term,estimate,std.error,statistic,p.value)
l6<-as_tibble(lasso6.data)%>%mutate(model="Lasso_lambda.1se_With Interaction Terms")

p1<-probitmfx(formula=decision_pros_dummy~exposure, data=dta_m)
term<-('Prosecutor System')
estimate<-(p1$mfxest[1])
std.error<-(p1$mfxest[2])
statistic<-p1$mfxest[3]
p.value<-p1$mfxest[4]
p1.data<-data.frame(term,estimate,std.error,statistic,p.value)
pt1<-as_tibble(p1.data)%>%mutate(model="Probit_Treatment_Only")

p2<-probitmfx(formula=decision_pros_dummy~exposure+ethnicity.yth, data=dta_m)
term<-c('Prosecutor System','African_American')
estimate<-c(p2$mfxest[1],p2$mfxest[4])
std.error<-c(p2$mfxest[1,2],p2$mfxest[4,2])
statistic<-c(p2$mfxest[1,3],p2$mfxest[4,3])
p.value<-c(p2$mfxest[1,4],p2$mfxest[4,4])
p2.data<-data.frame(term,estimate,std.error,statistic,p.value)
pt2<-as_tibble(p2.data)%>% mutate(model="Probit_Treatment_with_Ethnicity")

naive_Prosecuted<-rbind(l1,l2,l5,l6,pt1,pt2)

naive_Prosecuted$model <- factor(naive_Prosecuted$model, 
                                               levels = unique(naive_Prosecuted$model),
                                               labels = unique(naive_Prosecuted$model))

naive_Prosecuted %>%
  arrange(desc(model)) %>%
  dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("Matched (Prosecuted) Marginal Effects for Treatment and Race-Naive Models")+
  scale_color_brewer(palette="Dark2",
                     breaks=naive_Prosecuted$model)



##Dismissal-Naive Models(lasso and probits)

p21<-probitmfx(formula=decision_dis_dummy~exposure, data=dta_m)
term<-('Prosecutor System')
estimate<-(p21$mfxest[1])
std.error<-(p1$mfxest[2])
statistic<-p1$mfxest[3]
p.value<-p1$mfxest[4]
p21.data<-data.frame(term,estimate,std.error,statistic,p.value)
pt21<-as_tibble(p21.data)%>%mutate(model="Probit_Treatment_Only")

p22<-probitmfx(formula=decision_dis_dummy~exposure+ethnicity.yth, data=dta_m)
term<-c('Prosecutor System','African_American')
estimate<-c(p22$mfxest[1],p22$mfxest[4])
std.error<-c(p22$mfxest[1,2],p22$mfxest[4,2])
statistic<-c(p22$mfxest[1,3],p22$mfxest[4,3])
p.value<-c(p22$mfxest[1,4],p22$mfxest[4,4])
p22.data<-data.frame(term,estimate,std.error,statistic,p.value)
pt22<-as_tibble(p22.data)%>% mutate(model="Probit_Treatment_with_Ethnicity")


#Lasso
lasso_dis_effect<-read.csv("../../Build/Output/Lasso_effects_dis.csv")

#lambda min no interaction
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_dis_effect$eff[5], lasso_dis_effect$eff[7])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso21.data<-data.frame(term,estimate,std.error,statistic,p.value)
l21<-as_tibble(lasso21.data)%>%mutate(model="Lasso_lambda.min_No Interaction Terms")

#lambda 1se no interaction 
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_dis_effect$eff[6], lasso_dis_effect$eff[8])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso22.data<-data.frame(term,estimate,std.error,statistic,p.value)
l22<-as_tibble(lasso22.data)%>%mutate(model="Lasso_lambda.1se_No Interaction Terms")


# lambda min with interaction
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_dis_effect$eff[1], lasso_dis_effect$eff[3])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso25.data<-data.frame(term,estimate,std.error,statistic,p.value)
l25<-as_tibble(lasso25.data)%>%mutate(model="Lasso_lambda.min_With Interaction Terms")

# lambda 1se with interaction
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_dis_effect$eff[2], lasso_dis_effect$eff[4])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso26.data<-data.frame(term,estimate,std.error,statistic,p.value)
l26<-as_tibble(lasso26.data)%>%mutate(model="Lasso_lambda.1se_With Interaction Terms")

naive_dismissal<-rbind(l21,l22,l25,l26,pt21,pt22)
naive_dismissal$model <- factor(naive_dismissal$model, 
                                 levels = unique(naive_dismissal$model),
                                 labels = unique(naive_dismissal$model))

naive_dismissal %>%
  arrange(desc(model)) %>%
  dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("Matched (Dismissal) Marginal Effects for Treatment and Race-Naive Models")+
  scale_color_brewer(palette="Dark2",
                     breaks=naive_dismissal$model)


#probit
p31<-probitmfx(formula=decision_div_dummy~exposure, data=dta_m)

term<-('Prosecutor System')
estimate<-(p31$mfxest[1])
std.error<-(p1$mfxest[2])
statistic<-p1$mfxest[3]
p.value<-p1$mfxest[4]
p31.data<-data.frame(term,estimate,std.error,statistic,p.value)
pt31<-as_tibble(p31.data)%>%mutate(model="Probit_Treatment_Only")

p32<-probitmfx(formula=decision_div_dummy~exposure+ethnicity.yth, data=dta_m)
term<-c('Prosecutor System','African_American')
estimate<-c(p32$mfxest[1],p32$mfxest[4])
std.error<-c(p32$mfxest[1,2],p32$mfxest[4,2])
statistic<-c(p32$mfxest[1,3],p32$mfxest[4,3])
p.value<-c(p32$mfxest[1,4],p32$mfxest[4,4])
p32.data<-data.frame(term,estimate,std.error,statistic,p.value)
pt32<-as_tibble(p32.data)%>% mutate(model="Probit_Treatment_with_Ethnicity")


#Lasso
lasso_div_effect<-read.csv('../../Build/Output/Lasso_effects_div.csv')

#lambda min no interaction
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_div_effect$eff[5], lasso_div_effect$eff[7])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso31.data<-data.frame(term,estimate,std.error,statistic,p.value)
l31<-as_tibble(lasso31.data)%>%mutate(model="Lasso_lambda.min_No Interaction Terms")

#lambda 1se no interaction 
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_div_effect$eff[6], lasso_div_effect$eff[8])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso32.data<-data.frame(term,estimate,std.error,statistic,p.value)
l32<-as_tibble(lasso32.data)%>%mutate(model="Lasso_lambda.1se_No Interaction Terms")


# lambda min with interaction
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_div_effect$eff[1], lasso_div_effect$eff[3])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso35.data<-data.frame(term,estimate,std.error,statistic,p.value)
l35<-as_tibble(lasso35.data)%>%mutate(model="Lasso_lambda.min_With Interaction Terms")

# lambda 1se with interaction
term<-c('Prosecutor System','African_American')
estimate<-c(lasso_div_effect$eff[2], lasso_div_effect$eff[4])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso36.data<-data.frame(term,estimate,std.error,statistic,p.value)
l36<-as_tibble(lasso36.data)%>%mutate(model="Lasso_lambda.1se_With Interaction Terms")


naive_diversion<-rbind(l31,l32,l35,l36,pt31,pt32)
naive_diversion$model <- factor(naive_diversion$model, 
                                levels = unique(naive_diversion$model),
                                labels = unique(naive_diversion$model))

naive_diversion %>%
  arrange(desc(model)) %>%
  dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("Matched (Diversion) Marginal Effects for Treatment and Race-Naive Models")+
  scale_color_brewer(palette="Dark2",
                     breaks=naive_diversion$model)
