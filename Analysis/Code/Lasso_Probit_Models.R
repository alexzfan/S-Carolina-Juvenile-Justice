
library(broom)
library(GGally)
library(dotwhisker)
library(MatchIt)
library(mfx)

#Run these before mfx 
select <- dplyr::select
subsampleAA<-d1_filtered%>%
  filter(ethnicity.yth=='AA')%>%
  select(-ethnicity.yth)

subsampleWh<-d1_filtered%>%
  filter(ethnicity.yth=='Wh')%>%
  select(-ethnicity.yth)

subsampleAI<-d1_filtered%>%
  filter(ethnicity.yth=='AI')%>%
  select(-ethnicity.yth)

subsampleAP<-d1_filtered%>%
  filter(ethnicity.yth=='AP')%>%
  select(-ethnicity.yth)

subsampleHi<-d1_filtered%>%
  filter(ethnicity.yth=='Hi')%>%
  select(-ethnicity.yth)

subsampleOt<-d1_filtered%>%
  filter(ethnicity.yth=='Ot')%>%
  select(-ethnicity.yth)

#subsample by chargeType(school vs. nonschool)

subsample_School<-d1_filtered%>%
  filter(refsrc=="School")

subsample_nonSchool<-d1_filtered%>%
  filter(refsrc!="School")



#prosecuted 
probitcharge1<-probitmfx(formula = decision_pros_dummy~exposure, data = subsample_School)

term<-('Prosecutor_System')
estimate<-(probitcharge1$mfxest[1])
std.error<-(probitcharge1$mfxest[2])
statistic<-probitcharge1$mfxest[3]
p.value<-probitcharge1$mfxest[4]
probitcharge1.data<-data.frame(term,estimate,std.error,statistic,p.value)
charge_t1<-as_tibble(probitcharge1.data)%>%mutate(model="School Charge_Type subsample")


probitcharge2<-probitmfx(formula = decision_pros_dummy~exposure, data = subsample_nonSchool)

term<-('Prosecutor_System')
estimate<-(probitcharge2$mfxest[1])
std.error<-(probitcharge2$mfxest[2])
statistic<-probitcharge2$mfxest[3]
p.value<-probitcharge2$mfxest[4]
probitcharge2.data<-data.frame(term,estimate,std.error,statistic,p.value)
charge_t2<-as_tibble(probitcharge2.data)%>%mutate(model="NonSchool Charge_Type subsample")

Charge_allModels_Prosecuted<-rbind(charge_t1,charge_t2)


Charge_allModels_Prosecuted$model <- factor(Charge_allModels_Prosecuted$model, 
                                          levels = unique(Charge_allModels_Prosecuted$model),
                                          labels = unique(Charge_allModels_Prosecuted$model))

Charge_allModels_Prosecuted %>%
  arrange(desc(model)) %>%
  dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("(Prosecuted) Marginal Effects for Charge_Type Groups")+
  scale_color_brewer(palette="Dark2",
                     breaks=Charge_allModels_Prosecuted$model)



#Dismissal 

probitcharge21<-probitmfx(formula = decision_dis_dummy~exposure, data = subsample_School)

term<-('Prosecutor_System')
estimate<-(probitcharge21$mfxest[1])
std.error<-(probitcharge21$mfxest[2])
statistic<-probitcharge21$mfxest[3]
p.value<-probitcharge21$mfxest[4]
probitcharge21.data<-data.frame(term,estimate,std.error,statistic,p.value)
charge_t21<-as_tibble(probitcharge21.data)%>%mutate(model="School Charge_Type subsample")

probitcharge22<-probitmfx(formula = decision_dis_dummy~exposure, data = subsample_nonSchool)

term<-('Prosecutor_System')
estimate<-(probitcharge22$mfxest[1])
std.error<-(probitcharge22$mfxest[2])
statistic<-probitcharge22$mfxest[3]
p.value<-probitcharge22$mfxest[4]
probitcharge22.data<-data.frame(term,estimate,std.error,statistic,p.value)
charge_t22<-as_tibble(probitcharge22.data)%>%mutate(model="NonSchool Charge_Type subsample")


Charge_allModels_Dismissal<-rbind(charge_t21,charge_t22)


Charge_allModels_Dismissal$model <- factor(Charge_allModels_Dismissal$model, 
                                            levels = unique(Charge_allModels_Dismissal$model),
                                            labels = unique(Charge_allModels_Dismissal$model))

Charge_allModels_Dismissal %>%
  arrange(desc(model)) %>%
  dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("(Dismissal) Marginal Effects for Charge_Type Groups")+
  scale_color_brewer(palette="Dark2",
                     breaks=Charge_allModels_Dismissal$model)



#Diversion 

probitcharge31<-probitmfx(formula = decision_div_dummy~exposure, data = subsample_School)

term<-('Prosecutor_System')
estimate<-(probitcharge31$mfxest[1])
std.error<-(probitcharge31$mfxest[2])
statistic<-probitcharge31$mfxest[3]
p.value<-probitcharge31$mfxest[4]
probitcharge31.data<-data.frame(term,estimate,std.error,statistic,p.value)
charge_t31<-as_tibble(probitcharge31.data)%>%mutate(model="School Charge_Type subsample")

probitcharge32<-probitmfx(formula = decision_dis_dummy~exposure, data = subsample_nonSchool)

term<-('Prosecutor_System')
estimate<-(probitcharge32$mfxest[1])
std.error<-(probitcharge32$mfxest[2])
statistic<-probitcharge32$mfxest[3]
p.value<-probitcharge32$mfxest[4]
probitcharge32.data<-data.frame(term,estimate,std.error,statistic,p.value)
charge_t32<-as_tibble(probitcharge32.data)%>%mutate(model="NonSchool Charge_Type subsample")


Charge_allModels_Diversion<-rbind(charge_t31,charge_t32)


Charge_allModels_Diversion$model <- factor(Charge_allModels_Diversion$model, 
                                           levels = unique(Charge_allModels_Diversion$model),
                                           labels = unique(Charge_allModels_Diversion$model))

Charge_allModels_Diversion %>%
  arrange(desc(model)) %>%
  dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("(Diversion) Marginal Effects for Charge_Type Groups")+
  scale_color_brewer(palette="Dark2",
                     breaks=Charge_allModels_Diversion$model)

#library(mfx) #run after unless you want to mess up select functionality from tidy

#Prosecuted 

#African American sub-sample 
probit1<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleAA)

term<-('Prosecutor_System')
estimate<-(probit1$mfxest[1])
std.error<-(probit1$mfxest[2])
statistic<-probit1$mfxest[3]
p.value<-probit1$mfxest[4]
probit1.data<-data.frame(term,estimate,std.error,statistic,p.value)
t1<-as_tibble(probit1.data)%>%mutate(model="African American subsample")


#White subsample
probit2<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleWh)

term<-('Prosecutor_System')
estimate<-(probit2$mfxest[1])
std.error<-(probit2$mfxest[2])
statistic<-probit2$mfxest[3]
p.value<-probit2$mfxest[4]
probit2.data<-data.frame(term,estimate,std.error,statistic,p.value)
t2<-as_tibble(probit2.data)%>%mutate(model="White subsample")


#AI subsample 
probit3<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleAI)

term<-('Prosecutor_System')
estimate<-(probit3$mfxest[1])
std.error<-(probit3$mfxest[2])
statistic<-probit3$mfxest[3]
p.value<-probit3$mfxest[4]
probit3.data<-data.frame(term,estimate,std.error,statistic,p.value)
t3<-as_tibble(probit3.data)%>%mutate(model="American Indian subsample")


#AP subsample
probit4<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleAP)

term<-('Prosecutor_System')
estimate<-(probit4$mfxest[1])
std.error<-(probit4$mfxest[2])
statistic<-probit4$mfxest[3]
p.value<-probit4$mfxest[4]
probit4.data<-data.frame(term,estimate,std.error,statistic,p.value)
t4<-as_tibble(probit4.data)%>%mutate(model="Asian Pacific subsample")


#Hispanic subsample
probit5<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleHi)

term<-('Prosecutor_System')
estimate<-(probit5$mfxest[1])
std.error<-(probit5$mfxest[2])
statistic<-probit5$mfxest[3]
p.value<-probit5$mfxest[4]
probit5.data<-data.frame(term,estimate,std.error,statistic,p.value)
t5<-as_tibble(probit5.data)%>%mutate(model="Hispanic subsample")


#Ot subsample

probit6<-probitmfx(formula = decision_pros_dummy~exposure, data = subsampleOt)

term<-('Prosecutor_System')
estimate<-(probit6$mfxest[1])
std.error<-(probit6$mfxest[2])
statistic<-probit6$mfxest[3]
p.value<-probit6$mfxest[4]
probit6.data<-data.frame(term,estimate,std.error,statistic,p.value)
t6<-as_tibble(probit6.data)%>%mutate(model="Other_race subsample")


race_allModels_Prosecuted<-rbind(t1,t2,t3,t4,t5,t6)

#race_allModels_Prosecuted$model<-factor(race_allModels_Prosecuted$model,
                                        #levels = c("t1","t2","t3","t4","t5","t6"),
                                        #labels = c("t1","t2","t3","t4","t5","t6"))

race_allModels_Prosecuted$model <- factor(race_allModels_Prosecuted$model, 
                                          levels = unique(race_allModels_Prosecuted$model),
                                          labels = unique(race_allModels_Prosecuted$model))

race_allModels_Prosecuted %>%
  arrange(desc(model)) %>%
dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("(Prosecuted) Marginal Effects for Treatment_All Ethnicity Groups")+
  scale_color_brewer(palette="Dark2",
                     breaks=race_allModels_Prosecuted$model)

#theme(plot.title = element_text(face="bold"),
#legend.position = c(0.993, 0.99),
#legend.justification=c(1, 1),
#legend.background = element_rect(colour="grey80"))
#legend.title = element_blank())


#Dismissal

#African American sub-sample 
probit21<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleAA)

term<-('Prosecutor_System')
estimate<-(probit21$mfxest[1])
std.error<-(probit21$mfxest[2])
statistic<-probit21$mfxest[3]
p.value<-probit21$mfxest[4]
probit21.data<-data.frame(term,estimate,std.error,statistic,p.value)
t21<-as_tibble(probit21.data)%>%mutate(model="African American subsample")


#White subsample
probit22<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleWh)

term<-('Prosecutor_System')
estimate<-(probit22$mfxest[1])
std.error<-(probit22$mfxest[2])
statistic<-probit22$mfxest[3]
p.value<-probit22$mfxest[4]
probit22.data<-data.frame(term,estimate,std.error,statistic,p.value)
t22<-as_tibble(probit22.data)%>%mutate(model="White subsample")


#AI subsample 
probit23<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleAI)

term<-('Prosecutor_System')
estimate<-(probit23$mfxest[1])
std.error<-(probit23$mfxest[2])
statistic<-probit23$mfxest[3]
p.value<-probit23$mfxest[4]
probit23.data<-data.frame(term,estimate,std.error,statistic,p.value)
t23<-as_tibble(probit23.data)%>%mutate(model="American Indian subsample")

#AP subsample
probit24<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleAP)

term<-('Prosecutor_System')
estimate<-(probit24$mfxest[1])
std.error<-(probit24$mfxest[2])
statistic<-probit24$mfxest[3]
p.value<-probit24$mfxest[4]
probit24.data<-data.frame(term,estimate,std.error,statistic,p.value)
t24<-as_tibble(probit24.data)%>%mutate(model="Asian Pacific subsample")


#Hispanic subsample
probit25<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleHi)

term<-('Prosecutor_System')
estimate<-(probit25$mfxest[1])
std.error<-(probit25$mfxest[2])
statistic<-probit25$mfxest[3]
p.value<-probit25$mfxest[4]
probit25.data<-data.frame(term,estimate,std.error,statistic,p.value)
t25<-as_tibble(probit25.data)%>%mutate(model="Hispanic subsample")


#Ot subsample

probit26<-probitmfx(formula = decision_dis_dummy~exposure, data = subsampleOt)

term<-('Prosecutor_System')
estimate<-(probit26$mfxest[1])
std.error<-(probit26$mfxest[2])
statistic<-probit26$mfxest[3]
p.value<-probit26$mfxest[4]
probit26.data<-data.frame(term,estimate,std.error,statistic,p.value)
t26<-as_tibble(probit26.data)%>%mutate(model="Other_race subsample")

race_allModels_Dismissal<-rbind(t21,t22,t23,t24,t25,t26)

dwplot(race_allModels_Dismissal, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("(Dismissal) Marginal Effects for Treatment_All Ethnicity Groups")


#Diversion 

#African American sub-sample 
probit31<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleAA)

term<-('Prosecutor_System')
estimate<-(probit31$mfxest[1])
std.error<-(probit31$mfxest[2])
statistic<-probit31$mfxest[3]
p.value<-probit31$mfxest[4]
probit31.data<-data.frame(term,estimate,std.error,statistic,p.value)
t31<-as_tibble(probit31.data)%>%mutate(model="African American subsample")


#White subsample
probit32<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleWh)

term<-('Prosecutor_System')
estimate<-(probit32$mfxest[1])
std.error<-(probit32$mfxest[2])
statistic<-probit32$mfxest[3]
p.value<-probit32$mfxest[4]
probit32.data<-data.frame(term,estimate,std.error,statistic,p.value)
t32<-as_tibble(probit32.data)%>%mutate(model="White subsample")


#AI subsample 
probit33<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleAI)

term<-('Prosecutor_System')
estimate<-(probit33$mfxest[1])
std.error<-(probit33$mfxest[2])
statistic<-probit33$mfxest[3]
p.value<-probit33$mfxest[4]
probit33.data<-data.frame(term,estimate,std.error,statistic,p.value)
t33<-as_tibble(probit33.data)%>%mutate(model="American Indian subsample")

#AP subsample
probit34<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleAP)

term<-('Prosecutor_System')
estimate<-(probit34$mfxest[1])
std.error<-(probit34$mfxest[2])
statistic<-probit34$mfxest[3]
p.value<-probit34$mfxest[4]
probit34.data<-data.frame(term,estimate,std.error,statistic,p.value)
t34<-as_tibble(probit34.data)%>%mutate(model="Asian Pacific subsample")


#Hispanic subsample
probit35<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleHi)

term<-('Prosecutor_System')
estimate<-(probit35$mfxest[1])
std.error<-(probit35$mfxest[2])
statistic<-probit35$mfxest[3]
p.value<-probit35$mfxest[4]
probit35.data<-data.frame(term,estimate,std.error,statistic,p.value)
t35<-as_tibble(probit35.data)%>%mutate(model="Hispanic subsample")


#Ot subsample

probit36<-probitmfx(formula = decision_div_dummy~exposure, data = subsampleOt)
summary(probit36)

term<-('Prosecutor_System')
estimate<-(probit36$mfxest[1])
std.error<-(probit36$mfxest[2])
statistic<-probit36$mfxest[3]
p.value<-probit36$mfxest[4]
probit36.data<-data.frame(term,estimate,std.error,statistic,p.value)
t36<-as_tibble(probit36.data)%>%mutate(model="Other_race subsample")

## I put the matched sample in, but cant get a logo
race_allModels_Diversion<-rbind(t31,t32,t33,t34,t35,t36)

dwplot(race_allModels_Diversion, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("(Diversion) Marginal Effects for Treatment_All Ethnicity Groups")



lasso_pros_effect<-read.csv("../../Build/Output/Lasso_effects_pros.csv")

#AA sample
#subsample_lasso<-d1_filtered%>%
#filter(ethnicity.yth=='AA')%>%
#select(-ethnicity.yth)

#lambda min no interaction
term<-c('Prosecutor_System','African_American')
estimate<-c(lasso_pros_effect$eff[1],lasso_pros_effect$eff[3])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso1.data<-data.frame(term,estimate,std.error,statistic,p.value)
l1<-as_tibble(lasso1.data)%>%mutate(model="Lasso_lambda.min_No Interaction Terms")

#lambda 1se no interaction 
term<-c('Prosecutor_System','African_American')
estimate<-c(lasso_pros_effect$eff[2],lasso_pros_effect$eff[5])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso2.data<-data.frame(term,estimate,std.error,statistic,p.value)
l2<-as_tibble(lasso2.data)%>%mutate(model="Lasso_lambda.1se_No Interaction Terms")


# lambda min with interaction 
term<-c('Prosecutor_System','African_American')
estimate<-c(lasso_pros_effect$eff[5],lasso_pros_effect$eff[7])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso5.data<-data.frame(term,estimate,std.error,statistic,p.value)
l5<-as_tibble(lasso5.data)%>%mutate(model="Lasso_lambda.min_With Interaction Terms")

# lambda 1se with interaction
term<-c('Prosecutor_System','African_American')
estimate<-c(lasso_pros_effect$eff[6],lasso_pros_effect$eff[8])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso6.data<-data.frame(term,estimate,std.error,statistic,p.value)
l6<-as_tibble(lasso6.data)%>%mutate(model="Lasso_lambda.1se_With Interaction Terms")


p1<-probitmfx(formula=decision_pros_dummy~exposure, data=d1_filtered)
term<-('Prosecutor_System')
estimate<-(p1$mfxest[1])
std.error<-(p1$mfxest[2])
statistic<-p1$mfxest[3]
p.value<-p1$mfxest[4]
p1.data<-data.frame(term,estimate,std.error,statistic,p.value)
pt1<-as_tibble(p1.data)%>%mutate(model="Probit_Treatment_Only")


p2<-probitmfx(formula=decision_pros_dummy~exposure+ethnicity.yth, data=d1_filtered)

term<-c('Prosecutor_System','African_American')
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
  ggtitle("(Prosecuted) Marginal Effects for Treatment and Race-Naive Models")+
  scale_color_brewer(palette="Dark2",
                     breaks=naive_Prosecuted$model)


##Dismissal-Naive Models(lasso and probits)
p21<-probitmfx(formula=decision_dis_dummy~exposure, data=d1_filtered)
term<-('Prosecutor_System')
estimate<-(p21$mfxest[1])
std.error<-(p1$mfxest[2])
statistic<-p1$mfxest[3]
p.value<-p1$mfxest[4]
p21.data<-data.frame(term,estimate,std.error,statistic,p.value)
pt21<-as_tibble(p21.data)%>%mutate(model="Probit_Treatment_Only")


p22<-probitmfx(formula=decision_dis_dummy~exposure+ethnicity.yth, data=d1_filtered)

term<-c('Prosecutor_System','African_American')
estimate<-c(p22$mfxest[1],p22$mfxest[4])
std.error<-c(p22$mfxest[1,2],p22$mfxest[4,2])
statistic<-c(p22$mfxest[1,3],p22$mfxest[4,3])
p.value<-c(p22$mfxest[1,4],p22$mfxest[4,4])
p22.data<-data.frame(term,estimate,std.error,statistic,p.value)
pt22<-as_tibble(p22.data)%>% mutate(model="Probit_Treatment_with_Ethnicity")

#lasso

lasso_dis_effect<-read.csv("../../Build/Output/Lasso_effects_dis.csv")

#lambda min no interaction
term<-c('Prosecutor_System','African_American')
estimate<-c(lasso_dis_effect$eff[5],lasso_dis_effect$eff[7])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso21.data<-data.frame(term,estimate,std.error,statistic,p.value)
l21<-as_tibble(lasso21.data)%>%mutate(model="Lasso_lambda.min_No Interaction Terms")

#lambda 1se no interaction 
term<-c('Prosecutor_System','African_American')
estimate<-c(lasso_dis_effect$eff[6],lasso_dis_effect$eff[8])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso22.data<-data.frame(term,estimate,std.error,statistic,p.value)
l22<-as_tibble(lasso22.data)%>%mutate(model="Lasso_lambda.1se_No Interaction Terms")

# lambda min with interaction 
term<-c('Prosecutor_System','African_American')
estimate<-c(lasso_dis_effect$eff[1],lasso_dis_effect$eff[3])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso25.data<-data.frame(term,estimate,std.error,statistic,p.value)
l25<-as_tibble(lasso25.data)%>%mutate(model="Lasso_lambda.min_With Interaction Terms")

# lambda 1se with interaction -exposure
term<-c('Prosecutor_System','African_American')
estimate<-c(lasso_dis_effect$eff[2],lasso_dis_effect$eff[4])
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
  ggtitle("(Dismissal) Marginal Effects for Treatment and Race-Naive Models")+
  scale_color_brewer(palette="Dark2",
                     breaks=naive_dismissal$model)


#diversion -naive models (lasso and probits)

#probit
p31<-probitmfx(formula=decision_div_dummy~exposure, data=d1_filtered)
term<-('Prosecutor_System')
estimate<-(p31$mfxest[1])
std.error<-(p1$mfxest[2])
statistic<-p1$mfxest[3]
p.value<-p1$mfxest[4]
p31.data<-data.frame(term,estimate,std.error,statistic,p.value)
pt31<-as_tibble(p31.data)%>%mutate(model="Probit_Treatment_Only")


p32<-probitmfx(formula=decision_div_dummy~exposure+ethnicity.yth, data=d1_filtered)

term<-c('Prosecutor_System','African_American')
estimate<-c(p32$mfxest[1],p32$mfxest[4])
std.error<-c(p32$mfxest[1,2],p32$mfxest[4,2])
statistic<-c(p32$mfxest[1,3],p32$mfxest[4,3])
p.value<-c(p32$mfxest[1,4],p32$mfxest[4,4])
p32.data<-data.frame(term,estimate,std.error,statistic,p.value)
pt32<-as_tibble(p32.data)%>% mutate(model="Probit_Treatment_with_Ethnicity")

#lasso

lasso_div_effect<-read.csv("../../Build/Output/Lasso_effects_div.csv")

#lambda min no interaction
term<-c('Prosecutor_System','African_American')
estimate<-c(lasso_div_effect$eff[5],lasso_div_effect$eff[7])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso31.data<-data.frame(term,estimate,std.error,statistic,p.value)
l31<-as_tibble(lasso31.data)%>%mutate(model="Lasso_lambda.min_No Interaction Terms")

#lambda 1se no interaction 
term<-c('Prosecutor_System','African_American')
estimate<-(lasso_div_effect$eff[6],lasso_div_effect$eff[8])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso32.data<-data.frame(term,estimate,std.error,statistic,p.value)
l32<-as_tibble(lasso32.data)%>%mutate(model="Lasso_lambda.1se_No Interaction Terms")


# lambda min with interaction
term<-c('Prosecutor_System','African_American')
estimate<-c(lasso_div_effect$eff[1],lasso_div_effect$eff[3])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso35.data<-data.frame(term,estimate,std.error,statistic,p.value)
l35<-as_tibble(lasso35.data)%>%mutate(model="Lasso_lambda.min_With Interaction Terms")

# lambda 1se with interaction -exposure
term<-c('Prosecutor_System','African_American')
estimate<-c(lasso_div_effect$eff[2],lasso_div_effect$eff[4])
std.error<-c(0,0)
statistic<-c(0,0)
p.value<-c(0.0,0.0)
lasso36.data<-data.frame(term,estimate,std.error,statistic,p.value)
l36<-as_tibble(lasso36.data)%>%mutate(model="Lasso_lambda.1se_With Interaction Terms")


naive_diversion<-rbind(l31,l32,l35,L36,pt31,pt32)
naive_diversion$model <- factor(naive_diversion$model, 
                                levels = unique(naive_diversion$model),
                                labels = unique(naive_diversion$model))

naive_diversion %>%
  arrange(desc(model)) %>%
  dwplot(vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  theme_bw() + xlab("Marginal Effect") + ylab("Variables") +
  ggtitle("(Diversion) Marginal Effects for Treatment and Race-Naive Models")+
  scale_color_brewer(palette="Dark2",
                     breaks=naive_diversion$model)

#add all the controls selected by GBM
#prosecuted

#naive models 
pros_controls<-read.csv("../../Build/Output/gbm_coefs_pros.csv")

test <- as.character(pros_controls$coefs)
mylist<-list()
i = 1
while(i<50){
  mylist[[i]]<-test[i]
  i=i+1
}
mylist
allcontrols<-as.formula(paste("decision_pros_dummy~", paste(mylist, collapse="+")))
probit_allcontrols<-probitmfx(formula = allcontrols, data=d1_filtered)
write.csv(probit_allcontrols$mfxest,"../Output/(prosecuted) marginal effects with all controls.csv")

#subsampled by ethnicity 
#Prosecuted 
mylist12 <- mylist[-2]
allcontrols_sub<-as.formula(paste("decision_pros_dummy~", paste(mylist12, collapse="+")))

pros_controls_AA<-probitmfx(formula=allcontrols_sub,data=subsampleAA)
write.csv(pros_controls_AA$mfxest,"../Output/(prosecuted) AA subsample marginal effects with all controls.csv")

pros_controls_WH<-probitmfx(formula=allcontrols_sub,data=subsampleWh)
write.csv(pros_controls_WH$mfxest,"../Output/(prosecuted) Wh subsample marginal effects with all controls.csv")

pros_controls_AI<-probitmfx(formula=allcontrols_sub, data=subsampleAI)
write.csv(pros_controls_AI$mfxest,"../Output/(prosecuted) AI subsample marginal effects with all controls.csv")

pros_controls_AP<-probitmfx(formula=allcontrols_sub, data=subsampleAP)
write.csv(pros_controls_WH$mfxest,"../Output/(prosecuted) AP subsample marginal effects with all controls.csv")

pros_controls_Hi<-probitmfx(formula=allcontrols_sub, data=subsampleHi)
write.csv(pros_controls_Hi$mfxest,"../Output/(prosecuted) Hi subsample marginal effects with all controls.csv")

pros_controls_Ot<-probitmfx(formula=allcontrols_sub, data=subsampleOt)
write.csv(pros_controls_Ot$mfxest,"../Output/(prosecuted) Ot subsample marginal effects with all controls.csv")


#Dismissal
Dis_controls_AA<-probitmfx(formula=allcontrols2,data=subsampleAA)

Dis_controls_WH<-probitmfx(formula=allcontrols2,data=subsampleWh)

Dis_controls_AI<-probitmfx(formula=allcontrols2, data=subsampleAI)

Dis_controls_AP<-probitmfx(formula=allcontrols2, data=subsampleAP)

Dis_controls_Hi<-probitmfx(formula=allcontrols2, data=subsampleHi)

Dis_controls_Ot<-probitmfx(formula=allcontrols2, data=subsampleOt)


#Diversion

Div_controls_AA<-probitmfx(formula=allcontrols3,data=subsampleAA)

Div_controls_WH<-probitmfx(formula=allcontrols3,data=subsampleWh)

Div_controls_AI<-probitmfx(formula=allcontrols3, data=subsampleAI)

Div_controls_AP<-probitmfx(formula=allcontrols3, data=subsampleAP)

Div_controls_Hi<-probitmfx(formula=allcontrols3, data=subsampleHi)

Div_controls_Ot<-probitmfx(formula=allcontrols3, data=subsampleOt)

#subsampled by charge type 
#prosecuted 
pros_controls_schoolCharge<-probitmfx(formula=allcontrols, data=subsample_School)
pros_controls_nonschoolCharge<-probitmfx(formula=allcontrols, data=subsample_NonSchool)

#dismissal
dis_controls_schoolCharge<-probitmfx(formula=allcontrols2, data=subsample_School)
dis_controls_nonschoolCharge<-probitmfx(formula=allcontrols2, data=subsample_NonSchool)

#diversion
div_controls_schoolCharge<-probitmfx(formula=allcontrols3, data=subsample_School)
div_controls_nonschoolCharge<-probitmfx(formula=allcontrols3, data=subsample_NonSchool)


#dismissal
dis_controls<-read.csv("../../Build/Output/gbm_coefs_dis.csv")

test2 <- as.character(dis_controls$coefs)
mylist2<-list()
i = 1
while(i<58){
  mylist2[[i]]<-test2[i]
  i=i+1
}
mylist2
allcontrols2<-as.formula(paste("decision_dis_dummy~", paste(mylist2, collapse="+")))
dismissal_probit_allcontrols<-probitmfx(formula = allcontrols2, data=d1_filtered)
write.csv(dismissal_probit_allcontrols$mfxest,"../Output/(Dismissal) marginal effects with all controls.csv")

#diversion
div_controls<-read.csv("../../Build/Output/gbm_coefs_div.csv")

test3 <- as.character(div_controls$coefs)
mylist3<-list()
i = 1
while(i<50){
  mylist3[[i]]<-test3[i]
  i=i+1
}
mylist3
allcontrols3<-as.formula(paste("decision_div_dummy~", paste(mylist3, collapse="+")))
diversion_probit_allcontrols<-probitmfx(formula = allcontrols3, data=d1_filtered)
write.csv(diversion_probit_allcontrols$mfxest,"../Output/(Diversion) marginal effects with all controls.csv")

