
#################
# Data Cleaning #
#################

# dummy variables for different decisions on individual level
d1 <- d1 %>% 
  mutate(decision_pros_dummy = ifelse(decision1 == "Prosecuted", 1, 0),
         decision_div_dummy = ifelse(decision1 == "Diverted", 1, 0),
         decision_dis_dummy = ifelse(decision1 == "Dismissed", 1, 0),
         decision_UTD_dummy = ifelse(decision1 == "UTD", 1, 0),
         CHARGESEVWEIGHT2 = CHARGESEVWEIGHT^2,
         ethnicity.yth = relevel(ethnicity.yth, 'Wh'))

# make the scaled decision
d1$decision_scale <- 0
d1$decision_scale[d1$decision1 == 'UTD'] <- 0
d1$decision_scale[d1$decision1 == 'Dismissed'] <- 1
d1$decision_scale[d1$decision1 == 'Diverted'] <- 2
d1$decision_scale[d1$decision1 == 'Prosecuted'] <- 3

# Create county level data set
county_level <- d1 %>%
  group_by(county.exposure, exposure) %>%
  summarize(pros_rate = sum(decision1 == "Prosecuted")/n(),
            div_rate = sum(decision1 == "Diverted")/n(),
            dis_rate = sum(decision1 == "Dismissed")/n(),
            UTD_rate = sum(decision1 == "UTD")/n(),
            num_of_cases = n(),
            decision_scale = sum(decision_scale)/n())
  
# Clean County var of population by county for merge
population_by_county <- read_csv("~/Box Sync/QTM_Capstone_2020/Data/population_by_county.csv")
pop_by_county_new <- population_by_county %>%
  mutate(county = str_replace(`GEO.display-label`, ' County, South Carolina', '')) %>%
  select(respop72018, county)

# Join 2018 estimate of population to the county_level Dataset
county_level <- left_join(county_level, pop_by_county_new, by =c('county.exposure' = 'county'))

########################
# Exploratory Analysis #
########################

# Mean of prosecuted
d1 %>%
  group_by(exposure) %>%
  summarize(pros_rate = sum(decision1 == "Prosecuted")/n())

##variable
table(d1$CHARGEDESCRIPT)
d1 <- d1 %>%
  separate(col = CHARGEDESCRIPT, into = c("Charge_Type", "Charge_Descript"), sep = "(:| / ){1}")

# filter out low freq categories to reduce overfitting
charge_type_summary <-as.data.frame(table(d1$Charge_Type))
charge_type_summary$Var1 <- as.character(charge_type_summary$Var1)
charge_type_filter <- charge_type_summary$Var1[charge_type_summary$Freq > 100]
d1_filtered <- d1 %>%
  filter(Charge_Type %in% charge_type_filter)
d1_filtered$Charge_Type

################
# Hypo Testing #
################

# prop test of prosecution rate vs. exposure
prop.test(x = c(sum(d1$decision_pros_dummy[d1$exposure == "Prosecutor"]), sum(d1$decision_pros_dummy[d1$exposure == "Intake Officer"])), 
          n = c(length(d1$decision_pros_dummy[d1$exposure == "Prosecutor"]), length(d1$decision_pros_dummy[d1$exposure == "Intake Officer"])), 
          alternative = 'two.sided',
          correct = TRUE)

prop.test(x = c(sum(d1$decision_div_dummy[d1$exposure == "Prosecutor"]), sum(d1$decision_div_dummy[d1$exposure == "Intake Officer"])), 
          n = c(length(d1$decision_div_dummy[d1$exposure == "Prosecutor"]), length(d1$decision_div_dummy[d1$exposure == "Intake Officer"])), 
          alternative = 'two.sided',
          correct = TRUE)

prop.test(x = c(sum(d1$decision_dis_dummy[d1$exposure == "Prosecutor"]), sum(d1$decision_dis_dummy[d1$exposure == "Intake Officer"])), 
          n = c(length(d1$decision_dis_dummy[d1$exposure == "Prosecutor"]), length(d1$decision_dis_dummy[d1$exposure == "Intake Officer"])), 
          alternative = 'two.sided',
          correct = TRUE)

prop.test(x = c(sum(d1$decision_UTD_dummy[d1$exposure == "Prosecutor"]), sum(d1$decision_UTD_dummy[d1$exposure == "Intake Officer"])), 
          n = c(length(d1$decision_UTD_dummy[d1$exposure == "Prosecutor"]), length(d1$decision_UTD_dummy[d1$exposure == "Intake Officer"])), 
          alternative = 'two.sided',
          correct = TRUE)

###########################
# Graphs for Presentation #
###########################


d1%>%
  group_by(exposure)%>%
  summarize(mean_sev_weight= mean(CHARGESEVWEIGHT),
            ymin = (mean(CHARGESEVWEIGHT))-1.96*
            sd(CHARGESEVWEIGHT)/sqrt(n()),
            ymax = (mean(CHARGESEVWEIGHT))+1.96*
              sd((CHARGESEVWEIGHT))/sqrt(n()))%>%
  ggplot(aes(x = exposure, y = (mean_sev_weight))) + 
  geom_col(aes(fill = exposure)) +
  ylim(c(0,4)) +
  labs(title = "Prosecutor Systems and Intake Officer Systems \nHave Similar Charge Severity Weights", x = "Juvenile System", y = "Charge Severity Weight") +
  theme_fivethirtyeight()+
  geom_text(aes(label = round(mean_sev_weight, 4)), position = position_stack(vjust = 0.5))+
  geom_errorbar(aes(x = exposure, ymin = ymin, ymax = ymax), width=0.25, colour="black", alpha=0.75, size=0.75)+
  theme(plot.caption=element_text(hjust=1,size=9,colour="grey30"))

#t.test(d1$mean(CHARGESEVWEIGHT[d1$exposure == "Prosecutor"])~d1$mean(CHARGESEVWEIGHT[d1$exposure == "Intake Officer"]), data= d1, var.equal = TRUE)
ggplot(d1,aes(CHARGESEVWEIGHT, fill="red"))+
  geom_histogram(binwidth=1)






# Overall Pros. Rate vs. System
d1 %>%
  group_by(exposure) %>%
  summarize(pros_rate = sum(decision1 == "Prosecuted")/n(),
            ymin = sum(decision_pros_dummy)/length(decision_pros_dummy) - 1.96*
              sd(decision_pros_dummy/sqrt(length(decision_pros_dummy))),
            ymax = sum(decision_pros_dummy)/length(decision_pros_dummy) + 1.96*
              sd(decision_pros_dummy/sqrt(length(decision_pros_dummy)))) %>%
  ggplot(aes(x = exposure, y = pros_rate)) + 
  geom_col(aes(fill = exposure)) +
  ylim(0,1) +
  labs(title = "Overall Intake Officer vs. Prosecutor System Prosecution Rate", x = "Juvenile System", y = "Prosecution Rate") +
  theme_fivethirtyeight() +
  geom_text(aes(label = round(pros_rate, 4)), position = position_stack(vjust = 0.5)) +
  geom_errorbar(aes(x = exposure, ymin = ymin, ymax = ymax), width=0.25, colour="black", alpha=0.75, size=0.75)

# Treatment vs. decisions overall
ggplot(data=d1, aes(x=exposure, fill=decision1)) +
  geom_bar(position="fill", aes(fill= decision1), col = I("black"))+
  labs(title="Intake Officers Dismiss More and Prosecute Less than Prosecutors", x="Juvenile System", y="Proportion")+
  theme_fivethirtyeight()+
  theme(plot.caption=element_text(hjust=1,size=9,colour="grey30"))

# County level decisions 

county_decisions_prosecutor <- d1 %>% 
  group_by(county.exposure, exposure)%>%
  filter(exposure == "Prosecutor") %>%
  ggplot(aes(x = county.exposure)) +
  geom_bar(aes(fill = decision1), col = I("Black"), position = "fill") +
  coord_flip() +
  labs(title ="Decisions by Counties with Prosecutor System", x = "Counties", y = "Proportion of Decisions", fill = "Decisions")

county_decisions_intake <- d1 %>% 
  group_by(county.exposure, exposure)%>%
  filter(exposure == "Intake Officer") %>%
  ggplot(aes(x = county.exposure)) +
  geom_bar(aes(fill = decision1), col = I("Black"), position = "fill") +
  coord_flip()  +
  labs(title ="Decisions by Counties with Intake Officer System", x = "Counties", y = "", fill = "Decisions")


grid.arrange(county_decisions_intake, county_decisions_prosecutor)

###########################
# County Level Regression #
###########################
# regression probit county (Naive)
lm.1 <- lm(pros_rate ~ exposure , data = county_level)
summary(lm.1)

# regression probit with controls
lm.2 <- lm(pros_rate ~ exposure, data = county_level)
summary(lm.2)

lm.3 <- lm(div_rate ~ exposure, data = county_level)
summary(lm.3)

lm.4 <- lm(dis_rate ~ exposure, data = county_level)
summary(lm.4)

lm.5 <- lm(UTD_rate ~ exposure, data = county_level)
summary(lm.5)

# regression w/ scaled decision
lm_county_scale <- lm(decision_scale ~ exposure, data = county_level)
summary(lm_county_scale)

#################################
# Individual Probits Regression #
#################################
# regression probit individual level (Naive)
probit.1 <- glm(decision_pros_dummy ~ exposure, family = binomial(link = 'probit'), data = d1)
summary(probit.1)

# probit with some controls
probit.2 <- glm(decision_pros_dummy ~ exposure + CHARGESEVWEIGHT + ethnicity.yth + HANDICAPPED, family = binomial(link = 'probit'), data = d1)
summary(probit.2)

probit.3 <- glm(decision_div_dummy ~ exposure + CHARGESEVWEIGHT + ethnicity.yth + HANDICAPPED, family = binomial(link = 'probit'), data = d1)
summary(probit.3)

probit.4 <- glm(decision_dis_dummy ~ exposure + CHARGESEVWEIGHT + ethnicity.yth + HANDICAPPED, family = binomial(link = 'probit'), data = d1)
summary(probit.4)

probit.5 <- glm(decision_UTD_dummy ~ exposure + CHARGESEVWEIGHT + ethnicity.yth + HANDICAPPED, family = binomial(link = 'probit'), data = d1)
summary(probit.5)


probit.6 <- glm(decision_pros_dummy ~ exposure +Charge_Type + ethnicity.yth + HANDICAPPED, family = binomial(link = 'probit'), data = d1_filtered)
summary(probit.6)

#plot coefficients 
library(broom)
library(ggplot2)
library(GGally)
reg1<-glm(decision_pros_dummy~exposure,family = binomial(link = 'probit'), data = d1_filtered)
ggcoef(reg1)
reg<-glm(decision_pros_dummy ~ exposure +Charge_Type + ethnicity.yth + HANDICAPPED, family = binomial(link = 'probit'), data = d1_filtered)
ggcoef(reg)
ggcoef(reg, mapping = aes_string(y = "term", x = "estimate"),
       conf.int = TRUE,
       conf.level = 0.95,
       exponentiate = FALSE,
       exclude_intercept = FALSE,
       vline = TRUE,
       vline_intercept = "auto",
       vline_color = "gray50",
       vline_linetype = "dotted",
       vline_size = 1,
       errorbar_color = "gray25",
       errorbar_height = 0,
       errorbar_linetype = "solid",
       errorbar_size = 0.5,
       sort = "none")





# Regress w/ decision as a scale

lm_ind_scale <- lm(decision_scale ~ exposure + CHARGESEVWEIGHT + HANDICAPPED, data = d1)
summary(lm_ind_scale)

# probit with controls and county fixed effect?
probit.6 <- glm(decision_pros_dummy ~ exposure + ethnicity.yth +county.exposure, family = binomial(link = 'probit'), data = d1)
summary(probit.6)


d1$chargesevweight_2 <- cut(d1$CHARGESEVWEIGHT, c(-Inf,1,2,5,25))
summary(d1$chargesevweight_2)
table(d1$chargesevweight_2, d1$decision1)
ggplot(data=d1, aes(x=chargesevweight_2, fill=decision1))+
  geom_bar(stat="identity", position="fill")


print(levels(d1$chargeseverity_recode))
#regression for chargeseverity_recode
d1$chargeseverity_recode<- NULL
d1$chargeseverity_recode[d1$CHARGESEVWEIGHT==0]<-"severity0"
d1$chargeseverity_recode[d1$CHARGESEVWEIGHT==1]<-'severity1'
d1$chargeseverity_recode[d1$CHARGESEVWEIGHT==2] <- "severity2"
d1$chargeseverity_recode[d1$CHARGESEVWEIGHT==3]<-'severity3'
d1$chargeseverity_recode[d1$CHARGESEVWEIGHT==5]<-'severity5'
d1$chargeseverity_recode[d1$CHARGESEVWEIGHT==8]<-'severity8'
d1$chargeseverity_recode[d1$CHARGESEVWEIGHT==15]<-'severity15'
d1$chargeseverity_recode[d1$CHARGESEVWEIGHT==21]<-'severity21'
d1$chargeseverity_recode[d1$CHARGESEVWEIGHT==25]<-'severity25'
d1$chargeseverity_recode <- factor(d1$chargeseverity_recode)
d1$chargeseverity_recode <- relevel(d1$chargeseverity_recode, "severity2")

probit<- glm(decision_pros_dummy ~ exposure + chargeseverity_recode+ethnicity.yth, family = binomial(link = 'probit'), data = d1)
summary(probit)
coef_intercept<-summary(probit)$coefficients[1,1]
coef_sev1<-summary(probit)$coefficients[3,1]
coef_sev2<-summary(probit)$coefficients[5,1]
coef_sev3<-summary(probit)$coefficients[8,1]
coef_sev5<-summary(probit)$coefficients[9,1]
coef_sev9<-summary(probit)$coefficients[10,1]
coef_sev15<-summary(probit)$coefficients[4,1]
coef_sev21<-summary(probit)$coefficients[6,1]
coef_sev25<-summary(probit)$coefficients[7,1]
pnorm(coef_intercept+coef_sev1, mean =0, sd=1, lower.tail = 0, log.p=FALSE)
pnorm(coef_intercept+coef_sev2, mean =0, sd=1, lower.tail = 0, log.p=FALSE)
pnorm(coef_intercept+coef_sev3, mean =0, sd=1, lower.tail = 0, log.p=FALSE)
pnorm(coef_intercept+coef_sev5, mean =0, sd=1, lower.tail = 0, log.p=FALSE)
pnorm(coef_intercept+coef_sev9, mean =0, sd=1, lower.tail = 0, log.p=FALSE)
pnorm(coef_intercept+coef_sev15, mean =0, sd=1, lower.tail = 0, log.p=FALSE)
pnorm(coef_intercept+coef_sev21, mean =0, sd=1, lower.tail = 0, log.p=FALSE)
pnorm(coef_intercept+coef_sev25, mean =0, sd=1, lower.tail = 0, log.p=FALSE)


poverty_rate<- left_join(county_level, pop_by_county_new, by =c('county.exposure' = 'county'))


# Clean County var of poverty rate by county for merge
poverty_rate_by_county <- read_csv("~/Box Sync/QTM_Capstone_2020/Data/SC Poverty Rates.csv")
poverty_by_county_new <- poverty_rate_by_county %>%
  mutate(county = str_replace(`X2`, ' County, South Carolina','')) %>%
  mutate(poverty_rate = str_replace(`X4`,'poverty_rate',''))%>%
  select(poverty_rate, county)


# Join 2018 poverty rate to the county_level Dataset
county_level <- left_join(county_level, poverty_by_county_new, by =c('county.exposure' = 'county'))
View(county_level)
poverty_rate.x<-as.numeric(county_level$poverty_rate.x)

#regression
lm_county_scale <- lm(pros_rate ~ exposure +poverty_rate.x, data = county_level)
summary(lm_county_scale)


pr_score = predict(reg, type="response")
pr_score
