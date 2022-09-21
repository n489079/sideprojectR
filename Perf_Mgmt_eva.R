## Import data

Perf_Mgmt <- read.csv("PerfMgmtRewardSystemsExample.csv")

## create variable Perf_overall, Total Pay_2018 & Percentage of Variable Pay

Perf_Mgmt_eva <-add_column(Perf_Mgmt, Perf_overall = 
             Perf_Mgmt$Perf_Qual + Perf_Mgmt$Perf_Prod + Perf_Mgmt$Perf_Effort + Perf_Mgmt$Perf_Admin,
           .after = "Perf_Admin")
Perf_Mgmt_eva <-add_column(Perf_Mgmt_eva, TotPay_2018 = 
                             Perf_Mgmt_eva$BasePay_2018 + Perf_Mgmt_eva$VariablePay_2018,
                           .after = "VariablePay_2018")
Perf_Mgmt_eva <-add_column(Perf_Mgmt_eva, Percent_VP = 
                             round(Perf_Mgmt_eva$VariablePay_2018 / Perf_Mgmt_eva$TotPay_2018, 3),
                           .after = "TotPay_2018")

## Comparison between education levels 

Perf_Mgmt_eva_edu <- Perf_Mgmt_eva %>% group_by(EducationLevel) %>%
  summarise(n = n(),
            mean_Perf = mean(Perf_overall), mean_sales = mean(SalesRevenue),
            median_Perf = median(Perf_overall), median_sales = mean(SalesRevenue),
            mean_Perf = mean(Perf_overall), mean_sales = mean(SalesRevenue),
            sd_Perf = sd(Perf_overall), sd_sales = sd(SalesRevenue))
## linear regression

summary(lm(mean_Perf ~ EducationLevel, data = Perf_Mgmt_eva_edu))
summary(lm(mean_sales ~ EducationLevel, data = Perf_Mgmt_eva_edu))

# no significant difference

## Visualisation

ggplot(Perf_Mgmt_eva, aes(as.factor(EducationLevel), Perf_overall))+
  geom_boxplot()+
  labs(x = "Education Level", y = "Performance Value Overall")

ggplot(Perf_Mgmt_eva, aes(as.factor(EducationLevel), mean(SalesRevenue), fill = Sex))+
  geom_bar(stat = "identity")+
  labs(x = "Education Level", y = "Sales Revenue")

ggplot(Perf_Mgmt_eva, aes(as.factor(EducationLevel), SalesRevenue))+
  geom_jitter(aes(color = Sex))+
  geom_boxplot(alpha = 0.1)+
  labs(x = "Education Level", y = "Sales Revenue")

ggplot(Perf_Mgmt_eva, aes(as.factor(EducationLevel), TotPay_2018))+
  geom_jitter(aes(color = Sex))+
  geom_boxplot(alpha = 0.1)+
  labs(x = "Education Level", y = "Total Pay 2018")

## Result: education level 3 perform the best
## Total Pay, sales Revenue & Performance Overall may have positive correlation

ggplot(Perf_Mgmt_eva, aes(SalesRevenue, TotPay_2018))+
  geom_point(aes(color = Sex))+
  geom_smooth(method = lm)+
  labs(x = "Total Pay 2018", y = "Sales Revenue")

ggplot(Perf_Mgmt_eva, aes(Perf_overall, TotPay_2018))+
  geom_point(aes(color = Sex))+
  geom_smooth(method = lm)+
  labs(x = "Performance Overall", y = "Sales Revenue")

## Positive correlations are proved
## Difference between sex is slightly obvious

ggplot(Perf_Mgmt_eva, aes(as.factor(Sex), TotPay_2018))+
  geom_boxplot(alpha = 0.1)+
  geom_jitter(aes(color = Sex))+
  labs(x = "Sex", y = "Total Pay 2018")

ggplot(Perf_Mgmt_eva, aes(as.factor(Sex), SalesRevenue))+
  geom_boxplot(alpha = 0.1)+
  geom_jitter(aes(color = Sex))+
  labs(x = "Sex", y = "Sales Revenue")

ggplot(Perf_Mgmt_eva, aes(as.factor(Sex), Perf_overall))+
  geom_boxplot(alpha = 0.1)+
  geom_jitter(aes(color = Sex))+
  labs(x = "Sex", y = "Performance Overall")

## Payment and performance score from female are slightly lower
## However the sales revenues from both sides are similar

## Comparison between age groups

quantile(Perf_Mgmt_eva$Age, probs = c(0, 0.33, 0.66, 1))

Perf_Mgmt_eva <-add_column(Perf_Mgmt_eva, Age_group = 
                             cut(Perf_Mgmt_eva$Age, 
                                 breaks = c(-Inf, 35, 43, Inf), 
                                 labels = c("young", "middle", "old")), 
                           .after = "Age")

Perf_Mgmt_eva_age <- Perf_Mgmt_eva %>% group_by(Age_group) %>%
  summarise(n = n(),
            mean_Perf = mean(Perf_overall), mean_sales = mean(SalesRevenue),
            median_Perf = median(Perf_overall), median_sales = mean(SalesRevenue),
            mean_Perf = mean(Perf_overall), mean_sales = mean(SalesRevenue),
            sd_Perf = sd(Perf_overall), sd_sales = sd(SalesRevenue))

## Visualisation

ggplot(Perf_Mgmt_eva, aes(as.factor(Age_group), Perf_overall))+
  geom_jitter(aes(color = Sex))+
  geom_boxplot(alpha = 0.1)+
  labs(x = "Age Group", y = "Performance Score Overall")

ggplot(Perf_Mgmt_eva, aes(as.factor(Age_group), SalesRevenue))+
  geom_jitter(aes(color = Sex))+
  geom_boxplot(alpha = 0.1)+
  labs(x = "Age Group", y = "Sales Revenue")

ggplot(Perf_Mgmt_eva, aes(as.factor(Age_group), TotPay_2018))+
  geom_jitter(aes(color = Sex))+
  geom_boxplot(alpha = 0.1)+
  labs(x = "Age Group", y = "Total Pay 2018")

## Employees from old group sale more and are paid more

## Correlation between percentage of variable payment and performance

ggplot(Perf_Mgmt_eva, aes(Percent_VP, SalesRevenue))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x = "Percentage of Variable Payment", y = "Sales Revenue")

ggplot(Perf_Mgmt_eva, aes(Percent_VP, Perf_overall))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x = "Percentage of Variable Payment", y = "Performance Score Overall")





