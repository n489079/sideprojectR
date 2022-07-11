## Telecom customer churn from an Iranian telecom company

suppressPackageStartupMessages(library(tidyverse))

# Importing datasets
customer_churn <- read_csv('data/customer_churn.csv', show_col_types = FALSE) is.data.frame(customer_churn)
colnames(customer_churn) <- c("Call_Failure", "Complains",  "Subscription_Length",
                              "Charge_Amount", "Seconds_of_Use","Frequency_of_use",
                              "Frequency_of_SMS", "Distinct_Called_Numbers",
                              "Age_Group", "Tariff_Plan", "Status", "Age", 
                              "Customer_Value", "Churn")

# Frequent using customers group by age

customer_churn_Call_More <- customer_churn%>%
  group_by(Age_Group)%>%
  summarise(mean_Call = mean (Frequency_of_use), mean_SMS =␣
            mean(Frequency_of_SMS))%>%
  mutate(Call_More = ifelse(mean_Call >= mean_SMS, TRUE, FALSE))
customer_churn_Call_More
quantile(customer_churn$Seconds_of_Use, c(0.33, 0.66, 1))
customer_churn$call_length <- cut(customer_churn$Seconds_of_Use,
                                  breaks = c(-Inf, 1933, 4675, 17090), labels = c("short", "medium", "long"))

# Visualising

ggplot(customer_churn, aes(x = Age_Group, y = Distinct_Called_Numbers, fill = call_length))+
  geom_bar(stat="identity")+
  xlab("Age Group")+
  ylab("Distinct Called Numbers")

# T test and predict

customer_churn %>% count(Age_Group)
t.test(Seconds_of_Use~Tariff_Plan, data = customer_churn)
predict <- glm(Churn ~Call_Failure + Complains+␣
                 Subscription_Length+Charge_Amount + Seconds_of_Use+Frequency_of_use+Frequency_of_SMS+Distinct_Called_Numbers+Tariff_Plan+Status+   data = customer_churn, family = "binomial" )
customer_churn$predict <- predict(predict, type='response')
customer_churn_yes <- customer_churn%>%
  filter(predict > 0.5)%>%
  arrange(desc(predict))
customer_churn <- customer_churn%>%
  mutate(churn_predict = ifelse(predict > 0.5, TRUE, FALSE))
customer_churn

# table comparision with predict

table(customer_churn$churn_predict, customer_churn$Churn)
table(customer_churn$churn_predict, customer_churn$Age_Group)
table(customer_churn$churn_predict, customer_churn$Charge_Amount)

# Summarizing

customer_churn_predict <- customer_churn%>%
  group_by(churn_predict)%>%
  summarize(mean_Call_Failure = mean(Call_Failure),
            mean_Complains = mean(Complains),
            mean_Subscription_Length = mean(Subscription_Length),
            mean_Seconds_of_Use = mean(Seconds_of_Use),
            mean_Frequency_of_use = mean(Frequency_of_use),
            mean_Frequency_of_SMS = mean(Frequency_of_SMS),
            mean_Distinct_Called_Numbers =␣
              mean(Distinct_Called_Numbers),
            mean_Tariff_Plan = mean(Tariff_Plan), mean_Status = mean(Status),
            mean_Age = mean(Age),
            mean_Customer_Value = mean(Customer_Value))
customer_churn_predict