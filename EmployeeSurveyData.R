install.packages("lessR")
library(lessR)



EmpSurvData <- read_csv("EmployeeSurveyData.csv")

# the numbers of employees under different units and supervisors

EmpSurvData_unit_supervisor <- EmpSurvData %>%
  group_by(Unit, Supervisor) %>%
  summarize(UnitCount = n_distinct(EmployeeID)) %>%
  ungroup()

# Multiple descriptive statistics by group

JobSat_DescStat <- EmpSurvData %>%
  group_by(Unit) %>%
  summarize(
    Mean_JobSat1 = mean(JobSat1, na.rm=TRUE),
    SD_JobSat1 = sd(JobSat1, na.rm=TRUE),
    Mdn_JobSat1 = median(JobSat1, na.rm=TRUE),
    Var_JobSat1 = var(JobSat1, na.rm=TRUE),
    Min_JobSat1 = min(JobSat1, na.rm=TRUE),
    Max_JobSat1 = max(JobSat1, na.rm=TRUE),
    IQR_JobSat1 = IQR(JobSat1, na.rm=TRUE),
    Mean_JobSat2 = mean(JobSat2, na.rm=TRUE),
    SD_JobSat2 = sd(JobSat2, na.rm=TRUE),
    Mdn_JobSat2 = median(JobSat2, na.rm=TRUE),
    Var_JobSat2 = var(JobSat2, na.rm=TRUE),
    Min_JobSat2 = min(JobSat2, na.rm=TRUE),
    Max_JobSat2 = max(JobSat2, na.rm=TRUE),
    IQR_JobSat2 = IQR(JobSat2, na.rm=TRUE),
    Mean_JobSat3 = mean(JobSat3, na.rm=TRUE),
    SD_JobSat3 = sd(JobSat3, na.rm=TRUE),
    Mdn_JobSat3 = median(JobSat3, na.rm=TRUE),
    Var_JobSat3 = var(JobSat3, na.rm=TRUE),
    Min_JobSat3 = min(JobSat3, na.rm=TRUE),
    Max_JobSat3 = max(JobSat3, na.rm=TRUE),
    IQR_JobSat3 = IQR(JobSat3, na.rm=TRUE),
  )



# Visualizing Job Sat by group

ggplot(EmpSurvData, aes(JobSat1))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~Unit)+
  labs(title = "Job Satisfactions 1 group by Units")
ggplot(EmpSurvData, aes(JobSat2))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~Unit)+
  labs(title = "Job Satisfactions 2 group by Units")
ggplot(EmpSurvData, aes(JobSat3))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~Unit)+
  labs(title = "Job Satisfactions 3 group by Units")

# Cronbach's alpha practice

alpha(EmpSurvData[,c("JobSat1","JobSat2","JobSat3")])
alpha(EmpSurvData[,c("TurnInt1","TurnInt2","TurnInt3")])
alpha(EmpSurvData[,c("Engage1","Engage2","Engage3", "Engage4")])
alpha(EmpSurvData[,c("ExpIncivil1","ExpIncivil2","ExpIncivil3", "ExpIncivil4", "ExpIncivil5")])


# Create a Composite Variable

EmpSurvData <- EmpSurvData %>% 
  mutate(Engage_Overall = rowMeans(EmpSurvData[,c("Engage1","Engage2","Engage3","Engage4")],
         na.rm=TRUE),
         JobSat_Overall = rowMeans(EmpSurvData[,c("JobSat1","JobSat2")],
                                   na.rm=TRUE),
         TurnInt_Overall = rowMeans(EmpSurvData[,c("TurnInt1","TurnInt2","TurnInt3")],
                                   na.rm=TRUE),
         ExpIncivil_Overall = rowMeans(EmpSurvData[,c("ExpIncivil1","ExpIncivil2","ExpIncivil3", "ExpIncivil4", "ExpIncivil5")],
                                   na.rm=TRUE),
         )

EmpSurvData %>% group_by(Unit)%>%
ggplot(aes(Unit, JobSat_Overall))+
  geom_boxplot()+
  labs(title = "Adjusted Job Satisfactions Overall group by Units")

EmpSurvData %>% group_by(Unit)%>%
  ggplot(aes(Unit, TurnInt_Overall))+
  geom_boxplot()+
  labs(title = "Adjusted Turn Interest Overall group by Units")

EmpSurvData %>% group_by(Unit)%>%
  ggplot(aes(Unit, Engage_Overall))+
  geom_boxplot()+
  labs(title = "Adjusted Engagement Overall group by Units")

EmpSurvData %>% group_by(Unit)%>%
  ggplot(aes(Unit, ExpIncivil_Overall))+
  geom_boxplot()+
  labs(title = "Adjusted Experience in Civil Overall group by Units")
