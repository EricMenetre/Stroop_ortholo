##################################################################
##                    Results Stroop Ortholo                    ##
##################################################################
setwd("C:/Users/EricM/ownCloud/UNIGE/DOCTORAT/THESE/ETUDE 5_Stroop longueur ortho/dev/results/Stroop_ortholo")
source("tools_function_stroop_ortholo.R")

# Importation of the data

setwd("C:/Users/EricM/ownCloud/UNIGE/DOCTORAT/THESE/ETUDE 5_Stroop longueur ortho/dev/results/Stroop_ortholo/All subjects data")
list_files <- list.files(getwd())
list_files <- list_files[2:length(list_files)]

data_import(list_files = list_files)

# Data cleaning

data_cleaning(data = data_ortholo)

# Graphs and descriptive statistics

data_ortholo%>%
  mutate(accuracy_large = case_when(error_type == "W" | error_type == "Wrcor"| error_type == "NR" ~ 0,
                                    TRUE ~ 1))%>%
  group_by(condition)%>%
  summarise(mean_RT = mean(RT_clean, na.rm = T),
            SD_RT = sd(RT_clean, na.rm = T),
            accuracy_res = mean(accuracy, na.rm = T),
            SD_acc_res = sd(accuracy, na.rm = T),
            accuracy_large_res = mean(accuracy_large, na.rm = T),
            SD_acc_large =  sd(accuracy_large_res, na.rm = T))

data_ortholo%>%
  ggplot(aes(x = condition, y = RT_clean)) + geom_boxplot()

cong <- data_ortholo$RT_clean[data_ortholo$condition == "cong"]
incong <- data_ortholo$RT_clean[data_ortholo$condition == "incong"]

m0 <- lmer(RT_clean ~ condition + (1|subject) + (1|repcor), data = data_ortholo, REML = T, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                 optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(m0) # conditionincong   16.372      5.455 2174.256   3.001  0.00272

# Power analysis

mtest <- makeLmer(RT_clean ~ condition + (1|subject) + (1|repcor), data = data_ortholo, fixef=fixed, VarCorr=rand, sigma=res)
                                                                                                

powerSim(m0, nsim = 20)

