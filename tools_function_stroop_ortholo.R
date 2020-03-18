#################################################################
##                Functions to perform analyses                ##
#################################################################

# Libraries
library(readxl)
library(tidyverse)
library(stringr)

# Data importation

data_import <- function(list_files){
  # Creation of an empty list
  data_list <- list()
  # Extraction of the dataframe containing the general information
  gen_info <- read_excel(list_files[1])
  # Importation of the data
  for(i in 2:length(list_files)){
    data_list[[i]] <- read_excel(list_files[i])
  }
  # Building a data frame
  data_ortholo <- do.call(rbind, data_list)
  # adding the general information to the data
  data_ortholo <- left_join(data_ortholo, gen_info, by = "subject")
  # Selection of the necessary columns
  data_ortholo <- data_ortholo%>%
    select(-soundrep, -stim_CV, -hour_session)
  # Extraction of the conditions from the stim names
  data_ortholo$condition <- grepl("in", data_ortholo$stimulus)
  data_ortholo <- data_ortholo%>%
    mutate(condition = ifelse(condition == T, "incong", "cong"))
  # changing the data format
  data_ortholo$Rtc <- as.numeric(data_ortholo$Rtc)
  data_ortholo$cabin <- factor(data_ortholo$cabin)

  
  data_ortholo <<- data_ortholo
}

# Data cleaning

data <- data_ortholo

data_cleaning <- function(data){
clean.sd <- function(df.var.val, df.var.group, n.sd, data, fill) {
  
  plot.before.cleaning <<- qqnorm(df.var.val, main = "Distribution before cleaning") ; qqline(df.var.val)
  hist.before.cleaning <<- hist(df.var.val, main = "Distribution of the RT before cleaning")
  
  # Creation of a dataframe with inferior and upper limits 
  moy.df.test <- data.frame(aggregate(df.var.val, list(df.var.group), mean, na.rm = TRUE))
  sd.df.test <- data.frame(aggregate(df.var.val, list(df.var.group), sd, na.rm = TRUE))
  lim.df.test <- moy.df.test
  lim.df.test$sd <- sd.df.test$x
  lim.df.test$lim.inf <- lim.df.test$x - (n.sd*lim.df.test$sd)
  lim.df.test$lim.sup <- lim.df.test$x + (n.sd*lim.df.test$sd)
  print("Means, standard deviations, inferior and superior limits of the data:")
  print(lim.df.test)
  
  # Data cleaning
  
  for (i in 1:nrow(lim.df.test)) {
    for (j in 1:nrow(data)) {
      if (is.na(df.var.val[j])) {
        next
      } else if (lim.df.test$Group.1[i] == df.var.group[j]) {
        if (df.var.val[j] <= lim.df.test$lim.inf[i]) {
          df.var.val[j] <- fill
        } else if (df.var.val[j] >= lim.df.test$lim.sup[i]) {
          df.var.val[j] <- fill
        }
      }
    }
  }
  
  clean.val <<- df.var.val
  
  # Verification
  # Definition of the number of values above or under the limit for the first subject
  
  sum.lim.sup <<- sum(df.var.val[df.var.val == lim.df.test$Group.1[1]] >= lim.df.test$lim.sup[1], na.rm = TRUE) + sum(df.var.val[df.var.val == lim.df.test$Group.1[2]] >= lim.df.test$lim.sup[2], na.rm = TRUE)
  sum.lim.inf <<- sum(df.var.val[df.var.val == lim.df.test$Group.1[1]] <= lim.df.test$lim.inf[1], na.rm = TRUE) + sum(df.var.val[df.var.val == lim.df.test$Group.1[2]] <= lim.df.test$lim.inf[2], na.rm = TRUE)
  sum.lim.tot <<- sum(sum.lim.sup, sum.lim.inf)
  
  if (sum.lim.tot == 0) {
    print("The job is done and work was verified for the first 2 groups")
  } else if (sum.lim.tot != 0) {
    print("WARNING: BE CAREFUL, SOME EXTREME VALUES REMAIN AT LEAST IN THE FIRST 2 GROUPS")
  }
  
  plot.after.cleaning <<- qqnorm(df.var.val, main = "Distribution after cleaning") ; qqline(df.var.val)
  hist.after.cleaning <<- hist(clean.val, main = "Distribution of the RT after cleaning")
  plot.before.cleaning
  hist.before.cleaning
  plot.after.cleaning
  hist.after.cleaning
  
  # Normality tests of Kolmogorov-Smirnov anf Shapiro Wilks 
  
  ks.before <- ks.test(df.var.val, "pnorm", mean = mean(df.var.val, na.rm = T), sd = sd(df.var.val, na.rm = T))
  ks.after <- ks.test(clean.val, "pnorm", mean = mean(df.var.val, na.rm = T), sd = sd(df.var.val, na.rm = T))
  ks.after$statistic
  # results of the normality tests
  
  print(paste("The Kolmogorov-Smirnov normality test D statistic before cleaning is", round(ks.before$statistic,2), "while the corresponding p-val equals", round(ks.before$p.value,2),". After cleaning, the D statistic is", round(ks.after$statistic,2), "and the corresponding p-val equals", round(ks.after$p.value,2)))
  
}

data$group <- "group1"

clean.sd(df.var.val = data$Rtc, df.var.group = data$group,n.sd = 2, data = data, fill = NA)
data$RT_clean <- clean.val
data$group <- NULL

rm(hist.after.cleaning, hist.before.cleaning, plot.after.cleaning, plot.before.cleaning, clean.val, sum.lim.inf, sum.lim.sup, sum.lim.tot)
data_ortholo <<- data
}

