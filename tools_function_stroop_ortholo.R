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
  data_ortholo <<- data_ortholo
}
