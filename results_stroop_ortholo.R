##################################################################
##                    Results Stroop Ortholo                    ##
##################################################################

# Importation of the data

setwd("C:/Users/EricM/ownCloud/UNIGE/DOCTORAT/THESE/ETUDE 5_Stroop longueur ortho/dev/results/Stroop_ortholo/All subjects data")
list_files <- list.files(getwd())
list_files <- list_files[2:length(list_files)]

data_import(list_files = list_files)

# Graphs and descriptive statistics

