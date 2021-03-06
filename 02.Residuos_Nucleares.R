# Instruction to measure how much time does it take in my machine   <<<<<Time difference of 6.325827 secs>>>>>>
Start_Time <- Sys.time()
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################

list.of.packages <- c("dplyr", "tidyverse", "ggplot2", "tidyr", 'lubridate','viridis','R.utils','xlsx' )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



library(dplyr)
library(stringr)    
library(ggplot2)
library(tidyverse)
library(tidyr)
library(lubridate)
library(viridis)
library(R.utils)


# The information related to the nuclear wastes produced during the nuclear fision come from some xlsx files that
# are generated by a deeply consolidated model. 
# For that reason, we should import them and process them to keep only the usefull part.

require(xlsx)
rm(list.of.packages,new.packages)

Import_Nuclear_Wastes <- rbind(
            read.xlsx(str_c(getwd(),"/Imports/ORIGIN_NC_WASTES_201712.xlsx", sep = "", collapse = NULL), sheetName = "RESULT"),
            read.xlsx(str_c(getwd(),"/Imports/ORIGIN_NC_WASTES_201812.xlsx", sep = "", collapse = NULL), sheetName = "RESULT")
            )


Nuclear_Wastes <- Import_Nuclear_Wastes %>% 
  select(-c(ID_TIPO_PREVISION,ID_PERIODO_CONTABLE,ID_COMBUSTIBLE,FC_EXTRACCION)) %>% 
  filter(VALOR!=0) %>%
  filter(ID_UNIDAD == "EUROS") %>%
  rename(ID_CONCEPTO = ID_CUENTA) %>% 
  rename(ID_UPR = ID_NODO) %>%
  rename(ID_CONCEPTO_CTRL = ID_CONCEPTO) %>% 
  rename(VERSION = FC_PERIODO) %>% 
  mutate(VERSION = as.Date(VERSION, format = "%d/%m/%Y")) %>% 
  mutate(VERSION = format(as.Date(VERSION), "%Y%m")) %>% 
  mutate(ID_GRUPO_EMPRESARIAL ="EMPRESA1") %>%
  mutate(ID_AREA_SISTEMA = 'ESPAÑA') %>% 
  mutate(ID_TECNOLOGIA ="NC") %>% 
  mutate(VALOR = VALOR * -1) %>% 
  mutate(VALOR = format(VALOR, decimal.mark=",")) %>% 
  arrange(VERSION)
Nuclear_Wastes <-  Nuclear_Wastes[c(1,2,8,6,7,3,5,4)]


write.table(format(Nuclear_Wastes, drop0trailing=FALSE), 
            file = str_c(getwd(),"/Outputs/NUCLEAR_WASTES_EUROS.csv", sep = "", collapse = NULL),
            row.names=FALSE, 
            sep = ";")




End_Time <- Sys.time()
Duration_Process <- End_Time - Start_Time
Duration_Process

rm(Import_Nuclear_Wastes,End_Time,Start_Time,Duration_Process)

###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################


