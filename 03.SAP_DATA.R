# Instruction to measure how much time does it take in my machine   <<<<<Time difference of 1.927194 mins>>>>>>
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
library(xlsx)
rm(list.of.packages,new.packages)


# To create the Integrated Margin of generation, we need the data from every cost realted to power generation
# Unfortunately, this information comes directly from SAP, and it's structure is by monthly acumulated information.
# that means that the value from March 2018, is in fact the value of January + February + March.
# For the nature of this project, we need to import the dataset, and process it by monthly differences in order to 
# get the single monthly value

# I've made the extraction from SAP through E4E, which is a plug-in for excel that creates pivot tables with the 
# information based on accounts ( or wbes). <<Add here an example or screen capture to show the process >>

E4E_path <- str_c(getwd(),"/Imports/ORIGIN_COSTS_E4E.xlsx", sep = "", collapse = NULL)
COMB_TECH <- c("BP","BX","CI","EB","GN","HN","LN","NC")
REMOVABLE_CONCEPTS <- c("APLIC_LIQ_COB_CONTABLE",
                        "CO2_COSTE_CARTERA",
                        "CO2_MLDS_PROV",
                        "COB_CONTABLE_INEFIC",
                        "GESTION_DESVIOS_EGP",
                        "INGR_MERCADO",
                        "LOG_VENT_TER_CENIZAS - LOG_MARG_CARBON",
                        "OTROS: Brokerage",
                        "RELIQ_ATR",
                        "RELIQ_CANON_HID",
                        "RELIQ_CANON_NC_CATALUÑA",
                        "RELIQ_CANON_NC_EST",
                        "RELIQ_CO2_COSTE_CARTERA",
                        "RELIQ_COMPRAS_SEIE",
                        "RELIQ_COSTE_COMBUSTIBLE",
                        "RELIQ_IMPUESTO_ELECT",
                        "RELIQ_INGR_MERCADO",
                        "RELIQ_LIQ_COB_CONTABLE",
                        "RELIQ_LIQ_COB_ECONOMICA",
                        "RELIQ_MTM_COB_ECONOMICA",
                        "RELIQ_OTR_COMP_SEIE",
                        "RELIQ_OTROS",
                        "RELIQ_PEAJE_GEN",
                        "RELIQ_SERV_GEST_RES",
                        "RELIQ_VENTAS_SEIE"
                        )



#E4E_Prev <- read.csv(file=E4E_path,header = TRUE, sep = ";", quote = "\"", dec = ".", fill = F, comment.char = "")
Importacion <- read.xlsx(E4E_path , sheetName = "ORIGIN_COSTS_E4E")


# Considerations took into account
# 1. cohertion forced me to change the type of data from VALOR
# 2. Group by + summarize
# 3. ungroup for aplying the lag function
# 4. lag function to achieve the objective of getting monthly infomration
# 5. changing ID_AREA_SISTEMA due to an error during information
# 6. 
#Due to the f***ing cohertion, I have had to swith the type of data from the column VALUE from factor to integer.
#the problem was that the amount of factor was inferior to the one from rows...





#why is as.numeric(levels(f))[f] more efficent than as.numeric(as.character(f))?
#as.numeric(as.character(f)) is effectively as.numeric(levels(f)[f]), so you are performing the conversion to numeric on length(x) values, rather than on nlevels(x) values. The speed difference will be most apparent for long vectors with few levels. If the values are mostly unique, there won't be much difference in speed. However you do the conversion, this operation is unlikely to be the bottleneck in your code, so don't worry too much about it.
#Some timings

#library(microbenchmark)
#microbenchmark(
#  as.numeric(levels(f))[f],
#   as.numeric(levels(f)[f]),
#   as.numeric(as.character(f)),
#   paste0(x),
#   paste(x),
#   times = 1e5
## Unit: microseconds
##                         expr   min    lq      mean median     uq      max neval
##     as.numeric(levels(f))[f] 3.982 5.120  6.088624  5.405  5.974 1981.418 1e+05
##     as.numeric(levels(f)[f]) 5.973 7.111  8.352032  7.396  8.250 4256.380 1e+05
##  as.numeric(as.character(f)) 6.827 8.249  9.628264  8.534  9.671 1983.694 1e+05




# for that reason, I've been forced to convert it twice, first to character to have all the levels, and then 
# to numeric to have the values.(One whole morning)

# CON EL FILTRO DE LA UPR, EL VALOR Y LA TECNOLOGIA, CONSEGUIMOS TENER VALORES UNICAMENTE PARA ESPAÑA Y PORTUGAL

RESULTADO_E4E <- Importacion

RESULTADO_E4E <- RESULTADO_E4E %>% 
  select(-c(ID_CENTRAL,FECHA_EXTRACCION,DE_CONCEPTO_CTRL)) %>% 
  filter(ID_UPR!="") %>%
  filter(VALOR!="") %>% 
  filter(ID_TECNOLOGIA %in% COMB_TECH) %>% 
  filter(!ID_CONCEPTO_CTRL %in% REMOVABLE_CONCEPTS) %>% 
  mutate(ID_AREA_SISTEMA = if_else(ID_AREA_SISTEMA == 'PORTUGAL', 'PORTUGAL','ESPAÑA')) %>% 
  mutate(ANYO = substring(VERSION,1,4)) %>% 
  mutate(VALOR = as.numeric(as.character(VALOR))) %>% 
  select(-c(ID_E4E_NODO_AGRUP,ID_COMBUSTIBLE)) %>% 
  group_by(VERSION,
           ID_GRUPO_EMPRESARIAL,
           ID_UPR,ID_E4E_NODO,
           ID_CONCEPTO_CTRL,
           ID_TECNOLOGIA,
           ID_AREA_SISTEMA,
           ID_UNIDAD,
           ANYO) %>% 
  summarise(VALOR=sum(VALOR)) %>% 
  ungroup() %>% 
  arrange(ANYO,ID_UPR,ID_CONCEPTO_CTRL,VERSION) %>%
  mutate(new_value = if_else(ANYO == lag(ANYO)  &
                             ID_UPR == lag(ID_UPR)  &
                             ID_CONCEPTO_CTRL == lag(ID_CONCEPTO_CTRL) ,
         (VALOR - lag(VALOR)),
         VALOR)) %>% 
  mutate(VALOR = if_else(is.na(new_value),VALOR,new_value)) %>% 
  select(-c(ID_E4E_NODO,ANYO,new_value)) %>% 
  mutate(VALOR = format(VALOR, decimal.mark=",")) %>% 
  arrange(VERSION)

RESULTADO_E4E <- RESULTADO_E4E[c(1,3,5,2,6,4,8,7)]



#REVISAR FORMATO NUMERICO DE SALIDA Y SI ESO , CONCEPTO, AUNQUE NO TE AFECTA A LA 

write.table(RESULTADO_E4E, 
            file = str_c(getwd(),"/Outputs/COSTES_E4E_EUROS.csv", sep = "", collapse = NULL),
            row.names=FALSE, 
            sep = ";")




End_Time <- Sys.time()
Duration_Process <- End_Time - Start_Time
Duration_Process

rm(E4E_path,REMOVABLE_CONCEPTS,COMB_TECH,Importacion,End_Time,Start_Time,Duration_Process)

###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################






