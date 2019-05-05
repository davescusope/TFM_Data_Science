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

# To create the Integrated Margin of generation, we need the data from every cost realted to power generation
# Unfortunately, this information comes directly from SAP, and it's structure is by monthly acumulated information.
# that means that the value from March 2018, is in fact the value of January + February + March.
# For the nature of this project, we need to import the dataset, and process it by monthly differences in order to 
# get the single monthly value

# I've made the extraction from SAP through E4E, which is a plug-in for excel that creates pivot tables with the 
# information based on accounts ( or wbes). <<Add here an example or screen capture to show the process >>

E4E_path <- "WORK_COSTS_SAP.xlsx"
COMB_TECH <- c("BP","BX","CI","EB","GN","HN","LN","NC")


#E4E_Prev <- read.csv(file=E4E_path,header = TRUE, sep = ";", quote = "\"", dec = ".", fill = F, comment.char = "")

Importacion <- read.xlsx(E4E_path , sheetName = "WORK_COSTS_SAP")



E4E_Prev <- Importacion

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



E4E_Prev <- Importacion

E4E_Prev <- E4E_Prev %>% 
  select(-c(ID_CENTRAL,FECHA_EXTRACCION,DE_CONCEPTO_CTRL)) %>% 
  filter(ID_UPR!="") %>%
  filter(VALOR!="") %>% 
  filter( ID_TECNOLOGIA %in% COMB_TECH) %>% 
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
         VALOR))



E4E_Prev <- E4E_Prev[c(12,1,2,3,4,5,6,7,8,9,10,11)]


###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################


#LAPPLY


#hAY QUE DIVIDIR LOS DATOS EN DOS DATASETS, UNO POR AÑO Y LUEGO PROCESARLOS DE ACUERDO A ESTO
#AÑADIR VERSIONES 201700 Y 201800 PARA QUE SIEMMPRE TENGAMOS LA VERSIÓN FIRST COGIENDO LOS DIFERENTES
#SINO, BUSCAR FUNCION QUE HAGA LOOP DE ACUERDO A TODOS LOS CAMPOS SALVO LA VERSIÓN Y LOS RESTE, DE ESTA MANERA
#NOS AHORRAMOS EL PONER LAS VERSIONES CON 00



#data WORK.EUROS_COSTES_EG3;
#SET WORK.EUROS_COSTES_EG2;                 
#BY ID_UPR ID_CONCEPTO ID_COMBUSTIBLE VERSION;         
#IF FIRST.VERSION THEN
#NUEVO_VALOR=VALOR-LAG(VALOR);








#FINALMETNE SE VUELVEN A JUNTAR


###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################






