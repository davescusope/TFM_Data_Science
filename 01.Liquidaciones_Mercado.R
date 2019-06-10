# Instruction to measure how much time does it take in my machine   <<<<<Time difference of 2.439813 hours>>>>>>
Start_Time <- Sys.time()
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################

list.of.packages <- c("dplyr", "tidyverse", "ggplot2", "tidyr", 'lubridate','viridis','R.utils' )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



library(dplyr)
library(stringr)    #libreria para el tratamiento de strings, acuerdate que la has usado para filtrar por empieza con...
library(ggplot2)
library(tidyverse)
library(tidyr)
library(lubridate)
library(viridis)
library(R.utils)

rm(list.of.packages,new.packages)

data_path <- str_c(getwd(),"/Imports/ORIGIN_MARKET_LIQUIDATIONS.csv", sep = "", collapse = NULL)
#data_path <- "WORK_QUERY_FOR_V_TH_LIQ_OPERAC.csv"
#data_path <- "WORK_QUERY_FOR_V_TH_LIQ_OPERAC_ELECT_.csv"

#data_path <- readline(prompt="Enter name of the file")


#since the file is sensibly larger than the memory of my pc, I proceed to import only 1000 rows to check what's
# the file's structure and confirm if we want the whole thing or just some solumns

 

dftest <- read.csv(file=data_path,header = TRUE, sep = ";", quote = "\"",
                   dec = ".", fill = F, comment.char = "", nrows=1000)

str(dftest)
#view(head(dftest,10))
length(dftest)
colnames(dftest)

#We explore all the options to convert the date column, wich is now a factor, into a date data type
  #Define which is the real format
  #We can now forget about the time part of the date, it does not give us any useful information due to the monthly character of our study
  #Create a new column with the date information and the structure MMYYYY for its posterior group by


dftest %>% 
  distinct(FC_PERIODO)

a <- dftest %>% 
  select(FC_PERIODO) %>% 
  mutate(FC_PERIODO = dmy_hms(FC_PERIODO)) %>% 
  mutate(FC_PERIODO = format(as.Date(FC_PERIODO), "%Y%m"))
str(a)


# Once this is the date format that I am going to use to group by year-month, I can delete the variables created to reach this point
rm(a,dftest)


#From this first approach, we confirm that we do NOT need all those columns, instead of that , let's just import
#the ones that sums the total of every day by summing the hourly columns

colnames <- c("FC_PERIODO","ID_UPR","ID_SOCIEDAD_AGENTE","ID_GRUPO_EMPRESARIAL",
              "ID_TECNOLOGIA","ID_RDLCN","ID_MERCADO_ELECTRICO","ID_GRUPO",
              "ID_CONTRATO_BILATERAL","Q_V_TOT","Q_C_TOT","I_V_TOT","I_C_TOT",
              "FC_EXTRACCION","FC_CARGA")



# Make a second importation but this time bringing ONLY the useful columns, now I can bring a greater number of rows to start the real introspection-job

liquidaciones_mercado<-read.csv(data_path, header = TRUE, sep = ";", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", nrows=50000)[,colnames]

# Select distinct technologies to know precise spelling of those which are interesting
liquidaciones_mercado %>% 
  distinct(ID_TECNOLOGIA)

COMB_TECH <- c("BP","BX","CI","EB","GN","HN","LN","NC")

# Select distinct markets to know precise spelling of those which are interesting
liquidaciones_mercado %>% 
  distinct(ID_MERCADO_ELECTRICO)
#MMDD <- c('M. Diario','M. Intradiarios','Bilateral','RESTRICCIONESN')
#MMPP <- c('Banda','DESVIOS_MEDIDA','G. Potencia LP','G. Potencia MP','G. Potencia Pagos','OPCIONALIDAD','Redespachos','RESERVA_POTENCIA','RESTRICCIONESP','SECUNDARIA','Terciaria')


# Select distinct companies to restrict the dataset to those which are interesting for this TFM
liquidaciones_mercado %>% 
  distinct(ID_GRUPO_EMPRESARIAL)

COMPANIES <-c("EMPRESA1","EMPRESA3","EMPRESA2")


rm(liquidaciones_mercado)

###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
#I now create de functions Processing where are included all the transformations for each chunk 
#Having the values daily summarized to use the total columns, we can now generate two different dataframes, one for the amount in Euros and the other one
#for the vales of energy production in MWH. For that:
# Data cleaning for retaining just the desired information 
# Total-columns segregation according to the amount or the energy volume
# Creation of a summing columns which includes sells and buys
# Delete of the days with a net value equals zero for not having real presence or interaction with the markets
# Creation of the ID_UNIDAD column for its posterior discrimination
# Summarization of every value just in case there were two or more transaction on a single day (not very common)
# Restriction by technologies to those which give trustfull values in classical generation
# Restriction by companies to those three which this study is going to be focused. Later I will differentiate by any single one to train the models

Amount <- function(x) {
  return(x %>% 
      select(-c(Q_V_TOT,Q_C_TOT,FC_EXTRACCION,FC_CARGA)) %>% 
      mutate(I_N_TOT = I_V_TOT + I_C_TOT) %>% 
      select(-c(I_V_TOT , I_C_TOT)) %>%
      filter(I_N_TOT!=0) %>% 
      filter( ID_TECNOLOGIA %in% COMB_TECH) %>% 
      filter( ID_GRUPO_EMPRESARIAL %in% COMPANIES ) %>% 
      mutate(FC_PERIODO = dmy_hms(FC_PERIODO)) %>% 
      mutate(FC_PERIODO = format(as.Date(FC_PERIODO), "%Y%m")) %>% 
      group_by(FC_PERIODO, ID_UPR, ID_SOCIEDAD_AGENTE, ID_GRUPO_EMPRESARIAL, ID_TECNOLOGIA, ID_RDLCN, ID_MERCADO_ELECTRICO,ID_GRUPO,ID_CONTRATO_BILATERAL,ID_TECNOLOGIA) %>% 
      summarise(I_N_TOT=sum(I_N_TOT)) %>% 
      mutate(ID_UNIDAD = "EUROS") %>%
      as.data.frame()
  )
}


Power <- function(x) {
  return(x %>% 
      select(-c(I_V_TOT,I_C_TOT,FC_EXTRACCION,FC_CARGA)) %>%
      mutate(Q_N_TOT = Q_V_TOT + Q_C_TOT) %>% 
      select(-c(Q_V_TOT,Q_C_TOT )) %>% 
      filter(Q_N_TOT!=0) %>% 
      filter( ID_TECNOLOGIA %in% COMB_TECH) %>% 
      filter( ID_GRUPO_EMPRESARIAL %in% COMPANIES ) %>% 
      mutate(FC_PERIODO = dmy_hms(FC_PERIODO)) %>% 
      mutate(FC_PERIODO = format(as.Date(FC_PERIODO), "%Y%m")) %>% 
      group_by(FC_PERIODO, ID_UPR, ID_SOCIEDAD_AGENTE, ID_GRUPO_EMPRESARIAL, ID_TECNOLOGIA, ID_RDLCN, ID_MERCADO_ELECTRICO,ID_GRUPO,ID_CONTRATO_BILATERAL,ID_TECNOLOGIA) %>% 
      summarise(Q_N_TOT=sum(Q_N_TOT)) %>% 
      mutate(ID_UNIDAD = "MWH") %>% 
      as.data.frame()
  )
}




###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
# Take into account that our original files are so much larger than the memory of my pc, so I should import by parts and repeat untill the end of the file

File_size<-countLines(data_path)
Last_row = as.numeric(File_size)
rm(File_size)
Chunk_size = 50000
Start_chunk = 1
End_chunk = Chunk_size

#We create the list to rename the header later
Header <- read.csv(file=data_path,header = T, sep = ";", quote = "\"",
                   dec = ".", fill = TRUE, comment.char = "", nrows = 1)


Header_names <- colnames(Header)
Amount_Total <- data.frame()
Power_Total <- data.frame()

rm(Header)


#We create now the loop for importing a huge file , process it and save just the result desired
repeat {
  
  if (End_chunk>Last_row)
    End_chunk = Last_row
  
  print(paste('Processing rows:', Start_chunk, 'to', End_chunk))
  
  Chunk <- try(read.csv(file=data_path,header = F, sep = ";", quote = "\"",
                          dec = ".", fill = TRUE, comment.char = "", skip = Start_chunk, nrows=End_chunk))
  
  if (inherits(Chunk, "try-error")) break
  
  #Rename the columnames with the Header_names from the begining and delete all the variables used  
  names(Chunk) <- Header_names 
  
  liq_merc_amount  <- Amount(Chunk)
  liq_merc_power <- Power(Chunk)
 
  
  Amount_Total <- rbind(Amount_Total, liq_merc_amount)
  Power_Total <- rbind(Power_Total, liq_merc_power)
  
  Start_chunk = End_chunk
  End_chunk = End_chunk+Chunk_size
  
}





##############

# A last data treatment has become necessary since there could be duplicated lines just on the border of every section for the chunk-rbind process
MARKETS <- Amount_Total %>% distinct(ID_MERCADO_ELECTRICO)
MARKETS <- MARKETS[MARKETS != "Energia SEIE" ]
MARKETS <- MARKETS[MARKETS != "G. Potencia SEIE" ]



Amount_Total <- Amount_Total %>%
  select(-c(ID_SOCIEDAD_AGENTE,ID_RDLCN,ID_GRUPO,ID_CONTRATO_BILATERAL)) %>%
  filter(ID_MERCADO_ELECTRICO %in% MARKETS) %>% 
  rename(VERSION = FC_PERIODO ) %>%
  rename(ID_CONCEPTO_CTRL = ID_MERCADO_ELECTRICO) %>%
  group_by(VERSION,ID_UPR,ID_GRUPO_EMPRESARIAL,ID_TECNOLOGIA,ID_CONCEPTO_CTRL,ID_UNIDAD) %>% 
  summarise(I_N_TOT=sum(I_N_TOT)) %>% 
  rename(VALOR = I_N_TOT) %>%
  mutate(VALOR = format(VALOR, decimal.mark=",")) %>% 
  as.data.frame() %>% 
  arrange(VERSION)
  

Power_Total <- Power_Total %>%
  select(-c(ID_SOCIEDAD_AGENTE,ID_RDLCN,ID_GRUPO,ID_CONTRATO_BILATERAL)) %>%
  filter(ID_MERCADO_ELECTRICO %in% MARKETS) %>% 
  rename(VERSION = FC_PERIODO ) %>%
  rename(ID_CONCEPTO_CTRL = ID_MERCADO_ELECTRICO) %>%
  group_by(VERSION,ID_UPR,ID_GRUPO_EMPRESARIAL,ID_TECNOLOGIA,ID_CONCEPTO_CTRL,ID_UNIDAD) %>% 
  summarise(Q_N_TOT=sum(Q_N_TOT)) %>% 
  rename(VALOR = Q_N_TOT) %>%
  mutate(VALOR = format(VALOR, decimal.mark=",")) %>% 
  as.data.frame() %>% 
  arrange(VERSION)




# I save them on csv for future calculations without launching it again

write.table(Amount_Total, 
            file = str_c(getwd(),"/Outputs/LIQUIDACIONES_EUROS.csv", sep = "", collapse = NULL),
            row.names=FALSE, 
            sep = ";")


write.table(Power_Total, 
            file = str_c(getwd(),"/Outputs/LIQUIDACIONES_MWH.csv", sep = "", collapse = NULL),
            row.names=FALSE, 
            sep = ";")





End_Time <- Sys.time()
Duration_Process <- End_Time - Start_Time
Duration_Process


rm(Chunk)  
rm(Header_names,End_chunk,Last_row,Start_chunk,Chunk_size) 
rm(liq_merc_amount,liq_merc_power)
rm(Amount,Power)
rm(colnames,COMB_TECH,COMPANIES,MARKETS,data_path)
rm(End_Time,Start_Time,Duration_Process)



###############################################################################################################
###############################################################################################################
#########                                                                                         #############
#########                       VISUALIZATION THE DATA WITH GGPLOT                                #############
#########                                                                                         #############
###############################################################################################################
###############################################################################################################


#Let's talke a fast look into the data so we can try to use GGplot with all its benefits and shortcomings
#I prepare the df with basic changes by choosing only one year to inspect the data

Amount_Inspection <- Amount_Total %>% 
  filter(VALOR != '') %>%
  drop_na() %>%
  mutate(VERSION= paste(VERSION, "01", sep="")) %>% 
  mutate(VERSION = ymd(VERSION)) %>% 
  filter(VERSION > "2017-12-31" & VERSION <"2019-01-01") %>% 
  mutate(VALOR= as.numeric(gsub(",", ".", VALOR))) 

#---------------------------------------------------------------------------------------#
#----------------------------- Technology margin graph ---------------------------------#
#---------------------------------------------------------------------------------------#



Technolgy_inspection <- Amount_Inspection %>% 
  select(VERSION, ID_UPR, ID_TECNOLOGIA, VALOR) %>%
  drop_na() %>% 
  group_by(ID_TECNOLOGIA) %>% 
  summarise(VALOR=sum(VALOR)) %>%
  arrange(VALOR) %>% 
  ggplot(aes(x=reorder(ID_TECNOLOGIA, -VALOR), y=VALOR ,color=ID_TECNOLOGIA, fill=ID_TECNOLOGIA))+ 
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = 0)+
  theme(legend.position = "top")+
  ggtitle("Technolgy_inspection")

Technolgy_inspection



#---------------------------------------------------------------------------------------#
#------------------------------- Concept Margin Graph ----------------------------------#
#---------------------------------------------------------------------------------------#


Concept_Inspection <- Amount_Inspection %>% 
  select(VERSION, ID_UPR, ID_TECNOLOGIA,ID_CONCEPTO_CTRL, VALOR) %>%
  filter(ID_CONCEPTO_CTRL != '') %>% 
  drop_na() %>% 
  group_by(VERSION, ID_CONCEPTO_CTRL) %>% 
  summarise(VALOR=sum(VALOR)) %>%
  filter(VALOR < 1000000000 & VALOR > -1000000000) %>%
  ggplot(aes(x = VERSION, y = VALOR)) +
  geom_line(aes(linetype=ID_CONCEPTO_CTRL, color=ID_CONCEPTO_CTRL))+
  geom_point() + 
  geom_point(aes(color=ID_CONCEPTO_CTRL))+
  geom_hline(yintercept = 0)+
  theme(legend.position = "top") +
  ggtitle("Concept_Margin")


Concept_Inspection





    