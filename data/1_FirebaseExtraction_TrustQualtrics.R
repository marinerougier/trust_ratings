# To reset the workspace
rm(list=ls())

# package import ----------------------------------------------------------
library(tidyverse)
#library(jsonlite)
library(RJSONIO)
library(glue)
library(data.table)
library(magrittr)
library(gdata)
library(reshape)

#-----------------------------------------------------------
#                     Trust ratings
#-----------------------------------------------------------

 # Ensure that relative paths start from the same directory as this script
 rstudioapi::getActiveDocumentContext()$path %>% dirname %>% setwd
 
 json <- fromJSON("data/marineexpe-export (1).json")[["Trust_Ratings"]]
 DF_trust <- lapply(names(json), function(id){ fread(json[[id]]) }) %>% rbindlist

 DF_trust <- rename.vars (DF_trust, "id", "jspsych_id")
 
 # We create a face condition variable
 DF_trust$faceCond<-DF_trust$face
 DF_trust[, faceCond := str_match(faceCond, ".CFD-(.*?)-") %>% .[,2]]
 
 # We create a new black vs. white variable
 DF_trust[faceCond=="BM", faceCond := "B"]
 DF_trust[faceCond=="BF", faceCond := "B"]
 DF_trust[faceCond=="WM", faceCond := "W"]
 DF_trust[faceCond=="WF", faceCond := "W"]
 
 # We keep only black and white faces
 DF_trust <- subset (DF_trust, faceCond=="B" | faceCond=="W")
 
 # Data wrangling --------------------------------------------------------------
 DF_short_trust <- cast(DF_trust,
                  jspsych_id + prolificID ~ faceCond,
                  value = "rating", # to be rerun with "RT" to compute the means and se
                  mean,
                  fill = NA)
 
 DF_short_trust <- 
   within(DF_short_trust,
          {
            Trust_BW   <- W - B
          })
 
 save(DF_short_trust, file = "data/data_Trust.RData")

 
 
 #-----------------------------------------------------------
 #                     Qualtrics
 #-----------------------------------------------------------
 
 # Qualtrics (demo info) --> we use the file registered as csv sep ; 
 # here we already removed raw 2 & 3
 Qual <- fread("data/Qualtrics.csv")[, c(18:34)] 
 
 Qual <- rename.vars (Qual, "id", "jspsych_id")

 Qual <- within(Qual,{
   # EMS scale
   TrustB<-rowMeans(cbind(EMS_1, EMS_2, EMS_3, EMS_4, EMS_5),na.rm=TRUE)
   
   # Termo diff between black and white
   Termo_BW<- Termometer_2 - Termometer_1
 }) 
 
 save(Qual, file = "data/data_Qual.RData")

 
 
 
 