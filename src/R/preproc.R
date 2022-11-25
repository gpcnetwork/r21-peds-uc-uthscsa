# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: preproc.R
# Description: data preprocessing
# Dependency: extract.R

rm(list=ls()); gc()
setwd("C:/repo/R21_PEDS_UC")

# install.packages("pacman")
pacman::p_load(
  tidyverse,
  magrittr,
  dbplyr
)

# baseline bmi
bmi<-readRDS("./data/peds_uc_cov_bmi.rds") %>%
  group_by(PATID,SITE,VITAL_TYPE) %>%
  arrange(abs(DAYS_SINCE_INDEX)) %>% slice(1:1) %>%
  ungroup %>%
  # filter(DAYS_SINCE_INDEX<=30) %>%
  select(-VITAL_UNIT,-MEASURE_DATE,-DAYS_SINCE_INDEX) %>%
  spread(VITAL_TYPE,VITAL_VAL) %>%
  filter(!is.na(HT)&!is.na(WT)) %>%
  mutate(BMI = coalesce(BMI,round(WT/(HT^2)*703)))


# baseline encounter
enc<-readRDS("./data/peds_uc_cov_enc.rds") %>%
  filter(ADMIT_DAYS_SINCE_INDEX<=0 & ADMIT_DAYS_SINCE_INDEX+LOS>=0) %>%
  group_by(PATID,SITE) %>%
  arrange(ADMIT_DAYS_SINCE_INDEX) %>% slice(1:1) %>% ungroup %>%
  mutate(IP_IND = 1) %>%
  select(PATID, SITE, IP_IND, LOS)


# baseline labs
lab<-readRDS("./data/peds_uc_cov_lab.rds") %>%
  filter(!is.na(RESULT_NUM)) %>%
  group_by(PATID,SITE,LAB_NAME) %>%
  arrange(abs(coalesce(SPECIMEN_DAYS_SINCE_INDEX,RESULT_DAYS_SINCE_INDEX,ORDER_DAYS_SINCE_INDEX))) %>% slice(1:1) %>%
  ungroup %>%
  filter(coalesce(SPECIMEN_DAYS_SINCE_INDEX,RESULT_DAYS_SINCE_INDEX,ORDER_DAYS_SINCE_INDEX)<=30) %>%
  select(PATID, SITE, LAB_NAME, RESULT_NUM) %>%
  spread(LAB_NAME, RESULT_NUM)


# baseline diagnosis
dx<-readRDS("./data/peds_uc_cov_dx.rds") %>%
  filter(DAYS_SINCE_INDEX <= 0) %>%
  group_by(PATID, SITE, DX_GRP) %>%
  arrange(desc(DAYS_SINCE_INDEX)) %>%
  slice(1:1) %>% ungroup %>%
  select(PATID, SITE, DX_GRP) %>%
  mutate(ind = 1) %>%
  spread(DX_GRP,ind)


# endpoints
endpt<-readRDS("./data/peds_uc_endpt.rds") %>%
  filter(ENDPT_DAYS_SINCE_INDEX >=0) %>%
  group_by(PATID,SITE,ENDPT) %>% arrange(ENDPT_DAYS_SINCE_INDEX) %>% slice(1:1) %>% ungroup %>%
  select(PATID,SITE,ENDPT,ENDPT_DAYS_SINCE_INDEX) %>%
  spread(ENDPT,ENDPT_DAYS_SINCE_INDEX) %>%
  filter(!is.na(censor))


# combine
aset<-readRDS("./data/peds_uc_tbl1.rds") %>%
  left_join(endpt,by=c("PATID","SITE")) %>%
  mutate(status1=as.numeric(!is.na(colectomy)),time1=coalesce(colectomy,censor),
         status2=as.numeric(!is.na(corticosteroids)),time2=coalesce(corticosteroids,censor),
         status3=as.numeric(!is.na(immunomodulator)),time3=coalesce(immunomodulator,censor),
         status4=as.numeric(!is.na(mesalazine)),time4=coalesce(mesalazine,censor),
         status5=as.numeric(!is.na(biologics)),time4=coalesce(biologics,censor)) %>% 
  left_join(bmi,by=c("PATID","SITE")) %>%
  left_join(enc,by=c("PATID","SITE")) %>%
  left_join(lab,by=c("PATID","SITE")) %>%
  left_join(dx,by=c("PATID","SITE")) %>%
  replace_na(list(IP_IND = 0,
                  abdominal_pain = 0,
                  diarrhea = 0,
                  rectal_bleeding = 0))

saveRDS(aset,file="./data/peds_uc_aset.rds")
