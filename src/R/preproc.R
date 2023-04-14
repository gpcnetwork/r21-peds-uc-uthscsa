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

tbl1<-readRDS("./data/peds_uc_tbl1.rds")

# baseline bmi
bmi_orig<-readRDS("./data/peds_uc_cov_bmi.rds") %>%
  # clean up ht, wt, bmi
  mutate(VITAL_VAL = case_when(VITAL_TYPE=="HT"&grepl("in",VITAL_UNIT)&VITAL_VAL>20~VITAL_VAL/39.37, # inch -> m
                               VITAL_TYPE=="WT"&grepl("lb",VITAL_UNIT)~VITAL_VAL/2.20462,# lb -> kg
                               TRUE ~ VITAL_VAL)) %>%
  group_by(PATID,SITE,VITAL_TYPE) %>%
  # arbitrary thresholds based on inspection
  filter((VITAL_TYPE=="HT"&VITAL_VAL<3)|
         (VITAL_TYPE=="WT"&VITAL_VAL<200)|
         (VITAL_TYPE=="BMI"&VITAL_VAL<60)) %>%
  # most recent valid value
  arrange(abs(DAYS_SINCE_INDEX)) %>% slice(1:1) %>%
  ungroup %>%
  filter(DAYS_SINCE_INDEX<=30) %>%
  dplyr::select(-VITAL_UNIT,-MEASURE_DATE) %>%
  spread(VITAL_TYPE,VITAL_VAL) %>%
  filter(!is.na(HT)&!is.na(WT)) %>%
  mutate(BMI = coalesce(BMI,round(WT/(HT^2)))) %>%
  filter(WT>0&BMI>0&BMI<60) %>%
  inner_join(tbl1 %>% dplyr::select(PATID,AGE_AT_INDEX_DAYS,SEX),
             by="PATID") %>%
  mutate(AGEMOS = round((AGE_AT_INDEX_DAYS+DAYS_SINCE_INDEX)/30,1),
         AGEMOS_FUZZY = floor(AGEMOS/10)*10,
         SEX = case_when(SEX=="M" ~ 1,
                         TRUE ~ 2)) %>%
  dplyr::select(PATID,SITE,SEX,AGEMOS,AGEMOS_FUZZY,HT,WT,BMI)

## https://www.cdc.gov/growthcharts/percentile_data_files.htm
bmi_z<-bmi_orig %>% dplyr::select(PATID,SITE,SEX,AGEMOS,AGEMOS_FUZZY,BMI) %>%
  left_join(read.csv("https://www.cdc.gov/growthcharts/data/zscore/bmiagerev.csv") %>%
              mutate(Sex = as.numeric(Sex),
                     Agemos = as.numeric(Agemos),
                     L = as.numeric(L),
                     M = as.numeric(M),
                     S = as.numeric(S),
                     Agemos_FUZZY = floor(Agemos/10)*10) %>%
              filter(!is.na(Sex)) %>%
              dplyr::select(Sex,Agemos,Agemos_FUZZY,L,M,S),
            by=c("AGEMOS_FUZZY"="Agemos_FUZZY","SEX"="Sex")) %>%
  group_by(PATID) %>% arrange(abs(Agemos - AGEMOS)) %>% slice(1:1) %>% ungroup %>%
  mutate(BMI_Z = case_when(L==0 ~ log(BMI/M)/S,
                           TRUE ~ ((BMI/M)^L-1)/(L*S)))

ht_z<-bmi_orig %>% dplyr::select(PATID,SITE,SEX,AGEMOS,AGEMOS_FUZZY,HT) %>%
  left_join(read.csv("https://www.cdc.gov/growthcharts/data/zscore/statage.csv") %>%
              mutate(Sex = as.numeric(Sex),
                     Agemos = as.numeric(Agemos),
                     L = as.numeric(L),
                     M = as.numeric(M),
                     S = as.numeric(S),
                     Agemos_FUZZY = floor(Agemos/10)*10) %>%
              filter(!is.na(Sex)) %>%
              dplyr::select(Sex,Agemos,Agemos_FUZZY,L,M,S),
            by=c("AGEMOS_FUZZY"="Agemos_FUZZY","SEX"="Sex")) %>%
  group_by(PATID) %>% arrange(abs(Agemos - AGEMOS)) %>% slice(1:1) %>% ungroup %>%
  mutate(HT = HT*100,
         HT_Z = case_when(L==0 ~ log(HT/M)/S,
                           TRUE ~ ((HT/M)^L-1)/(L*S)))

wt_z<-bmi_orig %>% dplyr::select(PATID,SITE,SEX,AGEMOS,AGEMOS_FUZZY,WT) %>%
  left_join(read.csv("https://www.cdc.gov/growthcharts/data/zscore/wtage.csv") %>%
              mutate(Sex = as.numeric(Sex),
                     Agemos = as.numeric(Agemos),
                     L = as.numeric(L),
                     M = as.numeric(M),
                     S = as.numeric(S),
                     Agemos_FUZZY = floor(Agemos/10)*10) %>%
              filter(!is.na(Sex)) %>%
              dplyr::select(Sex,Agemos,Agemos_FUZZY,L,M,S),
            by=c("AGEMOS_FUZZY"="Agemos_FUZZY","SEX"="Sex")) %>%
  group_by(PATID) %>% arrange(abs(Agemos - AGEMOS)) %>% slice(1:1) %>% ungroup %>%
  mutate(WT_Z = case_when(L==0 ~ log(WT/M)/S,
                          TRUE ~ ((WT/M)^L-1)/(L*S)))
bmi<-bmi_orig %>%
  left_join(bmi_z %>% dplyr::select(PATID,BMI_Z),by="PATID") %>%
  left_join(ht_z %>% dplyr::select(PATID,HT_Z),by="PATID") %>%
  left_join(wt_z %>% dplyr::select(PATID,WT_Z),by="PATID") %>%
  dplyr::select(PATID,SITE,HT,WT,BMI,HT_Z,WT_Z,BMI_Z)

bmi_ind<-tbl1 %>% dplyr::select(PATID,SITE) %>%
  left_join(bmi %>%
              dplyr::select(PATID,SITE,HT,WT,BMI) %>%
              gather(var,val,-PATID,-SITE) %>%
              mutate(var = paste0(var,"_ind"),
                     val = as.numeric(!is.na(val))) %>%
              spread(var,val),
            by=c("PATID","SITE")) %>%
  replace(is.na(.), 0)

# baseline encounter
enc<-tbl1 %>% dplyr::select(PATID,SITE) %>%
  left_join(readRDS("./data/peds_uc_cov_enc.rds") %>%
              filter(ADMIT_DAYS_SINCE_INDEX<=0&ADMIT_DAYS_SINCE_INDEX+LOS>=0) %>%
              group_by(PATID,SITE) %>%
              arrange(ADMIT_DAYS_SINCE_INDEX) %>% slice(1:1) %>% ungroup %>%
              mutate(IP_IND = 1) %>%
              dplyr::select(PATID, SITE, IP_IND, LOS),
            by = c("PATID","SITE")) %>%
  replace_na(list(IP_IND = 0))


# baseline labs
lab<-readRDS("./data/peds_uc_cov_lab.rds") %>%
  filter(!is.na(RESULT_NUM)) %>%
  # clean up wbc and differentials
  filter(!LAB_NAME %in% c("leukocyte") |
        (LAB_NAME %in% c("leukocyte") & RESULT_UNIT != '%' & RESULT_NUM <= 1000)) %>% 
  filter(!LAB_NAME %in% c("neutrophil","monocyte","lymphocyte","eosinophil","basophil") |
         (LAB_NAME %in% c("neutrophil","monocyte","lymphocyte","eosinophil","basophil") & RESULT_UNIT == '%' & RESULT_NUM <= 100)) %>% 
  # clean up hemoglobin
  filter(!LAB_NAME %in% c("hemoglobin") |
         (LAB_NAME %in% c("hemoglobin") & RESULT_UNIT %in% c('g/dL','OT'))) %>% 
  # clean up CRP
  mutate(RESULT_NUM = case_when(LAB_NAME %in% c("CRP") & RESULT_UNIT %in% c('mg/L') ~ RESULT_NUM/10,
                                TRUE ~ RESULT_NUM)) %>%
  # clean up platelet count
  filter(!LAB_NAME %in% c("platelet_count") |
        (LAB_NAME %in% c("platelet_count") & RESULT_UNIT %in% c('10*3/uL','OT','10*9/L','g/L'))) %>% 
  # clean up albumin
  filter(!LAB_NAME %in% c("albumin") |
        (LAB_NAME %in% c("albumin") & RESULT_UNIT %in% c('g/dL'))) %>%
  # abstraction
  group_by(PATID,SITE,LAB_NAME) %>%
  arrange(abs(coalesce(SPECIMEN_DAYS_SINCE_INDEX,RESULT_DAYS_SINCE_INDEX,ORDER_DAYS_SINCE_INDEX))) %>% slice(1:1) %>%
  ungroup %>%
  filter(coalesce(SPECIMEN_DAYS_SINCE_INDEX,RESULT_DAYS_SINCE_INDEX,ORDER_DAYS_SINCE_INDEX)<=30) %>%
  dplyr::select(PATID, SITE, LAB_NAME, RESULT_NUM) %>%
  spread(LAB_NAME, RESULT_NUM)

lab_ind<-tbl1 %>% dplyr::select(PATID,SITE) %>%
  left_join(lab %>%
              gather(var,val,-PATID,-SITE) %>%
              mutate(var = paste0(var,"_ind"),
                     val = as.numeric(!is.na(val))) %>%
              spread(var,val),
            by=c("PATID","SITE")) %>%
  replace(is.na(.), 0)


# baseline diagnosis
dx<-tbl1 %>% dplyr::select(PATID,SITE) %>%
  left_join(readRDS("./data/peds_uc_cov_dx.rds") %>%
              filter(DAYS_SINCE_INDEX <= 0) %>%
              group_by(PATID, SITE, DX_GRP) %>%
              arrange(desc(DAYS_SINCE_INDEX)) %>%
              slice(1:1) %>% ungroup %>%
              dplyr::select(PATID, SITE, DX_GRP) %>%
              mutate(ind = 1) %>%
              spread(DX_GRP,ind),
            by=c("PATID","SITE")) %>%
  replace(is.na(.), 0)
  

# endpoints
endpt<-readRDS("./data/peds_uc_endpt.rds") %>%
  filter(ENDPT_DAYS_SINCE_INDEX >=0) %>%
  group_by(PATID,SITE,ENDPT) %>% arrange(ENDPT_DAYS_SINCE_INDEX) %>% slice(1:1) %>% ungroup %>%
  dplyr::select(PATID,SITE,ENDPT,ENDPT_DAYS_SINCE_INDEX) %>%
  spread(ENDPT,ENDPT_DAYS_SINCE_INDEX) %>%
  filter(!is.na(censor))

endpt2<-endpt %>% 
  dplyr::select(PATID,SITE,mesalazine,corticosteroids,immunomodulator, biologics, colectomy) %>%
  mutate(standard = pmin(mesalazine,corticosteroids,na.rm=T)) %>%
  dplyr::select(PATID,SITE, standard,immunomodulator, biologics, colectomy) %>%
  gather(var,val,-PATID,-SITE) %>%
  filter(!is.na(val)) %>%
  group_by(PATID,SITE) %>%
  mutate(ord = paste0("trt",order(val))) %>%
  ungroup %>%
  dplyr::select(PATID,SITE,var,ord) %>%
  spread(ord,var) %>%
  mutate(col_aft_trt1 = case_when(trt2=="colectomy"|trt3=="colectomy"|trt4=="colectomy" ~ 1, TRUE ~ 0),
         bio_aft_trt1 = case_when(trt2=="biologics"|trt3=="biologics"|trt4=="biologics" ~ 1, TRUE ~ 0),
         imm_aft_trt1 = case_when(trt2=="immunomodulator"|trt3=="immunomodulator"|trt4=="immunomodulator" ~ 1, TRUE ~ 0)) %>%
  mutate(trt1=relevel(as.factor(trt1),ref="standard"))

# combine
aset<-readRDS("./data/peds_uc_tbl1.rds") %>%
  mutate(INDEX_YEAR = lubridate::year(INDEX_DATE)) %>%
  mutate(RACE=relevel(as.factor(RACE),ref="white"),
         HISPANIC=relevel(as.factor(HISPANIC),ref="non-hispanic")) %>%
  inner_join(endpt,by=c("PATID","SITE")) %>%
  left_join(endpt2,by=c("PATID","SITE")) %>%
  mutate(status1=as.numeric(!is.na(colectomy)),time1=coalesce(colectomy,censor),
         status2=as.numeric(!is.na(corticosteroids)),time2=coalesce(corticosteroids,censor),
         status3=as.numeric(!is.na(immunomodulator)),time3=coalesce(immunomodulator,censor),
         status4=as.numeric(!is.na(mesalazine)),time4=coalesce(mesalazine,censor),
         status5=as.numeric(!is.na(biologics)),time5=coalesce(biologics,censor)) %>%
  left_join(bmi,by=c("PATID","SITE")) %>%
  left_join(bmi_ind,by=c("PATID","SITE")) %>%
  left_join(enc,by=c("PATID","SITE")) %>%
  left_join(lab,by=c("PATID","SITE")) %>%
  left_join(lab_ind,by=c("PATID","SITE")) %>%
  left_join(dx,by=c("PATID","SITE"))

aset %<>%
  mutate(time_std2bio=case_when(trt1=="standard"&time5*status5 - pmin(time2*status2,time4*status4)>0 ~ time5*status5 - pmin(time2*status2,time4*status4)),
         time_std2imm=case_when(trt1=="standard"&time3*status3 - pmin(time2*status2,time4*status4)>0 ~ time3*status3 - pmin(time2*status2,time4*status4)),
         time_std2col=case_when(trt1=="standard"&time1*status1 - pmin(time2*status2,time4*status4)>0 ~ time1*status1 - pmin(time2*status2,time4*status4)),
         time_bio2imm=case_when(trt1=="biologics"&time3*status3 - time5*status5>0 ~ time3*status3 - time5*status5),
         time_bio2col=case_when(trt1=="biologics"&time1*status1 - time5*status5>0 ~ time1*status1 - time5*status5),
         time_imm2bio=case_when(trt1=="immunomodulator"&time5*status5 - time3*status3>0 ~ time5*status5 - time3*status3),
         time_imm2col=case_when(trt1=="immunomodulator"&time1*status1 - time3*status3>0 ~ time1*status1 - time3*status3)) %>%
  mutate(status6=as.numeric(trt1=="standard"&status1+status3+status5>0),
         status6_col = as.numeric(trt1=="standard"&status1>0),
         status_bio_imm = as.numeric(trt1=="standard"&status3+status5>0),
         time6=case_when(status6==1 ~ pmin(time1,time3,time5) - pmin(time2*status2,time4*status4),
                         TRUE ~ censor))

saveRDS(aset,file="./data/peds_uc_aset.rds")

