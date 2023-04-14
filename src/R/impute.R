# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: impute.R
# Description: perform imputation
# Dependency: preproc.R

rm(list=ls()); gc()
setwd("C:/repo/R21_PEDS_UC")

# install.packages("pacman")
pacman::p_load(
   tidyverse
  ,magrittr
  ,mice
  # ,ImputeRobust
  # ,gamlss
)

var_lst<-c(
   "AGE_AT_INDEX"
  ,"SEX"
  ,"RACE"
  ,"HISPANIC"
  ,"IP_IND"
  ,"HT_Z"
  ,"WT_Z"
  ,"BMI_Z"
  ,"hemoglobin"
  ,"platelet_count"
  ,"leukocyte"
  ,"ESR"
  ,"CRP"
  ,"albumin"
  ,"basophil"
  ,"eosinophil"
  ,"neutrophil"
  ,"monocyte"
  ,"lymphocyte"
)

var_imp<-c(
    "HT_Z"
   ,"WT_Z"
   ,"BMI_Z"
   ,"neutrophil"
   ,"hemoglobin"
   ,"platelet_count"
   ,"leukocyte"
   ,"ESR"
   ,"CRP"
   ,"eosinophil"
   ,"monocyte"
   ,"lymphocyte"
   ,"albumin"
   ,"basophil"
)

aset<-readRDS("C:/repo/R21_PEDS_UC/data/peds_uc_aset.rds") %>%
  filter(INDEX_YEAR >= 2010 & INDEX_YEAR <= 2020) %>%
  filter(AGE_AT_INDEX <= 17 & AGE_AT_INDEX>=4) %>%
  mutate(AGE_AT_INDEX_12Y = as.numeric(AGE_AT_INDEX>=12)) %>%
  filter(trt1 %in% c("standard","biologics","immunomodulator")) %>% 
  filter(time6>=0) %>%
  filter(trt1 %in% c("standard")) %>% 
  dplyr::select(all_of(c(var_lst,"PATID","status6","time6"))) %>%
  replace_na(list(albumin = 4,
                  basophil = 1))

init<-mice(aset, maxit=0) 
meth<-init$method
predM<-init$predictorMatrix

predM[,c("PATID","status6","time6")]=0
meth[c("HT_Z","WT_Z","BMI_Z","neutrophil","hemoglobin")]="norm.predict"
meth[c("platelet_count","leukocyte","ESR","CRP","eosinophil","monocyte","lymphocyte")]="rf"

aset_mice_obj<-mice(aset, method=meth, predictorMatrix=predM, m=5)
aset_imputed_long<-complete(aset_mice_obj,"long",include = FALSE)

# sanity check
ggplot(aset_imputed_long %>% select(all_of(c(var_imp,".imp"))) %>%
         bind_rows(aset %>% select(all_of(var_imp)) %>% mutate(`.imp`=0)) %>%
         gather(var,val,-`.imp`),aes(x=val))+
  # geom_density(aes(fill=as.factor(`.imp`)),alpha=0.4)+
  geom_boxplot(aes(fill=as.factor(`.imp`)),alpha=0.3)+
  facet_wrap(~ var,scales="free",ncol=5)

# form imputed dataset
aset_imputed<-aset_imputed_long %>% 
  select(all_of(c(var_imp,".imp","PATID"))) %>%
  gather(var,val,-`.imp`,-PATID) %>%
  group_by(PATID,var) %>%
  summarise(val=mean(val),.groups="drop") %>%
  spread(var,val) %>%
  left_join(aset %>% 
              select(all_of(c(var_lst[!var_lst %in% var_imp],"PATID","status6","time6"))),
            by = "PATID")

saveRDS(aset_imputed,
        file=paste0("C:/repo/R21_PEDS_UC/data/peds_uc_aset",nrow(aset),"_imputed.rds"))

