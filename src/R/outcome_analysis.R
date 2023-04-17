## ----setup, include=FALSE--------------------------------------------------------------------------------------------------
rm(list=ls())
knitr::opts_chunk$set(echo = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      fig.height=6)
pacman::p_load(tidyverse,
               magrittr,
               broom,
               survival,
               survminer,
               kableExtra,
               devtools,
               cmprsk,
               ggridges,
               Matrix,
               glmnet,
               scales,
               islasso,
               caret,
               pROC,
               mice,
               riskRegression,
               SurvMetrics
               )
source_url("https://raw.githubusercontent.com/sxinger/utils/master/analysis_util.R")


## --------------------------------------------------------------------------------------------------------------------------
aset<-readRDS("../../data/peds_uc_aset.rds") %>%
  filter(INDEX_YEAR >= 2010 & INDEX_YEAR <= 2020) %>%
  filter(AGE_AT_INDEX <= 17 & AGE_AT_INDEX>=4) %>%
  mutate(AGE_AT_INDEX_12Y = as.numeric(AGE_AT_INDEX>=12)) %>%
  filter(trt1 %in% c("standard","biologics","immunomodulator")) %>% 
  filter(time6>=0) %>%
  filter(trt1 %in% c("standard"))


## --------------------------------------------------------------------------------------------------------------------------
aset %>% filter(status1==1) %>% nrow()
summary(aset %>% filter(status1==1) %>% dplyr::select(time6))

aset %>% filter(status3==1|status5==1) %>% nrow()
summary(aset %>% filter(status3==1|status5==1) %>% dplyr::select(time6))


## --------------------------------------------------------------------------------------------------------------------------
var_lst<-c(
  "AGE_AT_INDEX","AGE_AT_INDEX_12Y","SEX","RACE","HISPANIC","IP_IND"
 ,"HT_Z","WT_Z","BMI_Z","BMI_ind"
 ,"hemoglobin","hemoglobin_ind"
 ,"platelet_count","platelet_count_ind"
 ,"leukocyte","leukocyte_ind"
 ,"ESR","ESR_ind"
 ,"CRP","CRP_ind"
 ,"albumin","albumin_ind"
 ,"25_OH_VD","25_OH_VD_ind"
 ,"basophil","basophil_ind"
 ,"eosinophil","eosinophil_ind"
 ,"neutrophil","neutrophil_ind"
 ,"monocyte","monocyte_ind"
 ,"lymphocyte","lymphocyte_ind"
 # ,"abdominal_pain","diarrhea","rectal_bleeding"
 # ,"trt1"
)
           
var_fac<-c(
  "AGE_AT_INDEX_12Y","SEX","RACE","HISPANIC","IP_IND"
 ,"BMI_ind","hemoglobin_ind","platelet_count_ind","leukocyte_ind","ESR_ind","CRP_ind","albumin_ind","25_OH_VD_ind"
 ,"basophil_ind","eosinophil_ind","neutrophil_ind","monocyte_ind","lymphocyte_ind"
 # ,"abdominal_pain","diarrhea","rectal_bleeding"
 # ,"trt1"
)



## --------------------------------------------------------------------------------------------------------------------------
case_ctrl<-univar_analysis_mixed(df=aset,
                                 id_col = "PATID",
                                 var_lst = var_lst,
                                 facvar_lst = var_fac,
                                 pretty=T)

case_ctrl# %>% save_kable(paste0("C:/repo/R21_PEDS_UC/result/pat",nrow(aset),"_escalate_orig_summary.pdf"))


## --------------------------------------------------------------------------------------------------------------------------
case_ctrl<-univar_analysis_mixed(df=aset,
                                 id_col = "PATID",
                                 grp = aset$status6,
                                 var_lst = var_lst,
                                 facvar_lst = var_fac,
                                 pretty=T)
case_ctrl #%>% save_kable(paste0("C:/repo/R21_PEDS_UC/result/pat",nrow(aset),"_escalate_orig_contrast.pdf"))


## --------------------------------------------------------------------------------------------------------------------------
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

var_fac<-c(
   "SEX"
  ,"RACE"
  ,"HISPANIC"
  ,"IP_IND"
)


## --------------------------------------------------------------------------------------------------------------------------
# impute.R
aset_imputed<-readRDS(paste0("../../data/peds_uc_aset",nrow(aset),"_imputed.rds"))


## --------------------------------------------------------------------------------------------------------------------------
case_ctrl<-univar_analysis_mixed(df=aset_imputed,
                                 id_col = "PATID",
                                 grp = aset_imputed$status6,
                                 var_lst = var_lst,
                                 facvar_lst = var_fac,
                                 pretty=T)

case_ctrl #%>% save_kable(paste0("C:/repo/R21_PEDS_UC/result/pat",nrow(aset),"_escalate_imputed_contrast.pdf"))


## --------------------------------------------------------------------------------------------------------------------------
uni_coef_df<-c()
for(var in var_lst){
  fit_lr_filter<-glm(as.formula(paste("status6~",var)),data=aset_imputed,family="binomial")
  fit_surv_filter<-coxph(as.formula(paste("Surv(time6,status6)~",var)),data=aset_imputed)
  coef_combine<-data.frame(summary(fit_lr_filter)$coefficients) %>%
    rownames_to_column("var") %>% 
    filter(!grepl("(Intercept)+",var)) %>%
    mutate(OR=round(exp(Estimate),3),
           pval1=round(`Pr...z..`,4)) %>%
    dplyr::select(var,OR,pval1) %>%
    left_join(data.frame(summary(fit_surv_filter)$coefficients) %>%
                 rownames_to_column("var") %>%
                 mutate(HR=`exp.coef.`,
                        pval2=round(`Pr...z..`,4)) %>%
             dplyr::select(var,HR,pval2),
             by="var")
  uni_coef_df %<>% bind_rows(coef_combine)
}

kable(uni_coef_df,"html") %>%
  kable_styling("striped")# %>%
  #save_kable(paste0("C:/repo/R21_PEDS_UC/result/pat",nrow(aset),"_escalate_imputed_uni.pdf"))


## --------------------------------------------------------------------------------------------------------------------------
# Specify a null model with no predictors
null_model <- glm(status6 ~ 1, 
                  data = aset_imputed, family = binomial())
# Specify the full model using all of the potential predictors
full_model <- glm(as.formula(paste0("status6 ~",paste(var_lst,collapse = "+"))), 
                  data = aset_imputed, family = binomial())
# Use a bi-directional stepwise algorithm to build a parsimonious model
step_model <- step(null_model,
                   scope = list(lower = null_model, upper = full_model),
                   direction = "both")


## --------------------------------------------------------------------------------------------------------------------------
full_reg<-data.frame(summary(full_model)$coefficients) %>%
  mutate(OR=round(exp(Estimate),3),
         pval=round(`Pr...z..`,4)) %>%
  dplyr::select(OR,pval)

kable(full_reg,"html") %>%
  kable_styling("striped") #%>%
  #save_kable(paste0("C:/repo/R21_PEDS_UC/result/pat",nrow(aset),"_escalate_imputed_full.pdf"))


## --------------------------------------------------------------------------------------------------------------------------
fit_lr_roc<-pROC::roc(full_model$y,full_model$fitted.values)
pROC::ggroc(list(Model=fit_lr_roc))+
  geom_abline(intercept=1,linetype=2)+
  labs(subtitle = paste0("AUC:",paste0(round(pROC::ci.auc(fit_lr_roc),4),collapse = ",")))


## --------------------------------------------------------------------------------------------------------------------------
# print final model results
# aset_imputed %<>% 
#   mutate(trt1 = case_when(trt1=="standard" ~ "standard",
#                           TRUE ~ "non-standard"),
#          trt1 = relevel(as.factor(trt1),ref="standard"))
fit_lr<-glm(as.formula(paste0("status6 ~",
                              paste(c(names(step_model$coefficients)[!grepl("(Intercept|RACE|leukocyte|ESR)+",names(step_model$coefficients))]
                                      ,"RACE"
                                      ,"hemoglobin"
                                      ,"BMI_Z"
                                    ),collapse = "+"))),
            data = aset_imputed, family = binomial())

step_reg<-data.frame(summary(fit_lr)$coefficients) %>%
  mutate(OR=round(exp(Estimate),3),
         pval=round(`Pr...z..`,4)) %>%
  dplyr::select(OR,pval)

kable(step_reg,"html") %>%
  kable_styling("striped") #%>%
  #save_kable(paste0("C:/repo/R21_PEDS_UC/result/pat",nrow(aset),"_escalate_imputed_stepwise_lr.pdf"))


## --------------------------------------------------------------------------------------------------------------------------
fit_lr_roc<-pROC::roc(fit_lr$y,fit_lr$fitted.values)
pROC::ggroc(list(Model=fit_lr_roc))+
  geom_abline(intercept=1,linetype=2)+
  labs(subtitle = paste0("AUC:",paste0(round(pROC::ci.auc(fit_lr_roc),4),collapse = ",")))


## --------------------------------------------------------------------------------------------------------------------------
# Specify a null model with no predictors
null_model <- coxph(Surv(time6,status6) ~ 1, 
                    data = aset_imputed)
# Specify the full model using all of the potential predictors
full_model <- coxph(as.formula(paste0("Surv(time6,status6) ~",paste(var_lst,collapse = "+"))), 
                    data = aset_imputed)
# Use a bi-directional stepwise algorithm to build a parsimonious model
step_model <- step(null_model,
                   scope = list(lower = null_model, upper = full_model),
                   direction = "both")

fit_cox<-coxph(as.formula(paste0("Surv(time6,status6)~",
                              paste(c(names(step_model$coefficients)[!grepl("(Intercept|RACE|leukocyte|basophil|ESR|HT_Z)+",names(step_model$coefficients))]
                                      ,"RACE"
                                    ),collapse = "+"))),
               data = aset_imputed,x=TRUE)

step_cox<-data.frame(summary(fit_cox)$coefficients) %>%
  mutate(pval = round(`Pr...z..`,4)) %>%
  select(coef,`exp.coef.`,pval)

kable(step_cox,"html") %>%
  kable_styling("striped") #%>%
  #save_kable(paste0("C:/repo/R21_PEDS_UC/result/pat",nrow(aset),"_escalate_imputed_stepwise_cox.pdf"))

