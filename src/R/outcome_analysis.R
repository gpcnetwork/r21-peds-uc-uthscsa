rm(list=ls())
setwd("C:/repo/R21_PedsUC")
pacman::p_load(tidyverse,
               magrittr,
               broom,
               survival,
               survminer,
               kableExtra,
               devtools,
               cmprsk,
               ggridges,
               ggrepel,
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

##---- specify variables ----
var_lst<-c(
   "AGE_AT_INDEX"
  ,"SEX"
  ,"RACE"
  ,"HISPANIC"
  ,"IP_IND1"
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
  ,"IP_IND1"
)

##---- load data ----
# impute.R
aset_imputed_long<-readRDS(paste0("./data/peds_uc_aset502_imputed_long.rds"))

##---- univariate models with pooling ----
uni_coef_df<-c()
for(var in var_lst){
  coef_combine<-c()
  for(fold in 1:10){
    aset_imputed<-aset_imputed_long %>% filter(`.imp`==fold)
    fit_lr_filter<-glm(as.formula(paste("status6~",var)),data=aset_imputed,family="binomial")
    fit_surv_filter<-coxph(as.formula(paste("Surv(time6,status6)~",var)),data=aset_imputed)
    coef_combine %<>%
      bind_rows(
        data.frame(summary(fit_lr_filter)$coefficients) %>%
          rownames_to_column("var") %>% 
          filter(!grepl("(Intercept)+",var)) %>%
          mutate(
            logOR = Estimate,
            logOR_std = `Std..Error`,
            OR=round(exp(Estimate),3),
            pval1=round(`Pr...z..`,4)
          ) %>%
          dplyr::select(var,logOR, logOR_std, OR,pval1) %>%
          left_join(data.frame(summary(fit_surv_filter)$coefficients) %>%
                      rownames_to_column("var") %>%
                      mutate(
                        logHR = coef,
                        logHR_std = `se.coef.`,
                        HR=`exp.coef.`,
                        pval2=round(`Pr...z..`,4)) %>%
                      dplyr::select(var,logHR, logHR_std, HR,pval2),
                    by="var") %>%
          mutate(imp = fold)
      )
  }
  # calculate lambda
  # https://bookdown.org/mwheymans/bookmi/rubins-rules.html
  N<-nrow(aset_imputed)
  n<-10 # number of imputations
  
  # RR pooling
  coef_combine %<>%
    group_by(var) %>%
    mutate(
      logOR_m = mean(logOR),
      logHR_m = mean(logHR)
    ) %>%
    summarise(
      logOR_m = logOR_m[1],
      logOR_vb = sum((logOR-logOR_m)^2)/(n-1),
      logOR_vw = mean(logOR_std^2),
      logOR_vt = logOR_vw + logOR_vb + logOR_vb/n,
      logOR_pval_m = mean(pval1),
      logHR_m = logHR_m[1],
      logHR_vb = sum((logHR-logHR_m)^2)/(n-1),
      logHR_vw = mean(logHR_std^2),
      logHR_vt = logHR_vw + logHR_vb + logHR_vb/n,
      logHR_pval_m = mean(pval1)
    ) %>%
    mutate(
      logOR_wald = logOR_m/sqrt(logOR_vt),
      logOR_lambda = (logOR_vb+logOR_vb/n)/logOR_vt,
      logOR_df_old = (n-1)/(logOR_lambda^2),
      logOR_df_obs = (((N-2)+1)/((N-2)+3))*(N-2)*(1-logOR_lambda),
      logOR_df_adj = 1/(1/logOR_df_old+1/logOR_df_obs),
      pval_logOR_wald = 2*pt(logOR_wald,logOR_df_adj,lower.tail=(logOR_wald<=0)),
      logHR_wald = logHR_m/sqrt(logHR_vt),
      logHR_lambda = (logHR_vb+logHR_vb/n)/logHR_vt,
      logHR_df_old = (n-1)/(logHR_lambda^2),
      logHR_df_obs = (((N-2)+1)/((N-2)+3))*(N-2)*(1-logHR_lambda),
      logHR_df_adj = 1/(1/logHR_df_old+1/logHR_df_obs),
      pval_logHR_wald = 2*pt(abs(logHR_wald),logHR_df_adj,lower.tail=FALSE)
    ) %>%
    # add confidence intervals
    mutate(
      OR = exp(logOR_m),
      OR_ci_lower = exp(logOR_m - qt(0.975,logOR_df_adj)*sqrt(logOR_vt)),
      OR_ci_upper = exp(logOR_m + qt(0.975,logOR_df_adj)*sqrt(logOR_vt)),
      HR = exp(logOR_m),
      HR_ci_lower = exp(logOR_m - qt(0.975,logHR_df_adj)*sqrt(logHR_vt)),
      HR_ci_upper = exp(logOR_m + qt(0.975,logHR_df_adj)*sqrt(logHR_vt))
    )

  uni_coef_df %<>% 
    bind_rows(coef_combine)
}

kable(uni_coef_df,"html") %>%
  kable_styling("striped") %>%
  save_kable(paste0("./results/pat",nrow(aset_imputed),"_escalate_imputed_uni.pdf"))

##---- multivariable logistic reg models with pooling ----
full_lr<-c()
sel_lr<-c()
mansel_lr<-c()
for(fold in 1:10){
  aset_imputed<-aset_imputed_long %>% filter(`.imp`==fold)
  
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
  
  ##---- full model ----
  full_lr %<>%
    bind_rows(
      data.frame(summary(full_model)$coefficients) %>%
        rownames_to_column(.,var = "var") %>% 
        mutate(
          logOR = Estimate,
          logOR_std = `Std..Error`,
          OR=round(exp(Estimate),3),
          pval=round(`Pr...z..`,4)
        ) %>%
        dplyr::select(var,logOR, logOR_std, OR, pval) %>%
        mutate(imp=fold)
    )
  
  # fit_lr_roc<-pROC::roc(full_model$y,full_model$fitted.values)
  # pROC::ggroc(list(Model=fit_lr_roc))+
  #   geom_abline(intercept=1,linetype=2)+
  #   labs(subtitle = paste0("AUC:",paste0(round(pROC::ci.auc(fit_lr_roc),4),collapse = ",")))
  
  ##---- selected model ----
  sel_lr %<>%
    bind_rows(
      data.frame(summary(step_model)$coefficients) %>%
        rownames_to_column(.,var = "var") %>% 
        mutate(
          logOR = Estimate,
          logOR_std = `Std..Error`,
          OR=round(exp(Estimate),3),
          pval=round(`Pr...z..`,4)
        ) %>%
        dplyr::select(var,logOR, logOR_std, OR, pval) %>%
        mutate(imp=fold)
    )
  
  ##---- manual selected model ----
  fit_lr<-glm(as.formula(paste0("status6 ~",
                                paste(c(names(step_model$coefficients)[!grepl("(Intercept|RACE|leukocyte|ESR)+",names(step_model$coefficients))]
                                        ,"RACE"
                                        ,"hemoglobin"
                                        ,"BMI_Z"
                                ),collapse = "+"))),
              data = aset_imputed, family = binomial())
  mansel_lr %<>%
    bind_rows(
      data.frame(summary(fit_lr)$coefficients) %>%
        rownames_to_column(.,var = "var") %>% 
        mutate(
          logOR = Estimate,
          logOR_std = `Std..Error`,
          OR=round(exp(Estimate),3),
          pval=round(`Pr...z..`,4)
        ) %>%
        dplyr::select(var,logOR, logOR_std, OR, pval) %>%
        mutate(imp=fold)
    )
}

## RR pooling
full_lr %<>%
  group_by(var) %>%
  mutate(
    logOR_m = mean(logOR)
  ) %>%
  summarise(
    logOR_m = logOR_m[1],
    logOR_vb = sum((logOR-logOR_m)^2)/(n-1),
    logOR_vw = mean(logOR_std^2),
    logOR_vt = logOR_vw + logOR_vb + logOR_vb/n,
    logOR_pval_m = mean(pval)
  ) %>%
  mutate(
    logOR_wald = logOR_m/sqrt(logOR_vt),
    logOR_lambda = (logOR_vb+logOR_vb/n)/logOR_vt,
    logOR_df_old = (n-1)/(logOR_lambda^2),
    logOR_df_obs = (((N-2)+1)/((N-2)+3))*(N-2)*(1-logOR_lambda),
    logOR_df_adj = 1/(1/logOR_df_old+1/logOR_df_obs),
    pval_logOR_wald = 2*pt(abs(logOR_wald),logOR_df_adj,lower.tail=FALSE)
  ) %>%
  ungroup %>%
  # add pooled CI
  mutate(
    OR = exp(logOR_m),
    OR_ci_lower = exp(logOR_m - qt(0.975,logOR_df_adj)*sqrt(logOR_vt)),
    OR_ci_upper = exp(logOR_m + qt(0.975,logOR_df_adj)*sqrt(logOR_vt))
  )

kable(full_lr,"html") %>%
  kable_styling("striped") %>%
  save_kable(paste0("./results/pat",nrow(aset_imputed),"_escalate_imputed_full.pdf"))


sel_lr %<>%
  group_by(var) %>%
  mutate(
    logOR_m = mean(logOR)
  ) %>%
  summarise(
    logOR_m = logOR_m[1],
    logOR_vb = sum((logOR-logOR_m)^2)/(n-1),
    logOR_vw = mean(logOR_std^2),
    logOR_vt = logOR_vw + logOR_vb + logOR_vb/n,
    logOR_pval_m = mean(pval)
  ) %>%
  mutate(
    logOR_wald = logOR_m/sqrt(logOR_vt),
    logOR_lambda = (logOR_vb+logOR_vb/n)/logOR_vt,
    logOR_df_old = (n-1)/(logOR_lambda^2),
    logOR_df_obs = (((N-2)+1)/((N-2)+3))*(N-2)*(1-logOR_lambda),
    logOR_df_adj = 1/(1/logOR_df_old+1/logOR_df_obs),
    pval_logOR_wald = 2*pt(abs(logOR_wald),logOR_df_adj,lower.tail=FALSE)
  ) %>%
  ungroup %>%
  # add pooled CI
  mutate(
    OR = exp(logOR_m),
    OR_ci_lower = exp(logOR_m - qt(0.975,logOR_df_adj)*sqrt(logOR_vt)),
    OR_ci_upper = exp(logOR_m + qt(0.975,logOR_df_adj)*sqrt(logOR_vt))
  )

kable(sel_lr,"html") %>%
  kable_styling("striped") %>%
  save_kable(paste0("./results/pat",nrow(aset_imputed),"_escalate_imputed_sel.pdf"))


mansel_lr %<>%
  group_by(var) %>%
  mutate(
    logOR_m = mean(logOR)
  ) %>%
  summarise(
    logOR_m = logOR_m[1],
    logOR_vb = sum((logOR-logOR_m)^2)/(n-1),
    logOR_vw = mean(logOR_std^2),
    logOR_vt = logOR_vw + logOR_vb + logOR_vb/n,
    logOR_pval_m = mean(pval)
  ) %>%
  mutate(
    logOR_wald = logOR_m/sqrt(logOR_vt),
    logOR_lambda = (logOR_vb+logOR_vb/n)/logOR_vt,
    logOR_df_old = (n-1)/(logOR_lambda^2),
    logOR_df_obs = (((N-2)+1)/((N-2)+3))*(N-2)*(1-logOR_lambda),
    logOR_df_adj = 1/(1/logOR_df_old+1/logOR_df_obs),
    pval_logOR_wald = 2*pt(abs(logOR_wald),logOR_df_adj,lower.tail=FALSE)
  ) %>%
  ungroup %>%
  # add pooled CI
  mutate(
    OR = exp(logOR_m),
    OR_ci_lower = exp(logOR_m - qt(0.975,logOR_df_adj)*sqrt(logOR_vt)),
    OR_ci_upper = exp(logOR_m + qt(0.975,logOR_df_adj)*sqrt(logOR_vt))
  )

kable(mansel_lr,"html") %>%
  kable_styling("striped") %>%
  save_kable(paste0("./results/pat",nrow(aset_imputed),"_escalate_imputed_manual_sel.pdf"))


##---- multivariable cox proportional model ----
full_cox<-c()
sel_cox<-c()
mansel_cox<-c()
for(fold in 1:10){
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
  
  ##---- full model ----
  full_cox %<>%
    bind_rows(
      data.frame(summary(full_model)$coefficients) %>%
        rownames_to_column(.,var = "var") %>% 
        mutate(
          logHR = coef,
          logHR_std = `se.coef.`,
          HR=round(exp(coef),3),
          pval=round(`Pr...z..`,4)
        ) %>%
        dplyr::select(var,logHR, logHR_std, HR, pval) %>%
        mutate(imp=fold)
    )

  ##---- selected model ----
  sel_cox %<>%
    bind_rows(
      data.frame(summary(step_model)$coefficients) %>%
        rownames_to_column(.,var = "var") %>% 
        mutate(
          logHR = coef,
          logHR_std = `se.coef.`,
          HR=round(exp(coef),3),
          pval=round(`Pr...z..`,4)
        ) %>%
        dplyr::select(var,logHR, logHR_std, HR, pval) %>%
        mutate(imp=fold)
    )
  
  ##---- manual selected model ----
  fit_cox<-coxph(as.formula(paste0("Surv(time6,status6)~",
                                   paste(c(names(step_model$coefficients)[!grepl("(Intercept|RACE|leukocyte|basophil|ESR|HT_Z)+",names(step_model$coefficients))]
                                           ,"RACE"
                                   ),collapse = "+"))),
                 data = aset_imputed,x=TRUE)
  
  mansel_cox %<>%
    bind_rows(
      data.frame(summary(fit_cox)$coefficients) %>%
        rownames_to_column(.,var = "var") %>% 
        mutate(
          logHR = coef,
          logHR_std = `se.coef.`,
          HR=round(exp(coef),3),
          pval=round(`Pr...z..`,4)
        ) %>%
        dplyr::select(var,logHR, logHR_std, HR, pval) %>%
        mutate(imp=fold)
    )
  
}

## RR pooling
full_cox %<>%
  group_by(var) %>%
  mutate(
    logHR_m = mean(logHR)
  ) %>%
  summarise(
    logHR_m = logHR_m[1],
    logHR_vb = sum((logHR-logHR_m)^2)/(n-1),
    logHR_vw = mean(logHR_std^2),
    logHR_vt = logHR_vw + logHR_vb + logHR_vb/n,
    logHR_pval_m = mean(pval)
  ) %>%
  mutate(
    logHR_wald = logHR_m/sqrt(logHR_vt),
    logHR_lambda = (logHR_vb+logHR_vb/n)/logHR_vt,
    logHR_df_old = (n-1)/(logHR_lambda^2),
    logHR_df_obs = (((N-2)+1)/((N-2)+3))*(N-2)*(1-logHR_lambda),
    logHR_df_adj = 1/(1/logHR_df_old+1/logHR_df_obs),
    pval_logHR_wald = 2*pt(abs(logHR_wald),logHR_df_adj,lower.tail=FALSE)
  ) %>%
  ungroup %>%
  # add 95% CI
  mutate(
    HR = exp(logHR_m),
    HR_ci_lower = exp(logHR_m - qt(0.975,logHR_df_adj)*sqrt(logHR_vt)),
    HR_ci_upper = exp(logHR_m + qt(0.975,logHR_df_adj)*sqrt(logHR_vt))
  )

kable(full_cox,"html") %>%
  kable_styling("striped") %>%
  save_kable(paste0("./results/pat",nrow(aset_imputed),"_escalate_imputed_cox_full.pdf"))


sel_cox %<>%
  group_by(var) %>%
  mutate(
    logHR_m = mean(logHR)
  ) %>%
  summarise(
    logHR_m = logHR_m[1],
    logHR_vb = sum((logHR-logHR_m)^2)/(n-1),
    logHR_vw = mean(logHR_std^2),
    logHR_vt = logHR_vw + logHR_vb + logHR_vb/n,
    logHR_pval_m = mean(pval)
  ) %>%
  mutate(
    logHR_wald = logHR_m/sqrt(logHR_vt),
    logHR_lambda = (logHR_vb+logHR_vb/n)/logHR_vt,
    logHR_df_old = (n-1)/(logHR_lambda^2),
    logHR_df_obs = (((N-2)+1)/((N-2)+3))*(N-2)*(1-logHR_lambda),
    logHR_df_adj = 1/(1/logHR_df_old+1/logHR_df_obs),
    pval_logHR_wald = 2*pt(abs(logHR_wald),logHR_df_adj,lower.tail=FALSE)
  ) %>%
  ungroup %>%
  # add 95% CI
  mutate(
    HR = exp(logHR_m),
    HR_ci_lower = exp(logHR_m - qt(0.975,logHR_df_adj)*sqrt(logHR_vt)),
    HR_ci_upper = exp(logHR_m + qt(0.975,logHR_df_adj)*sqrt(logHR_vt))
  )

kable(sel_cox,"html") %>%
  kable_styling("striped") %>%
  save_kable(paste0("./results/pat",nrow(aset_imputed),"_escalate_imputed_cox_sel.pdf"))


mansel_cox %<>%
  group_by(var) %>%
  mutate(
    logHR_m = mean(logHR)
  ) %>%
  summarise(
    logHR_m = logHR_m[1],
    logHR_vb = sum((logHR-logHR_m)^2)/(n-1),
    logHR_vw = mean(logHR_std^2),
    logHR_vt = logHR_vw + logHR_vb + logHR_vb/n,
    logHR_pval_m = mean(pval)
  ) %>%
  mutate(
    logHR_wald = logHR_m/sqrt(logHR_vt),
    logHR_lambda = (logHR_vb+logHR_vb/n)/logHR_vt,
    logHR_df_old = (n-1)/(logHR_lambda^2),
    logHR_df_obs = (((N-2)+1)/((N-2)+3))*(N-2)*(1-logHR_lambda),
    logHR_df_adj = 1/(1/logHR_df_old+1/logHR_df_obs),
    pval_logHR_wald = 2*pt(abs(logHR_wald),logHR_df_adj,lower.tail=FALSE)
  ) %>%
  ungroup %>%
  # add 95% CI
  mutate(
    HR = exp(logHR_m),
    HR_ci_lower = exp(logHR_m - qt(0.975,logHR_df_adj)*sqrt(logHR_vt)),
    HR_ci_upper = exp(logHR_m + qt(0.975,logHR_df_adj)*sqrt(logHR_vt))
  )

kable(mansel_cox,"html") %>%
  kable_styling("striped") %>%
  save_kable(paste0("./results/pat",nrow(aset_imputed),"_escalate_imputed_manual_cox_sel.pdf"))
