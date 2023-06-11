rm(list=ls())
pacman::p_load(tidyverse,
               magrittr,
               devtools,
               kableExtra
               )
source_url("https://raw.githubusercontent.com/sxinger/utils/master/plot_util.R")
source_url("https://raw.githubusercontent.com/sxinger/utils/master/analysis_util.R")

##----load data----
#aset<-readRDS("C:/repo/R21_PEDS_UC/data/peds_uc_aset.rds") %>%
aset<-readRDS("./data/peds_uc_aset.rds") %>%
  filter(INDEX_YEAR >= 2010 & INDEX_YEAR <= 2020) %>%
  filter(AGE_AT_INDEX <= 17 & AGE_AT_INDEX>=4)

denom<-aset %>% group_by(INDEX_YEAR) %>%
  summarise(NN_YR = length(unique(PATID)),.groups = "drop")

##----first treatment type distribution----
endpt<-aset %>% 
  select(PATID,INDEX_YEAR,mesalazine,corticosteroids,immunomodulator, biologics, colectomy) %>%
  mutate(standard = pmin(mesalazine,corticosteroids,na.rm=T)) %>%
  select(PATID, INDEX_YEAR, standard,immunomodulator, biologics, colectomy) %>%
  gather(var,val,-PATID, -INDEX_YEAR) %>%
  filter(!is.na(val)) %>%
  group_by(PATID, INDEX_YEAR) %>%
  mutate(ord = order(val)) %>%
  ungroup 

endpt_summ<-endpt %>%
  filter(ord==1) %>%
  group_by(INDEX_YEAR) %>%
  mutate(N_YR = length(unique(PATID))) %>%
  ungroup %>%
  group_by(INDEX_YEAR,N_YR,var) %>%
  summarise(n_yr=length(unique(PATID)),
            .groups = "drop") %>%
  mutate(p_yr = round(n_yr/N_YR,3)) 

endpt_summ2<-endpt_summ %>%
  bind_rows(
    endpt_summ %>%
      select(INDEX_YEAR,N_YR) %>%
      mutate(n_yr = N_YR) %>%
      left_join(denom,by="INDEX_YEAR") %>%
      mutate(N_YR = NN_YR,
             p_yr = round(n_yr/N_YR,3),
             var = "any") %>%
      unique %>%
      select(-NN_YR)
  )

ggplot(endpt_summ2 %<>%
         mutate(new_col = convert_scale(p_yr,N_YR)[["val"]],
                axis_formula = convert_scale(p_yr,N_YR)[["formula"]]),
       aes(x=INDEX_YEAR)) +
  geom_col(aes(y=N_YR), size = 1, fill = "grey")+
  geom_line(aes(y=new_col,color=var,group=var), size = 1.5)+
  scale_y_continuous(sec.axis = sec_axis(as.formula(endpt_summ2$axis_formula[1]), name = "proportion"))+
  scale_x_discrete(limits=2010:2020, labels = 2010:2020)+
  theme(axis.text.x = element_text(angle = 80),text=element_text(face="bold"))+
  facet_wrap(~var,ncol=3)


##----time-to-first-treatment distribution----
aset %<>%
  mutate(trt1_time = pmin(time1,time2,time3,time4,time5,na.rm=T)*!is.na(trt1))


ggplot(aset %>% filter(!is.na(trt1)&trt1_time<=365),
       aes(y=trt1_time,x=trt1))+
  geom_boxplot(color="blue")+
  stat_summary(geom="text", fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.45), size=5)+
  geom_violin(alpha=0.5)+
  theme(text = element_text(face="bold"))
  

##----missing rates----
var_ind<-c(
   "BMI_ind"
  ,"hemoglobin_ind"
  ,"platelet_count_ind"
  ,"leukocyte_ind"
  ,"ESR_ind"
  ,"CRP_ind"
  ,"albumin_ind"
  ,"25_OH_VD_ind"
  ,"basophil_ind"
  ,"eosinophil_ind"
  ,"neutrophil_ind"
  ,"monocyte_ind"
  ,"lymphocyte_ind"
)

df<-aset %>% 
  select(c("PATID","INDEX_YEAR",all_of(var_ind))) %>%
  gather(var,val,-PATID, -INDEX_YEAR) %>%
  group_by(INDEX_YEAR) %>%
  mutate(N_YR = length(unique(PATID))) %>%
  ungroup %>%
  group_by(INDEX_YEAR,N_YR,var) %>%
  summarise(n_yr=sum(val),
            .groups = "drop") %>%
  mutate(p_yr = round(n_yr/N_YR,3)) 

ggplot(df %<>%
         mutate(new_col = convert_scale(p_yr,N_YR)[["val"]],
                axis_formula = convert_scale(p_yr,N_YR)[["formula"]]),
       aes(x=INDEX_YEAR)) +
  geom_col(aes(y=N_YR), size = 1, fill = "grey")+
  geom_line(aes(y=new_col,color=var,group=var), size = 1.5)+
  theme(axis.text.x = element_text(angle = 80),text=element_text(face="bold")) +
  scale_y_continuous(sec.axis = sec_axis(as.formula(endpt_summ2$axis_formula[1]), name = "proportion"))+
  scale_x_discrete(limits=2010:2020, labels = 2010:2020)+
  facet_wrap(~var,ncol=3)


##----keep those started with standard treatment-----
# excld: without observation of recieving treatments
aset %>% filter(is.na(trt1)) %>% nrow(.)
# 844

# excld: not start with standard treatments
aset %>% filter(trt1 %in% c("biologics","colectomy","immunomodulator")) %>% nrow(.)
# 266

# eligible cohort size
aset %<>%
  mutate(AGE_AT_INDEX_12Y = as.numeric(AGE_AT_INDEX>=12)) %>%
  filter(trt1 %in% c("standard","biologics","immunomodulator")) %>% 
  filter(time6>=0) %>%
  filter(trt1 %in% c("standard"))
N<-nrow(aset)

# IQR
aset %>% group_by(status6) %>% 
  summarise(
    n = length(unique(PATID)),
    p = n/N,
    med = median(time6), 
    q1 = quantile(time6,0.25), 
    q3 = quantile(time6,0.75)   
  )

aset %>% filter(status6==1&status1==1) %>%
  summarise(
    n = length(unique(PATID)),
    p = n/N,
    med = median(time6), 
    q1 = quantile(time6,0.25), 
    q3 = quantile(time6,0.75)   
  )

aset %>% filter(status6==1&(status3==1|status5==1)&status1==0) %>%
  summarise(
    n = length(unique(PATID)),
    p = n/N,
    med = median(time6), 
    q1 = quantile(time6,0.25), 
    q3 = quantile(time6,0.75)   
  )
  
##-----------------------------------------------------------------------------------------------------------
summary(aset %>% dplyr::select(time_idx2std))

aset %>% filter(status1==1) %>% nrow()
summary(aset %>% filter(status1==1) %>% dplyr::select(time6))

aset %>% filter(status3==1|status5==1) %>% nrow()
summary(aset %>% filter(status3==1|status5==1) %>% dplyr::select(time6))

## --------------------------------------------------------------------------------------------------------------------------
risk_tbl<-summary(survfit(Surv(time6,status6) ~ 1, data = aset),
                  times = 365*c(1:10))
km_mort_unadj<-ggsurvplot(
  fit = survfit(Surv(time6,status6) ~ 1, data = aset),
  break.x.by = 365,
  xlab = "Days", 
  ylab = "Time to TE")

km_mort_unadj$plot +
  geom_vline(xintercept=365*c(1:10),linetype=2)+
  geom_label_repel(data=data.frame(x=risk_tbl$time,
                                   y=risk_tbl$surv,
                                   label=round(risk_tbl$surv,3),
                                   label_int=paste0(round(risk_tbl$surv,3),"[",round(risk_tbl$lower,3),",",round(risk_tbl$upper,3),"]")),
                   aes(x=x,y=y,label=label))


## --------------------------------------------------------------------------------------------------------------------------
var_lst<-c(
  "AGE_AT_INDEX","AGE_AT_INDEX_12Y","SEX","RACE","HISPANIC"
  ,"IP_IND1","IP_IND2","IP_IND3"
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
  "AGE_AT_INDEX_12Y","SEX","RACE","HISPANIC"
  ,"IP_IND1","IP_IND2","IP_IND3"  
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

# webshot::install_phantomjs() ## save_kabel() may need to run this step first
case_ctrl %>% 
  save_kable(paste0("./results/pat",nrow(aset),"_escalate_orig_summary.pdf"))


## --------------------------------------------------------------------------------------------------------------------------
case_ctrl<-univar_analysis_mixed(df=aset,
                                 id_col = "PATID",
                                 grp = aset$status6,
                                 var_lst = var_lst,
                                 facvar_lst = var_fac,
                                 pretty=T) 
case_ctrl %>% 
  save_kable(paste0("./results/pat",nrow(aset),"_escalate_orig_contrast.pdf"))
