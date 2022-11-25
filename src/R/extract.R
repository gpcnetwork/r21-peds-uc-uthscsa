# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: extract.R
# Description: data import from database into R
# Dependency: all SQL scripts and database connection

rm(list=ls()); gc()
setwd("C:/repo/R21_PEDS_UC")

# install.packages("pacman")
pacman::p_load(
  DBI,
  jsonlite,
  odbc,
  tidyverse,
  magrittr,
  dbplyr
)

# make db connection
sf_conn <- DBI::dbConnect(drv = odbc::odbc(),
                          dsn = Sys.getenv("ODBC_DSN_NAME"),
                          uid = Sys.getenv("SNOWFLAKE_USER"),
                          pwd = Sys.getenv("SNOWFLAKE_PWD"))


# collect table1 (demographic)
## 39 individuals have multiple birth dates
dat<-tbl(sf_conn,in_schema("PEDS_UC","PED_UC_TABLE1")) %>% collect() %>%
  group_by(PATID) %>% arrange(BIRTH_DATE) %>% slice(1:1) %>% ungroup 
saveRDS(dat,file="./data/peds_uc_tbl1.rds")

# collect endpoints
dat<-tbl(sf_conn,in_schema("PEDS_UC","PED_UC_ENDPT")) %>% collect()
saveRDS(dat,file="./data/peds_uc_endpt.rds")

# collect covariates
dat<-tbl(sf_conn,in_schema("PEDS_UC","PED_UC_COV_BMI")) %>% collect()
saveRDS(dat,file="./data/peds_uc_cov_bmi.rds")

dat<-tbl(sf_conn,in_schema("PEDS_UC","PED_UC_COV_ENC")) %>% collect()
saveRDS(dat,file="./data/peds_uc_cov_enc.rds")

dat<-tbl(sf_conn,in_schema("PEDS_UC","PED_UC_COV_LAB")) %>% collect()
saveRDS(dat,file="./data/peds_uc_cov_lab.rds")

dat<-tbl(sf_conn,in_schema("PEDS_UC","PED_UC_COV_DX")) %>% collect()
saveRDS(dat,file="./data/peds_uc_cov_dx.rds")



# disconnect
DBI::dbDisconnect(sf_conn)
