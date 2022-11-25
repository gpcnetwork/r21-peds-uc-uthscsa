/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: covariate.sql
# Description: collect pre-selected covariates 
# Dependency: lookup_cd.sql
*/

create or replace table PED_UC_COV_BMI (
    PATID varchar(50) NOT NULL,
    SITE varchar(10) NOT NULL,
    VITAL_TYPE varchar(10),
    VITAL_VAL integer,
    VITAL_UNIT varchar(10), 
    MEASURE_DATE date,
    DAYS_SINCE_INDEX integer
);

create or replace table PED_UC_COV_ENC (
    PATID varchar(50) NOT NULL,
    SITE varchar(10) NOT NULL,
    ENC_TYPE varchar(3),
    ADMIT_DATE date,
    DISCHARGE_DATE date,
    ADMIT_DAYS_SINCE_INDEX integer,
    LOS integer
);

create or replace table PED_UC_COV_LAB (
    PATID varchar(50) NOT NULL,
    SITE varchar(10) NOT NULL,
    LAB_NAME varchar(100),
    LAB_ORDER_DATE date,
    SPECIMEN_DATE date,
    RESULT_DATE date,
    RESULT_NUM number(38,0),
    RESULT_QUAL varchar(100),
    RESULT_UNIT varchar(100),
    NORM_MODIFIER_HIGH varchar(10),
    ORDER_DAYS_SINCE_INDEX integer,
    SPECIMEN_DAYS_SINCE_INDEX integer,
    RESULT_DAYS_SINCE_INDEX integer
);

create or replace table PED_UC_COV_DX (
    PATID varchar(50) NOT NULL,
    SITE varchar(10) NOT NULL,
    DX_GRP varchar(30),
    DX varchar(10),
    DX_DATE date,
    DAYS_SINCE_INDEX integer
);

create or replace procedure get_covariates(SITES array)
returns variant
language javascript
as
$$
/**
 * Stored procedure to collect multiple endpoints based on procedures or medication codes
 * 
 * @param {array} SITES: an array of site acronyms (matching schema name suffix)
*/

var i;
for(i=0; i<SITES.length; i++){
    var site = SITES[i].toString();
    var site_cdm = 'PCORNET_CDM_' + site;
    
    // covariates - height, weight, bmi
    var sqlstmt_par = `INSERT INTO PED_UC_COV_BMI
                       -- height --
                       SELECT a.patid,'`+ site +`','HT',b.ht,'in',b.measure_date::date,
                              datediff(day,a.index_date,b.measure_date::date)
                       FROM ped_uc_table1 a
                       JOIN `+ site_cdm +`.deid_vital b ON a.patid = b.patid
                       WHERE b.ht is not null AND a.site = '`+ site +`'
                       UNION
                       select a.PATID,'`+ site +`','HT',oc.OBSCLIN_RESULT_NUM,oc.OBSCLIN_RESULT_UNIT,oc.OBSCLIN_START_DATE::date,
                              datediff(day,a.index_date,oc.OBSCLIN_START_DATE::date)
                       FROM ped_uc_table1 a
                       JOIN `+ site_cdm +`.deid_obs_clin oc ON a.patid = oc.patid AND
                            oc.OBSCLIN_TYPE = 'LC' and oc.OBSCLIN_CODE = '8302-2' AND a.site = '`+ site +`'
                       UNION
                       -- weight --
                       SELECT a.patid,'`+ site +`','WT',b.wt,'lb',b.measure_date::date,
                              datediff(day,a.index_date,b.measure_date::date)
                       FROM ped_uc_table1 a
                       JOIN `+ site_cdm +`.deid_vital b ON a.patid = b.patid
                       WHERE b.wt is not null AND a.site = '`+ site +`'
                       UNION
                       select a.PATID,'`+ site +`','WT',oc.OBSCLIN_RESULT_NUM,oc.OBSCLIN_RESULT_UNIT,oc.OBSCLIN_START_DATE::date,
                              datediff(day,a.index_date,oc.OBSCLIN_START_DATE::date)
                       FROM ped_uc_table1 a
                       JOIN `+ site_cdm +`.deid_obs_clin oc ON a.patid = oc.patid AND
                            oc.OBSCLIN_TYPE = 'LC' and oc.OBSCLIN_CODE = '29463-7' AND a.site = '`+ site +`'
                       UNION
                       -- bmi --
                       SELECT a.patid,'`+ site +`','BMI',b.ORIGINAL_BMI,'kg/m2',b.measure_date::date,
                              datediff(day,a.index_date,b.measure_date::date)
                       FROM ped_uc_table1 a
                       JOIN `+ site_cdm +`.deid_vital b ON a.patid = b.patid
                       WHERE b.ORIGINAL_BMI is not null AND a.site = '`+ site +`'
                       UNION
                       select a.PATID,'`+ site +`','BMI',oc.OBSCLIN_RESULT_NUM,oc.OBSCLIN_RESULT_UNIT,oc.OBSCLIN_START_DATE::date,
                              datediff(day,a.index_date,oc.OBSCLIN_START_DATE::date)
                       FROM ped_uc_table1 a
                       JOIN `+ site_cdm +`.deid_obs_clin oc ON a.patid = oc.patid AND
                            oc.OBSCLIN_TYPE = 'LC' and oc.OBSCLIN_CODE = '39156-5' AND a.site = '`+ site +`'
                       ;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    sqlstmt_run.execute();
    
    // covariates - diagnosis
    var sqlstmt_par = `INSERT INTO PED_UC_COV_DX
                       SELECT distinct
                              a.patid,
                              '`+ site +`',
                              'abdominal_pain',
                              b.DX,
                              NVL(b.dx_date::date,b.admit_date::date),
                              datediff(day,a.index_date,NVL(b.dx_date::date,b.admit_date::date))
                       FROM ped_uc_table1 a
                       JOIN `+ site_cdm +`.deid_diagnosis b ON a.patid = b.patid 
                       WHERE (b.DX like '789%' OR b.DX like 'R10%') AND a.site = '`+ site +`' 
                       UNION
                       SELECT distinct
                              a.patid,
                              '`+ site +`',
                              'diarrhea',
                              b.DX,
                              NVL(b.dx_date::date,b.admit_date::date),
                              datediff(day,a.index_date,NVL(b.dx_date::date,b.admit_date::date))
                       FROM ped_uc_table1 a
                       JOIN `+ site_cdm +`.deid_diagnosis b ON a.patid = b.patid 
                       WHERE (b.DX like '787.91%' OR b.DX like 'R19.7%') AND a.site = '`+ site +`'
                       UNION
                       SELECT distinct
                              a.patid,
                              '`+ site +`',
                              'rectal_bleeding',
                              b.DX,
                              NVL(b.dx_date::date,b.admit_date::date),
                              datediff(day,a.index_date,NVL(b.dx_date::date,b.admit_date::date))
                       FROM ped_uc_table1 a
                       JOIN `+ site_cdm +`.deid_diagnosis b ON a.patid = b.patid 
                       WHERE (b.DX like '569.3%' OR b.DX like '578.1%' OR b.DX like '578.9%' OR b.DX like '569.12%' OR
                              b.DX like 'K62.5%') AND a.site = '`+ site +`'
                       ;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    sqlstmt_run.execute();
    
    // covariates - labs
    var sqlstmt_par = `INSERT INTO PED_UC_COV_LAB
                       SELECT a.patid,
                              '`+ site +`',
                              l.lab_name,
                              b.lab_order_date,
                              b.specimen_date,
                              b.result_date,
                              b.result_num,
                              b.result_qual,
                              b.result_unit,
                              b.NORM_MODIFIER_HIGH,
                              datediff(day,a.index_date,b.lab_order_date),
                              datediff(day,a.index_date,b.specimen_date),
                              datediff(day,a.index_date,b.result_date)                              
                       FROM ped_uc_table1 a
                       JOIN `+ site_cdm +`.deid_lab_result_cm b ON a.patid = b.patid
                       JOIN conceptset_labs_loinc l ON l.loinc_num = b.lab_loinc
                       WHERE a.site = '`+ site +`'
                       ;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    sqlstmt_run.execute();   
    
    // covariates - encounter
    var sqlstmt_par = `INSERT INTO PED_UC_COV_ENC
                       SELECT a.patid,
                              '`+ site +`',
                              b.enc_type,
                              b.admit_date,
                              b.discharge_date,
                              datediff(day,a.index_date::date,b.admit_date::date),
                              datediff(day,b.admit_date::date,b.discharge_date::date)
                       FROM ped_uc_table1 a
                       JOIN `+ site_cdm +`.deid_encounter b ON a.patid = b.patid 
                       WHERE b.enc_type in ('IP','EI') AND a.site = '`+ site +`'
                       ;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    sqlstmt_run.execute();
}
$$
;


call get_covariates(array_construct(
     'ALLINA'
    ,'IHC'
    ,'MCRI'
    ,'MCW'
    ,'KUMC'
    ,'MU'
    ,'UTHSCSA'
    ,'UTSW'
    ,'UTHOUSTON'
    ,'WASHU')
);
