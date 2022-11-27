/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: table1.sql
# Description: collect denominator table
*/

create or replace table consort_diagram (
    CRITERION varchar(20) NOT NULL,
    PAT_CNT integer,
    SITE varchar(10)        
);

create or replace table PED_UC_TABLE1 (
    PATID varchar(50) NOT NULL,
    BIRTH_DATE date NOT NULL,
    INDEX_DATE date NOT NULL,
    AGE_AT_INDEX integer NOT NULL,
    AGE_AT_INDEX_DAYS integer NOT NULL,
    SEX varchar(3),
    RACE varchar(6),
    HISPANIC varchar(20),
    SITE varchar(10),
    constraint pk primary key(PATID)
);

create or replace procedure get_table1(SITES array)
returns variant
language javascript
as
$$
/**
 * Stored procedure to collect a Table 1 after applying inclusion and exclusion criteria 
 * only based on diagnosis codes
 * 
 * @param {array} SITES: an array of site acronyms (matching schema name suffix)
*/

var i;
for(i=0; i<SITES.length; i++){
    var site = SITES[i].toString();
    var site_cdm = 'PCORNET_CDM_' + site;
    
    // initial inclusion
    var sqlstmt_par = `CREATE OR REPLACE TEMPORARY TABLE pat_incld AS
                        SELECT a.patid, b.birth_date, MIN(NVL(a.dx_date,a.admit_date)) AS index_date,
                               round(datediff(day,b.birth_date,MIN(NVL(a.dx_date,a.admit_date)))/365.25) AS age_at_index, 
                               datediff(day,b.birth_date,MIN(NVL(a.dx_date,a.admit_date))) AS age_at_index_day, b.sex, 
                               CASE WHEN b.race IN ('05') THEN 'white' WHEN b.race IN ('03') THEN 'black' WHEN b.race in ('UN','NI') or b.race is NULL THEN 'unk' ELSE 'ot' END AS race, 
                               CASE WHEN b.hispanic = 'Y' THEN 'hispanic' WHEN b.hispanic = 'N' THEN 'non-hispanic' WHEN b.hispanic in ('UN','NI') or b.hispanic is NULL THEN 'unk' ELSE 'ot' END AS hispanic, 
                               '`+ site +`' AS site
                        FROM `+ site_cdm +`.DEID_DIAGNOSIS a
                        JOIN `+ site_cdm +`.DEID_DEMOGRAPHIC b
                        ON a.patid = b.patid
                        WHERE a.dx LIKE '556%' OR a.dx LIKE 'K51%' AND
                              a.dx_date >= b.birth_date
                        GROUP BY a.patid, b.birth_date, b.sex, b.race, b.hispanic;`
                        
    var consort_par = `INSERT INTO CONSORT_DIAGRAM
                        SELECT 'initial', COUNT(DISTINCT patid), '`+ site +`'
                        FROM pat_incld;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    var consort_run = snowflake.createStatement({sqlText:consort_par});
    sqlstmt_run.execute();
    consort_run.execute();
    
    // exclusion 
    var sqlstmt_par = `CREATE OR REPLACE TEMPORARY TABLE pat_excld AS
                         SELECT distinct patid FROM `+ site_cdm +`.DEID_DIAGNOSIS 
                         WHERE dx LIKE '556.2%' OR dx LIKE '556.4%' OR dx LIKE 'K51.2%' OR dx LIKE 'K51.4%';`
                        
    var consort_par = `INSERT INTO CONSORT_DIAGRAM
                        SELECT 'exclude-age', COUNT(DISTINCT patid), '`+ site +`' FROM pat_incld WHERE age_at_index > 18 
                        UNION
                        SELECT 'exclude-dx', COUNT(DISTINCT patid), '`+ site +`' FROM pat_excld;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    var consort_run = snowflake.createStatement({sqlText:consort_par});
    sqlstmt_run.execute();
    consort_run.execute();
    
    // eligible
    var sqlstmt_par = `CREATE OR REPLACE TEMPORARY TABLE pat_elig AS
                        SELECT a.* FROM pat_incld a
                        WHERE a.AGE_AT_INDEX < 18 AND -- strictly less than 18
                              NOT EXISTS (SELECT 1 from pat_excld WHERE pat_excld.patid = a.patid);`
        
    var consort_par = `INSERT INTO CONSORT_DIAGRAM
                        SELECT 'eligible', COUNT(DISTINCT patid), '`+ site +`'
                        FROM pat_elig;`
                                 
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    var consort_run = snowflake.createStatement({sqlText:consort_par});
    sqlstmt_run.execute();
    consort_run.execute();

    // table 1 insertion
    var insert_table1 = `INSERT INTO PED_UC_TABLE1
                            SELECT * FROM pat_elig;`
    var insert_table1_run = snowflake.createStatement({sqlText:insert_table1});
    insert_table1_run.execute();
}
$$
;

call get_table1(array_construct(
     'ALLINA'
    ,'IHC'
    ,'MCRI'
    ,'MCW'
    ,'KUMC'
    ,'MU'
    ,'UTHSCSA'
    ,'UTSW'
    ,'UTHOUSTON'
    ,'WASHU'
));

-- quick stats pretty printout
with site_N as (
    select site, count(distinct patid) AS N from PED_UC_TABLE1 group by site
),  all_N as (
    select count(distinct patid) AS N from PED_UC_TABLE1
)
select * from (
    select '1_N' summ_var, tbl1.site, count(distinct tbl1.patid) || ' (' || round(count(distinct tbl1.patid)/n.N*100) ||'%)' AS summ_val 
    from PED_UC_TABLE1 tbl1, site_N n where tbl1.site = n.site  group by tbl1.site,n.N
    union
    select '1_N' summ_var, 'ALL', count(distinct tbl1.patid) || ' (' || round(count(distinct tbl1.patid)/n.N*100) ||'%)' AS summ_val 
    from PED_UC_TABLE1 tbl1 cross join all_N n group by n.N
    union
    select '2_age_at_index', site, round(avg(age_at_index)) || ' (' || round(stddev(age_at_index),1) ||')' 
    from PED_UC_TABLE1 group by site
    union
    select '2_age_at_index', 'ALL', round(avg(age_at_index)) || ' (' || round(stddev(age_at_index),1) ||')' 
    from PED_UC_TABLE1
    union
    select '3_sex:' || tbl1.sex, tbl1.site, count(distinct tbl1.patid) || ' (' || round(count(distinct tbl1.patid)/n.N*100) ||'%)' 
    from PED_UC_TABLE1 tbl1, site_N n where tbl1.site = n.site  group by tbl1.sex,tbl1.site,n.N
    union
    select '3_sex:' || tbl1.sex, 'ALL', count(distinct tbl1.patid) || ' (' || round(count(distinct tbl1.patid)/n.N*100) ||'%)' 
    from PED_UC_TABLE1 tbl1 cross join all_N n group by tbl1.sex, n.N
    union
    select '4_race:' || tbl1.race, tbl1.site, count(distinct tbl1.patid) || ' (' || round(count(distinct tbl1.patid)/n.N*100) ||'%)'  
    from PED_UC_TABLE1 tbl1, site_N n where tbl1.site = n.site  group by tbl1.race,tbl1.site,n.N
    union
    select '4_race:' || tbl1.race, 'ALL', count(distinct tbl1.patid) || ' (' || round(count(distinct tbl1.patid)/n.N*100) ||'%)'  
    from PED_UC_TABLE1 tbl1 cross join all_N n  group by tbl1.race,n.N
    union
    select '5_hispanic:' || tbl1.hispanic, tbl1.site, count(distinct tbl1.patid) || ' (' || round(count(distinct tbl1.patid)/n.N*100) ||'%)'  
    from PED_UC_TABLE1 tbl1, site_N n where tbl1.site = n.site group by tbl1.hispanic,tbl1.site,n.N
    union
    select '5_hispanic:' || tbl1.hispanic, 'ALL', count(distinct tbl1.patid) || ' (' || round(count(distinct tbl1.patid)/n.N*100) ||'%)'  
    from PED_UC_TABLE1 tbl1 cross join all_N n group by tbl1.hispanic,n.N
) pivot (
    max(summ_val) for site in (
         'ALLINA'
        ,'IHC'
        ,'MCRI'
        ,'MCW'
        ,'KUMC'
        ,'MU'
        ,'UTHSCSA'
        ,'UTSW'
        ,'UTHOUSTON'
        ,'WASHU'
        ,'ALL'))
    AS p( SUMM_VAR
         ,SITE1
         ,SITE2
         ,SITE3
         ,SITE4
         ,SITE5
         ,SITE6
         ,SITE7
         ,SITE8
         ,SITE9
         ,SITE10
         ,"ALL")
order by summ_var
;