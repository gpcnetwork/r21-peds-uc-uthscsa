/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: table1.sql
# Description: collect endpoints
# Dependency: lookup_cd.sql
*/

-- select * from conceptset_trt_med_rxcui;
-- select * from conceptset_trt_med_ndc;

create or replace table PED_UC_ENDPT (
    PATID varchar(50) NOT NULL,
    INDEX_DATE date NOT NULL,
    ENDPT varchar(50) NULL,
    ENDPT_DATE date NULL,
    ENDPT_DAYS_SINCE_INDEX integer,
    SITE varchar(10) NOT NULL,
    constraint pk primary key(PATID, ENDPT)
);


create or replace procedure get_endpoints(SITES array)
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
    
    // endpoint 1 - colectomy
    var sqlstmt_par = `CREATE OR REPLACE TEMPORARY TABLE pat_endpt1 AS
                        SELECT a.patid, a.index_date, a.site, min(px.px_date) AS first_colectomy_date
                        FROM PED_UC_TABLE1 a
                        JOIN `+ site_cdm +`.DEID_PROCEDURES px
                        ON a.patid = px.patid AND a.site = '`+ site +`'
                        WHERE  (px in ('44121','44139','44140','44141','44143','44144','44145','44146','44147',
                                     '44150','44151','44152','44153','44154','44155','44156','44157','44158','44160',
                                     '44204','44205','544206','44207','44208','44210','44211','44212','44213'
                                     ) or
                               (px like '45.8%' or px like '17.3%')  or
                               (px like '0DP%' or px like '0DT%')
                              )
                              AND 
                              px.px_date >= a.index_date
                        GROUP BY a.patid, a.index_date, a.site;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    sqlstmt_run.execute();
    
    // endpoint 2 - therapies
    var sqlstmt_par = `CREATE OR REPLACE TEMPORARY TABLE pat_endpt2 AS
                        SELECT a.patid, a.index_date, a.site, cd.therapy, NVL(min(p.rx_start_date),min(p.rx_order_date)) AS first_med_date
                        FROM PED_UC_TABLE1 a
                        JOIN `+ site_cdm +`.DEID_PRESCRIBING p
                        ON a.patid = p.patid AND a.site = '`+ site +`'
                        JOIN conceptset_trt_med_rxcui cd
                        ON p.RXNORM_CUI = cd.RXCUI
                        WHERE p.rx_start_date >= a.index_date::date - 30
                        GROUP BY a.patid, a.index_date, a.site, cd.therapy
                        UNION
                        SELECT a.patid, a.index_date, a.site, cd.therapy, min(d.dispense_date)
                        FROM PED_UC_TABLE1 a
                        JOIN `+ site_cdm +`.DEID_DISPENSING d
                        ON a.patid = d.patid AND a.site = '`+ site +`'
                        JOIN conceptset_trt_med_ndc cd
                        ON d.NDC = cd.NDC
                        WHERE d.dispense_date >= a.index_date::date - 30
                        GROUP BY a.patid, a.index_date, a.site, cd.therapy
                        UNION
                        SELECT a.patid, a.index_date, a.site, cd.therapy, min(m.medadmin_start_date)
                        FROM PED_UC_TABLE1 a
                        JOIN `+ site_cdm +`.DEID_MED_ADMIN m
                        ON a.patid = m.patid AND a.site = '`+ site +`'
                        JOIN conceptset_trt_med_rxcui cd
                        ON m.medadmin_code = cd.RXCUI AND m.medadmin_type = 'RX'
                        WHERE m.medadmin_start_date >= a.index_date::date - 30
                        GROUP BY a.patid, a.index_date, a.site, cd.therapy                        
                        UNION
                        SELECT a.patid, a.index_date, a.site, cd.therapy, min(m.medadmin_start_date)
                        FROM PED_UC_TABLE1 a
                        JOIN `+ site_cdm +`.DEID_MED_ADMIN m
                        ON a.patid = m.patid AND a.site = '`+ site +`'
                        JOIN conceptset_trt_med_ndc cd
                        ON m.medadmin_code = cd.NDC AND m.medadmin_type = 'ND'
                        WHERE m.medadmin_start_date >= a.index_date::date - 30
                        GROUP BY a.patid, a.index_date, a.site, cd.therapy
                        ;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    sqlstmt_run.execute();
    
    // endpoint 3 - censor date
    var sqlstmt_par = `CREATE OR REPLACE TEMPORARY TABLE pat_endpt3 AS
                        WITH censor_stk AS (
                            SELECT a.patid, a.index_date, a.site, NVL(max(e.admit_date), max(e.dx_date)) AS censor_date
                            FROM PED_UC_TABLE1 a
                            JOIN `+ site_cdm +`.DEID_DIAGNOSIS e
                            ON a.patid = e.patid AND a.site = '`+ site +`'
                            WHERE NVL(e.admit_date, e.dx_date) >= a.index_date
                            GROUP BY a.patid, a.index_date, a.site
                            UNION
                            SELECT a.patid, a.index_date, a.site, NVL(max(e.admit_date), max(e.px_date)) AS censor_date
                            FROM PED_UC_TABLE1 a
                            JOIN `+ site_cdm +`.DEID_PROCEDURES e
                            ON a.patid = e.patid AND a.site = '`+ site +`'
                            WHERE NVL(e.admit_date, e.px_date) >= a.index_date
                            GROUP BY a.patid, a.index_date, a.site
                            UNION
                            SELECT a.patid, a.index_date, a.site, NVL(max(e.rx_start_date), max(e.rx_order_date)) AS censor_date
                            FROM PED_UC_TABLE1 a
                            JOIN `+ site_cdm +`.DEID_PRESCRIBING e
                            ON a.patid = e.patid AND a.site = '`+ site +`'
                            WHERE NVL(e.rx_start_date, e.rx_order_date) >= a.index_date
                            GROUP BY a.patid, a.index_date, a.site
                        )
                        SELECT patid, index_date, site, max(censor_date) AS censor_date
                        FROM censor_stk
                        GROUP BY patid, index_date, site
                        ;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    sqlstmt_run.execute();

    // endpoint table insertion
    var insert_endpt = `INSERT INTO PED_UC_ENDPT
                            SELECT ep1.patid, ep1.index_date,'colectomy', 
                                   ep1.first_colectomy_date, datediff(day,ep1.index_date,ep1.first_colectomy_date),
                                   ep1.site
                            FROM pat_endpt1 ep1
                            UNION
                            SELECT ep2.patid, ep2.index_date,ep2.therapy, 
                                   ep2.first_med_date, datediff(day,ep2.index_date,ep2.first_med_date),
                                   ep2.site
                            FROM pat_endpt2 ep2
                            UNION
                            SELECT ep3.patid, ep3.index_date, 'censor', 
                                   ep3.censor_date, datediff(day,ep3.index_date,ep3.censor_date), ep3.site
                            FROM pat_endpt3 ep3
                            ;`
    var insert_table1_run = snowflake.createStatement({sqlText:insert_endpt});
    insert_table1_run.execute();
}
$$
;

call get_endpoints(array_construct(
     'ALLINA'
    ,'IHC'
    ,'MCRI'
    ,'MCW'
    ,'KUMC'
    ,'MU'
    ,'UTHSCSA'
    ,'UTSW'
    ,'UTHOUSTON'
    ,'WASHU'));

-- select * from PED_UC_ENDPT;

-- quick stats pretty printout
with site_N as (
    select site, count(distinct patid) AS N from PED_UC_TABLE1 group by site
),  all_N as (
    select count(distinct patid) AS N from PED_UC_TABLE1
)
select * from (
    select '1_N' summ_var, tbl1.site, count(distinct tbl1.patid) || ' (' || round(count(distinct tbl1.patid)/n.N*100) ||'%)' AS summ_val 
    from PED_UC_TABLE1 tbl1 join site_N n on tbl1.site = n.site group by tbl1.site,n.N
    union
    select '1_N' summ_var, 'ALL', count(distinct tbl1.patid) || ' (' || round(count(distinct tbl1.patid)/n.N*100) ||'%)' AS summ_val
    from PED_UC_TABLE1 tbl1 cross join all_N n group by n.N
    union
    select '2_' || ep.ENDPT || '_w', ep.site, count(distinct ep.patid) || ' (' || round(count(distinct ep.patid)/n.N*100) ||'%)' AS summ_val 
    from PED_UC_ENDPT ep join site_N n on ep.site = n.site group by ep.ENDPT, ep.site, n.N
    union
    select '2_' || ep.ENDPT || '_w', 'ALL', count(distinct patid) || ' (' || round(count(distinct ep.patid)/n.N*100) ||'%)' AS summ_val 
    from PED_UC_ENDPT ep cross join all_N n group by ep.ENDPT, n.N
    union
    select '2_' || ep.ENDPT || '_wo', ep.site, n.N - count(distinct ep.patid)  || ' (' || round((1-count(distinct ep.patid)/n.N)*100) ||'%)' AS summ_val 
    from PED_UC_ENDPT ep join site_N n on ep.site = n.site group by ep.ENDPT, ep.site, n.N
    union
    select '2_' || ep.ENDPT || '_wo', 'ALL', n.N - count(distinct ep.patid)  || ' (' || round((1-count(distinct ep.patid)/n.N)*100) ||'%)' AS summ_val 
    from PED_UC_ENDPT ep cross join all_N n group by ep.ENDPT, n.N
) pivot (
    max(summ_val) for site in ( 
        -- 'ALLINA'
         'IHC'
        ,'MCRI'
        ,'MCW'
        ,'KUMC'
        ,'MU'
        ,'UTHSCSA'
        ,'UTSW'
        ,'UTHOUSTON'
        ,'WASHU'
        ,'ALL'
    ))
    AS p(SUMM_VAR,/*SITE1,*/SITE2,SITE3,SITE4,SITE5,SITE6,SITE7,SITE8,SITE9,SITE10,"ALL")
order by summ_var
;
