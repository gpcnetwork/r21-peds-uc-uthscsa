-- select * from conceptset_trt_med_rxcui;
-- select * from conceptset_trt_med_ndc;

create or replace table PED_UC_ENDPT (
    PATID varchar(50) NOT NULL,
    INDEX_DATE date NOT NULL,
    ENDPT varchar(50) NULL,
    ENDPT_DATE date NULL,
    CENSOR_DATE date NULL,
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
    var site_cdm = (site === 'MU') ? 'PCORNET_CDM.CDM_2022_APRIL' : 'PCORNET_CDM_' + site;
    
    // endpoint 1 - colectomy
    var sqlstmt_par = `CREATE OR REPLACE TEMPORARY TABLE pat_endpt1 AS
                        SELECT a.patid, a.index_date, a.site, min(px.px_date) AS first_colectomy_date
                        FROM PED_UC_TABLE1 a
                        JOIN `+ site_cdm +`.PROCEDURES px
                        ON a.patid = px.patid AND a.site = '`+ site +`'
                        WHERE px in ('1007468','1007463','44155','44156','44151','44150','44212','44210','44157','44158','44211') AND 
                              px.px_date >= a.index_date
                        GROUP BY a.patid, a.index_date, a.site;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    sqlstmt_run.execute();
    
    // endpoint 2 - therapies
    var sqlstmt_par = `CREATE OR REPLACE TEMPORARY TABLE pat_endpt2 AS
                        SELECT a.patid, a.index_date, a.site, cd.therapy, NVL(min(p.rx_start_date),min(p.rx_order_date)) AS first_med_date
                        FROM PED_UC_TABLE1 a
                        JOIN `+ site_cdm +`.PRESCRIBING p
                        ON a.patid = p.patid AND a.site = '`+ site +`'
                        JOIN conceptset_trt_med_rxcui cd
                        ON p.RXNORM_CUI = cd.RXCUI
                        WHERE p.rx_start_date >= a.index_date
                        GROUP BY a.patid, a.index_date, a.site, cd.therapy
                        UNION
                        SELECT a.patid, a.index_date, a.site, cd.therapy, min(d.dispense_date)
                        FROM PED_UC_TABLE1 a
                        JOIN `+ site_cdm +`.DISPENSING d
                        ON a.patid = d.patid AND a.site = '`+ site +`'
                        JOIN conceptset_trt_med_ndc cd
                        ON d.NDC = cd.NDC
                        WHERE d.dispense_date >= a.index_date
                        GROUP BY a.patid, a.index_date, a.site, cd.therapy
                        UNION
                        SELECT a.patid, a.index_date, a.site, cd.therapy, min(m.medadmin_start_date)
                        FROM PED_UC_TABLE1 a
                        JOIN `+ site_cdm +`.MED_ADMIN m
                        ON a.patid = m.patid AND a.site = '`+ site +`'
                        JOIN conceptset_trt_med_rxcui cd
                        ON m.medadmin_code = cd.RXCUI AND m.medadmin_type = 'RX'
                        WHERE m.medadmin_start_date >= a.index_date
                        GROUP BY a.patid, a.index_date, a.site, cd.therapy                        
                        UNION
                        SELECT a.patid, a.index_date, a.site, cd.therapy, min(m.medadmin_start_date)
                        FROM PED_UC_TABLE1 a
                        JOIN `+ site_cdm +`.MED_ADMIN m
                        ON a.patid = m.patid AND a.site = '`+ site +`'
                        JOIN conceptset_trt_med_ndc cd
                        ON m.medadmin_code = cd.NDC AND m.medadmin_type = 'ND'
                        WHERE m.medadmin_start_date >= a.index_date
                        GROUP BY a.patid, a.index_date, a.site, cd.therapy
                        ;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    sqlstmt_run.execute();
    
    // endpoint 3 - censor date
    var sqlstmt_par = `CREATE OR REPLACE TEMPORARY TABLE pat_endpt3 AS
                        SELECT a.patid, a.index_date, a.site, NVL(max(e.admit_date), max(e.discharge_date)) AS censor_date
                        FROM PED_UC_TABLE1 a
                        JOIN `+ site_cdm +`.ENCOUNTER e
                        ON a.patid = e.patid AND a.site = '`+ site +`'
                        WHERE e.discharge_date >= a.index_date
                        GROUP BY a.patid, a.index_date, a.site;`
    
    var sqlstmt_run = snowflake.createStatement({sqlText:sqlstmt_par});
    sqlstmt_run.execute();

    // endpoint table insertion
    var insert_endpt = `INSERT INTO PED_UC_ENDPT
                            SELECT ep1.patid, ep1.index_date,'colectomy', ep1.first_colectomy_date, ep3.censor_date, ep1.site
                            FROM pat_endpt1 ep1
                            LEFT JOIN  pat_endpt3 ep3 ON ep1.patid = ep3.patid AND ep1.site = ep3.site
                            UNION
                            SELECT ep2.patid, ep2.index_date,ep2.therapy, ep2.first_med_date, ep3.censor_date, ep2.site
                            FROM pat_endpt2 ep2
                            LEFT JOIN  pat_endpt3 ep3 ON ep2.patid = ep3.patid AND ep2.site = ep3.site
                            UNION
                            SELECT ep3.patid, ep3.index_date, 'censor', NULL, ep3.censor_date, ep3.site
                            FROM pat_endpt3 ep3
                            WHERE NOT EXISTS (SELECT 1 FROM pat_endpt1 ep1 WHERE ep1.patid = ep3.patid) AND
                                  NOT EXISTS (SELECT 1 FROM pat_endpt2 ep2 WHERE ep2.patid = ep3.patid)
                            ;`
    var insert_table1_run = snowflake.createStatement({sqlText:insert_endpt});
    insert_table1_run.execute();
}
$$
;

call get_endpoints(array_construct('MU','MCW','ALLINA','UU'));