create or replace table consort_diagram (
    CRITERION varchar(20) NOT NULL,
    PAT_CNT integer,
    SITE varchar(10)        
);

create or replace table PED_UC_TABLE1 (
    PATID varchar(20) NOT NULL,
    INDEX_DATE date NOT NULL,
    BIRTH_DATE date NOT NULL,
    AGE_AT_INDEX integer NOT NULL,
    SEX varchar(1),
    RACE varchar(2),
    HISPANIC varchar(3),
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
    var site_cdm = (site === 'MU') ? 'PCORNET_CDM.CDM_2022_MARCH' : 'PCORNET_CDM_' + site;
    
    // initial inclusion
    var sqlstmt_par = `CREATE OR REPLACE TEMPORARY TABLE pat_incld AS
                        SELECT a.patid, b.birth_date, MIN(NVL(a.dx_date,a.admit_date)) AS index_date,
                               datediff(day,b.birth_date,MIN(NVL(a.dx_date,a.admit_date)))/365.25 AS age_at_index, 
                               b.sex, b.race, b.hispanic, '`+ site +`' AS site
                        FROM `+ site_cdm +`.DIAGNOSIS a
                        JOIN `+ site_cdm +`.DEMOGRAPHIC b
                        ON a.patid = b.patid
                        WHERE dx LIKE '556%' OR dx LIKE 'K51%'
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
                         SELECT distinct patid FROM `+ site_cdm +`.DIAGNOSIS 
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
                        WHERE a.AGE_AT_INDEX <= 18 AND
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


call get_table1(array_construct('MU','MCW','ALLINA','UU'));