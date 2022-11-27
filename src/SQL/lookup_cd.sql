/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: lookup_cd.sql
# Description: valueset lookup using rxnorm and loinc metathesaurus
*/

/****************************************************
Therapeutic Medications
*****************************************************/
-- look up Rxnorm by generic name
create or replace table ConceptSet_TRT_Med_RXCUI as
select distinct
       STR as GN
      ,RXCUI
      ,SUPPRESS
      ,'corticosteroids' AS THERAPY
      ,TTY
from ontology.rxnorm.rxnconso
where (lower(STR) like '%hydrocortisone%' or
       lower(STR) like '%cortisone%' or
       lower(STR) like '%ethamethasoneb%' or
       lower(STR) like '%prednisone%' or
       lower(STR) like '%prednisolone%' or
       lower(STR) like '%triamcinolone%' or
       lower(STR) like '%methylprednisolone%' or
       lower(STR) like '%dexamethasone%' or
       lower(STR) like '%fludrocortisone%'
      ) and
      (TTY like 'SCD%' or --Semantic Clinical Drug
       TTY like '%IN')
union all
select distinct
       STR
      ,RXCUI
      ,SUPPRESS
      ,'mesalazine'
      ,TTY
from ontology.rxnorm.rxnconso
where (lower(STR) like '%mesalazine%' or
       lower(STR) like '%mesalamine%'
      ) and
      (lower(STR) not like '%rectal%' and
       lower(STR) not like '%enema%') and 
      (TTY like 'SCD%' or --Semantic Clinical Drug
       TTY like '%IN')
union all
select distinct
       STR
      ,RXCUI
      ,SUPPRESS
      ,'mesalazine'
      ,TTY
from ontology.rxnorm.rxnconso
where (lower(STR) like '%pentasa%' or
       lower(STR) like '%apriso%' or
       lower(STR) like '%colazal%' or
       lower(STR) like '%asacol%' or
       lower(STR) like '%sulfasalaz%' or
       lower(STR) like '%lialda%' or
       lower(STR) like '%delzicol%'
       ) 
      and
      (lower(STR) not like '%rectal%' and
       lower(STR) not like '%enema%')
      and      
      TTY like 'SBD%' --Semantic Branded Drug
union all
select distinct
       STR
      ,RXCUI
      ,SUPPRESS
      ,'biologics'
      ,TTY
from ontology.rxnorm.rxnconso
where (lower(STR) like '%infliximab%' or
       lower(STR) like '%adalimumab%' or
       lower(STR) like '%vedolizumab%' or
       lower(STR) like '%ustekinumab%'
      ) and
      (TTY like 'SCD%' or --Semantic Clinical Drug
       TTY like '%IN')
union all
select distinct
       STR
      ,RXCUI
      ,SUPPRESS
      ,'biologics'
      ,TTY
from ontology.rxnorm.rxnconso
where (lower(STR) like '%humira%' or
       lower(STR) like '%amgevita%' or
       lower(STR) like '%hulio%' or
       lower(STR) like '%hadlima%' or
       lower(STR) like '%hyrimoz%' or
       lower(STR) like '%idacio%' or
       lower(STR) like '%irmaldi%' or
       lower(STR) like '%remicade%' or
       lower(STR) like '%inflectra%' or
       lower(STR) like '%renflexis%' or
       lower(STR) like '%avsola%' or
       lower(STR) like '%entyvio%' or
       lower(STR) like '%stelara%'
      ) and
      TTY like 'SBD%' --Semantic Branded Drug
union all
select distinct
       STR
      ,RXCUI
      ,SUPPRESS
      ,'immunomodulator'
      ,TTY
from ontology.rxnorm.rxnconso
where (lower(STR) like '%azathioprine%' or
       lower(STR) like '%mercaptopurine%' or
       lower(STR) like '%methotrexate%'
      ) and
      (TTY like 'SCD%' or --Semantic Clinical Drug
       TTY like '%IN')
;

-- look up NDC by Rxnorm
create or replace table ConceptSet_TRT_Med_NDC as
select distinct
       rxn.THERAPY
      ,rxn.RXCUI
      ,rxn.GN
      ,rxmap.ATV as NDC
      ,rxmap.SUPPRESS
from ConceptSet_TRT_Med_RXCUI rxn
join ontology.rxnorm.rxnsat rxmap
on rxn.RXCUI = rxmap.RXCUI and
   rxmap.ATN = 'NDC'and rxmap.SAB = 'RXNORM' -- normalized 11-digit NDC codes
union all
select distinct
       rxn.THERAPY
      ,rxn.RXCUI
      ,rxn.GN
      ,rxmap.ATV as NDC
      ,rxmap.SUPPRESS
from ConceptSet_TRT_Med_RXCUI rxn
join ontology.rxnorm.rxnsat rxmap
on rxn.RXCUI = rxmap.RXCUI and
   rxmap.ATN = 'NDC'and rxmap.SAB = 'RXNORM' -- normalized 11-digit NDC codes
union all
select distinct
       rxn.THERAPY
      ,rxn.RXCUI
      ,rxn.GN
      ,rxmap.ATV as NDC
      ,rxmap.SUPPRESS
from ConceptSet_TRT_Med_RXCUI rxn
join ontology.rxnorm.rxnsat rxmap
on rxn.RXCUI = rxmap.RXCUI and
   rxmap.ATN = 'NDC'and rxmap.SAB = 'RXNORM' -- normalized 11-digit NDC codes
;

select * from ontology.rxnorm.rxnsat;

select therapy,count(distinct GN) med_cnt 
from ConceptSet_TRT_Med_RXCUI
group by therapy;

select therapy, count(distinct ndc) ndc_cnt
from ConceptSet_TRT_Med_NDC
group by therapy;

select * from ConceptSet_TRT_Med_RXCUI;
select * from ConceptSet_TRT_Med_NDC
where therapy = 'mesalazine';


/****************************************************
Covariate labs
*****************************************************/
create or replace table conceptset_labs_loinc as
select distinct
       'hemoglobin' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%hemoglobin%'
union
select distinct
       'platelet_count' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%platelet%count%'
union
select distinct
       'leukocyte' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%leukocyte%'
union
select distinct
       'albumin' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%albumin%'
union
select distinct
       'calprotectin' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%calprotectin%'
union
select distinct
       'ESR' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%erythrocyte%sedimentation%rate%'
union
select distinct
       'CRP' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where  lower(long_common_name) like '%c%reactive%protein%'
union
select distinct
       '25_OH_VD' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%25%hydroxyvitamin%d3%'
union
select distinct
       'neutrophil' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%neutrophil%'
union
select distinct
       'monocyte' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%monocyte%'
union
select distinct
       'lymphocyte' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%lymphocyte%'
union
select distinct
       'basophil' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%basophil%'
union
select distinct
       'eosinophil' as lab_name,
       loinc_num,
       component,
       long_common_name,
       time_aspct,
       system,
       scale_typ,
       class,
       status
from ONTOLOGY.LOINC.LOINC_V2_17
where lower(long_common_name) like '%eosinophil%'
;      