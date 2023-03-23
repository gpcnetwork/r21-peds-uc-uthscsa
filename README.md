# Predicting Outcomes for Children with Ulcerative Colitis

Funding agency: NIH/NIDDK <br/>
Funding period: 07/2021 - 06/2023 <br/>
PI: Wang Zhu (UTHSCSA) <br/>
Co-I: Xing Song (MU); Jeffrey Hyams (UTHSCSA) <br/>
NIH RePORT site: https://reporter.nih.gov/search/kLDROKswLEKLHUQDAltx2w/project-details/10286334  <br/>
DROC request: #95 <br/>

### Study Overview
Ulcerative colitis (UC) is a chronic intestinal disorder, a type of inﬂammatory bowel disease (IBD). UC causes abdominal pain, diarrhea, bleeding and weight loss. Each year there are about 38,000 new UC cases with about 25-30% being children. UC is often more severe in children than adults. There is no standard regimen for managing all people with UC. Treatment of UC includes 5-aminosalicylic acid (5-ASA), corticosteroids, immunomodulators, biologics and surgery to remove the colon (colectomy). Ideally a personalized medicine approach should be taken to tailor therapy to the individual patient based on their disease severity and predicted response. However, there is currently a lack of guidance to clinicians as to which children are going to do well with these medications and who needs more medication exposure or surgery. To address this important issue, a clinical trial of standardized medical therapy was launched enrolling 428 children newly diagnosed with UC at 29 pediatric medical centers in North America (Predicting Response to Standardized Pediatric Colitis Therapy: The [PROTECT](https://pubmed.ncbi.nlm.nih.gov/28939374/) Study). The PROTECT study collected clinical, genetic, environment and immune factors along with biospecimens including blood, stool, and colonic tissue. It was anticipated that a combination of clinical, genetic, and immunologic tests performed at diagnosis can construct a valuable predictive model for personalized medicine which can be implemented to improve clinical outcomes such as early and late remission on 5-ASA only without the concurrent use of steroid medications. Along this line, state-of-art machine learning algorithms can ultimately utilize the [PROTECT](https://pubmed.ncbi.nlm.nih.gov/28939374/) study data and more accurately predict clinical outcomes. The ability to develop accurate predictive models using basic clinical, endoscopic, histologic, and laboratory data would be extremely helpful to clinicians, as well as to patients/families in helping them better understand the decision making process. 

To validate model generalizability, PI proposed to use GPC EHR data as external validation dataset. However, there exists apparent data gaps between the prospectively collected PROTECT data and retrospectively collected GPC EHR data, while GPC EHR data is more reflective of common data elements 

### GPC Site Scope of Work
Participating sites from the Greater Plains Collaborative (GPC) will be performing the following: 

1.	Participating to the GPC Reuseable Observable Unified Study Environment (GROUSE) project and contributing site CDM datamart on an annual basis. This includes performing necessary administrative work (e.g., signing or amending DUA) to support the project. 
2.	Since this study will only use de-identified data, PI and Co-I sites are expected to request Non-Human-Subject (NHS) determination from local IRB which should be submitted along with GPC DROC request. 

The study cohort will be identified by International Classification of Disease (ICD) codes for a pediatric population with Ulcerative Colitis at the participating sites from 2010-2019, and we will also collect multiple clinical variables from all de-identified CDM tables for analysis. 

### Cohort Selection
#### Inclusion criteria
Any patients with at least 1 of the following ICD codes:
-	ICD9: 556.XX (ulcerative enterocolitis), which includes all the sub-codes 556
-	ICD10: K51.XX (ulcerative colitis), which includes all the sub-codes of K51
-	age at first one of the above diagnoses is below (<) 18 years old

#### Exclusion criteria
Existence of any of the following diagnoses: 
-	ICD9: Any 556.2X (ulcerative chronic proctitis) or 556.4X (pseudopolyposis of colon)
-   ICD10: Any of K51.2X (ulcerative chronic proctitis) or K51.4X (inflammatory polyps of colon)

### Non-Human-Subject Determination
Only De-identified GROUSE data from approval sites will be accessed by the approved study team member. This study has been determined by both leading and coordinating sites as Non-HUman-Subject research: 
- [IRB NHS determination - UTHSCSA - PI site](doc/IRB_UTHSCSA_NHS_apporval.pdf)
- [IRB NHS determination - MU - Co-I site](doc/IRB_MU_NHS_apporval.pdf) 

Based on DROC#95 responses, the following 10 GPC sites have agreed to participate in the study, with eligible cohort size ranked as below:  
| Index | GPC site | Cohort Size |
| --- | -------- | ----------- |
| 1 | IHC | 729 |
| 2 | WASHU | 666 |
| 3 | UTSW | 301 |
| 4 | MU | 157 | 
| 5 | UTH | 141 |
| 6 | KUMC | 92 |
| 7 | MCRI | 92 |
| 8 | ALLINA | 49 | 
| 9 | MCW | 49 |
| 10 | UTHSCSA | 20 |
| Total | GPC | 2,296 |  

### Endpoint assertainment
Covariates pre-selection: 

### Analysis I: 

