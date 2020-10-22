# ModelRepository
Joint project of EFSA, Federal Institute For Risk Assessment, DTU and ANSES to create an online model repository.

# content

## currently implemented models
### Nauta EFSA opinion Risk Reduction model of campylobacter in ready-to-eat chicken salad

containing
* 8 Consumer phase models
* 3 Dose Response models
* 1 Relative Risk Reduction Model based on above mentioned CPMs and DRMs
* 3 examples of joined CPM/DRM models 

#### Status
pending... waiting for Expert review (Nauta)
#### Reference
https://efsa.onlinelibrary.wiley.com/doi/full/10.2903/j.efsa.2020.6090 

### Lindqvist EFSA opinion
containing qmra model on listeria in several food types
#### Reference 
doi: 10.2903/j.efsa.2020.6092

#### Status
pending: waiting for Expert response (Lindqvist)
### Swart Taenia model
containing Taenia solium risk analysis model

#### Reference
...

#### Status
pending: waiting for expert review (Swart)

### Ranta models
containing
* chemical dose response model
* bayesian consumer phase model on campylobacter in chicken

#### Reference
....

#### Status
pending: waiting for expert review (Ranta)

## plan for next days

* support Petra on supply chain models
* Gropin
  * transfer complete annotation data to all models
    * mostly done, bugfixing for rare cases and parameter annotation
  * bugfixing rare cases model script transfer errors due to inconsistencies in gropin database
  * bugfixing visualisation for 4 or more variables
  * create transfer script for "time to multiply by X log steps"-models for all models
  * analyse macros in gropin 
  * create transfer script for "kinetic modelling" for growth models for all models
  * GNG models
* test all finished models on VRE

### 22/10
* gropin
  * visualisation for 4 or more parameters
  * asking developers about fsk-lab behaviour(Editor deletes simulations that are not default)
  * bugfixing transfer script
    * questions for devs:
      * MD schema editing data type for parameter DOUBLE or what?
      * MD schema date format?


## done so far


### 21/10
* gropin
  * bugfixing metadata schema editing
  * visualisation for 4 parameters approach, but still buggy


### 20/10
* Lindqvist
  * finish adaptions to fskx file
  * write answer
* gropin
  * edit uploaded model, done but due to unexpected fsk-lab behaviour, cannot be edited properly -> ask developers!
  * write questions for gropin creator
  * bugfixing transfer script




### 19/10
* support Petra on supply chain models
  * concept plan for hdf5 input of model
  * discussed concept of super/meta nodes network for product conversion 
* Lindqvist Model
  * handling comments from lindqvist

### 16/10
* gropin
  * finishing showcase model
  * bugfixing transfer script: metadata information ist transferred from gropinDB

### 15/10
* gropin
  * creating showcase model with all 3 modes: response surface, time2multiply, kinetic model 
  * bugfixing transfer script

### 14/10
* gropin
  * bugfixing transfer script (creating 360 annotation schemas, modelscripts and visualisation scripts) for response surface model
  * created knime workflow for taking these 360*3 fsk model components to build fskx models
* support Petra on supply chain models

### 13/10
* gropin
  * implementing conditions for growth models with 1,2 variables
  * bugfixing model creation for 3-variable-models
  * found non-functioning models, compiling a list of models not ready for transfer
  * ongoing: checking conditions for models with 4 variables and more... done
  * finished first draft of creating model scripts, visualisation scripts and metadata schema sheets for fskx -> next step: write knime workflow for creator node to create fskx files!
  
### 12/10
* gropin
  * reproducing kinetic models failed
    * model equations are not shown in excel DB, probably hard coded into macros
    * looked into reference given in manual, recreated kinetic model, values are similar but not a match (differences 10-50%)
    * -> need to look into macro coding itself -> problem with BfR's "no macro!" policy -> Lars suggests to use VK-notebook
* supply chain models
  * support for expanding network model

### 09/10
* gropin
  * change of plans after first tests with knime -> simpler to create an R script to transfer all gropin models to R
  * successfully extracted data from gropin DB and ordered all models by type:
    * growth models
    * growth / no growth models
    * gamma models with interaction
    * inactivation models
    * lethality models
    * integrated models not included, they seem to need data -> investigation for later
  * findings: 
    * every model type needs their own transfer script!
    * within one model type, there several different applications and models in itself
    * starting small: applying transfer script to growth models and concentrating on response surface model application
    * kinetic model application (differential equation) has some parameters that seem to be hardcoded into the macro of gropin, no documentation available, maybe need to investigate in publications
    * finished response surface growth model with 3 parameters
    * problems with 
      * 1 parameter -> different plot type
      * 2 parameters -> no subset to choose
      * 4 or more parameters -> subset to choose is more than 1D
    
  

### 08/10
* gropin
  * transfer analysis -> start with 1 group of models and only 1 simple part of the modeling: response surface
  * plan for adopting a 1st part of the models:
    * creating knime workflow for creating R script
      * extract equations and parameters of GRT (growth models) and create response surface models
      * model script contains transformed equation
      * visualisation script contains a switch which response surface should be shown 
    * creating knime workflow for editing metadata schema
      * check if all mandatory fields are filled
      * parameters!
      * in a 2nd iteration fill all optional fields with information from database
  * next steps for GRT: kinetic models contain parameters, that are not explained in the user guide, maybe i'll find something in the corresponding papers

### 07/10
* supply chain models
  * created roadmap for Petra
  * model code review
* Lindqvist model
  * review of comments
* testing recording equipment

### 06/10
* supply chain models
  * support Petra
* Gropin
  * assessment and transfer analysis


### 05/10
* Gropin
  * assessment and transfer analysis

### 02/10
* Taenia model finished, sent Arno related mail

### 01/10
* Taenia model
  * continuing  analyis and transfer assessment
  * code adaptations done --> functioning fskx file
  * annotation
  
### 30/09
* supporting Petra with 
  * testing new FSK Lab version
  * lindqvist model rewrites
* supporting Lars with Knime server python issues

### 29/09
* addressing issues of RAKIPInitiative, testing, testing, testing

### 28/09
* Taenia model
  * code testing and transfer effort assessment
* tubs model
  * code testing and transfer assessment -> no easy way of integrating COMSOL models into fskx, would need to familiarize with COMSOL, then rewrite of models -> estimate 10-20d of work and access to COMSOL license
* jukka models and openBUGS
  * found openBUGS problem, 2 jobs using openBUGS running on the server simultaneously: reproducable problem -> Lars
* bugfixing lindqvist model
  * minor update to match image output with publication format


### 25/09
* jukka model
  * tests on internal server: fine
  * tests on external server: weird behaviour with openBUGS communication between knime, R and openBUGS - investigating
  * contacted Jukka and told him about models to check out

### 24/09
* jukka model
  * finished annotating

### 23/09
* fixing minor mistakes in DRM metadata schema 
* jukka model
  * test on server - failed, investigating
* general tests on VRE

### 22/09
* jukka microbial criterion model
  * annotate


### 21/09
* nauta
  * 3 example models CPM/DRM combinations
    * annotate
    * test on server



### 18/09
* jukka microbial criterion model
  * transfer R code to fskx
  * annotate


### 17/09
* nauta
  * 3 example models CPM/DRM combinations
    * copy&paste R code into fskx
    * test functionality
    * annotate
    * test on server

### 10/09
* lindqvist Listeria
  * visualisation -> table as in results2.xlsx ... DONE
  * output of xlsx -> remove ... DONE
  * contact lindqvist with Maarten in cc for support ... DONE
  * test on server: ... DONE
    * check which packages are needed after rewrite of code ... DONE
    * tell Lars which packages need to be installed ... DONE




#### 09/09

    * rewrite code for fskx transformation ... DONE
      * rewrite m.g.QMRA.R as function inside master.R script
    * Define input parameters ... DONE
      * number of runs
      * population group




#### 08/09
  * nauta efsa opinion
    * review and test of all CPMs, DRMs and RRM fskx files (execution and annotation) ... in progress
      * adapting DRM module versions with explanation ... in progress
    * test CPMs on server ... DONE
    * implement Maartens comments ... in progress
      * table for quantile results as an optional visualisation ... DONE
        * insert title (CPM/DRM/MS) ... DONE
        * transform probabilities into percentage values with 1 decimal ... DONE
      * random CPM/DRM combination (for every MC iteration, may need to restructure code) ... REJECTED (until further notice)
  * lindqvist Listeria
    * analysed R code ... DONE
    * test run ... DONE, success
    * rewrite code for fskx transformation ... in progress
      * rewrite m.g.QMRA.R as function inside master.R script
      * Define input parameters
        * number of runs
        * population group
      * visualisation -> table as in results2.xlsx
      * output of xlsx -> remove
      * test on server: 
        * check which packages are needed after rewrite of code
        * tell Lars which packages need to be installed


#### 31/08 - 01/09
  * review and test of all CPMs, DRMs and RRM fskx files (execution and annotation) ... in progress
    * CPMs insert author list ... DONE
    * CPMonly versions? remove unnecessary input/output parameters ... REJECTED
    * DRMs insert author list ... DONE
    * adapting DRM module versions with explanation ... in progress
    * RRM insert author list
  * test CPMs on server as soon as CPM is ready to be tested ... in progress
    * DRMs can be uploaded and run properly, but encounters problems in FSK-Web, all models run on server
  * implement Maartens comments ... in progress
    * table for quantile results as an optional visualisation
      * insert title (CPM/DRM/MS)
      * transform probabilities into percentage values with 1 decimal
    * random CPM/DRM combination (for every MC iteration, may need to restructure code)


##### 28/08
  * distribute new visualisation code to all CPMs and DRMs ... in progress
    * finishing CPMs ... DONE
    * finishing DRMs ... in progress
      * creating DRMonly versions ... DONE
      * adapting DRM module versions with explanation: low priority since joiner doesnt work properly
    * finishing RRM ... in progress
      * visualisation errorbars ... DONE
      * random CPM/DRM/MS... DONE
      * test in FSK-Lab ... DONE
  * test CPMs on server as soon as CPM is ready to be tested ... pending, waiting for developers
  * finalise RRM annotation (need some input see below) ... DONE
  * fix a number of known errors in CPM/DRM annotation ... DONE
  * review and test of all CPMs, DRMs and RRM fskx files (execution and annotation) ... in progress
    * CPMs ... DONE
    * DRMs ... pending, waiting for Petras review
    * RRM ... DONE
  * create a few example CPM/DRM combination as fskx models ... in progress

##### 27/08
  * distribute new visualisation code to all CPMs and DRMs ... in progress
    * finishing CPMs ... in progress
    * finishing DRMs ... in progress
      * creating DRMonly versions ... in progress
      * adapting DRM module versions with explanation: low priority since joiner doesnt work properly
    * finishing RRM ... in progress
      * visualisation errorbars
      * random CPM/DRM/MS
      * test in FSK-Lab 
  * test CPMs on server as soon as CPM is ready to be tested ... pending, waiting for developers
  * finalise RRM annotation (need some input see below) ... in progress
  * fix a number of known errors in CPM/DRM annotation ... DONE
  * review and test of all CPMs, DRMs and RRM fskx files (execution and annotation) ... in progress
    * CPMs ... in progress
    * DRMs ... pending, waiting for Petras review
    * RRM ... in progress
  * create a few example CPM/DRM combination as fskx models


##### 26/08
  * distribute new visualisation code to all CPMs and DRMs ... in progress
    * finishing CPMs ... DONE
    * finishing DRMs ... pending, waiting for Maartens comments
    * finishing RRM ... pending, waiting for Maartens comments
   * test CPMs on server as soon as CPM is ready to be tested ... pending, waiting for developres
  * finalise RRM annotation (need some input see below) ... in progress
  * fix a number of known errors in CPM/DRM annotation ... DONE
  * review and test of all CPMs, DRMs and RRM fskx files (execution and annotation) ... in progress
  * create a few example CPM/DRM combination as fskx models

##### 25/08
  * clean up R code ... in progress
    * CPMs ... DONE
    * DRMs ... DONE
    * RRM ... DONE
  * distribute new visualisation code to all CPMs and DRMs ... in progress
    * finishing CPMs ... DONE
    * finishing DRMs ... pending, waiting for Maartens comments
    * finishing RRM ... pending, waiting for Maartens comments
  * cleaning up obsolete code snippets and output parameters to increase transparency ... DONE
  * test CPMs on server as soon as CPM is ready to be tested ... not possible at the moment, Thomas is investigating
  * finalise RRM annotation (need some input see below) ... in progress
  * fix a number of known errors in CPM/DRM annotation


##### 24/08
  * clean up R code ... in progress
    * CPMs ... DONE
  * distribute new visualisation code to all CPMs and DRMs ... in progress
    * finishing CPMs ... mostly done, mylius missing
  * cleaning up obsolete code snippets and output parameters to increase transparency
  * test CPMs on server as soon as CPM is ready to be tested ... in progress, stuck at uploading -> asking Lars/Thomas for help

##### 21/08
  * implement visualisation fixes for CPMs 
    * find out about reason for steplike function at low doses ... DONE
    * fix cdf should increase to 1 ... DONE
  * implement visualisation fixes for DRMs ... DONE
  * implement visualisation with all reduction (or "intervention") scenarios ... DONE
  * implement RRM standalone fskx model with all 24 CPM/DRM-combinations as a csv table input and have the user choose CPM and DRM as input parameter ... DONE






## Lindqvist EFSA opinion
### Reference 
doi: 10.2903/j.efsa.2020.6092



### Questions for Maarten - call 2
  * visualisation of CPM ... fine, according to Maarten, confirmation?
    * Maarten:  d
  * visualisation of DRM -> changes, more infos?
    * Maarten: 
  * visualisation of RRM
    * may show all intervention scenarios at the same time (load from table) = same vis as in @Risk -> better?
    * IF ONLY one intervention scenario: should the name (FA1/2/3...) be put into visualisation?
    * which text infos in visualisation?
    * Maarten: all intervention plus one from user
  * annotation of RRM:
    * Inputs from MS skin results
       * definition?
       * how to annotate?
       * EU values are weighted average of all MS, here is just taken as input value, not calculated -> problem?
    * meat conc distributions Input scale (CFU skin) as parameter or leave it as is? how to annotate?
    * slope - definition? or where to find
    * tau - definition? or where to find
    * delta fex - definition? or where to find
  


### Questions for Maarten - call 1
#### general questions model related
  * Cret in R code is randomly chosen (normal distribution), in @Risk as abscissa with certain range for Pill
    * -> new model?s 
    * Maarten: OK
  * new visualisation dose vs cret --> possible to simulate the other plot, sampling from Cret-axis and get dose distribution
    * possible to recreate old visualization -> would that be better? 
    * Maarten: YES
    * (current visualization RAKIP models: what is ordinate? what is info i get out of it?) Distribution given Cret
  * Maxportion in RAKIP = 1000, but in @Risk no upper bound is given... what to do? 
    * Maarten: take Maxportion = 1000
  * in R models is Prev (probability of prevalence) used --> also for efsa opinion relevant? 
    * Maarten: use it in code as is
  * dose in RAKIP models only binomial distribution, dose in @Risk model is normal -> which one ? 
    * Maarten: BOTH (for either case)

#### specific CPM related questions
  * Lindqvist: Ncarcass vs Nportion? 
    * Maarten: summ Nportion to Ncarcass (1097 average carcass size)
  * Van Asselt: washed board prob =0, but unwashed board depends on hands => shouldnt washed board with unwashed hands do something? 
    * Maarten: its ok
  * calistri model: empiric transfer probability hands to meat 2x 0.6? is that correct? its a cdf, isnt it? 
    * Maarten: Mistake;
    * Marcel checked in paper: 0.8 is correct, corrected in R code, but differences are minor <1%

#### regression model questions
  *  in Campy regression @Risk code in tab "meat conc distributions", column log dose interval : first and last values weird source cell in excel -> better? 
    * Maarten: reason is matching pattern in delta -> Marcel simulates this in R code (even if changes would be minor)
 

### general R problems
  * R 3.X: some functions(rbinom, gamma, etc.) have only a 32bit range of numbers --> some models use numbers outside of 32bit range - solved in R4 but FSK only supports R3.X

#### Christensen et al.: Risk Assessment of Campylobacter jejuni in Chicken Products
##### status
  * found fskx file in RAKIP model repository, not fully annotated, discrepancies between code and reference
  * refers to DOI: 10.1111/j.1539-6924.2010.01481.x --> checking that reference led to similar publication
##### todo
  * need to check if model code and code in reference are identical
  * annotation completing  
##### Reference
Christensen B, Sommer H, Nielsen N and Rosenquist H, 2001. Risk Assessment of Campylobacter jejuni in Chicken
Products. Danish Veterinary and Food Administration, Copenhagen, Denmark.


#### Brynestad et al.: Quantitative microbiological risk assessment of campylobacteriosis cases in the German population due to consumption of chicken prepared in homes
##### status
  * found fskx file in RAKIP model repository, not fully annotated, discrepancies between code and reference
  * refers to DOI: 10.1111/j.1539-6924.2010.01481.x --> checking that reference led to similar publication
##### todo
  * need to check if model code and code in reference are identical
  * annotation completing  
##### Reference
Brynestad S, Braute L, Luber P and Bartelt E, 2008. Quantitative microbiological risk assessment of
campylobacteriosis cases in the German population due to consumption of chicken prepared in homes.
International Journal of Risk Assessment and Management, 8, 194–213



#### Nauta et al.: Food safety in the domestic environment: the effect of consumer risk information on human disease risks
##### status
  * found fskx file in RAKIP model repository, not fully annotated, discrepancies between code and reference
  * refers to DOI: 10.1111/j.1539-6924.2010.01481.x --> checking that reference led to DIFFERENT publication
##### todo
  * need to check if model code and code in reference are identical
  * annotation completing  
##### Reference
Nauta MJ, Fischer AR, van Asselt ED, de Jong AE, Frewer LJ and de Jonge R, 2008. Food safety in the domestic
environment: the effect of consumer risk information on human disease risks. Risk Analysis, 28, 179–192.



#### WHO: Risk assessment of Campylobacter spp. in broiler chickens: technical report
##### status
  * found fskx file in RAKIP model repository, not fully annotated, discrepancies between code and reference
  * refers to DOI: 10.1111/j.1539-6924.2010.01481.x --> checking that reference led to DIFFERENT publication
##### todo
  * need to check if model code and code in reference are identical
  * annotation completing  
##### Reference
WHO, 2009. Risk assessment of Campylobacter spp. in broiler chickens: technical report. Microbiological Risk
Assessment Series, 12 

### CPMs currently in process of transfer to fskx

#### Mylius et al.: Cross-contamination during food preparation: a mechanistic modelapplied to chicken-borne Campylobacter
##### Reference
Mylius SD, Nauta MJ and Havelaar AH, 2007. Cross-contamination during food preparation: a mechanistic model
applied to chicken-borne Campylobacter. Risk Analysis, 27, 803–813.



#### van Asselt et al.: Cross-contamination in the kitchen: estimation of transfer rates for cutting boards, hands and knives
##### Reference
van Asselt ED, de Jong AE, de Jonge R and Nauta MJ, 2008. Cross-contamination in the kitchen: estimation of
transfer rates for cutting boards, hands and knives. Journal of Applied Microbiology, 105, 1392–1401.



#### Calistri and Giovannini: Quantitative risk assessment of human campylobacteriosis related to the consumption of chicken meat in two Italian regions
##### Reference
Calistri P and Giovannini A, 2008. Quantitative risk assessment of human campylobacteriosis related to the
consumption of chicken meat in two Italian regions. International Journal of Food Microbiology, 128, 274–287.



#### Lindqvist and Lindblad: Quantitative risk assessment of thermophilic Campylobacter spp. and crosscontamination during handling of raw broiler chickens evaluating strategies at the producer level to reduce human campylobacteriosis in Sweden
##### Reference
Lindqvist R and Lindblad M, 2008. Quantitative risk assessment of thermophilic Campylobacter spp. and crosscontamination during handling of raw broiler chickens evaluating strategies at the producer level to reduce
human campylobacteriosis in Sweden. International Journal of Food Microbiology, 121, 41–52.




### DRM
3 dose response models are compared to each other


## Jukka Ranta models
### Jukka Ranta et al.: unnamed Dose response model

### Jukka Ranta et al.: A Bayesian approach to the evaluation of risk-based microbiological criteria for Campylobacter in broiler meat
https://projecteuclid.org/download/pdfview_1/euclid.aoas/1446488745



# outlook
## EFSA /  Lindqvist et al.: mgQMRA Listeria monocytogenes in frozen fruit, vegetables and herbs
##### Reference
https://www.efsa.europa.eu/en/efsajournal/pub/6092


## Meester / Swart et al.: A quantitative risk assessment for human Taenia solium exposure from home slaughtered pigs in European countries

## Deng / Swart et al: Quantitative risk assessment of meat-borne Toxoplasma gondii infection in the mainland of China

## Swart et al:  A quantitative risk model for Trichinella spp. in pork and wild boar meat

## Teunis et al:  Acute illness from Campylobacter jejuni may require high doses while infection occurs at low doses

## Source attribution models (microbial subtyping for salmonellosis) – work funded by MATRIX project
##### Reference
https://onlinelibrary.wiley.com/doi/full/10.1111/zph.12645

## EFSA: tubs model → certain elements might be possible (growth model) KJ 

## Other ideas

  * https://www.sciencedirect.com/science/article/abs/pii/S0360132320303292
  * Campylobacter DRM: https://onlinelibrary.wiley.com/doi/full/10.1111/risa.12572
