# ModelRepository
Joint project of EFSA, Federal Institute For Risk Assessment, DTU and ANSES to create an online model repository.

# content
## Nauta EFSA opinion
##### Reference
https://efsa.onlinelibrary.wiley.com/doi/full/10.2903/j.efsa.2020.6090 

### Todo list
  * finish up R code of all CPMs -> visualisation ... DONE
  * edit all R model code based on Maartens Answers ... DONE
  * merge metadata sheets from Petras metadata and my parameters ... DONE
  * create fskx files for CPMs ... DONE
  * check content and make sure every CPM works fine ...  DONE
  * finalise R code for DRMs ... DONE
  * create metadata sheets for DRMs ... DONE
  * create fskx files for DRMs ... DONE
  * check content and make sure every DRM works fine ... DONE
  * finalise R code for risk reduction model ... DONE 
  * develop visualisation for risk reduction model ... DONE
  * create a KNIME workflow ... in Progress 
    * first functional version with 1 member state (EU) and all reduction scenarios ... DONE
    * implement member state reduction strategy parameters as input ... DONE
    * transfer to fskx ... in progress (difficulties with Simulation Configurator -> discussion with developers)
  * finalise visualisation code for CPM
  * finalise visualisation code for risk reduction model 
  * clean up: distribute new versions of CPMs/DRMs based on comments from Maarten
  * annotate risk reduction analysis model
  * finalise knime workflow with all CPMs and DRMs and opinion



### Questions for Maarten
#### general questions model related
  * Cret in R code is randomly chosen (normal distribution), in @Risk as abscissa with certain range for Pill
    * -> new model?s Maarten: OK
  * new visualisation dose vs cret --> possible to simulate the other plot, sampling from Cret-axis and get dose distribution
    * possible to recreate old visualization -> would that be better? Maarten: YES
    * (current visualization RAKIP models: what is ordinate? what is info i get out of it?) Distribution given Cret
  * Maxportion in RAKIP = 1000, but in @Risk no upper bound is given... what to do? MAarten: take it
  * in R models is Prev (probability of prevalence) used --> also for efsa opinion relevant? Maarten: take it
  * dose in RAKIP models only binomial distribution, dose in @Risk model is normal -> which one ? Maarten: BOTH

#### specific CPM related questions
  * Lindqvist: Ncarcass vs Nportion? Maarten: summ Nportion to Ncarcass (1097 average carcass size)
  * Van Asselt: washed board prob =0, but unwashed board depends on hands => shouldnt washed board with unwashed hands do something? Maarten: its ok
  * calistri model: empiric transfer probability hands to meat 2x 0.6? is that correct? its a cdf, isnt it? Maarten: Mistake
  * (obsolete now if Cret is not a distribution: found an error in Nauta updated R model: Input parameter piCret was unused, but needed in code --> fixed)

#### regression model questions
  *  in Campy regression @Risk code in tab "meat conc distributions", column log dose interval : first and last values weird source cell in excel -> better? Maarten: Pattern reason
 


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