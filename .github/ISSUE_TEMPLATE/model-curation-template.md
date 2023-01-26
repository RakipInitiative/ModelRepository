---
name: Model Curation Template
about: Document the steps done to curate and publish FSKX Model
title: "[MODEL_CURATION]"
labels: documentation
assignees: ''

---

# Model Information

**Model Name**

The name of the model

**References**

URL to the unpublished model or publication associated with model

**Model Uploaded By**
[rakip_user]

# Curation Pipeline

**Model Status**
- [ ] in review (publication pending)
- [ ] ready for publication
- [ ] model has been uploaded and published in Zenodo KJ

## Basic Quality Control
- By Chair of Curationboard: [email]

**General Check**
- [ ] Online Sanitation Check: [validate](https://knime.bfr.berlin/fskx_validator/)

**[MIRARAM](https://www.sciencedirect.com/science/article/pii/S0963996920309777?via%3Dihub) Check**
- [ ] Model Name 
- [ ] Model ID
- [ ] Model Creator (different from author!)
- [ ] Creation Date
- [ ] License
- [ ] Model Execution Metadata (script-language/tool/web-service API)
- [ ] Reference Description (URI)
- [ ] Scope (any)
- [ ] Parameter Metadata (Input/Output)

**Code Of Conduct**
- [ ] In case of re-implementations or re-use of digital objects without changes in the business logic of the model, the person who created the FSKX file is only listed as “Creator”. In cases where the business logic of the existing digital object was significantly modified or the Creator developed the business logic of the model himself the creator can be listed as Author as well. 
- [ ] The FSKX file Creator confirmed that in case of re-implementations or re-use of digital model objects the main author of the original work was contacted and did not express objections, unless the permissions given by the license of that digital model object already provides such clearance.
- [ ] An “Author” of an FSKX model is the person who created the majority of the model code or, in cases where the model code was published in an online repository, the person who is listed there as primary contact person. Other persons who contributed to the model code would be referenced as an additional “Author” in the FSKX file. In those cases where Authors contributed to the FSKX file generation (e.g. by doing quality control), they would also be listed as one of the “Creators”.

**Final Step**
- [ ] Curator assigned to this ticket

## Advanced Quality Control
**By Curator**
- Technical Curation by: [email]
- Scientific Curation by: [email]

**Technical Curation**
- [ ] Model does not contain malicious code
- [ ] Model metadata validates against FSKX schema
- [ ] Model can be executed with KNIME FSK-Lab
- [ ] All provided simulation scenarios can be executed
- [ ] Model can be executed on RAKIP Model Repository


**Scientific Curation**
- [ ] Default simulation results are in line with reference publication
- [ ] User defined simulation settings result in reasonable results
- [ ] Model annotation is sufficiently detailed


## Report
**Changes Made**

Changes made to the model in preparation for publication ([Zenodo KJ](https://zenodo.org/communities/efsa-kj))


**Additional notes**

Observations or remarks about the model or its curation
