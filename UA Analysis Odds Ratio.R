#----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")
#---------------Libraries-------------- (Step 1)
library(finalfit)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(tibble)
library(readxl)
library(data.table)
library(lubridate)
library(eeptools) 
library(zoo)
library(magrittr)
library(tibble)
library(compareGroups)
library(data.table)
library(qwraps2)
library(readxl)
library(plyr)
library(xlsxjars)
library(rJava)
library(psych)
library(gdata)
library(expss)
library(Hmisc)
library(foreign)
library(xlsx)
library(kableExtra)
library(knitr)
#---------------Loading Data---------- (Step 2)
UA <- read.spss("UAD Study Corrected.sav", use.value.label=TRUE, to.data.frame=TRUE)
#---------------Coding Baseline factors---------- (Step 3)
UA$RF1 <- ifelse(UA$Record == "Baseline", UA$RF, NA)
UA$DOD.years1 <- ifelse(UA$Record == "Baseline", UA$DOD.years, NA)
UA$AGE.years1 <- ifelse(UA$Record == "Baseline", UA$AGE.years, NA)
UA$Gender1 <- ifelse(UA$Record == "Baseline", UA$Gender, NA)
UA$Current1 <- ifelse(UA$Record == "Baseline", UA$Current, NA)
UA$CCP1 <- ifelse(UA$Record == "Baseline", UA$CCP, NA)
UA$ANA1 <- ifelse(UA$Record == "Baseline", UA$ANA, NA)
UA$Nodules1 <- ifelse(UA$Record == "Baseline", UA$Nodules, NA)
UA$SICCA1 <- ifelse(UA$Record == "Baseline", UA$SICCA, NA)
UA$Smoking1 <- ifelse(UA$Record == "Baseline", UA$Smoking, NA)
UA$Family_History1 <- ifelse(UA$Record == "Baseline", UA$Family_History, NA)
UA$Nationality1 <- ifelse(UA$Record == "Baseline", UA$Nationality, NA)

UA$RF1 <- factor(UA$RF1,
          levels = c(1,2),
          labels = c("Negative", "Positive"))
UA$Gender1 <- factor(UA$Gender1,
                 levels = c(1,2),
                 labels = c("female", "male"))
UA$Current1 <- factor(UA$Current1,
                     levels = c(1,2),
                     labels = c("No", "Yes"))
UA$CCP1 <- factor(UA$CCP1,
                      levels = c(1,2),
                      labels = c("Negative", "Positive"))
UA$ANA1 <- factor(UA$ANA1,
                  levels = c(1,2),
                  labels = c("Negative", "Positive"))
UA$Nodules1 <- factor(UA$Nodules1,
                  levels = c(1,2),
                  labels = c("No", "Yes"))
UA$SICCA1 <- factor(UA$SICCA1,
                      levels = c(1,2),
                      labels = c("No", "Yes"))
UA$Smoking1 <- factor(UA$Smoking1,
                    levels = c(1,2),
                    labels = c("No", "Yes"))
UA$Family_History1 <- factor(UA$Family_History1,
                      levels = c(1,2),
                      labels = c("Negative", "Positive"))
UA$Nationality1 <- factor(UA$Nationality1,
                             levels = c(1,2),
                             labels = c("Kuwaiti's", "non-Kuwaiti's"))

#---------------summary_factorlist for DAS28 Levels---------- (Step 3)
explanatory = c("VAS","ESR", "CRP", "HAQ", "Patient.s.Global.Assessment", "Physician.s.Global.Assessment",
                "SDAI", "CDAI", "Uric.acid","DAS28", "DOD.years1", "AGE.years1", "Tender", "Swollen", "Gender1", 
                "Current1", "RF1", "CCP1", "ANA1", "Nodules1", "SICCA1", "Smoking1", "Family_History1", "Asthma", 
                "Nationality1", "onbio", "Bio_Mono", "DM", "CAD", "Hyperlipidemia", "Hypertension", "UA_Group")

explanatory_multi = c("DOD.years1", "Gender1", "Current1", "RF1", "CCP1", "ANA1", "Nodules1", "SICCA1", "Smoking1")
dependent = "DAS28_Group"
Table1 <- summary_factorlist(UA, dependent, explanatory, p=TRUE, add_dependent_label=TRUE)
#knitr::kable(Table1)
#---------------summary_factorlist for UA Levels---------- (Step 4)
explanatory = c("VAS","ESR", "CRP", "HAQ", "Patient.s.Global.Assessment", "Physician.s.Global.Assessment",
                "SDAI", "CDAI", "Uric.acid", "DAS28", "DOD.years1", "AGE.years1", "Tender", "Swollen", "Gender1", 
                "Current1", "RF1", "CCP1", "ANA1", "Nodules1", "SICCA1", "Smoking1", "Family_History1", "Asthma", 
                "Nationality1", "onbio", "Bio_Mono", "DM", "CAD", "Hyperlipidemia", "Hypertension", "DAS28_Group")

explanatory_multi = c("DOD.years1", "Gender1", "Current1", "RF1", "CCP1", "ANA1", "Nodules1", "SICCA1", "Smoking1")
dependent = "UA_Group"
  Table2 <- summary_factorlist(UA, dependent, explanatory, p=TRUE, add_dependent_label=TRUE)
  #knitr::kable(Table2)
#---------------GLM Model for DAS28 Levels---------- (Step 5)
#glm(depdendent3 ~ explanatory3, family="binomial")
  explanatory = c("VAS","ESR", "CRP", "HAQ", "Patient.s.Global.Assessment", "Physician.s.Global.Assessment",
                  "SDAI", "CDAI", "Uric.acid", "DOD.years1", "AGE.years1", "Tender", "Swollen", "Gender1", 
                  "Current1", "RF1", "CCP1", "ANA1", "Nodules1", "SICCA1", "Smoking1", "Family_History1", "Asthma", 
                  "Nationality1", "onbio", "Bio_Mono", "DM", "CAD", "Hyperlipidemia", "Hypertension", "UA_Group")
  
  explanatory_multi = c("DOD.years1", "Gender1", "Current1", "RF1", "CCP1", "ANA1", "Nodules1", "SICCA1", "Smoking1")
  dependent = "DAS28_Group"
  Table3 <- finalfit(UA, dependent, explanatory, explanatory_multi, na_include=TRUE, add_dependent_label=TRUE)
  #knitr::kable(Table3)
  #---------------GLM Model for UA Levels---------- (Step 6)
  explanatory = c("VAS","ESR", "CRP", "HAQ", "Patient.s.Global.Assessment", "Physician.s.Global.Assessment",
                  "SDAI", "CDAI", "DAS28", "DOD.years1", "AGE.years1", "Tender", "Swollen", "Gender1", 
                  "Current1", "RF1", "CCP1", "ANA1", "Nodules1", "SICCA1", "Smoking1", "Family_History1", "Asthma", 
                  "Nationality1", "onbio", "Bio_Mono", "DM", "CAD", "Hyperlipidemia", "Hypertension", "DAS28_Group")
  
  explanatory_multi = c("DOD.years1", "Gender1", "Current1", "RF1", "CCP1", "ANA1", "Nodules1", "SICCA1", "Smoking1")
  dependent = "UA_Group"
  Table4 <- finalfit(UA, dependent, explanatory, explanatory_multi, p=TRUE, add_dependent_label=TRUE)
 # knitr::kable(Table4)
  #---------------Odds ratio for DAS28 Levels---------- (Step 7)
  explanatory = c("DOD.years1", "Gender1",
                  "RF1", "CCP1", "ANA1", "Nodules1", "SICCA1", "Smoking1",  
                   "DM", "CAD", "UA_Group")
  dependent = "DAS28_Group"
  or_plot(UA, dependent, explanatory)
  #---------------Random Effect for DAS28 Levels---------- (Step 8)
  explanatory = c("VAS","ESR", "CRP", "HAQ", "Patient.s.Global.Assessment", "Physician.s.Global.Assessment",
                  "SDAI", "CDAI", "Uric.acid", "DOD.years1", "AGE.years1", "Tender", "Swollen", "Gender1", 
                  "Current1", "RF1", "CCP1", "ANA1", "Nodules1", "SICCA1", "Smoking1", "Family_History1", "Asthma", 
                  "Nationality1", "onbio", "Bio_Mono", "DM", "CAD", "Hyperlipidemia", "Hypertension", "UA_Group")
  random_effect = "Hospital"
  explanatory_multi = c("DOD.years1", "Gender1", "Current1", "RF1", "CCP1", "ANA1", "Nodules1", "SICCA1", "Smoking1")
  dependent = "DAS28_Group"
  
  # Separate tables
  UA %>%
    summary_factorlist(dependent,
                       explanatory, fit_id=TRUE) -> Table5
  
  UA %>%
    glmuni(dependent, explanatory) %>%
    fit2df(estimate_suffix=" (univariable)") -> Table6
  
  
  
  # Pipe together
  Table5 %>%
    finalfit_merge(Table6) %>%
    
    
    select(-c(fit_id, index)) %>% # remove unnecessary columns
    dependent_label(colon_s, dependent, prefix="") -> Table9 # place dependent variable label
  #---------------saving tables---------- (Step 9)
  write.xlsx(Table1, file = "table1.xlsx", 
             sheetName="T1", append=TRUE)
  write.xlsx(Table2, file = "table2.xlsx", 
             sheetName="T2", append=TRUE)
  write.xlsx(Table3, file = "table3.xlsx", 
             sheetName="T3", append=TRUE)
  write.xlsx(Table4, file = "table4.xlsx", 
             sheetName="T4", append=TRUE)
  write.xlsx(Table9, file = "table5.xlsx", 
             sheetName="T9", append=TRUE)
  
 