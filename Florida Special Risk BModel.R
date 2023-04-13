rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
library(zoo)
#setwd(getwd())
source("utility_functions.R")

FileName <- 'Florida.xlsx'
EntryYear <- 1980:2052
Years <- 1980:2154    #(why 2152? Because 120 - 18 + 2052 = 2154)
YearStart <- 2022
Age <- 18:120
YOS <- 0:70
#RetirementAge <- 20:120
RetYear <- 2005:2154


ModelPeriod <- 100    #Projection period (typically 30 years)
MinAge <- 18          #Age of the typical youngest member
MaxAge <- 120         #Max age from mortality assumptions
YearStart <- 2022     #Year of the latest val report
MinYear <- 1980       #No hard rule about this. Should get back to about 40 years from now.   
MaxYear <- YearStart + ModelPeriod + MaxAge - MinAge

EntryYear <- MinYear:(YearStart + ModelPeriod)
RetYear <- MinYear:(YearStart + ModelPeriod)
Years <- MinYear:MaxYear
Age <- MinAge:MaxAge
YOS <- 0:70
RetirementAge <- Age

#Assigning individual  Variables
model_inputs <- read_excel(FileName, sheet = 'Main')

for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}

#Import key data tables
SurvivalRates <- read_excel(FileName, sheet = 'Mortality Rates')#Updated* (to RP-2014 General)
MaleMP <- read_excel(FileName, sheet = 'MP-2018_Male') #Updated* (to MP-2019)
FemaleMP <- read_excel(FileName, sheet = 'MP-2018_Female')#Updated* (to MP-2019)
SalaryGrowth <- read_excel(FileName, sheet = "Salary Growth")#Updated* (How to combined YOS & AGE increases?)
WithdrawalRates <- read_excel(FileName, sheet = 'Withdrawal Rates')#Updated*
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')#Updated*

SalaryMatrix <- read_excel(FileName, sheet = "Salary Distribution Special")
HeadCountMatrix <- read_excel(FileName, sheet = "HeadCount Distribution Special")


SalaryMatrix_long <- SalaryMatrix %>% 
  pivot_longer(cols = -1, names_to = "YOS", values_to = "Salary")
HeadCountMatrix_long <- HeadCountMatrix %>% 
  pivot_longer(cols = -1, names_to = "YOS", values_to = "Count")

SalaryHeadCountData <- SalaryMatrix_long %>%
  left_join(HeadCountMatrix_long) %>% 
  replace(is.na(.), 0) %>% 
  mutate(YOS = as.numeric(YOS),
         CurrentYear = YearStart,
         entry_age = Age - YOS,
         EntryYear = CurrentYear - YOS) %>% 
  filter(Salary > 0, entry_age >= 18)

#Salary entry is the starting salary data 
SalaryEntry <- SalaryHeadCountData %>%  
  filter(YOS == 0) %>% 
  select(entry_age, CurrentYear, Salary, Count) %>% 
  mutate(entrant_dist = Count/sum(Count)) %>% 
  rename(start_sal = Salary)

##############################################################################################################################

IsRetirementEligible_Normal <- function(Age, YOS, EntryYear){
  Check_Special_Tier1 = ifelse(YOS >= 25 | (Age >= 55 & YOS >= 6) | (Age >= 52 & YOS >= 25), TRUE, FALSE)
  Check_Special_Tier2 = ifelse(YOS >= 30 | (Age >= 60 & YOS >= 8), TRUE, FALSE)
  
  Check = ifelse(EntryYear >= 2011, Check_Special_Tier2, Check_Special_Tier1)
  return(Check)
}

IsRetirementEligible_Early <- function(Age, YOS, EntryYear){
  Check_Early_Tier1 = ifelse(Age >= 50 & (YOS >= 6), TRUE, FALSE)
  Check_Early_Tier2 = ifelse(Age >= 55 & (YOS >= 8), TRUE, FALSE)
  
  Check = ifelse(EntryYear >= 2011, Check_Early_Tier2, Check_Early_Tier1)
  return(Check)
}

IsRetirementEligible <- function(Age, YOS, EntryYear){
  Check = ifelse(IsRetirementEligible_Normal(Age, YOS, EntryYear) | IsRetirementEligible_Early(Age, YOS, EntryYear), TRUE, FALSE)
  
  return(Check)
}

RetirementType <- function(Age, YOS, EntryYear){
  
  Check = ifelse(IsRetirementEligible_Normal(Age, YOS, EntryYear), 'Regular',
                 ifelse(IsRetirementEligible_Early(Age, YOS, EntryYear), 'Early','None'))
  
  return(Check)
}


SeparationType <- function(Age, YOS, EntryYear){
  Vested_Tier1 = ifelse(YOS >= 6, 'Termination Vested', 'Termination Non-Vested')
  Vested_Tier2 = ifelse(YOS >= 8, 'Termination Vested', 'Termination Non-Vested')
  
  Check = ifelse(IsRetirementEligible_Normal(Age, YOS, EntryYear) == T, 'Normal Retirement',
                 ifelse(IsRetirementEligible_Early(Age, YOS, EntryYear) == T, 'Early Retirement',
                        ifelse(EntryYear < 2011, Vested_Tier1, Vested_Tier2)))
  
  return(Check)
}

##############################################################################################################################

#Custom function to calculate cumulative future values
cumFV <- function(interest, cashflow){
  cumvalue <- double(length = length(cashflow))
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}

LinearInterpolation <- function(Data,AgeStart,AgeEnd,Columns,Increment){
  TempMatrix <- matrix(0,nrow= (AgeEnd - AgeStart + 1),Columns)
  TempMatrix[,1] <- AgeStart:AgeEnd
  colnames(TempMatrix) <- colnames(Data)
  TempMatrix <- as.data.frame(TempMatrix)
  
  for(i in 1:nrow(Data)){
    Index <- which(TempMatrix[,1] == as.double(Data[i,1]))
    for(j in 2:Columns){
      TempMatrix[Index,j] <- Data[i,j]
    }
  }
  
  for(i in 1:(nrow(Data)-1)){
    for(k in 2:Columns){
      for(j in 1:(Increment - 1)){
        BaseRowIndex <- (i-1)*Increment + 1
        Addition <- (Data[(i+1),k] - Data[i,k]) / (Data[(i+1),1] - Data[i,1])
        TempMatrix[(BaseRowIndex+j),k] <- Data[i,k] + (Addition*j)
      }
    }
  }
  
  return(TempMatrix)
}

CompoundSalaryIncrease <- function(Data){
  #Data[,1] <- 1
  for(i in 2:nrow(Data)){
    for(j in 2:ncol(Data)){
      #Column 1 is the Age label so we want to avoid computing j-1 at j=2
      if(j > 2){
        Data[i,j] <- Data[i,j]*Data[i-1,j-1]
      }
    }
  }
  
  return(Data)
}

##############################################################################################################################

MaleMP <- MaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
  mutate(Years = as.numeric(Years))

MaleMP_ultimate <- MaleMP %>%        #ultimate rates = rates for the last year in the MP table
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_male = MP_male) %>% 
  select(-Years)

MaleMP_final <- expand_grid(Age, Years = 1951:MaxYear) %>% 
  left_join(MaleMP, by = c("Age", "Years")) %>% 
  left_join(MaleMP_ultimate, by = "Age") %>% 
  mutate(
    # MP_final_male = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male)   
    #Since the plan assumes "immediate convergence" of MP rates, the "ultimate rates" are used for all years
    MP_final_male = MP_ultimate_male) %>% 
  group_by(Age) %>% 
  mutate(MPcumprod_male_raw = cumprod(1 - MP_final_male),
         MPcumprod_male_adj = MPcumprod_male_raw / MPcumprod_male_raw[Years == 2010]) %>%   #Adjust the mort improvement rates for the 2014 base year
  ungroup()


FemaleMP <- FemaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
  mutate(Years = as.numeric(Years))

FemaleMP_ultimate <- FemaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_female = MP_female) %>% 
  select(-Years)

FemaleMP_final <- expand_grid(Age, Years = 1951:MaxYear) %>% 
  left_join(FemaleMP, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_ultimate, by = "Age") %>% 
  mutate(
    # MP_final_female = ifelse(Years > max(FemaleMP$Years), MP_ultimate_female, MP_female)
    #Since the plan assumes "immediate convergence" of MP rates, the "ultimate rates" are used for all years
    MP_final_female = MP_ultimate_female
  ) %>%
  group_by(Age) %>% 
  mutate(MPcumprod_female_raw = cumprod(1 - MP_final_female),
         MPcumprod_female_adj = MPcumprod_female_raw / MPcumprod_female_raw[Years == 2010]) %>% 
  ungroup()

MortalityTable_int <- expand_grid(EntryYear, entry_age = SalaryEntry$entry_age, Age, YOS) %>% 
  mutate(term_year = EntryYear + YOS,
         Years = EntryYear + Age - entry_age) %>% 
  filter(term_year <= Years) %>% 
  arrange(EntryYear, entry_age, YOS, Age)


#Join base mortality table with mortality improvement table and calculate the final mortality rates
MortalityTable <- MortalityTable_int %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP_final, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_final, by = c("Age", "Years")) %>% 
  
  #MPcumprod is the cumulative product of (1 - MP rates), starting from 2011. We use it later so make life easy and calculate now
  mutate(RetirementCond = IsRetirementEligible(Age, YOS, EntryYear),
    mort_male = ifelse(RetirementCond == T, Pub2010_healthy_retiree_male, 
                       Pub2010_employee_male * ScaleMultiple) * MPcumprod_male_adj, 
    mort_female = ifelse(RetirementCond == T, Pub2010_healthy_retiree_female, 
                         Pub2010_employee_female * ScaleMultiple) * MPcumprod_female_adj,
    mort = (mort_male + mort_female)/2)

#filter out the necessary variables
MortalityTable <- MortalityTable %>% 
  select(EntryYear, term_year, Years, entry_age, Age, YOS, mort)


#Create a second mortality table for current retirees
MortalityTable_retire <- expand_grid(Age = Age[Age >= 40], Years = Years[Years >= YearStart]) %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP_final, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_final, by = c("Age", "Years")) %>% 
  mutate(base_age = Age - (Years - YearStart),
         mort_male = Pub2010_employee_male * MPcumprod_male_adj,
         mort_female = Pub2010_employee_female * MPcumprod_female_adj,
         mort = (mort_male + mort_female)/2) %>% 
  select(base_age, Age, Years, mort) %>% 
  filter(base_age >= 40) %>% 
  arrange(base_age)

##############################################################################################################################

#Separation Rates
SeparationRates <- expand_grid(EntryYear, Age, YOS) 
SeparationRates <- SeparationRates %>%
  mutate(entry_age = Age - YOS,
         Years = EntryYear + YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(EntryYear, entry_age, Age) %>% 
  left_join(WithdrawalRates, by = "YOS") %>%
  left_join(RetirementRates, by = "Age") %>%
  ### Additions ###
  #mutate_all(as.numeric) %>% 
  replace(is.na(.), 0)

#If you're retirement eligible, use the retirement rates, then checks YOS < 5 and use the regular termination rates
SeparationRates <- SeparationRates %>% 
  mutate(retirement_regular = IsRetirementEligible_Normal(Age,YOS, EntryYear),
         retirement_early = IsRetirementEligible_Early(Age,YOS, EntryYear),
         
         SepRate_Withdrawal_Male = ifelse(YOS < 25, SpecialRisk_Male_Under25,
                                          ifelse(YOS >= 25 & YOS < 29, SpecialRisk_Male_25to29,
                                                 ifelse(YOS >= 29 & YOS < 35, SpecialRisk_Male_30to34,
                                                        ifelse(YOS >= 35 & YOS < 45, SpecialRisk_Male_35to44,
                                                               ifelse(YOS >= 45 & YOS < 55, SpecialRisk_Male_45to54,
                                                                      ifelse(YOS >= 55, SpecialRisk_Male_55, 0)))))),
         
         SepRate_Withdrawal_Female = ifelse(YOS < 25, SpecialRisk_Female_Under25,
                                            ifelse(YOS >= 25 & YOS < 29, SpecialRisk_Female_25to29,
                                                   ifelse(YOS >= 29 & YOS < 35, SpecialRisk_Female_30to34,
                                                          ifelse(YOS >= 35 & YOS < 45, SpecialRisk_Female_35to44,
                                                                 ifelse(YOS >= 45 & YOS < 55, SpecialRisk_Female_45to54,
                                                                        ifelse(YOS >= 55, SpecialRisk_Female_55, 0)))))),
         
         SepRate_Retire_Male_Tier1 = ifelse(retirement_early == T, SpecialRisk_Tier1_DROP_Male,
                                            ifelse(retirement_regular == T, SpecialRisk_Tier1_NonDROP_Male, SepRate_Withdrawal_Male)),
         SepRate_Retire_Male_Tier2 = ifelse(retirement_early == T, SpecialRisk_Tier2_DROP_Male,
                                            ifelse(retirement_regular == T, SpecialRisk_Tier2_NonDROP_Male, SepRate_Withdrawal_Male)),
         
         
         SepRate_Retire_Female_Tier1 = ifelse(retirement_early == T, SpecialRisk_Tier1_DROP_Female,
                                              ifelse(retirement_regular == T, SpecialRisk_Tier1_NonDROP_Female, SepRate_Withdrawal_Female)),
         SepRate_Retire_Female_Tier2 = ifelse(retirement_early == T, SpecialRisk_Tier2_DROP_Female,
                                              ifelse(retirement_regular == T, SpecialRisk_Tier2_NonDROP_Female, SepRate_Withdrawal_Female)),
         
         
         
         SepRate_DB = ifelse(EntryYear < 2011, (SepRate_Retire_Male_Tier1 + SepRate_Retire_Female_Tier1) / 2, 
                             (SepRate_Retire_Male_Tier2 + SepRate_Retire_Female_Tier2) / 2)) %>% 
  group_by(EntryYear, entry_age) %>% 
  mutate(RemainingProb_DB = cumprod(1 - lag(SepRate_DB, default = 0)),
         SepProb_DB = lag(RemainingProb_DB, default = 1) - RemainingProb_DB) %>% 
  ungroup()

#Filter out unecessary values
SeparationRates <- SeparationRates %>% select(EntryYear, entry_age, Years, Age, YOS, RemainingProb_DB, SepProb_DB, SepRate_DB)

##############################################################################################################################

#Create a long-form table of Age and YOS and merge with salary data
SalaryData <- expand_grid(EntryYear, Age, YOS) %>%  
  mutate(entry_age = Age - YOS,
         Years = EntryYear + YOS) %>%
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(EntryYear, entry_age, YOS)
SalaryData <- left_join(SalaryData, SalaryEntry, by = c("entry_age"))
SalaryData <- left_join(SalaryData, SalaryGrowth, by = c("YOS"))

SalaryData <- SalaryData %>%
  group_by(EntryYear, entry_age) %>%
  mutate(Salary = start_sal*cumprod(1+lag(salary_increase_special_risk,default = 0))*(1 + payroll_growth)^(Years - YOS - YearStart),
         #Salary = start_sal*salary_increase_compound*(1 + payroll_growth)^(Years - YOS - YearStart),
         FinalAvgSalary_5YR = rollmean(lag(Salary), k = 5, fill = NA, align = "right"),
         FinalAvgSalary_8YR = rollmean(lag(Salary), k = 8, fill = NA, align = "right"),
         #DB_EEContrib = (DB_EE_cont + DC_DB_EE_cont)*Salary,
         DB_EEContrib = DB_EE_cont_Special*Salary,
         DBEEBalance = ifelse(EntryYear < 2011, cumFV(Interest_Tier1, DB_EEContrib), cumFV(Interest_Tier2, DB_EEContrib)),
         DC_EEBalance = DC_EE_cont*Salary,
         
         #DC_ERBalance = 0.2*DC_ER_cont*Salary,
         #CBEEContAmount = CB_EE_paycredit * Salary,
         #CBERContAmount = CB_ER_paycredit * Salary,
         #CBEEBalance = cumFV(ICR, CBEEContAmount),
         #CBERBalance = cumFV(ICR, CBERContAmount),
         #CBBalance = CBEEBalance + ifelse(YOS >= CB_vesting, CBERBalance, 0),
         CumulativeWage = cumFV(ARR, Salary)) %>% 
  ungroup()


#Survival Probability and Annuity Factor for active members
AnnFactorData <- MortalityTable %>% 
  group_by(EntryYear, entry_age, YOS) %>% 
  mutate(Pre2011YOS = ifelse(term_year > 2011, pmax(0, 2011 - EntryYear), term_year - EntryYear),
         COLA = ifelse(YOS > 0, (Pre2011YOS/YOS)*0.03, 0),
         surv = cumprod(1 - lag(mort, default = 0)),
         surv_DR = surv/(1 + ARR)^(Age - min(Age)),
         #surv_ICR = surv/(1 + ICR)^(Age - min(Age)),
         #surv_ACR = surv/(1 + ACR)^(Age - min(Age)),
         surv_DR_COLA = surv_DR * (1 + COLA)^(Age - min(Age)),
         #surv_ACR_COLA = surv_ACR * (1 + COLA)^(Age - min(Age)),
         #AnnuityFactor_ACR = rev(cumsum(rev(surv_ACR_COLA)))/surv_ACR_COLA,
         AnnuityFactor_DR = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA) %>% 
  ungroup()

#Survival Probability and Annuity Factor for retirees
AnnFactorData_retire <- MortalityTable_retire %>% 
  group_by(base_age) %>% 
  mutate(surv = cumprod(1 - lag(mort, default = 0)),
         surv_DR = surv/(1 + ARR)^(Age - min(Age)),
         surv_DR_COLA = surv_DR * (1 + COLA)^(Age - min(Age)),
         AnnuityFactor_DR = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA)

##############################################################################################################################

ReducedFactor <- expand_grid(Age, YOS, EntryYear) %>% 
  left_join(AnnFactorData, by = c("EntryYear" ,"Age", "YOS")) %>%
  replace(is.na(.), 0) %>% 
  filter(Age >= YOS) %>%
  mutate(norm_retire = ifelse(RetirementType(Age, YOS, EntryYear) == 'Regular', 1, 0)) %>%
  group_by(EntryYear, entry_age, YOS) %>%
  mutate(AgeNormRet = max(Age) - sum(norm_retire) + 1,
         sum_norm_retire = sum(norm_retire),
         YearsNormRet = AgeNormRet - Age,
         RetType = RetirementType(Age, YOS, EntryYear),
         RF = ifelse(RetType == 'Early', 1 - YearsNormRet*12*(5/12/100),
                     ifelse(RetType == "None", 0, 1))) %>% 
  rename(RetirementAge = Age) %>% 
  ungroup()


BenefitsTable <- AnnFactorData %>%
  rename(RetirementAge = Age) %>%
  mutate(term_age = entry_age + YOS) %>%
  left_join(SalaryData, by = c("term_age" = "Age", "YOS", "term_year" = "Years", "entry_age", "EntryYear")) %>% 
  left_join(ReducedFactor %>% select(RetirementAge, YOS, RF, EntryYear), by = c("RetirementAge", "YOS", "EntryYear")) %>%
  rename(surv_DR_ret = surv_DR, AF_Ret = AnnuityFactor_DR) %>% 
  # YOS is in the benefit section because of graded multipliers
  mutate(BenMult_Tier2 = 0.03,
         BenMult_Tier1 = 0.03,
         # BenMult_Tier1 = ifelse((Age >= 55 & YOS >= 6) | (Age >= 52 & YOS >= 25), 0.016,
         #                        ifelse((Age >= 56 & YOS >= 6) | (Age >= 53 & YOS >= 26), 0.0163,
         #                               ifelse((Age >= 57 & YOS >= 6) | (Age >= 54 & YOS >= 27), 0.0165,
         #                                      ifelse((Age >= 58 & YOS >= 6) | (Age >= 55 & YOS >= 28), 0.0168, 0.0168)))),
         # 
         # BenMult_Tier2 = ifelse(YOS >= 30 | (Age >= 60 & YOS >= 8), 0.016,
         #                        ifelse(YOS >= 31 | (Age >= 61 & YOS >= 8), 0.0163,
         #                               ifelse(YOS >= 32 | (Age >= 62 & YOS >= 8), 0.0165,
         #                                      ifelse(YOS >= 33 | (Age >= 63 & YOS >= 8), 0.0168, 0.0168)))),
         
         Multiplier_YOS = ifelse(EntryYear < 2011, BenMult_Tier1*YOS, BenMult_Tier2*YOS),
         Benefit1 = ifelse(EntryYear < 2011, pmax(Multiplier_YOS,1)*FinalAvgSalary_5YR, pmax(Multiplier_YOS,1)*FinalAvgSalary_8YR),
         Benefit2 = 36.34*12*YOS,
         Benefit = pmax(Benefit1, Benefit2),
         AnnFactorAdj_DB = AF_Ret*surv_DR_ret / surv,
         #AnnFactorAdj_DB = AnnuityFactor_DR * surv_DR,
         DB_Benefit = RF * Benefit,
         PV_DB_Benefit = DB_Benefit*AnnFactorAdj_DB,
         can_retire = IsRetirementEligible(Age = RetirementAge, YOS, EntryYear))
#CBBalance_final = CBBalance / surv_ICR,                                                       #project the cash balance forward to the annuitization day 
#CB_Benefit = CBBalance_final / AnnuityFactor_ACR)
# replace(is.na(.), 0)


#For a given combination of entry age and termination age, the member is assumed to choose the retirement age that maximizes the PV of future retirement benefits. That value is the "optimum benefit". 
# OptimumBenefit <- BenefitsTable %>% 
#   group_by(EntryYear, entry_age, term_age) %>% 
#   summarise(MaxBenefit = max(PV_DB_Benefit)) %>%
#   mutate(MaxBenefit = ifelse(is.na(MaxBenefit), 0, MaxBenefit)) %>% 
#   ungroup()


# OptimumBenefit_DB <- BenefitsTable %>% 
#   group_by(EntryYear, entry_age, term_age) %>% 
#   summarise(Max_PV_DB = max(PV_DB_Benefit)) %>%
#   mutate(Max_PV_DB = ifelse(is.na(Max_PV_DB), 0, Max_PV_DB)) %>%
#   # filter(entry_age %in% SalaryEntry$entry_age, term_age >= 20) %>%
#   ungroup() %>% 
#   #join the BenefitsTable to get the "optimal" retirement age
#   left_join(BenefitsTable %>% filter(PV_DB_Benefit > 0), by = c("EntryYear", "entry_age", "term_age", "Max_PV_DB" = "PV_DB_Benefit")) %>%
#   select(EntryYear, entry_age, term_age, RetirementAge, Max_PV_DB) %>% 
#   mutate(RetirementAge = ifelse(is.na(RetirementAge), term_age, RetirementAge))    #Assume retire age = term age for non-vested members


#Employees are assumed to retire at the earliest age of retirement eligibility
retire_age_tab <- BenefitsTable %>%
  group_by(EntryYear, entry_age, term_age) %>%
  summarise(RetirementAge = n() - sum(can_retire) + min(RetirementAge)) %>% 
  ungroup() %>% 
  mutate(RetirementAge = ifelse(RetirementAge == 121, term_age, RetirementAge))


BenefitsTable_retire <- BenefitsTable %>% 
  semi_join(retire_age_tab) %>% 
  select(EntryYear, entry_age, term_age, RetirementAge, PV_DB_Benefit, AnnFactorAdj_DB) %>% 
  mutate(PV_DB_Benefit = ifelse(is.na(PV_DB_Benefit), 0, PV_DB_Benefit))


##################################################################################################

####### Benefit Accrual & Normal Cost #######
#### Real Pension Wealth = Pension Wealth adjusted for inflation
#### Actuarial PV of Pension Wealth = Pension Wealth 
#Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
#####################################

source("utility_functions.R")
FinalData <- SalaryData %>% 
  left_join(BenefitsTable_retire, by = c("EntryYear", "entry_age", "Age" = "term_age")) %>% 
  #left_join(OptimumBenefit_CB, by = c("EntryYear", "entry_age", "Age" = "term_age", "RetirementAge")) %>% 
  left_join(SeparationRates, by = c("EntryYear", "Age", "YOS", "entry_age", "Years")) %>%
  group_by(EntryYear, entry_age) %>%
  mutate(#SepType = SeparationType(Age,YOS, Years),
    #LumpSumPct = ifelse(EntryYear >= 2022, 0.2, 0.05),
    #DBWealth = ifelse(SepType == 'Retirement', pmax(DBEEBalance,Max_PV_DB), 
    #                  ifelse(SepType == 'Termination Vested', LumpSumPct*DBEEBalance + (1-LumpSumPct)*Max_PV_DB, DBEEBalance)),
    
    SepType = SeparationType(Age, YOS, EntryYear),
    LumpSumPct = 0.5,
    retire_refund_ratio = 1- LumpSumPct,
    #Term vested DB members are assumed to choose between a refund and deferred benefits based on a retire/refund ratio. This ratio can change for calibration purposes.
    DBWealth = ifelse(SepType == 'Retirement', PV_DB_Benefit, 
                      ifelse(SepType == 'Termination Vested', retire_refund_ratio * PV_DB_Benefit + (1 - retire_refund_ratio) * DBEEBalance, DBEEBalance)),
    ben_decision = ifelse(YOS == 0, NA, ifelse(SepType == "Retirement", "retire",
                                               ifelse(SepType == "Termination Vested", "mix", "refund"))),
    
    #DBWealth = 0.2*DBEEBalance + 0.8*Max_PV_DB,
    #DBWealth = pmax(DBEEBalance,Max_PV_DB),
    #PenWealth = pmax(DBEEBalance,MaxBenefit),  #50% lump sum, 50% optimal retirement
    #PenWealth = 0.5*(DBEEBalance + MaxBenefit),
    RealPenWealth = DBWealth/(1 + assum_infl)^YOS,
    #PVPenWealth = DBWealth/(1 + ARR)^YOS * SepProb,
    #PVCumWage = CumulativeWage/(1 + ARR)^YOS * SepProb,
    
    #CBWealth = ifelse(DBWealth == DBEEBalance, CBBalance, PV_CB_Benefit),   #mimic DB members' behavior. This is to simplify the workforce projection done later.
    Real_DBWealth = DBWealth/(1 + assum_infl)^YOS,
    #Real_CBWealth = CBWealth/(1 + assum_infl)^YOS,
    PVFB_DB = PVFB(sep_rate_vec = SepRate_DB, interest = ARR, value_vec = DBWealth),
    #PVFB_CB = PVFB(sep_rate_vec = SepRate_DB, interest = ARR, value_vec = CBWealth),
    PVFS = PVFS(remaining_prob_vec = RemainingProb_DB, interest = ARR, sal_vec = Salary),
    normal_cost_DB = ifelse(PVFS[YOS == 0] > 0, PVFB_DB[YOS == 0] / PVFS[YOS == 0], 0),
    #normal_cost_CB = PVFB_CB[YOS == 0] / PVFS[YOS == 0],
    PVFNC_DB = PVFS * normal_cost_DB) %>%
  replace(is.na(.), 0) %>%
  ungroup()


NormalCost <- FinalData %>% 
  filter(YOS == 0) %>% 
  select(EntryYear, entry_age, normal_cost_DB)

#Calculate the aggregate normal cost for current year (for testing purposes)
NC_aggregate <- NormalCost %>% 
  left_join(SalaryEntry, by = c("entry_age")) %>%
  left_join(SalaryData %>% select(EntryYear, entry_age, Age, Salary), by = c("EntryYear", "entry_age")) %>% 
  filter(!is.na(Count)) %>% 
  summarise(normal_cost_aggregate_DB = sum(normal_cost_DB * Salary * Count) / sum(Salary * Count))
