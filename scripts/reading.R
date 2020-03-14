library(tidyverse)
library(mlr)
library(ucimlr)
library(actools)

# install.packages("mlr", dependencies = TRUE, suggests = TRUE)
# install.packages("mlrMBO", dependencies = TRUE)
# install.packages("tidyverse", dependencies = TRUE)
# devtools::install_github("Prometheus77/ucimlr")
# devtools::install_github("Prometheus77/actools")

#########################################
# Load Data
#########################################
german <- read_ucimlr("german")
poker  <- read_csv("")


german <- german %>%
  mutate(Status_existing_chkg_acct = recode(Status_existing_chkg_acct,
                                            A11 = '<0',
                                            A12 = '0-<200',
                                            A13 = '200+',
                                            A14 = 'no checking account'),
         Credit_history = recode(Credit_history,
                                 A30 = 'No remaining balance at any bank',
                                 A31 = 'No remaining balance at this bank',
                                 A32 = 'No prior delinquency',
                                 A33 = 'Prior delinquency',
                                 A34 = 'Remaining balance at another bank'),
         Purpose = recode(Purpose,
                          A40 = 'Car (new)',
                          A41 = 'Car (used)',
                          A42 = 'Furniture/equipment',
                          A43 = 'Radio/television',
                          A44 = 'Domestic appliances',
                          A45 = 'Repairs',
                          A46 = 'Education',
                          A47 = 'Vacation',
                          A48 = 'Retraining',
                          A49 = 'Business',
                          A410 = 'Other'),
         Savings_acct = recode(Savings_acct,
                               A61 = '<100',
                               A62 = '100-<500',
                               A63 = '500-<1000',
                               A64 = '1000+',
                               A65 = 'unknown/none'),
         Present_employment_since = recode(Present_employment_since,
                                           A71 = 'Unemployed',
                                           A72 = '<1 year',
                                           A73 = '1-<4 years',
                                           A74 = '4-<7 years',
                                           A75 = '7+ years'),
         Marital_status = recode(Marital_status,
                                 A91 = 'Male - Divorced/Separated',
                                 A92 = 'Female - Divorced/Separated/Married',
                                 A93 = 'Male - Single',
                                 A94 = 'Male - Married/Widowed',
                                 A95 = 'Female - Single'),
         Other_applicants = recode(Other_applicants,
                                   A101 = 'none',
                                   A102 = 'co-applicant',
                                   A103 = 'guarantor'),
         Property = recode(Property,
                           A121 = 'real estate',
                           A122 = 'savings or life insurance',
                           A123 = 'car or other',
                           A124 = 'unknown or none'),
         Other_installment_plans = recode(Other_installment_plans,
                                          A141 = 'bank',
                                          A142 = 'stores',
                                          A143 = 'none'),
         Housing = recode(Housing,
                          A151 = 'rent',
                          A152 = 'own',
                          A153 = 'for free'),
         Job = recode(Job,
                      A171 = 'unemployed/unskilled/non-resident',
                      A172 = 'unskilled - resident',
                      A173 = 'skilled/official',
                      A174 = 'management/self-employed/highly qualified'),
         Telephone = recode(Telephone,
                            A191 = 'none',
                            A192 = 'yes'),
         Foreign_worker = recode(Foreign_worker,
                                 A201 = 'yes',
                                 A202 = 'no'),
         Performance = recode_factor(Performance,
                                     `1` = 'Good',
                                     `2` = 'Bad'))

german_info <- 