#This is an outdated version of the code used to create lm regressions

#packages used
library(stargazer)
library(sas7bdat)
library(corrplot)
library(tidyverse)
library(rcompanion)
library(ggplot2)
library(bayesAB)

setwd("file/location")

ACS12_16 <- read.sas7bdat("R2016.sas7bdat")
ACS06_10 <- read.sas7bdat("R2006.sas7bdat")

#Testing against minutes to the CBD

#OLS Regression, test income against a log of how many minutes it takes to drive to the Central business district, MBY, MRN, 
#log number of people who have a degree after a bachelors, log number of people who drive to work

#also do the same with Income, Rent, Value

ACS06_10_ATLIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
             PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==1))

ACS06_10_DCIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==4))

ACS06_10_SFIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==10))

ACS06_10_ATLRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==1))

ACS06_10_DCRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==4))

ACS06_10_SFRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==10))

ACS06_10_ATLValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==1))

ACS06_10_DCValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                        PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==4))

ACS06_10_SFValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                        PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==10))

ACS06_10_BOSIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==2))

ACS06_10_CHIIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==3))

ACS06_10_NYCIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==8))

ACS06_10_PHLIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==9))

ACS06_10_BOSRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==2))

ACS06_10_CHIRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==3))

ACS06_10_NYCRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==8))

ACS06_10_PHLRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==9))

ACS06_10_BOSValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==2))

ACS06_10_CHIValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==3))

ACS06_10_NYCValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==8))

ACS06_10_PHLValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==9))

ACS06_10_DENIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==5))

ACS06_10_HOUIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==6))

ACS06_10_LAIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==7))

ACS06_10_DENRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==5))

ACS06_10_HOURent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==6))

ACS06_10_LARent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==7))

ACS06_10_DENValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==5))

ACS06_10_HOUValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==6))

ACS06_10_LAValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                        PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==7))

ACS12_16_ATLIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS12_16,City=="ATL"))

ACS12_16_DCIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==4))

ACS12_16_SFIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==10))

ACS12_16_ATLRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS12_16,City=="ATL"))

ACS12_16_DCRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                        PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==4))

ACS12_16_SFRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                        PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==10))

ACS12_16_ATLValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS12_16,City=="ATL"))

ACS12_16_DCValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==4))

ACS12_16_SFValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==10))

ACS12_16_BOSIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==2))

ACS12_16_CHIIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==3))

ACS12_16_NYCIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==8))

ACS12_16_PHLIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==9))

ACS12_16_BOSRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==2))

ACS12_16_CHIRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==3))

ACS12_16_NYCRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==8))

ACS12_16_PHLRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==9))

ACS12_16_BOSValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==2))

ACS12_16_CHIValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==3))

ACS12_16_NYCValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==8))

ACS12_16_PHLValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==9))

ACS12_16_DENIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==5))

ACS12_16_HOUIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                           PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==6))

ACS12_16_LAIncome <- lm(logIncome ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==7))

ACS12_16_DENRent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==5))

ACS12_16_HOURent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==6))

ACS12_16_LARent <- lm(logRent ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                        PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==7))

ACS12_16_DENValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==5))

ACS12_16_HOUValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                          PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==6))

ACS12_16_LAValue <- lm(logValue ~ LogBGMinutes+MedianBuildYear+MedianRoomNumber+ 
                         PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==7))

#organizes regressions results

stargazer(ACS06_10_BOSValue, ACS06_10_CHIValue, ACS06_10_NYCValue, 
          ACS06_10_PHLValue, ACS06_10_ATLValue, ACS06_10_DCValue, 
          ACS06_10_SFValue, ACS06_10_DENValue, ACS06_10_HOUValue, 
          ACS06_10_LAValue, type="html", column.labels = c('Monocentric', 'Mixed', 'Polycentric'), column.separate = c(4,3,3), out="AllValue06_10.doc")

stargazer(ACS12_16_BOSValue, ACS12_16_CHIValue, ACS12_16_NYCValue, 
          ACS12_16_PHLValue, ACS12_16_ATLValue, ACS12_16_DCValue, 
          ACS12_16_SFValue, ACS12_16_DENValue, ACS12_16_HOUValue, 
          ACS12_16_LAValue, type="html", column.labels = c('Monocentric', 'Mixed', 'Polycentric'), column.separate = c(4,3,3), out="AllValue12_16.doc")


#Testing against Commute to work



ACS06_10_ATLIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==1))

ACS06_10_DCIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==4))

ACS06_10_SFIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==10))

ACS06_10_ATLRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==1))

ACS06_10_DCRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                               PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==4))

ACS06_10_SFRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                               PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==10))

ACS06_10_ATLValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==1))

ACS06_10_DCValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==4))

ACS06_10_SFValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==10))

ACS06_10_BOSIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==2))

ACS06_10_CHIIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==3))

ACS06_10_NYCIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==8))

ACS06_10_PHLIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==9))

ACS06_10_BOSRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==2))

ACS06_10_CHIRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==3))

ACS06_10_NYCRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==8))

ACS06_10_PHLRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==9))

ACS06_10_BOSValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==2))

ACS06_10_CHIValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==3))

ACS06_10_NYCValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==8))

ACS06_10_PHLValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==9))

ACS06_10_DENIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==5))

ACS06_10_HOUIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==6))

ACS06_10_LAIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==7))

ACS06_10_DENRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==5))

ACS06_10_HOURentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==6))

ACS06_10_LARentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                               PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==7))

ACS06_10_DENValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==5))

ACS06_10_HOUValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==6))

ACS06_10_LAValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS06_10,cityplus==7))

ACS12_16_ATLIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS12_16,City=="ATL"))

ACS12_16_DCIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==4))

ACS12_16_SFIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==10))

ACS12_16_ATLRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS12_16,City=="ATL"))

ACS12_16_DCRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                               PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==4))

ACS12_16_SFRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                               PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==10))

ACS12_16_ATLValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS12_16,City=="ATL"))

ACS12_16_DCValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==4))

ACS12_16_SFValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==10))

ACS12_16_BOSIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==2))

ACS12_16_CHIIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==3))

ACS12_16_NYCIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==8))

ACS12_16_PHLIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==9))

ACS12_16_BOSRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==2))

ACS12_16_CHIRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==3))

ACS12_16_NYCRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==8))

ACS12_16_PHLRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==9))

ACS12_16_BOSValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==2))

ACS12_16_CHIValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==3))

ACS12_16_NYCValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==8))

ACS12_16_PHLValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==9))

ACS12_16_DENIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==5))

ACS12_16_HOUIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                  PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==6))

ACS12_16_LAIncomeCommute <- lm(logIncome ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==7))

ACS12_16_DENRentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==5))

ACS12_16_HOURentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==6))

ACS12_16_LARentCommute <- lm(logRent ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                               PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==7))

ACS12_16_DENValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==5))

ACS12_16_HOUValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                 PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==6))

ACS12_16_LAValueCommute <- lm(logValue ~ LogCommute+MedianBuildYear+MedianRoomNumber+ 
                                PostBachelors+CarToWork, data = subset(ACS12_16,cityplus==7))


stargazer(ACS06_10_BOSValueCommute, ACS06_10_CHIValueCommute, ACS06_10_NYCValueCommute, 
          ACS06_10_PHLValueCommute, ACS06_10_ATLValueCommute, ACS06_10_DCValueCommute, 
          ACS06_10_SFValueCommute, ACS06_10_DENValueCommute, ACS06_10_HOUValueCommute, 
          ACS06_10_LAValueCommute, type="html", column.labels = c('Monocentric', 'Mixed', 'Polycentric'), column.separate = c(4,3,3), out="AllValue06_10_Commute.doc")

stargazer(ACS12_16_BOSValueCommute, ACS12_16_CHIValueCommute, ACS12_16_NYCValueCommute, 
          ACS12_16_PHLValueCommute, ACS12_16_ATLValueCommute, ACS12_16_DCValueCommute, 
          ACS12_16_SFValueCommute, ACS12_16_DENValueCommute, ACS12_16_HOUValueCommute, 
          ACS12_16_LAValueCommute, type="html", column.labels = c('Monocentric', 'Mixed', 'Polycentric'), column.separate = c(4,3,3), out="AllValue12_16_Commute.doc")

