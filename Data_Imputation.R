# #working directory 
# setwd('D:/UofT/M.Sc/CHL5250/Group Project/Project')
# 
# 
# #read file
# cycle1.1 <- read.csv('Data/cchs-82M0013-E-2001-c1-1-general-file_F1.csv')
# cycle2.1 <- read.csv('Data/cchs-82M0013-E-2003-c2-1-GeneralFile_F1.csv')
# cycle3.1 <- read.csv('Data/cchs-82M0013-E-2005-c3-1-main-file_F1.csv')

#working directory 
setwd('/Users/linkeli/Desktop/CHL5250')


#read file
cycle1.1 <- read.csv('cchs-82M0013-E-2001-c1-1-general-file_F1.csv')
cycle2.1 <- read.csv('cchs-82M0013-E-2003-c2-1-GeneralFile_F1.csv')
cycle3.1 <- read.csv('cchs-82M0013-E-2005-c3-1-main-file_F1.csv')

cyc1.1 <- cycle1.1
cyc2.1 <- cycle2.1
cyc3.1 <- cycle3.1


#=====================Variable Mutation===========================
#Variable Description see: 
#2001 cycle1.1: https://search1.odesi.ca/#/details?uri=%2Fodesi%2Fcchs-82M0013-E-2001-c1-1-general-file.xml
#2003 cycle2.1: https://search1.odesi.ca/#/details?uri=%2Fodesi%2Fcchs-82M0013-E-2003-c2-1-GeneralFile.xml
#2005 cycle3.1: https://search1.odesi.ca/#/details?uri=%2Fodesi%2Fcchs-82M0013-E-2005-c3-1-main-file.xml


#=============Age=============
# NO MISSING CASE
#*** Linke: Change 59- to 60-
#*** Edith: age groups: 20-34, 35-49, 50-64
cyc1.1$DHHAGAGE <- ifelse(cyc1.1$DHHAGAGE == 1|cyc1.1$DHHAGAGE == 2|
                            cyc1.1$DHHAGAGE == 12|cyc1.1$DHHAGAGE == 13|
                            cyc1.1$DHHAGAGE == 14|cyc1.1$DHHAGAGE == 15, NA, cyc1.1$DHHAGAGE)
cyc1.1$DHHAGAGE <- ifelse(cyc1.1$DHHAGAGE == 3|cyc1.1$DHHAGAGE == 4|
                            cyc1.1$DHHAGAGE == 5, "20-34", cyc1.1$DHHAGAGE)
cyc1.1$DHHAGAGE <- ifelse(cyc1.1$DHHAGAGE == 6|cyc1.1$DHHAGAGE == 7|
                            cyc1.1$DHHAGAGE == 8,"35-49", cyc1.1$DHHAGAGE)
cyc1.1$DHHAGAGE <- ifelse(cyc1.1$DHHAGAGE == 9|cyc1.1$DHHAGAGE == 10|
                            cyc1.1$DHHAGAGE == 11,"50-64", cyc1.1$DHHAGAGE)
cyc1.1 <- na.omit(cyc1.1)

cyc2.1$DHHCGAGE <- ifelse(cyc2.1$DHHCGAGE == 1|cyc2.1$DHHCGAGE == 2|
                            cyc2.1$DHHCGAGE == 12|cyc2.1$DHHCGAGE == 13|
                            cyc2.1$DHHCGAGE == 14|cyc2.1$DHHCGAGE == 15|
                            cyc2.1$DHHCGAGE == 96|cyc2.1$DHHCGAGE == 97|
                            cyc2.1$DHHCGAGE == 98|cyc2.1$DHHCGAGE == 99, NA, cyc2.1$DHHCGAGE)
cyc2.1$DHHCGAGE <- ifelse(cyc2.1$DHHCGAGE == 3|cyc2.1$DHHCGAGE == 4|
                            cyc2.1$DHHCGAGE == 5,"20-34", cyc2.1$DHHCGAGE)
cyc2.1$DHHCGAGE <- ifelse(cyc2.1$DHHCGAGE == 6|cyc2.1$DHHCGAGE == 7|
                            cyc2.1$DHHCGAGE == 8,"35-49", cyc2.1$DHHCGAGE)
cyc2.1$DHHCGAGE <- ifelse(cyc2.1$DHHCGAGE == 9|cyc2.1$DHHCGAGE == 10|
                            cyc2.1$DHHCGAGE == 11,"50-64", cyc2.1$DHHCGAGE)
cyc2.1 <- na.omit(cyc2.1)

cyc3.1$DHHEGAGE <- ifelse(cyc3.1$DHHEGAGE == 1|cyc3.1$DHHEGAGE== 2|
                            cyc3.1$DHHEGAGE == 3|cyc3.1$DHHEGAGE == 13|
                            cyc3.1$DHHEGAGE == 14|cyc3.1$DHHEGAGE == 15|
                            cyc3.1$DHHEGAGE == 16, NA, cyc3.1$DHHEGAGE)
cyc3.1$DHHEGAGE <- ifelse(cyc3.1$DHHEGAGE == 4|cyc3.1$DHHEGAGE == 5|
                            cyc3.1$DHHEGAGE == 6,"20-34", cyc3.1$DHHEGAGE)
cyc3.1$DHHEGAGE <- ifelse(cyc3.1$DHHEGAGE == 7|cyc3.1$DHHEGAGE == 8|
                            cyc3.1$DHHEGAGE == 9,"35-49", cyc3.1$DHHEGAGE)
cyc3.1$DHHEGAGE <- ifelse(cyc3.1$DHHEGAGE == 10|cyc3.1$DHHEGAGE == 11|
                            cyc3.1$DHHEGAGE == 12,"50-64", cyc3.1$DHHEGAGE)
cyc3.1 <- na.omit(cyc3.1)
age <- as.factor(c(cyc1.1$DHHAGAGE, cyc2.1$DHHCGAGE, cyc3.1$DHHEGAGE))



#===========Province==========
#10 = NFL, 11 = PEI, 12 = NS, 13 = NB, 24 = QUEBEC, 35 = ON, 46 = MA, 47 = SA, 48 = AB, 59 = BC, 60
cyc1.1$GEOAGPRV <- as.factor(ifelse(cyc1.1$GEOAGPRV == 96|cyc1.1$GEOAGPRV == 97|cyc1.1$GEOAGPRV == 98|
                                      cyc1.1$GEOAGPRV == 99, NA, cyc1.1$GEOAGPRV))

cyc2.1$GEOCGPRV <- as.factor(ifelse(cyc2.1$GEOCGPRV== 96|cyc2.1$GEOCGPRV == 97|
                                      cyc2.1$GEOCGPRV == 98|cyc2.1$GEOCGPRV == 99, NA, cyc2.1$GEOCGPRV))

cyc3.1$GEOEGPRV <- as.factor(ifelse(cyc3.1$GEOEGPRV== 96|cyc3.1$GEOEGPRV == 97|
                                      cyc3.1$GEOEGPRV == 98|cyc3.1$GEOEGPRV == 99, NA, cyc3.1$GEOEGPRV))

province <- c(cyc1.1$GEOAGPRV, cyc2.1$GEOCGPRV, cyc3.1$GEOEGPRV)

#Recode Northwest Territories,Nunavut, Yukon as 'north' and the rest of the provinces/territories as 'south'.

province <- as.factor(ifelse(province == 60, "NORTH", ifelse(is.na(province), NA, "SOUTH")))



#===============Sex=============
# 1 = MALE, 2= FEMALE, OTHERWISE NA

cyc1.1$DHHA_SEX <- ifelse(cyc1.1$DHHA_SEX == 6|cyc1.1$DHHA_SEX == 7|
                            cyc1.1$DHHA_SEX == 8|cyc1.1$DHHA_SEX == 9,
                          NA, cyc1.1$DHHA_SEX)
cyc2.1$DHHC_SEX <- ifelse(cyc2.1$DHHC_SEX == 6|cyc2.1$DHHC_SEX == 7|
                            cyc2.1$DHHC_SEX == 8|cyc2.1$DHHC_SEX == 9,
                          NA, cyc2.1$DHHC_SEX)
cyc3.1$DHHE_SEX <- ifelse(cyc3.1$DHHE_SEX == 6|cyc3.1$DHHE_SEX == 7|
                            cyc3.1$DHHE_SEX == 8|cyc3.1$DHHE_SEX == 9,
                          NA, cyc3.1$DHHE_SEX)
sex <- c(cyc1.1$DHHA_SEX, cyc2.1$DHHC_SEX, cyc3.1$DHHE_SEX)
#sex <- as.factor(ifelse(sex == 1, "male", ifelse(sex == 2, "female", sex)))
sex <- as.factor(ifelse(sex == 1, "male", "female"))

#==============Marital Status=========
#1 = married, 2 = common-law, 3 = widow, 4 =  single, OTHERWISE NA

cyc1.1$DHHAGMS <- ifelse(cyc1.1$DHHAGMS == 6|cyc1.1$DHHAGMS == 7|
                           cyc1.1$DHHAGMS == 8|cyc1.1$DHHAGMS == 9,
                         NA, cyc1.1$DHHAGMS)
cyc2.1$DHHCGMS <- ifelse(cyc2.1$DHHCGMS == 6|cyc2.1$DHHCGMS == 7|
                           cyc2.1$DHHCGMS == 8|cyc2.1$DHHCGMS == 9,
                         NA, cyc2.1$DHHCGMS)
cyc3.1$DHHEGMS <- ifelse(cyc3.1$DHHEGMS== 6|cyc3.1$DHHEGMS == 7|
                           cyc3.1$DHHEGMS == 8|cyc3.1$DHHEGMS == 9,
                         NA, cyc3.1$DHHEGMS)
marital_status <- as.factor(c(cyc1.1$DHHAGMS, cyc2.1$DHHCGMS, cyc3.1$DHHEGMS))

#==============Sample Weight====================
#	Divide them by 3 to get a nationally representative sample (on average).

weight <- c(cyc1.1$WTSAM/3, cyc2.1$WTSC_M/3, cyc3.1$WTSE_M/3)


#=================BMI==================
# Recode into categories 3 categories: underweight (<18.5), healthy weight (between 18.5 and 25), overweight (>25).

cyc1.1$HWTAGBMI <- ifelse(cyc1.1$HWTAGBMI == 999.6|cyc1.1$HWTAGBMI == 999.9, NA, cyc1.1$HWTAGBMI)
cyc2.1$HWTCGBMI <- ifelse(cyc2.1$HWTCGBMI == 999.6|cyc2.1$HWTCGBMI == 999.7|
                            cyc2.1$HWTCGBMI == 999.8|cyc2.1$HWTCGBMI == 999.9,
                          NA, cyc2.1$HWTCGBMI)
cyc3.1$HWTEGBM <- ifelse(cyc3.1$HWTEGBM == 999.96|cyc3.1$HWTEGBM == 999.97|
                           cyc3.1$HWTEGBM == 999.98|cyc3.1$HWTEGBM == 999.99,
                         NA, cyc3.1$HWTEGBM)
BMI <- c(cyc1.1$HWTAGBMI, cyc2.1$HWTCGBMI, cyc3.1$HWTEGBM)
BMI <- as.factor(ifelse(BMI < 18.5, "underweight", 
                        ifelse(18.5 <= BMI & BMI <= 25,
                               "healthy", ifelse(is.na(BMI), NA, "overweight"))))


#================Has arthritis or rheumatism=============
# 1 = Yes, 2 = No, otherwise NA
#*** Linke: In this variable, those who answered ‘NO’ are considered as ‘NOT APPLICABLE’ 
#*** in the next variable ‘kind of arthritis’.
cyc1.1$CCCA_051 <- ifelse(cyc1.1$CCCA_051 == 6|cyc1.1$CCCA_051 == 7|
                            cyc1.1$CCCA_051 == 8|cyc1.1$CCCA_051 == 9,
                          NA, ifelse(cyc1.1$CCCA_051 == 1, "Yes", "No"))
cyc2.1$CCCC_051 <- ifelse(cyc2.1$CCCC_051 == 6|cyc2.1$CCCC_051  == 7|
                            cyc2.1$CCCC_051  == 8|cyc2.1$CCCC_051  == 9,
                          NA, ifelse(cyc2.1$CCCC_051  == 1, "Yes", "No"))
cyc3.1$CCCE_051 <- ifelse(cyc3.1$CCCE_051 == 6|cyc3.1$CCCE_051 == 7|
                            cyc3.1$CCCE_051 == 8|cyc3.1$CCCE_051 == 9,
                          NA, ifelse(cyc3.1$CCCE_051 == 1, "Yes", "No"))
arthritis <- as.factor(c(cyc1.1$CCCA_051, cyc2.1$CCCC_051, cyc3.1$CCCE_051))


#=================== Kind of arthritis ===============================
#*** Linke: "RHEUMATOID ARTHRITIS" and "OTHER" are labeled as excluded,  
#***        "No" in variable `Has arthritis or rheumatism' is labeled as "Unexposed"
#***        "OSTEOARTHRITIS" is labeled as "Exposed"
cyc1.1$CCCA_05A <- ifelse(cyc1.1$CCCA_05A == 7|
                            cyc1.1$CCCA_05A == 8|cyc1.1$CCCA_05A == 9,
                          NA, ifelse(cyc1.1$CCCA_051 == 1 | cyc1.1$CCCA_05A == 3, "Excluded", "Exposed"))
cyc2.1$CCCC_05A <- ifelse(cyc2.1$CCCC_05A == 7|
                            cyc2.1$CCCC_05A == 8|cyc2.1$CCCC_05A == 9,
                          NA, ifelse(cyc2.1$CCCC_05A == 1 | cyc2.1$CCCC_05A == 3, "Excluded", "Exposed"))
cyc3.1$CCCE_05A <- ifelse(cyc3.1$CCCE_05A == 7|
                            cyc3.1$CCCE_05A == 8|cyc3.1$CCCE_05A == 9,
                          NA, ifelse(cyc3.1$CCCE_05A == 1 | cyc3.1$CCCE_05A == 3, "Excluded", "Exposed"))
exposed <- c(cyc1.1$CCCA_05A, cyc2.1$CCCA_05A, cyc3.1$CCCA_05A)
exposed[which(arthritis == "No")] <- "Unexposed"
exposed <- as.factor(exposed)





#================ Blood Pressure ========================
# 1 = Yes, 2 = No, otherwise NA

cyc1.1$CCCA_071 <- ifelse(cyc1.1$CCCA_071 == 6|cyc1.1$CCCA_071 == 7|
                            cyc1.1$CCCA_071 == 8|cyc1.1$CCCA_071 == 9,
                          NA, ifelse(cyc1.1$CCCA_071 == 1, "Yes", "No"))

cyc2.1$CCCC_071 <- ifelse(cyc2.1$CCCC_071 == 6|cyc2.1$CCCC_071 == 7|
                            cyc2.1$CCCC_071 == 8|cyc2.1$CCCC_071 == 9,
                          NA, ifelse(cyc2.1$CCCC_071 == 1, "Yes", "No"))

cyc3.1$CCCE_071 <- ifelse(cyc3.1$CCCE_071 == 6|cyc3.1$CCCE_071 == 7|
                            cyc3.1$CCCE_071 == 8|cyc3.1$CCCE_071 == 9,
                          NA, ifelse(cyc3.1$CCCE_071 == 1, "Yes", "No"))
bp <- as.factor(c(cyc1.1$CCCA_071, cyc2.1$CCCC_071, cyc3.1$CCCE_071))

#================= Diabetes =============================
# 1 = Yes, 2 = No, otherwise NA

cyc1.1$CCCA_101 <- ifelse(cyc1.1$CCCA_101 == 6|cyc1.1$CCCA_101 == 7|
                            cyc1.1$CCCA_101 == 8|cyc1.1$CCCA_101 == 9,
                          NA, ifelse(cyc1.1$CCCA_101 == 1, "Yes", "No"))

cyc2.1$CCCC_101 <- ifelse(cyc2.1$CCCC_101 == 6|cyc2.1$CCCC_101 == 7|
                            cyc2.1$CCCC_101 == 8|cyc2.1$CCCC_101 == 9,
                          NA, ifelse(cyc2.1$CCCC_101 == 1, "Yes", "No"))

cyc3.1$CCCE_101 <- ifelse(cyc3.1$CCCE_101 == 6|cyc3.1$CCCE_101 == 7|
                            cyc3.1$CCCE_101 == 8|cyc3.1$CCCE_101 == 9,
                          NA, ifelse(cyc3.1$CCCE_101 == 1, "Yes", "No"))
diabetes <- as.factor(c(cyc1.1$CCCA_101, cyc2.1$CCCC_101, cyc3.1$CCCE_101))

#================= Has Heart Disease =============================
# 1 = Yes, 2 = No, otherwise NA

cyc1.1$CCCA_121 <- ifelse(cyc1.1$CCCA_121 == 6|cyc1.1$CCCA_121 == 7|
                            cyc1.1$CCCA_121 == 8|cyc1.1$CCCA_121 == 9,
                          NA, ifelse(cyc1.1$CCCA_121 == 1, "Yes", "No"))

cyc2.1$CCCC_121 <- ifelse(cyc2.1$CCCC_121 == 6|cyc2.1$CCCC_121 == 7|
                            cyc2.1$CCCC_121 == 8|cyc2.1$CCCC_121 == 9,
                          NA, ifelse(cyc2.1$CCCC_121 == 1, "Yes", "No"))

cyc3.1$CCCE_121 <- ifelse(cyc3.1$CCCE_121 == 6|cyc3.1$CCCE_121 == 7|
                            cyc3.1$CCCE_121 == 8|cyc3.1$CCCE_121 == 9,
                          NA, ifelse(cyc3.1$CCCE_121 == 1, "Yes", "No"))

heart_disease <- as.factor(c(cyc1.1$CCCA_121, cyc2.1$CCCC_121, cyc3.1$CCCE_121))

#================== Type of Smoker ===============================
#smoker: 1 = daily, 2= occasionally, 3 = not a smoker, 4 = Former daily
# 5 = former occasionally, 6 = never smoked, otherwise NA

cyc1.1$SMKADSTY <- ifelse(cyc1.1$SMKADSTY == 96|cyc1.1$SMKADSTY == 97|
                            cyc1.1$SMKADSTY == 98|cyc1.1$SMKADSTY == 99,
                          NA, cyc1.1$SMKADSTY)
cyc2.1$SMKCDSTY <- ifelse(cyc2.1$SMKCDSTY == 96|cyc2.1$SMKCDSTY == 97|
                            cyc2.1$SMKCDSTY == 98|cyc2.1$SMKCDSTY == 99,
                          NA, cyc2.1$SMKCDSTY)
cyc3.1$SMKEDSTY <- ifelse(cyc3.1$SMKEDSTY == 96|cyc3.1$SMKEDSTY == 97|
                            cyc3.1$SMKEDSTY == 98|cyc3.1$SMKEDSTY == 99,
                          NA, cyc3.1$SMKEDSTY)

smoker <- as.factor(c(cyc1.1$SMKADSTY, cyc2.1$SMKCDSTY, cyc3.1$SMKEDSTY))


#================== Type of Drinker ===============================
# 1 = regular, 2 = occasionally, 3 = former, 4 = never, otherwise NA

cyc1.1$ALCADTYP <- ifelse(cyc1.1$ALCADTYP == 6|cyc1.1$ALCADTYP == 7|
                            cyc1.1$ALCADTYP == 8|cyc1.1$ALCADTYP == 9,
                          NA, cyc1.1$ALCADTYP)
cyc2.1$ALCCDTYP <- ifelse(cyc2.1$ALCCDTYP == 6|cyc2.1$ALCCDTYP == 7|
                            cyc2.1$ALCCDTYP == 8|cyc2.1$ALCCDTYP == 9,
                          NA, cyc2.1$ALCCDTYP)
cyc3.1$ALCEDTY <- ifelse(cyc3.1$ALCEDTY == 6|cyc3.1$ALCEDTY == 7|
                           cyc3.1$ALCEDTY == 8|cyc3.1$ALCEDTY == 9,
                         NA, cyc3.1$ALCEDTY)
drinker <- as.factor(c(cyc1.1$ALCADTYP, cyc2.1$ALCCDTYP, cyc3.1$ALCEDTY))

#================== Physical Activity Index ===============================
# 1 = ACTIVE, 2 = MODERATE, 3 = INACTIVE, otherwise NA

cyc1.1$PACADPAI <- ifelse(cyc1.1$PACADPAI == 1|cyc1.1$PACADPAI == 2|
                            cyc1.1$PACADPAI == 3, cyc1.1$PACADPAI, NA)
cyc2.1$PACCDPAI <- ifelse(cyc2.1$PACCDPAI == 1|cyc2.1$PACCDPAI== 2|
                            cyc2.1$PACCDPAI == 3, cyc2.1$PACCDPAI, NA)
cyc3.1$PACEDPAI <- ifelse(cyc3.1$PACEDPAI  == 1|cyc3.1$PACEDPAI  == 2|
                            cyc3.1$PACEDPAI == 3, cyc3.1$PACEDPAI, NA)
PAI <- as.factor(c(cyc1.1$PACADPAI, cyc2.1$PACCDPAI, cyc3.1$PACEDPAI))


#================== Stress ===============================
# 1 = NOT AT ALL, 2= NOT VERY, 3 = A BIT, 4 = QUITE A BIT, 5 = EXTREMELY, OW = NA

cyc1.1$GENA_07 <- ifelse(cyc1.1$GENA_07 == 6|cyc1.1$GENA_07 == 7|
                           cyc1.1$GENA_07 == 8|cyc1.1$GENA_07== 9,
                         NA, cyc1.1$GENA_07)
cyc2.1$GENC_07 <- ifelse(cyc2.1$GENC_07 == 6|cyc2.1$GENC_07 == 7|
                           cyc2.1$GENC_07 == 8|cyc2.1$GENC_07== 9,
                         NA, cyc2.1$GENC_07)
cyc3.1$GENE_07 <- ifelse(cyc3.1$GENE_07 == 6|cyc3.1$GENE_07 == 7|
                           cyc3.1$GENE_07 == 8|cyc3.1$GENE_07== 9,
                         NA, cyc3.1$GENE_07)

stress <- as.factor(c(cyc1.1$GENA_07, cyc2.1$GENC_07, cyc3.1$GENE_07))

#===================== Daily Consumption ========================

cyc1.1$FVCADTOT <- ifelse(cyc1.1$FVCADTOT  == 999.6|cyc1.1$FVCADTOT  == 999.7|
                            cyc1.1$FVCADTOT  == 999.8|cyc1.1$FVCADTOT  == 999.9,
                          NA, cyc1.1$FVCADTOT)

cyc2.1$FVCCDTOT <- ifelse(cyc2.1$FVCCDTOT  == 999.6|cyc2.1$FVCCDTOT  == 999.7|
                            cyc2.1$FVCCDTOT  == 999.8|cyc2.1$FVCCDTOT  == 999.9,
                          NA, cyc2.1$FVCCDTOT)

cyc3.1$FVCEDTOT <- ifelse(cyc3.1$FVCEDTOT  == 999.6|cyc3.1$FVCEDTOT  == 999.7|
                            cyc3.1$FVCEDTOT  == 999.8|cyc3.1$FVCEDTOT  == 999.9,
                          NA, cyc3.1$FVCEDTOT)

daily_consumption <- c(cyc1.1$FVCADTOT, cyc2.1$FVCCDTOT, cyc3.1$FVCEDTOT)

daily_consumption <- ifelse(0 <= daily_consumption & daily_consumption <= 3, "0-3",
                            ifelse(3 < daily_consumption & daily_consumption <= 6, "3-6",
                                   ifelse(6 < daily_consumption, "6+", daily_consumption)))

daily_consumption <- as.factor(daily_consumption)

#===================== Immigrant status ======================
# 1= Yes, 2 = No, Otherwise NA
#*** Linke: In this variable, those who answered ‘NO’ is considered as ‘NOT APPLICABLE’ in 
#***        the next variable ‘Length of time in Canada since immigration’. 

cyc1.1$SDCAFIMM <- ifelse(cyc1.1$SDCAFIMM == 1, "Yes", 
                          ifelse(cyc1.1$SDCAFIMM == 2, "No", NA))
cyc2.1$SDCCGRES <- ifelse(cyc2.1$SDCCGRES == 1, "Yes", 
                          ifelse(cyc2.1$SDCCGRES == 2, "No", NA))
cyc3.1$SDCEFIMM <- ifelse(cyc3.1$SDCEFIMM == 1, "Yes", 
                          ifelse(cyc3.1$SDCEFIMM == 2, "No", NA))

immigrant_status <- as.factor(c(cyc1.1$SDCAFIMM, cyc2.1$SDCCGRES, cyc3.1$SDCEFIMM))

#==================== LENGTH OF TIME SINCE IMMITGRATION ===========
#1= 0-9 years, 2= >=10 years, otherwise NA
#*** Linke: Useful for creating immigration status (potential categories: 
#***        “not immigrant,” “recent immigrant,” “immigrated more than 10 years ago”)

length_of_time <- c(cyc1.1$SDCAGRES, cyc2.1$SDCCGRES, cyc3.1$SDCEGRES)

length_of_time <- ifelse(length_of_time == 1, "recent_immigrant", 
                         ifelse(length_of_time == 2,"immigrated_more_than_10_years_ago", NA))

length_of_time[which(immigrant_status == "No")] <- "not_immigrant"
length_of_time <- as.factor(length_of_time)

#======================Has a medical doctor ===============================
# 1 = Yes, 2 = No, otherwise NA

cyc1.1$TWDA_5 <- ifelse(cyc1.1$TWDA_5 == 6|cyc1.1$TWDA_5== 7|
                          cyc1.1$TWDA_5 == 8|cyc1.1$TWDA_5== 9,
                        NA, cyc1.1$TWDA_5)

cyc2.1$HCUC_1AA <- ifelse(cyc2.1$HCUC_1AA == 6|cyc2.1$HCUC_1AA== 7|
                            cyc2.1$HCUC_1AA == 8|cyc2.1$HCUC_1AA== 9,
                          NA, cyc2.1$HCUC_1AA)

cyc3.1$HCUE_1AA <- ifelse(cyc3.1$HCUE_1AA == 6|cyc3.1$HCUE_1AA== 7|
                            cyc3.1$HCUE_1AA == 8|cyc3.1$HCUE_1AA== 9,
                          NA, cyc3.1$HCUE_1AA)

doctor <- c(cyc1.1$TWDA_5, cyc2.1$HCUC_1AA, cyc3.1$HCUE_1AA)
doctor <- as.factor(ifelse(doctor == 1, "Yes", ifelse(doctor == 2, "No", 
                                                      doctor)))

#======================= Education ==============================
#1= < than secondary 2= secondary grad. 3= other post-sec 4= post-sec. grad, otherwise NA

cyc1.1$EDUADR04 <- ifelse(cyc1.1$EDUADR04 == 6|cyc1.1$EDUADR04== 7|
                            cyc1.1$EDUADR04 == 8|cyc1.1$EDUADR04== 9,
                          NA, cyc1.1$EDUADR04)

cyc2.1$EDUCDR04 <- ifelse(cyc2.1$EDUCDR04 == 6|cyc2.1$EDUCDR04== 7|
                            cyc2.1$EDUCDR04 == 8|cyc2.1$EDUCDR04== 9,
                          NA, cyc2.1$EDUCDR04)

cyc3.1$EDUEDR04 <- ifelse(cyc3.1$EDUEDR04 == 6|cyc3.1$EDUEDR04== 7|
                            cyc3.1$EDUEDR04 == 8|cyc3.1$EDUEDR04== 9,
                          NA, cyc3.1$EDUEDR04)

education <- as.factor(c(cyc1.1$EDUADR04, cyc2.1$EDUCDR04, cyc3.1$EDUEDR04))

#======================== CuLTRUAL ORIGIN ========================
#1= white, 2= visible minority, otherwise NA

racial_origin <- c(cyc1.1$SDCAGRAC, cyc2.1$SDCCGRAC, cyc3.1$SDCEGCGT)

racial_origin <- as.factor(ifelse(racial_origin == 1, "white", ifelse(racial_origin == 2,
                                                                      "non-white", NA)))




#===================== Income ======================================
#1= No income, 2= Less than 15000, 3= 15000-29999, 4= 30000-49999 5= 50000-79999, 6= 80000+, otherwise NA
income <- c(cycle1.1$INCAGHH, cycle2.1$INCCGHH, cycle3.1$INCEGHH)
#*** Linke: I did not find out the description for variable "imcome" from the website, but using 
#***        this cleaning method, no family will have income > 80000 so I am not sure if this is correct.

cyc1.1$INCAGHH <- ifelse(cyc1.1$INCAGHH == 96|cyc1.1$INCAGHH == 99, NA, 
                         ifelse(cyc1.1$INCAGHH == 1, 2, cyc1.1$INCAGHH))
cyc1.1$INCAGHH <- cyc1.1$INCAGHH - 1


cyc2.1$INCCGHH <- ifelse(cyc2.1$INCCGHH == 6|cyc2.1$INCCGHH == 7| 
                           cyc2.1$INCCGHH == 8| cyc2.1$INCCGHH == 9, NA, 
                         cyc2.1$INCCGHH)

cyc3.1$INCEGHH <- ifelse(cyc3.1$INCEGHH == 6|cyc3.1$INCEGHH == 7| 
                           cyc3.1$INCEGHH == 8|cyc3.1$INCEGHH == 9, NA, 
                         cyc3.1$INCEGHH)

income <- as.factor(c(cyc1.1$INCAGHH, cyc2.1$INCCGHH, cyc3.1$INCEGHH))

#===================== HAs COPD ======================================
#1 = yes, 2 = no, otherwise NA

COPD <- c(cyc1.1$CCCA_91B, cyc2.1$CCCC_91B, cyc3.1$CCCE_91F)
COPD <- as.factor(ifelse(COPD == 1, "Yes", ifelse(COPD == 2, "No", NA)))



#==================== Cleaned Data ===================================
# data.modifiedage <- data.frame(
#   'heart_disease' = heart_disease,
#   'arthritis' = arthritis,
#   'exposed' =exposed,
#   'age' = age,
#   'sex' = sex,
#   'marital_status' = marital_status,
#   'racial_origin' =racial_origin,
#   'immigrant_status' = immigrant_status,
#   'length_of_time' = length_of_time,
#   'education' = education,
#   'income' = income,
#   'BMI' = BMI,
#   'PAI' = PAI,
#   'doctor' = doctor,
#   'smoker' = smoker,
#   'drinker' = drinker,
#   'bp' = bp,
#   'diabetes' = diabetes,
#   'COPD' = COPD,
#   'daily_consumption' = daily_consumption,
#   'stress' = stress,
#   'province' = province,
#   'weight' = weight
# )
# 
# 
# write.csv(data.modifiedage, "Data.cleaned.csv")


#=================== VARIABLE SUMMARY=================================
summary(heart_disease)
summary(arthritis)
summary(exposed)
# summary(arthri)
summary(age)
summary(sex)
summary(marital_status)
summary(racial_origin)
summary(immigrant_status)
summary(length_of_time)
summary(education)
summary(income)
summary(BMI)
summary(PAI)
summary(doctor)
summary(smoker)
summary(drinker)
summary(bp)
summary(diabetes)
summary(COPD)
summary(daily_consumption)
summary(stress)
summary(province)
summary(weight)

# Descriptive statistics at baseline:
library(Hmisc)
library(htmlTable)
library(Gmisc)

getDescriptionStatsBy(x = age, by = arthritis)
getDescriptionStatsBy(x = sex, by = arthritis)
getDescriptionStatsBy(x = racial_origin, by = arthritis)
getDescriptionStatsBy(x = education, by = arthritis)
getDescriptionStatsBy(x = income, by = arthritis)
getDescriptionStatsBy(x = BMI, by = arthritis)
getDescriptionStatsBy(x = doctor, by = arthritis)
getDescriptionStatsBy(x = smoker, by = arthritis)
getDescriptionStatsBy(x = drinker, by = arthritis)
getDescriptionStatsBy(x = bp, by = arthritis)
getDescriptionStatsBy(x = diabetes, by = arthritis)
getDescriptionStatsBy(x = PAI, by = arthritis)


#=================== Model Fitting =================================

library(nnet)

# Univariate Models:

## Arthritis
arthritis <- relevel(arthritis, ref="No")
mod.arth <- glm(heart_disease ~ arthritis, 
                weights=weight/mean(weight), 
                family = "binomial",
                na.action = na.omit)
summary(mod.arth)


## Age
age <- relevel(age, ref="35-49")
mod.age <- glm(heart_disease ~ age, 
               weights=weight/mean(weight), 
               family = "binomial",
               na.action = na.omit)
summary(mod.age)

## Sex
sex <- relevel(sex, ref="male")
mod.sex <- glm(heart_disease ~ sex, 
               weights=weight/mean(weight), 
               family = "binomial",
               na.action = na.omit)
summary(mod.sex)

## Ethnicity
racial_origin <- relevel(racial_origin, ref="white")
mod.ethn <- glm(heart_disease ~ racial_origin, 
                weights=weight/mean(weight), 
                family = "binomial",
                na.action = na.omit)
summary(mod.ethn)

## Education
education <- relevel(education, ref="4")
mod.edu <- glm(heart_disease ~ education, 
               weights=weight/mean(weight), 
               family = "binomial",
               na.action = na.omit)
summary(mod.edu)

## Income
income <- relevel(income, ref="4")
mod.inc <- glm(heart_disease ~ income, 
               weights=weight/mean(weight), 
               family = "binomial",
               na.action = na.omit)
summary(mod.inc)

## BMI
BMI <- relevel(BMI, ref="overweight")
mod.bmi <- glm(heart_disease ~ BMI, 
               weights=weight/mean(weight), 
               family = "binomial",
               na.action = na.omit)
summary(mod.bmi)

## PAI
# Physical Activity Index # 1 = ACTIVE, 2 = MODERATE, 3 = INACTIVE, otherwise NA
getDescriptionStatsBy(x = PAI, by = arthritis)
PAI <- relevel(PAI, ref = 3)
summary(PAI)
fit.PAI <- glm( formula = heart_disease ~  PAI, family="binomial", weights = weight/mean(weight),na.action=na.omit)
summary(fit.PAI)
round(exp(coef(fit.PAI)), 4)

## Access to regular doctor
#Has a medical doctor # 1 = Yes, 2 = No, otherwise NA
getDescriptionStatsBy(x = doctor, by = arthritis)
doctor <- relevel(doctor,ref="Yes")
summary(doctor)
fit.doctor <- glm( formula = heart_disease ~  doctor, family="binomial", weights = weight/mean(weight),na.action=na.omit)
summary(fit.doctor)
round(exp(coef(fit.doctor)), 4)

## Smoker
#smoker: 1 = daily, 2= occasionally, 3 = not a smoker, 4 = Former daily
# 5 = former occasionally, 6 = never smoked, otherwise NA
getDescriptionStatsBy(x = smoker, by = arthritis)
smoker <- relevel(smoker, ref=6)
summary(smoker)
fit.smoker <- glm( formula = heart_disease ~  smoker, family="binomial", weights = weight/mean(weight),na.action=na.omit)
summary(fit.smoker)
round(exp(coef(fit.smoker)), 4)

## drinker
# 1 = regular, 2 = occasionally, 3 = former, 4 = never, otherwise NA
getDescriptionStatsBy(x = drinker, by = arthritis)
drinker <- relevel(drinker, ref=1)
summary(drinker)
fit.drinker <- glm( formula = heart_disease ~  drinker, family="binomial", weights = weight/mean(weight),na.action=na.omit)
summary(fit.drinker)
round(exp(coef(fit.PAI)), 4)

## BP
#Has high blood pressure 1 = Yes, 2 = No, otherwise NA
getDescriptionStatsBy(x = bp, by = arthritis)
bp <- relevel(drinker, ref=1)
summary(bp)
fit.bp <- glm( formula = heart_disease ~  bp, family="binomial", weights = weight/mean(weight),na.action=na.omit)
summary(fit.bp)
round(exp(coef(fit.bp)), 4)

## Diabetes
# 1 = Yes, 2 = No, otherwise NA
getDescriptionStatsBy(x = diabetes, by = arthritis)
diabetes <- relevel(diabetes, ref=1)
summary(diabetes)
fit.diabetes <- glm( formula = heart_disease ~  diabetes, family="binomial", weights = weight/mean(weight),na.action=na.omit)
summary(fit.diabetes)
round(exp(coef(fit.diabetes)), 4)

## COPD
#1 = yes, 2 = no, otherwise NA
getDescriptionStatsBy(x = COPD, by = arthritis)
COPD <- relevel(COPD, ref=1)
summary(COPD)
fit.COPD<- glm( formula = heart_disease ~  COPD, family="binomial", weights = weight/mean(weight),na.action=na.omit)
summary(fit.COPD)
round(exp(coef(fit.COPD)), 4)

## Daily consumption
getDescriptionStatsBy(x = daily_consumption, by = arthritis)
daily_consumption <- relevel(daily_consumption, ref="3-6")
summary(daily_consumption)
fit.daily_consumption<- glm( formula = heart_disease ~  daily_consumption, family="binomial", weights = weight/mean(weight),na.action=na.omit)
summary(fit.daily_consumption)
round(exp(coef(fit.daily_consumption)), 4)

## Stress
# 1 = NOT AT ALL, 2= NOT VERY, 3 = A BIT, 4 = QUITE A BIT, 5 = EXTREMELY, OW = NA
getDescriptionStatsBy(x = stress, by = arthritis)
stress <- relevel(stress, ref=3)
summary(stress)
fit.stress<- glm( formula = heart_disease ~  stress, family="binomial", weights = weight/mean(weight),na.action=na.omit)
summary(fit.stress)
round(exp(coef(fit.stress)), 4)

#Recode Northwest Territories,Nunavut, Yukon as 'north' and the rest of the provinces/territories as 'south'.
getDescriptionStatsBy(x = province, by = arthritis)
province <- relevel(province, ref=1)
summary(province)
fit.province<- glm( formula = heart_disease ~ province, family="binomial", weights = weight/mean(weight),na.action=na.omit)
summary(province)
round(exp(coef(fit.PAI)), 4)


# Backward elimination:

dat <- data.frame(heart_disease, 
                  arthritis, 
                  age, 
                  sex, 
                  racial_origin, 
                  education, 
                  income, 
                  BMI, 
                  PAI, 
                  doctor,
                  smoker,
                  bp, 
                  diabetes,
                  COPD, 
                  daily_consumption,
                  stress, 
                  weight)

library(MASS)
full.mod <- glm(heart_disease ~ arthritis + age + sex + racial_origin + education 
                + income + BMI + PAI + doctor + smoker + bp + diabetes + COPD 
                + daily_consumption,
                weights = weight/mean(weight),
                family = "binomial", data = na.omit(dat))
summary(full.mod)

#step.model <- step(full.mod, direction = "backward", trace = TRUE)
#summary(step.model)

#============================= correlation analysis================================

# all the variables except weight are categorical
# pariwise assocation can be measured by building pairwise contingency tables 
# and association can be assessed by Pearson's chi-square test statistics

summary(dat)

dat.char <- subset(dat, select=-c(weight))

Var1 <- c(); Var2 <- c(); pearson.sta <- c(); pvalue <- c(); cont.table <- list()
for (i in 1:ncol(dat.char)){
  for (j in 1:ncol(dat.char)){
    Var1 <- c(Var1, as.character(colnames(dat.char)[i]))
    Var2 <- c(Var2, as.character(colnames(dat.char)[j]))
    cont.table[[length(cont.table)+1]] <- table(dat.char[,i], dat.char[,j])
    pearson.sta <- c(pearson.sta,chisq.test(dat.char[,i], dat.char[,j])$statistic)
    pvalue <- c(pvalue,chisq.test(dat.char[,i], dat.char[,j])$p.value)
  }
}

cor.df <- data.frame(Var1 = Var1, Var2 = Var2, pvalue = pvalue)


# assocation between multi-level categorical and numerical can be assesses by ANOVA
# Use F-test

Var1 <- c(); Var2 <- c(); p.value <- c()
for (k in 1:ncol(dat.char)){
  Var1 <- c(Var1, "weight", as.character(colnames(dat.char)[k]))
  Var2 <- c(Var2, as.character(colnames(dat.char)[k]), "weight")
  anova <- aov(dat$weight ~ dat[,k])
  p.value <- c(p.value,summary(anova)[[1]][["Pr(>F)"]][[1]],summary(anova)[[1]][["Pr(>F)"]][[1]])
}

cor.df <- rbind(cor.df, data.frame(Var1 = c(Var1, "weight", "weight"),
                                   Var2 = c(Var2, "weight", "weight"), 
                                   pvalue = c(p.value, 0, 0)))
cor.df$strength <- ifelse(cor.df$pvalue < 0.0001, "very strong", 
                          ifelse(cor.df$pvalue < 0.01,"strong",ifelse(cor.df$pvalue < 0.05, "moderate", "weak")))


# all pairwise assocations
cor.df

# generate final plot
ggplot(data = cor.df, aes(x=Var1, y=Var2, fill=strength)) +
  geom_tile()+scale_fill_manual(breaks = c("very strong", "strong", "moderate", "weak"), 
                                values=c("skyblue4", "skyblue3", "skyblue", "lightsteelblue1")) +
  ggtitle("Pairwise Associations Heatmap") + theme(axis.text.x = element_text(angle = 90, hjust = 1))





#####======================= Imputation analysis=====================#########
require(mice)
require(pROC)
require(ggplot2)
## Missing Ratio of income
sum(is.na(dat$income)) / dim(dat)[1]

## Divide total sample set into train & test sets, Ratio of test set & train set = 1:3
smp_size <- floor(0.75 * nrow(dat))
set.seed(3000614)
train_ind <- sample(seq_len(nrow(dat)), size = smp_size)

# Train model using the umimputed training set
train.set.true <- na.omit(dat[-train_ind, ])

full.mod.true <- glm( heart_disease ~ arthritis + age + sex + racial_origin + education 
                      + income + BMI + PAI + doctor + smoker + bp + diabetes + COPD 
                      + daily_consumption,
                      weights = weight/mean(weight),
                      family = "binomial", data = train.set.true )
test.pred.value <- predict(full.mod.true, na.omit(dat[-train_ind, ]), type = "response")
roc.true <- pROC::roc(response = na.omit(dat[-train_ind,])$heart_disease , 
                      predictor = test.pred.value);roc.true
roc.list <- list() 
roc.list[[1]] <- roc.true

# Train model using the training set where income is imputed
# Imputation method is chosen to be: "true","polyreg","polr","lda","cart"
method.list <- c("true","polyreg","polr","lda","cart")
for (i in 2:(length(method.list)+1) ) {
  print(paste("Let's test",method.list[i],"imputation model~"))
  
  init <- mice(dat[1:10,], maxit=0) 
  meth <- init$method
  predM <- init$predictorMatrix
  non.income.var <- (names(meth) != "income")
  meth[non.income.var] <- ""
  ## Impute data with method.list[i] method
  meth["income"] <- method.list[i]
  
  train.set.imp <- mice(dat[train_ind, ], m=1, printFlag=TRUE, maxit = 10, 
                        seed=3000614,method = meth,predictorMatrix = predM)
  
  train.set.imp <- complete(train.set.imp)
  
  train.set.imp <- na.omit(train.set.imp)
  
  full.mod.imp <- glm( heart_disease ~ arthritis + age + sex + racial_origin + education 
                       + income + BMI + PAI + doctor + smoker + bp + diabetes + COPD 
                       + daily_consumption,
                       weights = weight/mean(weight),
                       family = "binomial", data = train.set.imp)
  imp.pred.value <- predict(full.mod.imp, na.omit(dat[-train_ind, ]), type = "response")
  roc.imp <- pROC::roc(response = na.omit(dat[-train_ind,])$heart_disease , 
                       predictor = imp.pred.value);roc.imp
  roc.list[[i]] <- roc.imp
}

## Plot the ROC curves using ggplot2, since "polyreg"&"polr" have similar results, we plot "polr" only.

roc.data <- data.frame(specificities = c(1-roc.list[[1]]$specificities,
                                         1-roc.list[[3]]$specificities,
                                         1-roc.list[[4]]$specificities,
                                         1-roc.list[[5]]$specificities),
                       sensitivities = c(roc.list[[1]]$sensitivities,
                                         roc.list[[3]]$sensitivities,
                                         roc.list[[4]]$sensitivities,
                                         roc.list[[5]]$sensitivities),
                       type = c(rep("Original Data",length(roc.list[[1]]$sensitivities)),
                                rep("Polyreg Imputation",length(roc.list[[3]]$sensitivities)),
                                rep("LDA Imputation",length(roc.list[[4]]$sensitivities)),
                                rep("CART Imputation",length(roc.list[[5]]$sensitivities))
                       )
)
ggplot(roc.data, aes(x = specificities, y = sensitivities, color = type, linetype= type)) + 
  geom_line(size = 1,alpha = 0.5)+ xlab("False positive probability") + 
  ylab("True positive probability") + theme(legend.position="none") +
  annotate(geom="text", x=0.7, y=0.1, label=paste("Original data AUC =",round(roc.list[[1]]$auc,4) ),color="#F8766D",size = 5)+
  annotate(geom="text", x=0.7, y=0.2, label=paste("Polyreg Imputation AUC =",round(roc.list[[3]]$auc,4) ),color="#7CAE00",size = 5)+
  annotate(geom="text", x=0.7, y=0.3, label=paste("LDA Imputation AUC =",round(roc.list[[4]]$auc,4) ),color="#00BFC4",size = 5)+
  annotate(geom="text", x=0.7, y=0.4, label=paste("CART Imputation AUC =",round(roc.list[[5]]$auc,4) ),color="#DB72FB",size = 5)+
  geom_abline(intercept =0 , slope = 1,  linetype= 2, alpha = 0.5)
