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

#===========Province==========
#10 = NFL, 11 = PEI, 12 = NS, 13 = NB, 24 = QUEBEC, 35 = ON, 46 = MA, 47 = SA, 48 = AB, 59 = BC, 60
cyc1.1$GEOAGPRV <- as.factor(ifelse(cyc1.1$GEOAGPRV == 96|cyc1.1$GEOAGPRV == 97|cyc1.1$GEOAGPRV == 98|
                                      cyc1.1$GEOAGPRV == 99, NA, cyc1.1$GEOAGPRV))

cyc2.1$GEOCGPRV <- as.factor(ifelse(cyc2.1$GEOCGPRV== 96|cyc2.1$GEOCGPRV == 97|
                                      cyc2.1$GEOCGPRV == 98|cyc2.1$GEOCGPRV == 99, NA, cyc2.1$GEOCGPRV))

cyc3.1$GEOEGPRV <- as.factor(ifelse(cyc3.1$GEOEGPRV== 96|cyc3.1$GEOEGPRV == 97|
                                      cyc3.1$GEOEGPRV == 98|cyc3.1$GEOEGPRV == 99, NA, cyc3.1$GEOEGPRV))

province <- c(cycle1.1$GEOAGPRV, cycle2.1$GEOCGPRV, cycle3.1$GEOEGPRV)

#Recode Northwest Territories,Nunavut, Yukon as 'north' and the rest of the provinces/territories as 'south'.

province <- as.factor(ifelse(province == 60, "NORTH", ifelse(is.na(province), NA, "SOUTH")))


#=============Age=============
# NO MISSING CASE
#*** Linke: Change 59- to 60-
cyc1.1$DHHAGAGE <- ifelse(cyc1.1$DHHAGAGE == 1|cyc1.1$DHHAGAGE == 2|
                            cyc1.1$DHHAGAGE == 3|cyc1.1$DHHAGAGE == 4|
                            cyc1.1$DHHAGAGE == 5,"12-34", cyc1.1$DHHAGAGE)
cyc1.1$DHHAGAGE <- ifelse(cyc1.1$DHHAGAGE == 6|cyc1.1$DHHAGAGE == 7|
                            cyc1.1$DHHAGAGE == 8|cyc1.1$DHHAGAGE == 9|
                            cyc1.1$DHHAGAGE == 10,"35-59", cyc1.1$DHHAGAGE)
cyc1.1$DHHAGAGE <- ifelse(cyc1.1$DHHAGAGE == 11|cyc1.1$DHHAGAGE == 12|
                            cyc1.1$DHHAGAGE == 13|cyc1.1$DHHAGAGE == 14|
                            cyc1.1$DHHAGAGE == 15,"60-", cyc1.1$DHHAGAGE)

cyc2.1$DHHCGAGE <- ifelse(cyc2.1$DHHCGAGE == 1|cyc2.1$DHHCGAGE== 2|
                              cyc2.1$DHHCGAGE == 3|cyc2.1$DHHCGAGE == 4|
                              cyc2.1$DHHCGAGE == 5,"12-34", cyc2.1$DHHCGAGE)
cyc2.1$DHHCGAGE <- ifelse(cyc2.1$DHHCGAGE == 6|cyc2.1$DHHCGAGE == 7|
                            cyc2.1$DHHCGAGE == 8|cyc2.1$DHHCGAGE== 9|
                            cyc2.1$DHHCGAGE == 10,"35-59", cyc2.1$DHHCGAGE)
cyc2.1$DHHCGAGE <- ifelse(cyc2.1$DHHCGAGE == 11|cyc2.1$DHHCGAGE == 12|
                            cyc2.1$DHHCGAGE == 13|cyc2.1$DHHCGAGE == 14|
                            cyc2.1$DHHCGAGE == 15,"60-", cyc2.1$DHHCGAGE)

cyc3.1$DHHEGAGE <- ifelse(cyc3.1$DHHEGAGE == 1|cyc3.1$DHHEGAGE== 2|
                            cyc3.1$DHHEGAGE == 3|cyc3.1$DHHEGAGE == 4|
                            cyc3.1$DHHEGAGE == 5|cyc3.1$DHHEGAGE == 6,
                          "12-34",cyc3.1$DHHEGAGE)
cyc3.1$DHHEGAGE <- ifelse(cyc3.1$DHHEGAGE == 7|cyc3.1$DHHEGAGE == 8|
                            cyc3.1$DHHEGAGE == 9|cyc3.1$DHHEGAGE== 10|
                            cyc3.1$DHHEGAGE == 11,"35-59", cyc3.1$DHHEGAGE)
cyc3.1$DHHEGAGE <- ifelse(cyc3.1$DHHEGAGE == 12|cyc3.1$DHHEGAGE == 13|
                            cyc3.1$DHHEGAGE == 14|cyc3.1$DHHEGAGE == 15|
                            cyc3.1$DHHEGAGE == 16,"60-", cyc3.1$DHHEGAGE)
age <- as.factor(c(cyc1.1$DHHAGAGE, cyc2.1$DHHCGAGE, cyc3.1$DHHEGAGE))


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

racial_origin <- c(cycle1.1$SDCAGRAC, cycle2.1$SDCCGRAC, cycle3.1$SDCEGCGT)

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
