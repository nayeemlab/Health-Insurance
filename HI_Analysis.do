clear all 
cd "E:\ResearchProject\Baker Bhai\Cost of Delivery"

set maxvar 32767

*import 2022 data 
use "E:\ResearchProject\Baker Bhai\Health Insurance\BD_2022_DHS_Stata\BDIR81DT\BDIR81FL", replace 

*tables 3.9

*******************************************************************************
*******************************************************************************
*				inclusion exclusion 
********************************************************************************
*******************************************************************************

keep if v020 == 1

gen wgt=v005/1000000
label variable wgt "sample weight"
* check with and without weight 
*tab v025 year
*tab v025 year [iweight=wgt]
svyset[pw=wgt],psu(v021) strata(v022)

svy: tab v481, count format(%9.0f)
svy: tab v481, format(%9.4f)

*(xxx observations deleted)

*******************************************************************************

*age
tab v013
gen age_cat=v013
recode age_cat 1/2=1
recode age_cat 3/4=2
recode age_cat 5/7=3
tab age_cat 
label define age_cat 1 "15-24" 2 "25-34" 3 "35-49"
label val age_cat age_cat
tab age_cat,m

svy: tab age_cat v481, count format(%9.0f)
svy: tab age_cat v481, col format(%9.4f)


* women edducation 
tab v106,m
svy: tab v106 v481, count format(%9.0f)
svy: tab v106 v481, col format(%9.4f)


*Current marital status
*tab v501
*svy: tab v501 v481, count format(%9.0f)
*svy: tab v501 v481, col format(%9.4f)

*Women Currently working
tab v714
svy: tab v714 v481, count format(%9.0f)
svy: tab v714 v481, col format(%9.4f)


*BMI v445
gen bmi=v445/100
gen bmi4a=.
replace bmi4a=0 if bmi<=18.49
replace bmi4a=1 if bmi>=18.50 & bmi<=24.99
replace bmi4a=2 if bmi>=25.00 & bmi<=29.99
replace bmi4a=3 if bmi>=30
tab bmi4a, missing 
label variable bmi4a "asian standard bmi four category"
label define bmi4alabel 0 "Underweight" 1 "Normalweight" 2 "Overweight" 3 "Obese" 
label value bmi4a bmi4alabel
tab bmi4a,m

svy: tab bmi4a v481, count format(%9.0f)
svy: tab bmi4a v481, col format(%9.4f)

*Husband age
tab v730
gen hage_cat=v730
recode hage_cat 15/29=1
recode hage_cat 30/44=2
recode hage_cat 45/95=3
tab hage_cat 
label define hage_cat 1 "15-29" 2 "30-44" 3 "45-95"
label val hage_cat hage_cat
tab hage_cat,m

svy: tab hage_cat v481, count format(%9.0f)
svy: tab hage_cat v481, col format(%9.4f)

*husband education 
tab v701,m 
replace v701=. if v701==8
svy: tab v701 v481, count format(%9.0f)
svy: tab v701 v481, col format(%9.4f)


*husband occupation 
*v705 (Farmer, day labourer, factory worker, driver, service holder, business, other)
tab v704,m
recode v704 99998=.
recode v704 98=.
recode v704 (0 61/62=1 "Not working") (12/15=1 "Farmer, Agricultural,fishing & poultry Worker") (51/52 16=2 "Business") (23 31 41=3 "Carpenter, Masson, Bus/taxi driver, Construction supervisor, Seamstresses/Tailor, Policeman, Armed services, Dai, Community health worker, FWA, Similar services") (21 11 22 96=4 "others"), gen(husband_occu)

label define husband_occu 0 "Not working" 1 "Farmer, Agricultural,fishing & poultry Worker" 2 "Business" 3 "skilled worker" 4 "others", replace
tab v704 husband_occu,m
tab husband_occu v007,m

svy: tab husband_occu v481, count format(%9.0f)
svy: tab husband_occu v481, col format(%9.4f)



*husband currently working
recode v705 (0 = 1) (1 2 3 4 5 6 7 8 9 10= 2) (98=.), generate(husband_works_currently2)
label define occupation_labels1 1 "Not working" 2 "working"
label values husband_works_currently2 occupation_labels1


svy: tab husband_works_currently2 v481, count format(%20.0f)
svy: tab husband_works_currently2 v481, col format(%9.4f)


*residence
tab v025

svy: tab v025 v481, count format(%9.0f)
svy: tab v025 v481, col format(%9.4f)

*diviion 
tab1 v024

recode v024 (3=0 "Dhaka") (1/2 4/8=1 "Others"), gen(Division)

label define Division 0 "Dhaka" 1 "Others", replace
tab v024 Division,m

svy: tab Division v481, count format(%9.0f)
svy: tab Division v481, col format(%9.4f)

*religion ( muslim and others)
tab v130
gen religon_cat=v130
recode religon_cat 1=1
recode religon_cat 2/4=2
recode religon_cat 96=.
label define religon_catlabel 1 "Islam" 2 "Others" 
label value religon_cat religon_catlabel
tab religon_cat v130,m

svy: tab religon_cat v481, count format(%9.0f)
svy: tab religon_cat v481, col format(%9.4f)

* wealth index 
tab v190

recode v190 (1/2=1 "Poor") (3=2 "Middle") (4/5=3 "Rich"), gen(WealthIndex)

label define WealthIndex 1 "Poor" 2 "Middle" 3 "Rich", replace

svy: tab WealthIndex v481, count format(%9.0f)
svy: tab WealthIndex v481, col format(%9.4f)

*household size  (<4, 4-5, >5)
tab v136
gen household_member=v136
recode household_member 1/3=1
recode household_member 4/5=2
recode household_member 5/30=3
label define household_member 1 "<4" 2 "4-5" 3 ">5" 
label val household_member household_member
tab household_member,m

svy: tab household_member v481, count format(%9.0f)
svy: tab household_member v481, col format(%9.4f)


*media
svy: tab v121
gen tv=v121
svy: tab tv
recode tv 0=0
recode tv 1=1
recode tv 7=0


svy: tab tv v481, count format(%20.0f)
svy: tab tv v481, col format(%9.4f)


svy: tab v120
gen radio=v120
svy: tab radio
recode radio 0=0
recode radio 1=1
recode radio 7=0


svy: tab radio v481, count format(%20.0f)
svy: tab radio v481, col format(%9.4f)

*internet
gen inte=v171a
recode inte 0=0
recode inte 1=1
recode inte 2=1

svy:tab inte v481,count format(%20.0f)
svy: tab inte v481, col format(%9.4f)

gen media1 = tv + radio + inte
svy: tab media1
recode media1 0=0
recode media1 1/3=1
label define media1 0 "No" 1 "Yes"
label values media media1


svy: tab media1 v481, count format(%20.0f)
svy: tab media1 v481, col format(%9.4f)




*number of living children 
tab v218

recode v218 (0=1 "0") (1/2=2 "1-2") (3/10=3 "3+"), gen(numofchild)

label define numofchild 1 "0" 2 "1-2" 3 "3+", replace

svy: tab numofchild v481, count format(%9.0f)
svy: tab numofchild v481, col format(%9.4f)

*Logistic Model

set cformat %9.2f
svy: logit v481 ib2.age_cat, or
svy: logit v481 i.v106, or
svy: logit v481 i.v714, or
svy: logit v481 ib3.bmi4a, or
svy: logit v481 i.hage_cat, or
svy: logit v481 i.v701, or
svy: logit v481 i.husband_occu, or
svy: logit v481 ib2.v025, or
svy: logit v481 ib1.Division, or
svy: logit v481 ib1.religon_cat, or
svy: logit v481 ib1.WealthIndex, or
svy: logit v481 ib3.household_member, or
svy: logit v481 ib3.numofchild, or

svy: logit v481 ib2.age_cat i.v106 i.v714 ib3.bmi4a i.hage_cat i.v701 i.husband_occu ib2.v025 ib1.Division ib1.religon_cat ib1.WealthIndex ib3.household_member ib3.numofchild, or

regress v481 age_cat v106 v714 bmi4a hage_cat v701 husband_occu v025 Division religon_cat WealthIndex household_member numofchild
vif
rocreg

logit v481 ib2.age_cat i.v106 i.v714 ib3.bmi4a i.hage_cat i.v701 i.husband_occu ib2.v025 ib1.Division ib1.religon_cat ib1.WealthIndex ib3.household_member ib3.numofchild, or
estat ic

lroc
roctab v481 p
