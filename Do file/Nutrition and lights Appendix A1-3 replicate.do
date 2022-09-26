*--------------------------------------------------------------------------*
****************Nutrition and Light Analysis Replication**********
*--------------------------------------------------------------------------*

/* This do file replicates  Appendix A Tables 1-3
Before you start, please apply to DHS for microdata access and then download the Nigerian DHS Data from the DHS program including the geography files in the GE folders (remember that you cannot redistribute the DHS micro data).
*/

*Stata version 16.1
*set maxvar 120000 
*ssc install mmerge, replace //if not installed
*net install collin, replace //if not installed

clear

*************Setting up environment variable *************
global Root "G:\My Drive\Nutrition and light replicated" /*"C:\Users\niyia\Desktop\Night-lights-and-Nutrition-Replication-Alimi-Boe-Gibson-and-Gibson--main"*/ //change root to where downloaded files is stored in your computer

/* Run only once to create directory
mkdir "${Root}\Data"
mkdir "${Root}\Log"
mkdir "${Root}\Output"
*/


global Data      "${Root}\Data"
global Log       "${Root}\Log"
global Output    "${Root}\Output"
global Raw       "${Root}\Raw"
global DHS2013   "${Raw}\DHS 2013-data" //Location of 2013 DHS Data. Download all files including geography files
global DHS2008   "${Raw}\DHS 2008-data" //Location of 2008 DHS Data. Download all files including geography files

cd  "${Root}\Data"

//log files
capture log close
log using "${Log}\replicate.txt", replace

*--------------------------------------------------------------------------*
**********Data cleaning of 2008 and 2013 DHS data**************************
*--------------------------------------------------------------------------*

*--------------------------------------------------------------------------*
	*2008  Data Cleaning
*--------------------------------------------------------------------------*

	*Can use women or kids recode (IR or KR) files
	use  "${DHS2008}\NGIR53DT\NGIR53FL", clear
	*use  "${DHS2008}\NGKR53DT\NGKR53FL", clear // 18,893 observations
	gen weight = v005/1000000

	keep caseid midx_* v000 v001 v002 v003 v004 v005 v006 v007 v008 v009 v010 v011 v012 v013 v014 v015 v016 v017 v018 v019 v019a v020 v021 v022 v023 v024 v025 v026 v027 v028 v030 v031 v032 v034 v040 v042 v044 v101 v102 v103 v104 v105 v106 v107 v113 v115 v116 v119 v120 v121 v122 v123 v124 v125 v127 v128 v129 v130 v131 v133 v134 v135 v136 v137 v138 v139 v140 v141 v149 v150 v151 v152 v153 v190 v191 v212  v157 v393 v729 v701 v702 v715 awfactt awfactu awfactr awfacte awfactw v155 v191 ml101 v201 hw5_* hw8_* hw11_* b4_* b5_* hw1_* hw70_* hw71_* hw72_* bord_* weight 

	rename (b4_01 b4_02 b4_03 b4_04 b4_05 b4_06 b4_07 b4_08 b4_09) (b4_1 b4_2 b4_3 b4_4 b4_5 b4_6 b4_7 b4_8 b4_9) //sex of child
	rename (b5_01 b5_02 b5_03 b5_04 b5_05 b5_06 b5_07 b5_08 b5_09) (b5_1 b5_2 b5_3 b5_4 b5_5 b5_6 b5_7 b5_8 b5_9) //child is alive
	rename (bord_01 bord_02 bord_03 bord_04 bord_05 bord_06 bord_07 bord_08 bord_09) (bord_1 bord_2 bord_3 bord_4 bord_5 bord_6 bord_7 bord_8 bord_9) //birth order
	reshape long midx_ bord_ hw1_ b5_ hw70_ hw71_ hw72_ hw5_ hw8_ hw11_ b4_, i(caseid) j(child_num)

	order midx_ b5_ hw1_ hw70_ hw71_ hw72_ hw5_ hw8_ hw11_, after(v004)

	tab v135, miss
	keep if v135==1 //usual resident

	drop if midx_==. //not children
	rename midx_ midx
	keep if b5_ ==1 //child is alive


	//Outcome variables
	drop if  hw70 >= 9996  //Missing New WHO standard HAZ, WHZ and WAZ 
	drop if hw70==. //Missing New WHO standard HAZ, WHZ and WAZ 

	gen HAZ = hw70_/100
	gen WAZ= hw71_/100
	gen WHZ = hw72_/100
	gen stunted = cond(HAZ<-2,1,0)
	gen wasted = cond(WHZ<-2,1,0)
	gen underweight = cond(WAZ<-2,1,0)

	numlabel, add
	
	//Controls

	//boy child
	tab b4_, miss
	gen boy_child = cond(b4_==1,1,0)

	//age of child
	tab hw1_, miss
	gen age_of_child = hw1_
	
	//age of child squared
	gen age_of_child_sq = age_of_child^2


	//birth order
	tab bord, miss
	gen birth_order = bord

	//mother education in single years
	tab v133,miss
	gen mother_edu = v133
	replace mother_edu =. if mother_edu==99

	//age of mother at first birth
	tab v212, miss
	gen age_mother_first = v212

	//Woman's partner's education. It may be different from child father's education
	tab v715, miss
	clonevar father_edu = v715 //Partner edu in single years
	replace father_edu =. if inlist(father_edu,98,99)

	//Wealth quintile
	tab v190, miss
	tab v190, gen(wlth_q_)

	//Household has TV
	tab v121, miss
	clonevar has_tv =v121
	replace has_tv=. if has_tv==9

	//Woman reads newspaper (0/1)
	tab v157, miss
	gen reads_newspaper =v157
	tab reads_newspaper
	replace reads_newspaper=. if reads_newspaper==9
	replace reads_newspaper=1 if inlist(reads_newspaper,1,2,3)
	tab v157 reads_newspaper

	//Woman visited family planning worker in the last 12 months
	tab v393, miss
	clonevar visit_family_planning = v393
	replace visit_family_planning=. if visit_family_planning==9
	tab v393 visit_family_planning

	//Descriptive Statistics
	summ HAZ stunted WHZ wasted WAZ underweight boy_child age_of_child birth_order mother_edu age_mother_first father_edu  wlth_q_* has_tv reads_newspaper visit_family_planning 

	save "${Data}\OA_2008_full_cluster.dta", replace //2008 DHS data

*-----------------------------------------------------------------------------*
*Merge with 2008 DMSP Night lights
*-----------------------------------------------------------------------------*

use "${Data}\OA_2008_full_cluster.dta", clear

//Merge with cluster geography files
	preserve
	import dbase using "${DHS2008}\NGGE52FL\NGGE52FL.dbf", clear case(lower) //use geography file from DHS data
	save "${Raw}\DHS2008_cluster_info.dta", replace
	restore

	gen dhsclust = v001 //cluster id
	mmerge dhsclust using "${Raw}\DHS2008_cluster_info.dta", type(n:1) ukeep(latnum longnum urban_rura) //keep latnum and longnum from geography file
	tab _merge
	drop if _merge==2 //drop two clusters that does not have kids cluster 635 and 636)

//Merge with DMSP light
	preserve
	import dbase using "${Raw}\2008_DMSP_DN_2008DHS.dbf", clear case(lower) //DMSP light
	rename dhsclust dhsclust08
	save "${Raw}\2008_DMSP_DN_2008DHS.dta", replace
	restore
	
rename dhsclust dhsclust08
mmerge dhsclust08 using "${Raw}\2008_DMSP_DN_2008DHS.dta",type(n:1) ukeep(rastervalu) urename(rastervalu DMSP_point)  //point value
tab _merge

drop if _merge==2 //drops two clusters that does not have kids - cluster 635 and 636)

//Generating log of lights
foreach var in DMSP_point {
gen l_`var' = ln(`var')
replace l_`var'=0 if l_`var'==. // dealing with zero lights. 
}

//Descriptive of 2008 DHS
summ DMSP_point l_DMSP_point HAZ stunted WHZ wasted WAZ underweight boy_child age_of_child age_of_child_sq birth_order mother_edu age_mother_first father_edu  wlth_q_* has_tv reads_newspaper visit_family_planning 

save "${Data}\OA_2008_full_cluster_DMSP_base.dta", replace

	*--------------------------------------------------------------------------*
	*2013 DHS  Data cleaning
	*--------------------------------------------------------------------------*
	use  "${DHS2013}\NGIR6ADT\NGIR6AFL", clear //Use Mothers recode

	gen weight=v005/1000000

	keep caseid midx_* v000 v001 v002 v003 v004 v005 v006 v007 v008 v009 v010 v011 v012 v013 v014 v015 v016 v017 v018 v019 v019a v020 v021 v022 v023 v024 v025 v026 v027 v028 v030 v031 v032 v034 v040 v042 v044 v101 v102 v103 v104 v105 v106 v107 v113 v115 v116 v119 v120 v121 v122 v123 v124 v125 v127 v128 v129 v130 v131 v133 v134 v135 v136 v137 v138 v139 v140 v141 v149 v150 v151 v152 v153 v190 v191 v212  v157 v393 v729 v701 v702 v715 awfactt awfactu awfactr awfacte awfactw v155 v191 ml101 v201 hw5_* hw8_* hw11_* b4_* b5_* hw1_* hw70_* hw71_* hw72_* bord_* weight

	rename (b4_01 b4_02 b4_03 b4_04 b4_05 b4_06 b4_07 b4_08 b4_09) (b4_1 b4_2 b4_3 b4_4 b4_5 b4_6 b4_7 b4_8 b4_9) //sex of child
	rename (b5_01 b5_02 b5_03 b5_04 b5_05 b5_06 b5_07 b5_08 b5_09) (b5_1 b5_2 b5_3 b5_4 b5_5 b5_6 b5_7 b5_8 b5_9) //child is alive
	rename (bord_01 bord_02 bord_03 bord_04 bord_05 bord_06 bord_07 bord_08 bord_09) (bord_1 bord_2 bord_3 bord_4 bord_5 bord_6 bord_7 bord_8 bord_9) //birth order
	reshape long midx_ bord_ hw1_ b5_ hw70_ hw71_ hw72_ hw5_ hw8_ hw11_ b4_, i(caseid) j(child_num)

	order midx_ b5_ hw1_ hw70_ hw71_ hw72_ hw5_ hw8_ hw11_, after(v004)

	tab v135, miss
	keep if v135==1 //usual resident

	drop if midx_==. //not children
	rename midx_ midx
	keep if b5_ ==1 //child is alive


	drop if  hw70 >= 9996  //Missing New WHO standard HAZ, WHZ and WAZ 
	drop if hw70==. //Missing New WHO standard HAZ, WHZ and WAZ 

	//Generating outcome variables
	gen HAZ = hw70_/100
	gen WAZ= hw71_/100
	gen WHZ = hw72_/100
	gen stunted = cond(HAZ<-2,1,0)
	gen wasted = cond(WHZ<-2,1,0)
	gen underweight = cond(WAZ<-2,1,0)

	numlabel, add

	//boy child
	tab b4_, miss
	gen boy_child = cond(b4_==1,1,0)

	//age of child
	tab hw1_, miss
	gen age_of_child = hw1_
	
	//age of child squared
	gen age_of_child_sq = age_of_child^2

	//birth order
	tab bord, miss
	gen birth_order = bord

	//mother education in single years
	tab v133,miss
	gen mother_edu = v133
	replace mother_edu =. if mother_edu==99

	//age of mother at first birth
	tab v212, miss
	gen age_mother_first = v212

	//Woman's partner's education. It may be different from child father's education
	tab v715, miss
	clonevar father_edu = v715 //Partner edu in single years
	replace father_edu =. if inlist(father_edu,98,99) 

	//Wealth quintile
	tab v190, miss
	tab v190, gen(wlth_q_)

	//Household has TV
	tab v121, miss
	clonevar has_tv =v121
	replace has_tv=. if has_tv==9

	//Woman reads newspaper (0/1)
	tab v157, miss
	clonevar reads_newspaper =v157
	tab reads_newspaper
	replace reads_newspaper=. if reads_newspaper==9
	replace reads_newspaper=1 if inlist(reads_newspaper,1,2)

	//Woman visited  family planning worker in the last 12 months
	tab v393, miss
	clonevar visit_family_planning = v393
	replace visit_family_planning=. if visit_family_planning==9

	summ HAZ stunted WHZ wasted WAZ underweight boy_child age_of_child age_of_child_sq birth_order mother_edu age_mother_first father_edu  wlth_q_* has_tv reads_newspaper visit_family_planning 

	save "${Data}\OA_2013_full_cluster.dta", replace

*-----------------------------------------------------------------------------*
*Merge with 2013 DMSP Night lights
*-----------------------------------------------------------------------------*	
	use "${Data}\OA_2013_full_cluster.dta", clear
	gen dhsclust = v001

	//Merge with cluster geography files
		preserve
		import dbase using "${DHS2013}\NGGE6AFL\NGGE6AFL.dbf", clear case(lower) //use geography file from DHS data
		save "${Raw}\DHS2013_cluster_info.dta", replace
		restore

	mmerge dhsclust using "${Raw}\DHS2013_cluster_info.dta", type(n:1) ukeep(latnum longnum urban_rura) //merge with latnum and longnum from geography file
	tab _merge
	drop if _merge==2 //drop one cluster (cluster 226) that does not have any kids in it

	****************Merge with DMSP Night lights****************************
		preserve
		import dbase using "${Raw}\2013_DMSP_DN_2013DHS.dbf", clear case(lower)
		rename dhsclust dhsclust13
		save "${Raw}\2013_DMSP_DN_2013DHS.dta", replace
		restore

	rename dhsclust dhsclust13
	mmerge dhsclust13 using "${Raw}\2013_DMSP_DN_2013DHS.dta",  type(n:1) ukeep(rastervalu) urename(rastervalu DMSP_point)  // keeping DMSP point value

	tab dhsclust if _merge==1 // DHS clusters 302,373,422,514,557,569, 639 are missing latitude and longitude infomation. This is missing from original DHS data, so no lights for them
	tab dhsclus latnum if _merge==1

	keep if _merge==3 //Exclude those with missing lat and long & one cluster with no kids

	//Creating Log of DMSP
	foreach var in DMSP_point {
	gen l_`var' = ln(`var')
	replace l_`var'=0 if l_`var'==. // dealing with zero lights. 
	}

	//Saving 2013 DHS and DMSP light
	save "${Data}\OA_2013_full_cluster_DMSP_base.dta", replace //2013 DHS data


*-----------------------------------------------------------------------------*
*Appendix Tables 1-3 is based on Pooled data
*-----------------------------------------------------------------------------*	

use "${Data}\OA_2008_full_cluster_DMSP_base.dta", clear
append using "${Data}\OA_2013_full_cluster_DMSP_base.dta"

gen year=v007


local sample l_DMSP_point, boy_child, age_of_child, age_of_child_sq, birth_order, mother_edu, age_mother_first, father_edu,  v190, has_tv, reads_newspaper, visit_family_planning
keep if !missing(`sample')


local controls boy_child, age_of_child, age_of_child_sq, birth_order, mother_edu, age_mother_first, father_edu, v190, has_tv, reads_newspaper, visit_family_planning 
summ  if !missing(`controls') 

summ l_DMSP_point
gen  l_DMSP_point_c=.  
 
foreach var in l_DMSP_point   {
summ `var' if !missing(`controls') 
di r(mean)
replace `var'_c = `var'-r(mean) 
summ `var'_c 
}

summ l_DMSP_point_c  DMSP_point

gen dhsclust2 = dhsclust13 if v007==2013
replace dhsclust2 = dhsclust08+1000 if v007==2008 //This is to ensure that 2013 clusters ID are different from 2008 ones. 2013 IDs is 1000+ 2008 IDs

tab year, gen(yr_)


foreach var in l_DMSP_point_c    {
gen `var'_sq = (`var')^2 
gen `var'_cb = (`var')^3
gen `var'_qd = (`var')^4
gen `var'_fv = (`var')^5
gen `var'_sx = (`var')^6
}

summ HAZ stunted l_DMSP_point l_DMSP_point_c boy_child age_of_child age_of_child_sq birth_order mother_edu age_mother_first father_edu  ib1.v190 has_tv reads_newspaper visit_family_planning 

*********Replicating Appendix Tables 1 to 3************************************

foreach light in l_DMSP_point_c  {

 mat define CN_`light' = J(6,1,.) 
 mat rownames CN_`light' = CN_`light'  CN_`light'_sq  CN_`light'_cb CN_`light'_qd CN_`light'_fv CN_`light'_sx
 mat list  CN_`light'

 local controls boy_child age_of_child age_of_child_sq birth_order mother_edu age_mother_first father_edu wlth_q_2 wlth_q_3 wlth_q_4 wlth_q_5 has_tv reads_newspaper visit_family_planning yr_2
 collin `light'  `controls' 
 mat CN_`light'[1,1]=r(cn)
 local CN_`light' = r(cn)
 di `CN_`light''
 
 
 collin `light' `light'_sq  `controls' 
 mat CN_`light'[2,1]=r(cn)
 local CN_`light'_sq = r(cn)

 collin `light' `light'_sq  `light'_cb  `controls' 
 mat CN_`light'[3,1]=r(cn)
 local CN_`light'_cb = r(cn)
 
 collin `light'  `light'_sq  `light'_cb `light'_qd `controls' 
 mat CN_`light'[4,1]=r(cn)
 local CN_`light'_qd = r(cn)
 
 collin `light'  `light'_sq  `light'_cb `light'_qd `light'_fv `controls'
 mat CN_`light'[5,1]=r(cn)
 local CN_`light'_fv = r(cn)

 collin `light'  `light'_sq  `light'_cb `light'_qd `light'_fv `light'_sx `controls' 
 mat CN_`light'[6,1]=r(cn)
 local CN_`light'_sx = r(cn)

 mat list  CN_`light'
}

mat list  CN_l_DMSP_point_c 

mat CN= CN_l_DMSP_point_c
mat rownames CN = centred squared cube quartic fifth sixth
mat colnames CN = DMSP 
mat list CN

*********Each regression for VIF/BIC/Margins/Joint-test to the power of 6

local controls boy_child age_of_child age_of_child_sq birth_order mother_edu age_mother_first father_edu  ib1.v190 has_tv reads_newspaper visit_family_planning
 
global regopts se bra
local apprep replace
foreach light in  l_DMSP_point_c {
foreach var in HAZ WHZ WAZ {
*local light  l_DMSP_point_c   
*local var HAZ

regress `var' `light' `controls' i.year[w=weight] , cluster(dhsclust2)

	*F-stat
	testparm `light'  
	local F = r(F)
	local pval = r(p)

	*BIC
	estat ic
	local BIC = r(S)[1,6]
	di `BIC'

margins, dydx(*)
matlist r(table)
local margins = r(table)[1,1]
local margins_se = r(table)[2,1]
local margins_p = r(table)[4,1]

local condition_number =  CN[1,1]
di `condition_number'

outreg2 using "${Output}\AppdxA_`light'", excel keep(`light' 2013.year) `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval', Condition Number, `condition_number', BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value),`margins_p')
outreg2 using "${Output}\AppdxA_`light'_full", excel `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval', Condition Number, `condition_number', BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value), `margins_p')

local apprep append

*linear and sq
regress `var' `light' c.`light'#c.`light' `controls' i.year[w=weight] , cluster(dhsclust2)
	
	*F-stat
	testparm `light'  c.`light'#c.`light' 
	local F = r(F)
	local pval = r(p)

	*BIC
	estat ic
	local BIC = r(S)[1,6]
	di `BIC'


margins, dydx(*)
matlist r(table)
local margins = r(table)[1,1]
local margins_se = r(table)[2,1]
local margins_p = r(table)[4,1]

local condition_number =  CN[2,1]
di `condition_number'

outreg2 using "${Output}\AppdxA_`light'", excel keep(`light' c.`light'#c.`light' 2013.year) `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval', Condition Number, `condition_number',  BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value), `margins_p')
outreg2 using "${Output}\AppdxA_`light'_full", excel `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval', Condition Number, `condition_number', BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value), `margins_p')

*linear and sq and cube
regress `var' `light' c.`light'#c.`light' c.`light'#c.`light'#c.`light' `controls' i.year[w=weight] , cluster(dhsclust2)
	
	*F-stat
	testparm `light'  c.`light'#c.`light' c.`light'#c.`light'#c.`light' 
	local F = r(F)
	local pval = r(p)

	*BIC
	estat ic
	local BIC = r(S)[1,6]
	di `BIC'

margins, dydx(*)
matlist r(table)
local margins = r(table)[1,1]
local margins_se = r(table)[2,1]
local margins_p = r(table)[4,1]

local condition_number =  CN[3,1]
di `condition_number'

outreg2 using "${Output}\AppdxA_`light'", excel keep(`light' c.`light'#c.`light' c.`light'#c.`light'#c.`light' 2013.year) `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval', Condition Number, `condition_number', BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value), `margins_p')
outreg2 using "${Output}\AppdxA_`light'_full", excel `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval', Condition Number, `condition_number', BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value), `margins_p')

*linear and sq and cube and quad
regress `var' `light' c.`light'#c.`light' c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light'  `controls' i.year[w=weight] , cluster(dhsclust2)

	*F-stat
	testparm `light'  c.`light'#c.`light' c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light' 
	local F = r(F)
	local pval = r(p)

	*BIC
	estat ic
	local BIC = r(S)[1,6]
	di `BIC'

margins, dydx(*)
matlist r(table)
local margins = r(table)[1,1]
local margins_se = r(table)[2,1]
local margins_p = r(table)[4,1]

local condition_number =  CN[4,1]
di `condition_number'

outreg2 using "${Output}\AppdxA_`light'",excel keep(`light' c.`light'#c.`light' c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light' 2013.year) `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval',  Condition Number, `condition_number', BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value), `margins_p')

outreg2 using "${Output}\AppdxA_`light'_full", excel `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval', Condition Number, `condition_number', BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value), `margins_p')

*linear and sq and cube and quad and fifth
regress `var' `light' c.`light'#c.`light' c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light'#c.`light'  `controls' i.year[w=weight] , cluster(dhsclust2)

	*F-stat
	testparm `light'  c.`light'#c.`light' c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light'#c.`light'  
	local F = r(F)
	local pval = r(p)

	*BIC
	estat ic
	local BIC = r(S)[1,6]
	di `BIC'

margins, dydx(*)
matlist r(table)
local margins = r(table)[1,1]
local margins_se = r(table)[2,1]
local margins_p = r(table)[4,1]

local condition_number =  CN[5,1]
di `condition_number'

outreg2 using "${Output}\AppdxA_`light'", excel keep(`light' c.`light'#c.`light' c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light'#c.`light' 2013.year ) `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval', Condition Number, `condition_number', BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value), `margins_p')

outreg2 using "${Output}\AppdxA_`light'_full", excel `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval', Condition Number, `condition_number', BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value), `margins_p')

*linear and sq and cube and quad and fifth and sixth
regress `var' `light'  c.`light'#c.`light' c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light'#c.`light'  c.`light'#c.`light'#c.`light'#c.`light'#c.`light'#c.`light'  `controls' i.year[w=weight] , cluster(dhsclust2)

	*F-stat
	testparm `light'  c.`light'#c.`light' c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light'#c.`light'  c.`light'#c.`light'#c.`light'#c.`light'#c.`light'#c.`light'
	local F = r(F)
	local pval = r(p)

	*BIC
	estat ic
	local BIC = r(S)[1,6]
	di `BIC'


margins, dydx(*)
matlist r(table)
local margins = r(table)[1,1]
local margins_se = r(table)[2,1]
local margins_p = r(table)[4,1]

local condition_number =  CN[6,1]
di `condition_number'

outreg2 using "${Output}\AppdxA_`light'", excel keep(`light' c.`light'#c.`light' c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light' c.`light'#c.`light'#c.`light'#c.`light'#c.`light'  c.`light'#c.`light'#c.`light'#c.`light'#c.`light'#c.`light' 2013.year) `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval', Condition Number, `condition_number', BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value), `margins_p')

outreg2 using "${Output}\AppdxA_`light'_full", excel `apprep' ${regopts} dec(4) ctitle(`var'_`light') addtext(Child and parental characteristics, Yes) addstat(F-test, `F', F-testP-value, `pval', Condition Number, `condition_number', BIC, `BIC', Margins(dy/dx), `margins', Margins(se), `margins_se', Margins(p-value), `margins_p')

}
}

end



