// PullUSMGvsIMGData.do
//
// Pull data for Salman's USMG/IMG project.
//
// 5/5/15: KDS created file from MergeGenSurg.do

/****** change globals here ******/
global fldir "optimal_tradeoffs_code\data\Florida Data\STATA\"
global nydir "optimal_tradeoffs_code\data\New York Data\STATA\"
global indir "optimal_tradeoffs_code\data\"
global outdir "optimal_tradeoffs_code\data\"

global dataVars "obsid year state sidx ridx hidx"
global ptVars "age age_dec mf race_cat ethnicity med_income payer_cat comorb comorb_cat ptstate ptcounty"
global listVars "dx1-dx32 poa1-poa30 pr1-pr31 days1-days31"
global comorbVars "aids alcohol anemdef arth bldloss chf chrnlung coag depress dm dmcx drug htn_c hypothy liver lymph lytes mets neuro obese para perivasc psych pulmcirc renlfail tumor ulcer valve wghtloss"
global hospVars "hospid fac_county fips_code htype setting bedsize gen_quint surg_quint" 
global surgVars "emergency admsrc adm_prior proc_type lap idxind idxdays idxproc idxdes nprocs score elective first24 transfer"
global procTypes "femoral inguinal umbilical ventral oth_hernia chole gb_anast hepatectomy panc_dist panc_other spleen esophagus reflux bariatric gastrectomy gast_ec gast_eu gast_cx ulcer enterectomy colon_part colon_tot appy ostomy proctectomy proctopexy adhesions mast_simp mast_rad adrenal parathyroid thyroid"
global physVars "yearsold mddec decexp subspec AgeNow DOFlag MedSchoolID MedSchoolYOG MedTrainFrom MedicalSchoolState PrimarySpecialty SecondarySpecialty mdsex USTrained YOB YOT licno rescity rescode resdecade resend resspec1 resspec2 resstate yearsexp s_gensurg s_othsurg spec1 spec2 reslength yearsexp acgme final_name img"
global resVars "prog_offered affiliation government research"
global outcomeVars "died los prolonged complication cmp_* failresc"

set more off
/****** end changes ******/


/************ merge *************/
use "${indir}FloridaAllYearsGS.dta", clear
rename admsrc src_fl
label define fllbl 1 "non-health care" 2 "clinic or physician's office" 3 "HMO" 4 "transfer hospital" 5 "transfer nursing" 6 "transfer other" 7 "ED" 8 "court/law" 9 "unknown" 10 "normal delivery" 11 "premature delivery" 12 "sick baby" 13 "outside delivery" 14 "other" 15 "within hospital" 16 "ambulatory surgery" 17 "hospice"
la val src_fl fllbl
append using "${indir}NewYorkAllYearsGS.dta"
rename admsrc src_ny
label define nylbl 1 "non-health care" 2 "clinic" 3 "other" 4 "transfer between hospitals" 5 "transfer nursing" 6 "transfer other" 7 "ED" 8 "court/law" 9 "unknown" 10 "transfer rural" 11 "transfer within hospital" 12 "transfer ambulatory" 13 "transfer hospice"
la val src_ny nylbl
merge m:1 licno using "${indir}SurgInfoStates.dta", keep(match master) nogen
compress

/************** clean data *****************/
capture label drop yesno
label define yesno 0 "No" 1 "Yes"

label define datalbl 1 "California" 2 "Florida" 3 "New York"
label values state datalbl

// standardize name
rename losdays los

// standardize diagnosis and procedure variables names
rename prindiag dx1
rename admitdiag dx2
rename othdiag# dx#, renumber(3)

rename prinproc pr1
rename othproc# pr#, renumber(2)
order pr*, seq last
order dx*, seq last
order days*, seq last
order poa*, seq last

// save "${dirname}MergedGS.dta", replace

// code obesity for gastric surgery
gen obeseflg = 0
forval i=1/32 {
	replace obeseflg = 1 if inrange(dx`i', "2780", "27801") | dx`i'=="27803" ///
		| inrange(dx`i', "64910", "64914") | inrange(dx`i', "V8530", "V8545") ///
		| dx`i'=="V8554" | dx`i'=="79391"
}

// code admission information
gen admsrc = .
la def srccom 1 "non-health care" 2 "transfer from clinic" 3 "transfer from hospital" 4 "transfer from nursing" ///
	5 "transfer from other" 6 "transfer from ambulatory" 7 "transfer from hospice" 8 "transfer within hospital" ///
	9 "court/law enforcement" 10 "emergency department" 100 "unknown"
la val admsrc srccom

replace admsrc = 1 if inlist(src_fl, 1, 3) | src_ny == 1
replace admsrc = 2 if src_fl == 2 | src_ny == 2
replace admsrc = 3 if src_fl == 4 | inlist(src_ny, 4, 10)
replace admsrc = 4 if src_fl == 5 | src_ny == 5
replace admsrc = 5 if src_fl == 6 | src_ny == 6
replace admsrc = 6 if src_fl == 16 | src_ny == 12
replace admsrc = 7 if src_fl == 17 | src_ny == 13
replace admsrc = 8 if src_fl == 15 | src_ny == 11
replace admsrc = 9 if src_fl == 8 | src_ny == 8
replace admsrc = 10 if src_fl == 7 | src_ny == 7 | condtn == "P7" | edflag == "Y"
replace admsrc = 100 if admsrc == . & (inlist(src_fl, 9, 14) | inlist(src_ny, 3, 9))

replace admsrc = . if adm_prior == 4 // no source for newborns
note admsrc: Codes 1 and 2 may be wrong for Florida pre-2010.
note admsrc: Definition of code 10 changed in 2010-11.
// FL 1:	pre-2010 referral from pt's phys
//			2010 upon order of phys
//			post-2010 from outside health system
// FL 2:	pre-2010 referral from fac's clinic phys
//			2010 tranfer/referral from phys
//			post-2010 admitted from clinic
// FL 3: referral from HMO (discontinued 2010)
// NY 3: reserved for assignment
// ED discontinued 7/1/10 in NY, 2011 in FL
drop src_fl src_ny

replace emergency = admsrc == 10
gen transfer = inrange(admsrc, 2, 8)
gen elective = adm_prior == 3

la val emergency transfer elective yesno

/************ clean surgeon data **********/
gen s_gensurg = 0
replace s_gensurg = 1 if spec1=="GS" | spec2=="GS"
gen s_othsurg = 0
replace s_othsurg = 1 if (strpos(PrimarySpecialty,"Surg")>0 & spec1!="GS") ///
	| (strpos(SecondarySpecialty,"Surg")>0 & spec2!="GS") 
la val s_gensurg s_othsurg yesno

gen subspec = spec1 ~= "GS" | ~inlist(spec2, "GS", "US")
la val subspec yesno

label define dolbl 0 "MD" 1 "DO"
label values DOFlag dolbl
label define us 0 "yes" 1 "no"
label values USTrained us
label define trainlbl 0 "res completed" 1 "res in progress"
label values MedTrainFlag trainlbl

gen yearsexp = year - YOT
gen decexp = floor(yearsexp/10)
la val decexp agelbl

//rename Sex mdsex
encode Sex, gen(mdsex)
drop resyr

gen img = USTrained
la val img yesno

gen yearsold = year - YOB
gen mddec = floor(yearsold/10)
replace mddec = . if mddec>9
la values mddec agelbl


/************ clean program info ************/
// encode specialties offered by res program
la def origlbl 1 "surgery - general" 2 "surgery - general / CR surg" 3 "surgery - general / CGSO" 4 "surgery - general / CGSO / CR surg" 5 "CR surg" 6 "CGSO"
encode avail_spec , gen(prog_offered) label(origlbl)
replace prog_offered = . if prog_offered > 6
label def newlbl 1 "GS" 2 "GS/CRS" 3 "GS/CGSO" 4 "GS/CRS/CGSO" 5 "CRS" 6 "CGSO"
label val prog_offered newlbl
label var prog_offered "programs offered"

// encode affiliation of res program
rename affiliation tmp
encode tmp, gen(affiliation)
la define affiliation 2 "university-affiliated", modify
drop tmp
rename government tmp
encode tmp, gen(government) label(yesno)
drop tmp


/*********** split hospital variables *********/
capture drop setting bedsize
gen setting = .
replace setting = 1 if htype==1 | htype==2
replace setting = 2 if htype==3 | htype==4
replace setting = 3 if htype > 4 & htype < .
la def ownrur 1 "investor-owned" 2 "not-for-profit rural" 3 "not-for-profit urban"
la val setting ownrur

gen bedsize = .
replace bedsize = 1 if htype == 1 | htype == 3 | htype == 5
replace bedsize =2 if htype == 2 | htype == 4 | htype == 6
replace bedsize = 3 if htype == 7
la def size 1 "small" 2 "medium" 3 "large"
la val bedsize size


/********** code procedure type *********/

// make binary variable for each procedure
foreach vname of global procTypes {
	capture drop `vname'
	capture drop i_`vname'
	gen `vname' = 0
	gen i_`vname' = .
}
la val $procTypes yesno

forval i=1/31 {
	// hernia
	replace femoral = 1 if inrange(pr`i', "532", "5339") // femoral
	replace i_femoral = `i' if i_femoral == . & femoral
	replace inguinal = 1 if (inrange(pr`i', "171", "1724") | inrange(pr`i', "5300", "5317")) // inguinal
	replace i_inguinal = `i' if i_inguinal == . & inguinal
	replace umbilical = 1 if pr`i' >= "534" & pr`i' <= "5349" // umbilical
	replace i_umbilical = `i' if i_umbilical == . & umbilical
	replace ventral = 1 if pr`i' >= "535" & pr`i' <= "5369" // ventral
	replace i_ventral = `i' if i_ventral == . & ventral
	replace oth_hernia = 1 if pr`i' >= "537" & pr`i' <= "5384" // other
	replace i_oth_hernia = `i' if i_oth_hernia == . & oth_hernia
	
	// abdomen
	replace chole = 1 if pr`i' >= "512" & pr`i' <= "5124" // chole
	replace i_chole = `i' if i_chole == . & chole
	replace gb_anast = 1 if pr`i' >= "513" & pr`i' <= "5139" // gallbladder anast
	replace i_gb_anast = `i' if i_gb_anast == . & gb_anast
	replace hepatectomy = 1 if inlist(pr`i', "5022", "5023", "5025", "503", "504") // liver
	replace i_hepatectomy = `i' if i_hepatectomy == . & hepatectomy
	replace panc_dist = 1 if pr`i' == "5252" // distal pancreas
	replace i_panc_dist = `i' if i_panc_dist == . & panc_dist
	replace panc_other = 1 if inlist(pr`i', "5251", "5253", "5259", "526", "527") // other pancreas
	replace i_panc_other = `i' if i_panc_other == . & panc_other
	replace spleen = 1 if inlist(pr`i', "4142", "4143", "415") // spleen
	replace i_spleen = `i' if i_spleen == . & spleen
	
	// esophagus/stomach
	replace esophagus = 1 if (pr`i' >= "424" & pr`i' <= "4242") | pr`i' == "427" // esophagus
	replace i_esophagus = `i' if i_esophagus == . & esophagus
	replace reflux = 1 if pr`i' >= "4465" & pr`i' <= "4467" // antireflux
	replace i_reflux = `i' if i_reflux == . & reflux
	replace bariatric = 1 if obeseflg & (inlist(pr`i', "437", "4382", "4389", "4391", "4399", "4431", "4438", "4439", "4468") ///
		| inlist(pr`i', "4495", "4498", "5596")) // bariatric
	replace i_bariatric = `i' if i_bariatric == . & bariatric
	replace gastrectomy = 1 if inrange(pr`i', "435", "4369") | pr`i' == "4381" ///
		| (~obeseflg & inlist(pr`i', "437", "4382", "4389", "4391", "4399")) // gastrectomy
	replace i_gastrectomy = `i' if i_gastrectomy == . & gastrectomy
	replace gast_ec = 1 if ~obeseflg & inlist(pr`i', "443", "4438", "4439") // EC gastric
	replace i_gast_ec = `i' if i_gast_ec == . & gast_ec
	replace gast_eu = 1 if inlist(pr`i', "4400", "4401") // EU gastric
	replace i_gast_eu = `i' if i_gast_eu == . & gast_eu
	replace gast_cx = 1 if inlist(pr`i', "4402", "4403", "445") // complex
	replace i_gast_cx = `i' if i_gast_cx == . & gast_cx
	replace ulcer = 1 if pr`i' >= "4440" & pr`i' <= "4442" // ulcer 
	replace i_ulcer = `i' if i_ulcer == . & ulcer
	
	// intestines
	replace enterectomy = 1 if (pr`i' >= "456" & pr`i' <= "4563") // small bowel
	replace i_enterectomy = `i' if i_enterectomy == . & enterectomy
	replace colon_part = 1 if (pr`i' >= "173" & pr`i' <= "1739") | (pr`i' >= "4571" & pr`i' <= "4579") // partial colon
	replace i_colon_part = `i' if i_colon_part == . & colon_part
	replace colon_tot = 1 if inrange(pr`i', "458", "4583")  // total colon
	replace i_colon_tot = `i' if i_colon_tot == . & colon_tot
	replace appy = 1 if inlist(pr`i', "470", "4701", "4709") // appy
	replace i_appy = `i' if i_appy == . & appy
	replace ostomy = 1 if inlist(pr`i', "4601", "4603") | (pr`i' >= "461" & pr`i' <= "4639") ///
		| (pr`i' >= "465" & pr`i' <= "4652") // ostomy
	replace i_ostomy = `i' if i_ostomy == . & ostomy
	//replace anast = 1 if inrange(pr`i', "459", "4595") // anastomosis
	replace proctectomy = 1 if pr`i' >= "484" & pr`i' <= "4869" // proctectomy 
	replace i_proctectomy = `i' if i_proctectomy == . & proctectomy
	replace proctopexy = 1 if inlist(pr`i', "4875", "4876") // proctopexy
	replace i_proctopexy = `i' if i_proctopexy == . & proctopexy
	replace adhesions = 1 if inlist(pr`i', "5451", "5459") // adhesiolysis
	replace i_adhesions = `i' if i_adhesions == . & adhesions
	//replace fistula = 1 if inlist(pr1, "4672", "4674", "4676") // closure of fistula
	
	// breast
	replace mast_simp = 1 if inrange(pr`i', "8541", "8544") // simple mastectomy
	replace i_mast_simp = `i' if i_mast_simp == . & mast_simp
	replace mast_rad = 1 if inrange(pr`i', "8545", "8548") // radical mastectomy
	replace i_mast_rad = `i' if i_mast_rad == . & mast_rad
	
	// endocrine
	replace adrenal = 1 if (pr`i' >= "070" & pr`i' <= "0702") | (pr`i' >= "072" & pr`i' <= "073") // adrenal
	replace i_adrenal = `i' if i_adrenal == . & adrenal
	replace parathyroid = 1 if inlist(pr`i', "068", "0681", "0689") // parathyroid
	replace i_parathyroid = `i' if i_parathyroid == . & parathyroid
	replace thyroid = 1 if pr`i' >= "062" & pr`i' <= "0652" // thyroid
	replace i_thyroid = `i' if i_thyroid == . & thyroid
}	


egen nprocs = rowtotal($procTypes)

// make single variable to define index procedure
label define proclbl 1 "femoral hernia" 2 "inguinal hernia" 3 "umbilical hernia" 4 "ventral hernia" ///
	5 "other hernia" 6 "cholecystectomy" 7 "gallbladder anast" 8 "hepatectomy" 9 "distal pancreatectomy" ///
	10 "other pancreatectomy" 11 "splenectomy" 12 "esophagectomy" 13 "antireflux" 14 "bariatric" 15 "gastrectomy" ///
	16 "EC gastric" 17 "EU gastric" 18 "complex gastric" 19 "ulcer" 20 "enterectomy" 21 "partial colectomy" ///
	22 "total colectomy" 23 "appendectomy" 24 "ostomy" 25 "proctectomy" 26 "proctopexy" 27 "adhesions" ///
	28 "simple mastectomy" 29 "radical mastectomy" 30 "adrenalectomy" 31 "parathyroidectomy" 32 "thyroidectomy"
gen proc_type = . /* procedure used to classify */
label values proc_type proclbl
gen idxind = . /* index to procedure used to classify */
gen idxdays = . /* day of index procedure */
gen idxproc = "" /* ICD-9 code for index procedure */

replace proc_type = 9 if panc_dist
replace proc_type = 10 if panc_other & proc_type == .
replace proc_type = 12 if esophagus & proc_type == .
replace proc_type = 15 if gastrectomy & proc_type == .
replace proc_type = 18 if gast_cx & proc_type == .
replace proc_type = 17 if gast_eu & proc_type == .
replace proc_type = 13 if reflux & proc_type == .
replace proc_type = 8 if hepatectomy & proc_type == .
replace proc_type = 6 if chole & proc_type == .
replace proc_type = 7 if gb_anast & proc_type == .
replace proc_type = 14 if bariatric & proc_type == .
replace proc_type = 16 if gast_ec & proc_type == .
replace proc_type = 22 if colon_tot & proc_type == .
replace proc_type = 25 if proctectomy & proc_type == .
replace proc_type = 11 if spleen & proc_type == .
replace proc_type = 20 if enterectomy & proc_type == .
replace proc_type = 21 if colon_part & proc_type == .
replace proc_type = 26 if proctopexy & proc_type == .
replace proc_type = 23 if appy & proc_type == .
replace proc_type = 24 if ostomy & proc_type == .
replace proc_type = 4 if ventral & proc_type == .
replace proc_type = 1 if femoral  & proc_type == .
replace proc_type = 2 if inguinal & proc_type == .
replace proc_type = 3 if umbilical & proc_type == .
replace proc_type = 5 if oth_hernia & proc_type == .
replace proc_type = 29 if mast_rad & proc_type == .
replace proc_type = 28 if mast_simp & proc_type == .
replace proc_type = 30 if adrenal & proc_type == .
replace proc_type = 31 if parathyroid & proc_type == .
replace proc_type = 32 if thyroid & proc_type == .
replace proc_type = 27 if adhesions & proc_type == .
replace proc_type = 19 if ulcer & proc_type == .

replace idxind = i_femoral if proc_type == 1
replace idxind = i_inguinal if proc_type == 2
replace idxind = i_umbilical if proc_type == 3
replace idxind = i_ventral if proc_type == 4
replace idxind = i_oth_hernia if proc_type == 5
replace idxind = i_chole if proc_type == 6
replace idxind = i_gb_anast if proc_type == 7
replace idxind = i_hepatectomy if proc_type == 8
replace idxind = i_panc_dist if proc_type == 9
replace idxind = i_panc_other if proc_type == 10
replace idxind = i_spleen if proc_type == 11
replace idxind = i_esophagus if proc_type == 12
replace idxind = i_reflux if proc_type == 13
replace idxind = i_bariatric if proc_type == 14
replace idxind = i_gastrectomy if proc_type == 15
replace idxind = i_gast_ec if proc_type == 16
replace idxind = i_gast_eu if proc_type == 17 
replace idxind = i_gast_cx if proc_type == 18 
replace idxind = i_ulcer if proc_type == 19
replace idxind = i_enterectomy if proc_type == 20
replace idxind = i_colon_part if proc_type == 21
replace idxind = i_colon_tot if proc_type == 22
replace idxind = i_appy if proc_type == 23
replace idxind = i_ostomy if proc_type == 24
replace idxind = i_proctectomy if proc_type == 25
replace idxind = i_proctopexy if proc_type == 26
replace idxind = i_adhesions if proc_type == 27
replace idxind = i_mast_simp if proc_type == 28
replace idxind = i_mast_rad if proc_type == 29
replace idxind = i_adrenal if proc_type == 30
replace idxind = i_parathyroid if proc_type == 31
replace idxind = i_thyroid if proc_type == 32

// find day of index procedure and ICD-9 code
forval i=1/31 {
	replace idxdays = days`i' if idxind == `i'
	replace idxproc = pr`i' if idxind == `i'
}
icd9p gen idxdes = idxproc, des long
gen first24 = idxdays == 0
la val first24 yesno

// lap  456-4563
gen lap = .
la val lap yesno
replace lap = 0 if inlist(idxproc, "4389", "4431", "4439", "4465", "4466", "4582", "4709", "4843", "4849") ///
	| inlist(idxproc, "4852", "4859", "5121", "5122", "5341", "5349", "5361", "5369", "5459") ///
	| inrange(idxproc, "5300", "5317") | inrange(idxproc, "4571", "4579")
replace lap = 1 if inlist(idxproc, "4382", "4467", "4438", "4468", "4495", "4496", "4498", "4581", "4701") ///
	| inlist(idxproc, "4842", "4851", "5123", "5124", "5342", "5343", "5362", "5363", "5451") ///
	| inrange(idxproc, "1711", "1724") | inrange(idxproc, "1731", "1739")
gen checkflg = inlist(proc_type, 1, 9, 10, 11, 20, 26)
forval i=1/31 {
	replace lap = 1 if pr`i' == "5421" & days`i' == idxdays & checkflg
}
replace lap = 0 if lap == . & checkflg

// classify by SCORE category
gen score = .
la def score 1 "essential - common" 2 "essential - uncommon" 3 "complex"
replace score = 1 if inlist(proc_type, 1, 2, 3, 4, 6, 11, 16, 21, 23) | inlist(proc_type, 24, 27, 28, 31, 32) ///
	| (proc_type == 13 & lap) | (proc_type == 20 & ~lap)
replace score = 2 if inlist(proc_type, 22, 7, 15, 17, 29, 5, 9, 26, 19) ///
	| (proc_type == 13 & ~lap) | (proc_type == 20 & lap)
replace score = 3 if inlist(proc_type, 8, 10, 12, 14, 18, 25, 30)
la val score score


/********** code outcomes *********/
// find prolonged LOS by hospital and procedure
bysort hospid proc_type: egen loscut = pctile(los), p(75)
gen prolonged = (los > loscut) if los < .
la val prolonged yesno

// recode complications
capture drop complication cmp_*

// make codes
gen cmp_asp = 0 /* aspiration or other respiratory complications */
gen cmp_card = 0 /* cardiac complications */
gen cmp_pneu = 0 /* pneumonia */
gen cmp_inf = 0 /* post-op infection */
gen cmp_pulm = 0 /* pulmonary insufficiency */
gen cmp_renl = 0 /* acute renal failure */
gen cmp_sept = 0 /* septicemia */
gen cmp_surg = 0 /* surgical complications */
gen cmp_bld = 0 /* transfusion */
gen cmp_tpn = 0 /* total parenteral nutrition */
gen cmp_obst = 0 /* obstruction requiring return to OR */
gen cmp_ret = 0 /* return to OR */
gen cmp_vte = 0 /* DVT or PE */
gen cmp_ns = 0 /* nervous system */
gen cmp_voc = 0 /* hoarseness */

// label variables
la var cmp_asp "aspiration or other respiratory complications"
la var cmp_card "cardiac complications"
la var cmp_pneu "pneumonia"
la var cmp_inf "post-op infection"
la var cmp_pulm "pulmonary insufficiency"
la var cmp_renl "acute renal failure"
la var cmp_sept "septicemia"
la var cmp_surg "surgical complications"
la var cmp_bld "transfusion"
la var cmp_obst "obstruction requiring return to OR"
la var cmp_ret "return to OR"
la var cmp_vte "DVT or PE"
la var cmp_ns "nervous system"
la var cmp_voc "hoarseness"

// code complication if not specified as POA
forval i=1/32 {
	replace cmp_asp = 1 if poa`i'~=2 & (inrange(dx`i', "9973", "99739") | inrange(dx`i', "507", "5079"))
	replace cmp_card = 1 if poa`i'~=2 & dx`i' == "9971"
	replace cmp_pneu = 1 if poa`i'~=2 & (inrange(dx`i', "481", "48283") | inlist(dx`i', "48289", "4829", "483", "4838", "484") ///
		| inrange(dx`i', "4847", "48699"))
	replace cmp_inf = 1 if poa`i'~=2 & inlist(dx`i', "9985", "99851", "99859")	
	replace cmp_pulm = 1 if poa`i'~=2 & ///
		(inlist(dx`i', "5184", "5185", "51851", "51852", "51853") | inrange(dx`i', "5188", "51889"))
	replace cmp_renl = 1 if poa`i'~=2 & inrange(dx`i', "584", "5849")
	replace cmp_sept = 1 if poa`i'~=2 & (inrange(dx`i', "038", "0389") | inlist(dx`i', "7907"))
	replace cmp_obst = 2 if poa`i'~=2 & inlist(dx`i', "5570", "5608", "56081", "5609", "9974", "99741", "99749")
	replace cmp_bld = 2 if poa`i'~=2 & inlist(dx`i', "9981", "9982")
	replace cmp_surg = 1 if poa`i'~=2 & ///
		(inrange(dx`i', "9981", "99832") | inlist(dx`i', "9988", "99889", "9989", "E8700", "E8788", "E8789"))
	replace cmp_vte = 1 if poa`i'~=2 & (inlist(dx`i', "41511", "41519") | inrange(dx`i', "4534", "4539"))
	replace cmp_ns = 1 if poa`i'~=2 & (inrange(dx`i', "9970", "99709") | inrange(dx`i', "951", "9519") ///
		| inrange(dx`i', "957", "9579"))
	replace cmp_voc = 1 if poa`i'~=2 & (inrange(dx`i', "4783", "47834") | inrange(dx`i', "7844", "78449"))
}

forval i=1/31 {
	replace cmp_bld = 1 if cmp_bld==2 & (pr`i'=="5461" | inrange(pr`i', "990", "9909"))
	replace cmp_obst = 1 if cmp_obst==2 & inlist(pr`i', "5411", "5412", "5461")
	replace cmp_ret = 1 if inlist(pr`i', "0602", "3403", "3409", "5411", "5412", "5461", "5492")
}
replace cmp_obst = 0 if cmp_obst == 2 /* only count obstruction if returned to OR */
replace cmp_bld = 0 if cmp_bld == 2 /* only count bleeding if blood transfusion */

// overall complications
egen complication = anymatch(cmp_*), values(1)
la val complication cmp_* yesno

// failure-to-rescue
gen failresc = .
replace failresc = died if (complication | died) & first24 & elective & ~transfer
la val failresc yesno

/********** merge with Hospital Compare data ********/
merge m:1 hospid using "${dirname}HospCompareQuints.dta", keep(match master) nogen
replace avgsurgscorequintile = 0 if avgsurgscorequintile == .
replace VBPquintile = 0 if VBPquintile == .
rename avgsurgscorequintile surg_quint
rename VBPquintile gen_quint


/*********** label variables **********/
la var year "Year"
la var age_dec "Age"
la var mf "Sex"
la var payer_cat "Principal payer"
la var comorb_cat "Elixhauser index"
la var state "State"
la var race_cat "Race"
la var ethnicity "Ethnicity"
la var htype "Hospital type"
la var emergency "Admitted through ED"
la var elective "Elective admission"
la var transfer "Transfered in"
la var first24 "Operation on day of admission"
la var score "SCORE category"
la var died "In-hospital mortality"
la var prolonged "Prolonged LOS"
la var complication "Any complication"
la var died "In-hospital mortality"
la var failresc "Failure to rescue"
la var mddec "Physician age"
la var mdsex "Physician sex"
la var resdecade "Training completion"
la var s_gensurg "General surgery specialty"
la var s_othsurg "Other surgical specialty"
la var img "International medical graduate"
la var prog_offered "Training programs offered"
la var affiliation "Residency affiliation"
la var government "Government-affiliated"
la var research "Research requirement"


/*********** save **********/
// only keep relevant variables and observations
noisily di %20s "original cohort" %12.0fc _N
drop if adm_prior == 4
noisily di %20s "not newborn" %12.0fc _N
drop if USTrained == .
noisily di %20s "US/IMG known" %12.0fc _N
drop if ~s_gensurg & ~s_othsurg
noisily di %20s "surgeon" %12.0fc _N

// make indices for summarizing 
egen sidx = tag(licno)
egen hidx = tag(hospid)
egen ridx = tag(acgme)

keep $dataVars $ptVars $listVars $comorbVars $hospVars $surgVars $procTypes $physVars $resVars $outcomeVars
compress
dropmiss, force
save "${outdir}TwoStateIMGUSMG.dta", replace
