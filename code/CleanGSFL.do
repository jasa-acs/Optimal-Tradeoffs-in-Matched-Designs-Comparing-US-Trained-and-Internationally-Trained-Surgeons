// CleanGSFL.do
//
// Clean general surgery data from Florida.
//
// 12/16/14: KDS created file.


/****** change globals here ******/
global indir "optimal_tradeoffs_code\data\Florida Data\STATA\"
global outdir "optimal_tradeoffs_code\data\"
set more off
/****** end changes ******/


// loop over years:
foreach yr in "08" "09" "10" "11" "12" {

	// loop over quarters within each year
	foreach qtr in "1" "2" "3" "4" {
		
		noisily display "processing 20`yr' quarter `qtr' data..."
		
		quietly {
		
		use "${indir}inp`yr'q`qtr'gs.dta", clear
		quietly: compress // reduce variable to smaller data types when possible
		save "${outdir}inp`yr'q`qtr'gs.dta", replace

		// give labels to variable values:
		label define yesno 0 "No" 1 "Yes"
		label values chf valve pulmcirc perivasc para neuro chrnlung dm dmcx hypothy renlfail liver ulcer aids lymph mets tumor arth coag obese wghtloss lytes bldloss anemdef alcohol drug psych depress htn_c yesno

		// encode string variables:
		label define sexorig 1 "M" 2 "F" 3 "U"
		label define sexlbl 1 "male" 2 "female" 3 "unknown"
		encode sex, gen(tmp) label(sexorig)
		drop sex
		rename tmp sex
		label values sex sexlbl
		gen mf = .
		replace mf=sex if sex==1 | sex==2
		label values mf sexlbl

		encode ethnicity, gen(tmp)
		drop ethnicity
		rename tmp ethnicity
		label define ethlbl 1 "Hispanic" 2 "Non-Hispanic" 3 "Unknown"
		label values ethnicity ethlbl
		label drop tmp

		gen tmp = real(race)
		drop race
		rename tmp race
		label values race racelbl
		label define racelbl 1 "American Indian" 2 "Asian" 3 "Black" 4 "Pacific Islander" 5 "White" 6 "Other" 7 "Unknown"

		gen race_cat = .
		replace race_cat = 1 if race==5
		replace race_cat = 2 if race==3
		replace race_cat = 3 if race==1 | race==2 | race==4 | race==6
		label define race_cat 1 "white" 2 "black" 3 "other"
		label values race_cat race_cat

		gen tmp = real(weekday)
		drop weekday
		rename tmp weekday
		label define daylbl 1 "Monday" 2 "Tuesday" 3 "Wednesday" 4 "Thursday" 5 "Friday" 6 "Saturday" 7 "Sunday"
		label values weekday daylbl

		gen tmp = real(dischstat)
		drop dischstat
		rename tmp dischstat
		label define dischlbl 1 "home" 2 "short-term gen hosp" 3 "Medicare nursing" 4 "intermediate care" 5 "cancer center" ///
			6 "home health care" 7 "left against medical advice" 8 "IV provider" 20 "expired" 21 "law enforcement" ///
			50 "hospice - home" 51 "hospice facility" 62 "inpatient rehab" 63 "Medicare long-term" 64 "Medicaid nursing" ///
			65 "psychiatric" 66 "critical access" 70 "other"
		label values dischstat dischlbl	

		label define payorig 1 "A" 2 "B" 3 "C" 4 "D" 5 "E" 6 "F" 7 "G" 8 "H" 9 "I" ///
			10 "J" 11 "K" 12 "L" 13 "M" 14 "N" 15 "O" 16 "P" 17 "Q" 
		label define paylbl 1 "Medicare" 2 "Medicare Managed" 3 "Medicaid" 4 "Medicaid Managed" ///
			5 "Commercial" 6 "Commercial HMO" 7 "Commercial PPO" 8 "Workers' Comp" 9 "Other Fed Govt" ///
			10 "VA" 11 "Other State/Local Govt" 12 "Self" 13 "Other" 14 "Non-payment" 15 "Kidcare" ///
			16 "P" 17 "Commercial Liability"
		encode payer, gen(tmp) label(payorig)
		drop payer
		rename tmp payer
		label values payer paylbl
		
		gen payer_cat = .
		replace payer_cat = 1 if payer==1 | payer==2
		replace payer_cat = 2 if payer==3 | payer==4
		replace payer_cat = 3 if payer==5 | payer==6 | payer==7
		replace payer_cat = 4 if payer==12
		replace payer_cat = 5 if payer>12 & payer<.
		replace payer_cat = 5 if payer>=8 & payer<=11
		label define payer_cat 1 "Medicare" 2 "Medicaid" 3 "Commercial" 4 "Self" 5 "Other"
		label values payer_cat payer_cat


		replace edhr_arr = "" if edhr_arr=="99"
		gen tmp = real(edhr_arr)
		drop edhr_arr
		rename tmp edhr_arr

		replace adm_time = "" if adm_time=="99"
		gen tmp = real(adm_time)
		drop adm_time
		rename tmp adm_time

		replace dis_time = "" if dis_time=="99"
		gen tmp = real(dis_time)
		drop dis_time
		rename tmp dis_time

		label define srcorig 1 "01" 2 "02" 3 "03" 4 "04" 5 "05" 6 "06" 7 "07" 8 "08" 9 "09" 10 "10" ///
			11 "11" 12 "12" 13 "13" 14 "14" 15 "D" 16 "E" 17 "F"
		label define srclbl 1 "non-health care" 2 "clinic or physician's office" 3 "HMO" 4 "transfer hospital" ///
			5 "transfer nursing" 6 "transfer other" 7 "ED" 8 "court/law" 9 "unknown" ///
			10 "normal delivery" 11 "premature delivery" 12 "sick baby" 13 "outside delivery" ///
			14 "other" 15 "within hospital" 16 "ambulatory surgery" 17 "hospice"
		encode admsrc, gen(tmp) label(srcorig)
		drop admsrc
		rename tmp admsrc
		label values admsrc srclbl
		
		gen tmp = real(adm_prior)
		drop adm_prior
		rename tmp adm_prior
		label define admlbl 1 "emergency" 2 "urgent" 3 "elective" 4 "newborn" 5 "trauma"
		label values adm_prior admlbl

		gen emergency = (admsrc==7 | adm_prior==1)
		label values emergency yesno

		gen tmp = real(type_serv)
		drop type_serv
		rename tmp type_serv
		labe define servlbl 1 "inpatient/long-term/psych" 2 "rehab"
		label values type_serv servlbl
		
		label define poaorig 1 "N" 2 "Y" 3 "U" 4 "W" 5 "X" 6 "E" 7 "1"
		label define poalbl 1 "No" 2 "Yes" 3 "Unknown" 4 "Clin. Undetermined" 5 "Exempt"
		foreach v in "_prin_d" "_ext_in" "_ext__a" "_ext__b" {
			encode poa`v', gen(tmp) label(poaorig)
			drop poa`v'
			rename tmp poa`v'
			replace poa`v' = 5 if poa`v' == 6
			replace poa`v' = . if poa`v' == 7
			label values poa`v' poalbl
		}

		forvalues N = 30(-1)1 {
			local M = `N'+2
			encode poa`N', gen(tmp) label(poaorig)
			drop poa`N'
			rename tmp poa`M'
			replace poa`M' = 5 if poa`M' == 6
			replace poa`M' = . if poa`M' == 7
			label values poa`M' poalbl
		}
		rename poa_prin_d poa1
		gen poa2 = 2 /* admitting dx always present on admission */

		// code age in decades:
		gen age_dec = .
		forvalues i=2/8 {
			replace age_dec = `i' if 10*`i'<=age & age<10*(`i'+1)
		}
		replace age_dec = 2 if age<20
		replace age_dec = 8 if 80<=age
		label define agelbl 0 "0-9" 1 "10-17" 2 "18-29" 3 "30-39" 4 "40-49" 5 "50-59" 6 "60-69" 7 "70-79" 8 "80+" 
		label values age_dec agelbl
		
		// code as in- vs. out-of-state:
		gen instate = .
		replace instate = 1 if ptstate == "FL"
		replace instate = 0 if ptstate != "FL"
		label define statelbl 0 "out-of-state" 1 "in-state"
		label values instate statelbl

		// name days to procedure uniformly
		rename daysproc days1
		global counter = 2
		forval i=1/9 {
			rename days_proc`i' days$counter
			global counter = $counter + 1
		}
		foreach let in a b c d e f g h i j k l m n o p q r s t u {
			rename days_pro_`let' days$counter
			global counter = $counter +1
		}
		
		quietly: compress
		
		// compute total comorbidity index:
		gen comorb = .
		replace comorb = chf+ valve+ pulmcirc+ perivasc+ para+ neuro+ chrnlung+ dm+ dmcx+ hypothy+ renlfail+ liver+ ulcer+ aids+ lymph+ mets+ tumor+ arth+ coag+ obese+ wghtloss+ lytes+ bldloss+ anemdef+ alcohol+ drug+ psych+ depress+ htn_c
		label var comorb "# of comorbidities"
		gen comorb_cat = .
		replace comorb_cat = 0 if comorb==0
		replace comorb_cat = 1 if comorb==1
		replace comorb_cat = 2 if comorb==2
		replace comorb_cat = 3 if comorb>2 & comorb<.
		label define comorblbl 0 "0" 1 "1" 2 "2" 3 "3+"
		label values comorb_cat comorblbl

		// code negative outcomes:
		gen died = .
		replace died = 1 if dischstat==20
		replace died = 0 if dischstat<20
		replace died = 0 if dischstat>20 & dischstat<.
		label define deathlbl 0 "survived" 1 "died"
		label values died deathlbl
		label var died "in-hospital mortality"

		
		// prepare for merging with other states
		gen state = 2
		gen tmp = real(fac_county)
		drop fac_county
		rename tmp fac_county
		rename oper_phyid licno
		gen readm = .
		gen double obsid = sys_recid 
		drop sys_recid

		// include income data
		gen tmp = real(ptcounty)
		drop ptcounty
		rename tmp ptcounty
		merge m:1 ptcounty using "optimal_tradeoffs_code\data\Florida Data\STATA\CountyCodes.dta", keepusing(ptcounty fips_code med_income) keep(match master) nogen
		quietly centile med_income, c(0 25 50 75 100)
		gen inc_stateqtr = .
		replace inc_stateqtr = 1 if r(c_1)<=med_income & med_income<=r(c_2)
		replace inc_stateqtr = 2 if r(c_2)<med_income & med_income<=r(c_3)
		replace inc_stateqtr = 3 if r(c_3)<med_income & med_income<=r(c_4)
		replace inc_stateqtr = 4 if r(c_4)<med_income & med_income<=r(c_5)

		
		// include charge-to-cost ratios
		rename faclnbr hospid
		merge m:1 hospid year using "optimal_tradeoffs_code\data\Florida Data\STATA\CostChargeRatioFL.dta", keep(match master) nogen
		gen cost = .
		replace cost = tchgs * apicc * wi_x if apicc<.
		replace cost = tchgs * gapicc * wi_x if apicc==.
		
		// code hospital properties:
		gen bedsize = .
		replace bedsize = 1 if htype==1 | htype==3 | htype==5
		replace bedsize = 2 if htype==2 | htype==4 | htype==6 
		replace bedsize = 3 if htype==7
		label define bedlbl 1 "small" 2 "medium" 3 "large"
		label values bedsize bedlbl
		gen owner = .
		replace owner = 1 if htype==1 | htype==2
		replace owner = 2 if htype>=3 & htype<=7
		label define ownlbl 1 "investor" 2 "not-for-profit"
		label values owner ownlbl
		gen setting = .
		replace setting = 3 if htype==1 | htype==2
		replace setting = 1 if htype==3 | htype==4
		replace setting = 2 if htype>=5 & htype<=7
		label define rururb 1 "rural" 2 "urban" 3 "unlisted"
		label values setting rururb
		
		// label hospital types
		la def hosplbl 1 "investor-owned <100 beds" 2 "investor-owned 100+ beds" 3 "not-for-profit rural <100 beds" ///
			4 "not-for-profit rural 100+ beds" 5 "not-for-profit urban <100 beds" 6 "not-for-profit urban 100-299 beds" ///
			7 "not-for-profit urban 300+ beds"
		la val htype hosplbl	

		
		quietly compress 
		save "${outdir}inp`yr'q`qtr'gs.dta", replace
		}
	}
}

/********** combine Florida data **********/
foreach yr in "08" "09" "10" "11" "12" {
	use "${outdir}inp`yr'q1gs.dta", replace
	
	quietly append using "${outdir}inp`yr'q2gs.dta" "${outdir}inp`yr'q3gs.dta" "${outdir}inp`yr'q4gs.dta" 
	
	save "${outdir}fl20`yr'gs.dta", replace

}
use "${outdir}fl2008gs.dta", clear
quietly append using "${outdir}fl2009gs.dta" "${outdir}fl2010gs.dta" ///
	"${outdir}fl2011gs.dta" "${outdir}fl2012gs.dta"
save "${outdir}FloridaAllYearsGS.dta", replace

set more on
