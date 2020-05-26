// CleanGSNY.do
//
// Clean general surgery data from New York.
//
// 12/16/14: KDS created file.



/****** change globals here ******/
global indir "optimal_tradeoffs_code\data\New York Data\STATA\"
global outdir "optimal_tradeoffs_code\data\"
set more off
/****** end changes ******/


// loop over years:
foreach yr in "2007" "2008" "2009" "2010" "2011" {
		
	noisily display "processing `yr' data..."
		
	quietly {
		
		use "${indir}ny`yr'gs.dta", clear
		quietly: compress // reduce variable to smaller data types when possible
		save "${outdir}ny`yr'gs.dta", replace

		// give labels to variable values:
		label define yesno 0 "No" 1 "Yes"
		label values chf valve pulmcirc perivasc para neuro chrnlung dm dmcx hypothy renlfail liver ulcer aids lymph mets tumor arth coag obese wghtloss lytes bldloss anemdef alcohol drug psych depress htn_c yesno

		// make variable names consistent across states:
		rename totchgs tchgs
		
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

		label define ethnorig 1 "1" 2 "2" 3 "9"
		label define ethlbl 1 "Hispanic" 2 "Non-Hispanic" 3 "Unknown"
		encode ethnicity, gen(tmp) label(ethnorig)
		drop ethnicity
		rename tmp ethnicity
		label values ethnicity ethlbl

		label define raceorig 1 "03" 2 "04" 3 "02" 4 "05" 5 "01" 6 "88" 7 "99"
		encode race, gen(tmp) label(raceorig)
		drop race
		rename tmp race
		label define racelbl 1 "American Indian" 2 "Asian" 3 "Black" 4 "Pacific Islander" 5 "White" 6 "Other" 7 "Unknown"
		label values race racelbl

		gen race_cat = .
		replace race_cat = 1 if race==5
		replace race_cat = 2 if race==3
		replace race_cat = 3 if race==1 | race==2 | race==4 | race==6
		label define race_cat 1 "white" 2 "black" 3 "other"
		label values race_cat race_cat

		gen tmp = real(dischstat)
		drop dischstat
		rename tmp dischstat
		label define dischlbl 1 "home" 2 "short-term gen hosp" 3 "Medicare nursing" 4 "assisted living" 5 "cancer center" ///
			6 "home health care" 7 "left against medical advice" 8 "home IV" 9 "admitted" 20 "expired" 21 "law enforcement" ///
			40 "expired at home" 41 "expired in med fac" 42 "expired - place unknown" 43 "federal facility" ///
			50 "hospice - home" 51 "hospice facility" 61 "swing bed" 62 "inpatient rehab" 63 "Medicare long-term" 64 "Medicaid nursing" ///
			65 "psychiatric" 66 "critical access" 69 "disaster" 70 "other" ///
			81 "home - plan readmit" 82 "short-term gen hosp - plan readmit" 83 "Medicare nursing - plan readmit" ///
			84 "assisted living - plan readmit" 85 "cancer center - plan readmit" 86 "home health care - plan readmit" ///
			87 "law/court - plan readmit" 88 "federal facility - plan readmit" 89 "swing bed - plan readmit" ///
			90 "inpatient rehab - planned readmit" 91 "Medicare long-term - plan readmit" 92 "Medicaid nursing - plan readmit" ///
			93 "psychiatric - plan readmit" 94 "critical access - plan readmit" 95 "other - plan readmit"
		label values dischstat dischlbl	

		label define payorig 1 "A" 2 "B" 3 "C" 4 "D" 5 "E" 6 "F" 7 "G" 8 "H" 9 "I" 10 "J" 11 "K" 12 "L"
		label define paylbl 1 "self" 2 "workers' comp" 3 "Medicare" 4 "Medicaid" 5 "other federal" 6 "commercial" ///
			7 "Blue Cross" 8 "CHAMPUS" 9 "other non-federal" 10 "disability" 11 "Title V" 12 "unknown"
		encode paysrc1, gen(tmp) label(payorig)
		rename tmp payer
		label values payer paylbl

		gen payer_cat = .
		replace payer_cat = 1 if payer==3
		replace payer_cat = 2 if payer==4
		replace payer_cat = 3 if payer==6 | payer==7
		replace payer_cat = 4 if payer==1
		replace payer_cat = 5 if payer==2 | payer==5 | (payer>=8 & payer<.)
		label define payer_cat 1 "Medicare" 2 "Medicaid" 3 "Commercial" 4 "Self" 5 "Other"
		label values payer_cat payer_cat


		label define srcorig 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" ///
			10 "A" 11 "D" 12 "E" 13 "F"
		label define srclbl 1 "non-health care" 2 "clinic" 3 "other" 4 "transfer between hospitals" ///
			5 "transfer nursing" 6 "transfer other" 7 "ED" 8 "court/law" 9 "unknown" ///
			10 "transfer rural" 11 "transfer within hospital" 12 "transfer ambulatory" 13 "transfer hospice"
		encode admsrc, gen(tmp) label(srcorig)
		drop admsrc
		rename tmp admsrc
		label values admsrc srclbl

		gen tmp = real(admtype)
		drop admtype
		rename tmp adm_prior
		replace adm_prior = . if adm_prior == 9
		label define admlbl 1 "emergency" 2 "urgent" 3 "elective" 4 "newborn" 5 "trauma"
		label values adm_prior admlbl

		gen emergency = (admsrc==7 | adm_prior==1)
		label values emergency yesno
		
		gen tmp = real(servcat)
		drop servcat
		rename tmp servcat
		labe define servlbl 1 "medical" 2 "surgical" 3 "pediatric" 4 "obstetrical" 5 "nursery/newborn" 6 "psychiatric"
		label values servcat servlbl
		
		label define poaorig 1 "2" 2 "1" 3 "3" 4 "9" 5 "X"
		label define poalbl 1 "No" 2 "Yes" 3 "Unknown" 4 "Clin. Undetermined" 5 "Exempt"
		
		forvalues N = 1/24 {
			encode poa`N', gen(tmp) label(poaorig)
			drop poa`N'
			rename tmp poa`N'
			label values poa`N' poalbl
		}

		// code age in decades:
		gen age_dec = .
		
		forvalues i=2/8 {
			replace age_dec = `i' if 10*`i'<=age & age<10*(`i'+1)
		}
		replace age_dec = 2 if age<20
		replace age_dec = 8 if 80<=age
		label define agelbl 0 "0-9" 1 "10-19" 2 "20-29" 3 "30-39" 4 "40-49" 5 "50-59" 6 "60-69" 7 "70-79" 8 "80+"
		label values age_dec agelbl
		
		// code as in- vs. out-of-state:
		gen instate = .
		replace instate = 1 if ptstate == "NY"
		replace instate = 0 if ptstate != "NY"
		label define statelbl 0 "out-of-state" 1 "in-state"
		label values instate statelbl
		
		// name days to procedure uniformly
		rename daysproc days1
		replace days1 = -days1 if preadm=="-"
		forval i=1/14 {
			local j = `i'+1
			gen days`j' = real(daysoth`i')
			replace days`j' = -days`j' if preoth`i'=="-"
			drop daysoth`i'
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

		// compute negative outcomes:
		gen died = .
		replace died = 1 if dischstat==20
		replace died = 0 if dischstat<20
		replace died = 0 if dischstat>20 & dischstat<.
		label define deathlbl 0 "survived" 1 "died"
		label values died deathlbl
		label var died "in-hospital mortality"

		/*
		gen comp_aspir = 0
		
		forval i=1/30 {
			replace comp_aspir = 1 if inlist(dx`i', "507", "9973") & poa`i'~="N"
			replace comp_card = 1 if dx`i' == "9971"
			replace comp_pneumo = 1 if inrange(dx`i', "480", "487")
			replace comp_inf = 1 if dx`i'=="9985"
		}
		*/
		
		// prepare for merging with other states
		gen state = 3
		gen tmp = real(facid)
		rename tmp hospid
		gen tmp = real(tchgs)
		drop tchgs
		rename tmp tchgs
		replace tchgs = tchgs/100 // data entered with implied decimal point
		rename faccounty fac_county
		gen licno = substr(operid, 3, 6)
		gen double obsid = real(string(year) + substr(dischnum,-7,.)) 
		drop operid dischnum
		
		// include income data
		merge m:1 ptcounty using "${indir}CountyCodes.dta", keepusing(ptcounty fips_code med_income) keep(match master) nogen
		quietly centile med_income, c(0 25 50 75 100)
		gen inc_stateqtr = .
		replace inc_stateqtr = 1 if r(c_1)<=med_income & med_income<=r(c_2)
		replace inc_stateqtr = 2 if r(c_2)<med_income & med_income<=r(c_3)
		replace inc_stateqtr = 3 if r(c_3)<med_income & med_income<=r(c_4)
		replace inc_stateqtr = 4 if r(c_4)<med_income & med_income<=r(c_5)

		
		// include charge-to-cost ratios
		merge m:1 hospid year using "optimal_tradeoffs_code\data\New York Data\STATA\CostChargeRatioNY.dta", keep(match master) nogen
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


		// population of hospital county:
		merge m:1 state fac_county using "optimal_tradeoffs_code\data\New York Data\STATA\CountyPopulation.dta", keepusing(population) keep(match master) nogen
				
		quietly compress

		save "${outdir}ny`yr'gs.dta", replace
	}

}

/********** combine New York data **********/
use "${outdir}ny2007gs.dta", clear
quietly append using "${outdir}ny2008gs.dta" "${outdir}ny2009gs.dta" "${outdir}ny2010gs.dta" "${outdir}ny2011gs.dta" 
save "${outdir}NewYorkAllYearsGS.dta", replace

set more on
