/****************************************************************/
/*                                                             	*/
/* PROGRAM:     PullGenSurgFL.sas                     			*/
/*                                                              */
/* Description: Pull all general surgery procedures.			*/
/*				Use codes in misctables_121514rrk.xlsx.			*/
/*																*/
/* 16 DEC 14	KDS created file.								*/ 
/* 09 MAR 15	KDS included additional pancreas (52.7), spleen	*/
/*				(41.42), and liver (50.22, 50.23, 50.25) 		*/
/*				procedure codes.								*/ 
/* 10 NOV 15	KDS corrected ventral hernia codes.				*/
/* 21 NOV 15	KDS corrected bariatric codes.					*/
/*																*/
/****************************************************************/

LIBNAME FL 'optimal_tradeoffs_code\data\Florida Data\SAS\';
LIBNAME FLRAW 'optimal_tradeoffs_code\data\Florida Data\SAS\RawData\';

LIBNAME LIBRARY 'optimal_tradeoffs_code\data\Florida Data\SAS\';


* Use for debugging;
options nosymbolgen errors=0;

%MACRO ProcFLFile(fname=,yr=);
%put Input file: &fname;

* Classify by procedure;
%LET INNAME = FL.&fname.IN;
%LET OUTNAME = &fname.CODED;

data &OUTNAME;
	set &INNAME;
	where age>=18;

	* use smallest possible data size;
	length default = 3;

	* classify procedure codes;
	hrn_femoral = 0;
	hrn_inguinal = 0;
	hrn_umbil = 0;
	hrn_ventral = 0;
	hrn_othabd = 0;
	chole = 0;
	gall_anast = 0;
	hepatectomy = 0;
	pancreas = 0;
	spleen = 0;
	esophagus = 0; /* esophagectomy, esophagomyotomy */
	eso_sphincter = 0; /*esophagogastroplasty */
	gastric = 0; /* gastrectomy */
	gast_other = 0; /* vagotomy, pyloroplasty, gastroenterostomy, anastomosis revison, gastric restrictive */ 
	bari = 0; /* bariatric - confirm obesity */
	gast_ulcer = 0;
	smbowel = 0;
	colon = 0;
	appy = 0;
	ostomy = 0;
	anastomosis = 0;
	fistula = 0;
	adhesions = 0;
	rectal = 0;
	proctopexy = 0;
	breast = 0;
	adrenal = 0;
	parathyroid = 0;
	thyroid = 0;
	
	array proc_cd_{31} $ PRINPROC OTHPROC1-OTHPROC30;

	do I=1 to 31;
		if proc_cd_(i) ge '532' and proc_cd_(i) lt '5339' then hrn_femoral = 1;
		if (proc_cd_(i) ge '171' and proc_cd_(i) le '1724') or (proc_cd_(i) ge '5300' and proc_cd_(i) le '5317') 
			then hrn_inguinal = 1;
		if proc_cd_(i) ge '534' and proc_cd_(i) le '5349' then hrn_umbil = 1;
		if proc_cd_(i) ge '535' and proc_cd_(i) le '5369' then hrn_ventral = 1;
		if proc_cd_(i) ge '537' and proc_cd_(i) le '538e' then hrn_othabd = 1;
		if proc_cd_(i) ge '512' and proc_cd_(i) le '5124' then chole = 1;
		if proc_cd_(i) ge '513' and proc_cd_(i) le '5139' then gall_anast = 1;
		if proc_cd_(i) ge '502' and proc_cd_(i) le '5059' then hepatectomy = 1;
		if (proc_cd_(i) ge '525' and proc_cd_(i) le '527') or proc_cd_(i) in ('5282', '5284')
			then pancreas = 1;
		if proc_cd_(i) in ('4142', '4143', '415') then spleen = 1;
		if (proc_cd_(i) ge '424' and proc_cd_(i) le '4242') or proc_cd_(i) eq '427'
			then esophagus = 1;
		if proc_cd_(i) ge '4465' and proc_cd_(i) le '4467' then eso_sphincter = 1;
		if proc_cd_(i) ge '435' and proc_cd_(i) le '4399' then gastric = 1;
		if (proc_cd_(i) ge '440' and proc_cd_(i) le '4403') or (proc_cd_(i) ge '442' and proc_cd_(i) le '4439') 
			or (proc_cd_(i) in ('445', '4468', '4495')) then gast_other = 1;
		if proc_cd_(i) in ('437', '4382', '4389', '4391', '4399', '4431', '4438', '4439', '4468', '4495', '4496', '4498')
			then bari = 1;
		if proc_cd_(i) ge '4440' and proc_cd_(i) le '4442' then gast_ulcer = 1;
		if proc_cd_(i) ge '456' and proc_cd_(i) le '4563' or proc_cd_(i) eq '5783' then smbowel = 1;
		if (proc_cd_(i) ge '173' and proc_cd_(i) le '1739') or (proc_cd_(i) ge '4571' and proc_cd_(i) le '4583') 
			then colon = 1;
		if proc_cd_(i) in ('470', '4701', '4709') then appy = 1;
		if (proc_cd_(i) in ('4601', '4603')) or (proc_cd_(i) ge '461' and proc_cd_(i) le '4639') 
			or (proc_cd_(i) ge '465' and proc_cd_(i) le '4652') then ostomy = 1;
		if proc_cd_(i) ge '459' and proc_cd_(i) le '4595' then anastomosis = 1;
		if proc_cd_(i) in ('4672', '4674', '4676') then fistula = 1;
		if proc_cd_(i) in ('5451', '5459') then adhesions = 1;
		if proc_cd_(i) ge '484' and proc_cd_(i) le '4869' then rectal = 1;
		if proc_cd_(i) in ('4875', '4876') then proctopexy = 1;
			if proc_cd_(i) ge '854' and proc_cd_(i) le '8548' then breast = 1;
		if (proc_cd_(i) ge '070' and proc_cd_(i) le '0702') or (proc_cd_(i) ge '072' and proc_cd_(i) le '073') 
			then adrenal = 1;
		if proc_cd_(i) in ('068', '0681', '0689') then parathyroid = 1;
		if proc_cd_(i) ge '062' and proc_cd_(i) le '0652' then thyroid = 1;
	end;

	drop I;

	if hrn_femoral or hrn_inguinal or hrn_umbil or hrn_ventral or hrn_othabd or chole or gall_anast
		or hepatectomy or pancreas or spleen or esophagus or eso_sphincter or gastric or gast_other or bari 
		or gast_ulcer or smbowel or colon or appy or ostomy or anastomosis or fistula or adhesions
		or rectal or proctopexy or breast or adrenal or parathyroid or thyroid;
run;


%put ...procedures coded;


* Get comorbidity measures from ICD-9 codes;
%LET INNAME = &fname.CODED;
%LET OUTNAME = &fname.COMORB;
%INCLUDE 'optimal_tradeoffs_code\data_assembly\comoanaly_hcup_fl.sas';


* Include comorbidity measures in data set;
%LET INNAME = &fname.CODED;
%LET OUTNAME = FL.&fname.GS;
data &OUTNAME;
    MERGE &INNAME
        &fname.comorb;
    by SYS_RECID;
run;
%put ...comorbidities coded;


* Save for STATA;
%LET OUTNAME = &fname.gs;
PROC EXPORT DATA = FL.&OUTNAME
    OUTFILE= "optimal_tradeoffs_code\data\Florida Data\STATA\&OUTNAME..dta"
    DBMS=STATA REPLACE;
RUN;
%put ...saved to STATA;

%MEND ProcFLFile;


%MACRO ProcAll;
%do n=1 %to 4; 
	%ProcFLFile(fname=inp08q&n,yr=2008);
	%ProcFLFile(fname=inp09q&n,yr=2009);
	%ProcFLFile(fname=inp10q&n,yr=2010);
	%ProcFLFile(fname=inp11q&n,yr=2011);
	%ProcFLFile(fname=inp12q&n,yr=2012);
%end;
%MEND ProcAll;

%ProcAll;
