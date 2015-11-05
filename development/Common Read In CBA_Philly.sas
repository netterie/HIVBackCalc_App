/******************************************************************************************
* Filename: Common Read In CBA.sas                                                        *
* Written: 01/22/2015                                                                     *
******************************************************************************************/


/******************************************************************************************
* SET VALUES FOR MACRO VARIABLES (no quotation marks, not case sensitive):                *
*   Location of eHARS Person View data set                                                *
*     ex. %let perlib = H:\DATA_OUT\GA00\Person;                                          *
*   Project area                                                                          *
*     ex. %let area = WA;                                                                 *
******************************************************************************************/

*%let perlib = F:\EXTRA SPACE\eHARS\eHARS files for HIS dataset;
%let city = PHILADELPHIA;

/******************************************************************************************
* NOTES:                                                                                  *
*   Do not forget to end each of the above %let statements with a semicolon               *
*   Do not modify code beyond this point                                                  *
******************************************************************************************/
options nofmterr;

libname perlib "G:\Surveillance Data Group\ehars\data\person\person_sas7bdat";
libname bc "G:\Surveillance Data Group\Data Manager\Back Calculation" ;

proc contents data=perlib.person ; run ;

data ehars;	
	set perlib.person;
	where dx_status in ('1' '2' '4' '5') and status_flag in ('A' 'W' 'R') ;
	keep 
		/*stateno*/				/* unique id */
		hiv_aids_age_yrs 	/* age at diagnosis in years */
		birth_sex			/* sex at birth */
		race 				/* race, includes Hisp/non-Hisp, categorical */
		trans_categ			/* mode of HIV transmission, categorical */
		hiv_dx_dt			/* date of first pos HIV test (lab confirmed) */
		aids_dx_dt			/* date of first AIDS classifying condition */
		screen_last_neg_dt 	/* date of last neg HIV test (lab confirmed) */

		cd4_first_hiv_dt	/* date of earliest CD4 */
		cd4_first_hiv_type	/* type of CD4 test - count or percent */
		cd4_first_hiv_value	/* result of CD4 test */

		vl_first_det_dt		/* date of earliest detectable viral load */
		vl_first_det_value	/* value of detectable viral load */

		rsh_state_cd 		/* state of residence at HIV diagnosis */

		tth_ever_neg		/* TTH: Y/N ever had a neg HIV test */
		tth_last_neg_dt		/* TTH: date of last neg HIV test */
		tth_first_pos_dt	/* TTH: date of first pos HIV test */
		;

		if rsh_city_name="&city"; *exclude out of jurisdiction cases;


		
		run;


proc format;
value $racecalc '1'="Other"
				'2'="Other"
				'3'="Other"
				'4'="Black"
				'5'="Other"
				'6'="White"
				'7'="Other"
				'8'="Other"
				'9'="Other";

value $moden '01'="MSM"
			'02'="IDU"
			'03'="MSM/IDU"
			'04'="Other"
			'05'="Heterosexual"
			'06'="Other"
			'07'="Other"
			'09'="No Identified Risk"
			'10'="No Risk Reported"
			'11'="Other"
			'12'='Perinatal'
			'18'='Other'
			'19'='No Identified Risk'
			'20'='No Risk Reported'
			'99'='Other'
			;
value $neg	'D' = 'NA'
			'Y' = 'TRUE'
			'N' = 'FALSE'
			'U' = 'NA'
			' ' = 'NA'
			;

run;

proc format ;
value $age ' '='Unknown'
			'0'-'12', '2', '3', '4', '5','6', '7', '8', '9'='0-12'
		'13'-'17'='13-17'
		'18'-'19'='18-19'
		'20'-'24'='20-24'
		'25'-'29'='25-29'
		'30'-'34'='30-34'
		'35'-'39'='35-39'
		'40'-'44'='40-44'
		'45'-'49'='45-49'
		'50'-'54'='50-54'
		'55'-'59'='55-59'
		'60'-'69'='60+'
		'70'-'79'='60+'
		'80'-'89'='60+'
		'90'-'99'='60+';
		run;


data ehars1 ;
	set ehars ;
		hdx_age = put(hiv_aids_age_yrs, 3.) ;
		yearDx = put(substr(hiv_dx_dt, 1, 4), 4.) ;
		everHadNegTest=tth_ever_neg ;
		mode = trans_categ ;
		agecat5 = hiv_aids_age_yrs ;
	id + 1 ;
	if first.obs = 1 then id = 1 ;

	if substr(hiv_dx_dt,5,2) in ('01' '02' '03') then do ;
			timeDx=catx('.',substr(hiv_dx_dt,1,4), '0') ;
	end ;
	else if substr(hiv_dx_dt,5,2) in ('04' '05' '06') then do ;
			timeDx=catx('.',substr(hiv_dx_dt,1,4), '25') ;
	end ;
	else if substr(hiv_dx_dt,5,2) in ('07' '08' '09') then do ;
			timeDx=catx('.',substr(hiv_dx_dt,1,4),'50') ;
	end ;
	else if substr(hiv_dx_dt,5,2) in ('10' '11' '12') then do ;
			timeDx=catx('.',substr(hiv_dx_dt,1,4),'75') ;
	end ;
	else timeDx=catx('.',substr(hiv_dx_dt,1,4),'0') ;


	last_neg  = mdy(substr(tth_last_neg_dt,5,2), substr(tth_last_neg_dt,7,2), substr(tth_last_neg_dt,1,4)) ;
	dx_day =substr(hiv_dx_dt,7,2) ;
	if substr(hiv_dx_dt,7,2) = '..' then dx_day ='15' ;
	dx_mo = substr(hiv_dx_dt,5,2) ;
	if substr(hiv_dx_dt,5,2) = '..' then dx_mo = '06' ; 
	dx_year = mdy(dx_mo, dx_day, substr(hiv_dx_dt,1,4)) ;

	infPeriod = YRDIF(last_neg, dx_year, 'ACT/ACT') ;
run ;

proc freq data=ehars1 ;
	table trans_categ ;
run;



data bc.ehars2 ;
	set ehars1 ;
	Population = 'PA' ;
	*if infperiod = . then infperiod = 'NA' ;
	if everhadnegtest = ' ' then everhadnegtest = 'NA' ;
	keep id mode hdx_age yearDx everhadnegtest race agecat5 timeDx infPeriod population ;
	format race $racecalc. everhadnegtest $neg.  mode $moden. agecat5 $age.  ;
run ;


proc export
  data= bc.ehars2
  dbms=csv
  outfile='G:\Surveillance Data Group\Data Manager\Back Calculation\import_file08.10.2015.csv' 
  replace;
run;

	
proc freq data=bc.ehars2 ;
	table infperiod everhadnegtest mode ;
run ;
