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

%let perlib = F:\EXTRA SPACE\eHARS\eHARS files for HIS dataset;
%let state = WA;

/******************************************************************************************
* NOTES:                                                                                  *
*   Do not forget to end each of the above %let statements with a semicolon               *
*   Do not modify code beyond this point                                                  *
******************************************************************************************/
options nofmterr;

libname perlib "&perlib";



data ehars;	
	set perlib.person;
	keep 
		stateno				/* unique id */
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

		if rsh_state_cd="&state"; *exclude out of jurisdiction cases;

		run;

proc contents data=ehars;
	run;

proc print data=ehars (obs=10);
	run;

proc freq data=ehars;
	table rsh_state_cd;
	run;
