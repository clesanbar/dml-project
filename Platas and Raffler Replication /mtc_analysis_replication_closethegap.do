* Replication code for "Closing the Gap: Information and Mass Support in a Dominant Party Regime"
* Authors: Melina R. Platas & Pia J. Raffler 
* The Journal of Politics
* Date last edited: May 17, 2020



************************************************************
************************** Set up **************************
************************************************************


cap restore
clear all
set more off

	*Install non-standard packages in case not already installed
	cap net install gr0002_3, from(http://www.stata-journal.com/software/sj4-3)
	cap ssc install coefplot
	cap ssc install estout
	cap ssc install unique
	cap ssc install latab
	cap ssc install reghdfe
	cap ssc install ftools
	cap reghdfe, compile
	


	loc today : di %tdCCYY.NN.DD date(c(current_date), "DMY")
	cap log close
	log using "mtc_analysis_repjop_`today'.smcl", replace

	use "mtc_analysis_closethegap.dta", clear
	

	*Define vectors of covariates
	
	*All
	global controls "age_gepub_st female_gepub_st educyr_gepub_st wealthindex_gepub_st nrmpref_gepub_st lastturnout_gepub_st  powerful_gepub_st counting_gepub_st salience_gepub_st source_gepub_st news_gepub_st"
	global controls_treat "treat_age_gepub_st treat_female_gepub_st treat_educyr_gepub_st treat_wealthindex_gepub_st treat_nrmpref_gepub_st treat_lastturnout_gepub_st treat_powerful_gepub_st treat_counting_gepub_st treat_salience_gepub_st treat_source_gepub_st treat_news_gepub_st"

	
	*All, conditional on turnout
	global controlsgetout "age_gepubtout_st female_gepubtout_st educyr_gepubtout_st wealthindex_gepubtout_st nrmpref_gepubtout_st lastturnout_gepubtout_st  powerful_gepubtout_st counting_gepubtout_st salience_gepubtout_st source_gepubtout_st news_gepubtout_st"		
	global controlsgetout_treat "treat_age_gepubtout_st treat_female_gepubtout_st treat_educyr_gepubtout_st treat_wealthindex_gepubtout_st treat_nrmpref_gepubtout_st treat_lastturnout_gepubtout_st  treat_powerful_gepubtout_st treat_counting_gepubtout_st treat_salience_gepubtout_st treat_source_gepubtout_st treat_news_gepubtout_st"


	*Intend NRM
	global controlsintnrm "age_ge_intnrm_st female_ge_intnrm_st educyr_ge_intnrm_st wealthindex_ge_intnrm_st nrmpref_ge_intnrm_st lastturnout_ge_intnrm_st  powerful_ge_intnrm_st counting_ge_intnrm_st salience_ge_intnrm_st source_ge_intnrm_st news_ge_intnrm_st"
	global controlsintnrm_treat "treat_age_ge_intnrm_st treat_female_ge_intnrm_st treat_educyr_ge_intnrm_st treat_wealthindex_ge_intnrm_st treat_nrmpref_ge_intnrm_st treat_lastturnout_ge_intnrm_st treat_powerful_ge_intnrm_st treat_counting_ge_intnrm_st treat_salience_ge_intnrm_st treat_source_ge_intnrm_st treat_news_ge_intnrm_st"
	

	*Intend NRM, conditional on turnout
	global controlsintnrmtout "age_intnrmtout_st female_intnrmtout_st educyr_intnrmtout_st wealthindex_intnrmtout_st nrmpref_intnrmtout_st lastturnout_intnrmtout_st  powerful_intnrmtout_st counting_intnrmtout_st salience_intnrmtout_st source_intnrmtout_st news_intnrmtout_st"		
	global controlsintnrmtout_treat "treat_age_intnrmtout_st treat_female_intnrmtout_st treat_educyr_intnrmtout_st treat_wealthindex_intnrmtout_st treat_nrmpref_intnrmtout_st treat_lastturnout_intnrmtout_st  treat_powerful_intnrmtout_st treat_counting_intnrmtout_st treat_salience_intnrmtout_st treat_source_intnrmtout_st treat_news_intnrmtout_st"
	
	
	*NOT intend NRM
	global controlsintnn "age_ge_intnn_st female_ge_intnn_st educyr_ge_intnn_st wealthindex_ge_intnn_st nrmpref_ge_intnn_st lastturnout_ge_intnn_st  powerful_ge_intnn_st counting_ge_intnn_st salience_ge_intnn_st source_ge_intnn_st news_ge_intnn_st"		
	global controlsintnn_treat "treat_age_ge_intnn_st treat_female_ge_intnn_st treat_educyr_ge_intnn_st treat_wealthindex_ge_intnn_st treat_nrmpref_ge_intnn_st treat_lastturnout_ge_intnn_st  treat_powerful_ge_intnn_st treat_counting_ge_intnn_st treat_salience_ge_intnn_st treat_source_ge_intnn_st treat_news_ge_intnn_st"


	*NOT intend NRM, conditional on turnout
	global controlsintnntout "age_intnntout_st female_intnntout_st educyr_intnntout_st wealthindex_intnntout_st nrmpref_intnntout_st lastturnout_intnntout_st  powerful_intnntout_st counting_intnntout_st salience_intnntout_st source_intnntout_st news_intnntout_st"		
	global controlsintnntout_treat "treat_age_intnntout_st treat_female_intnntout_st treat_educyr_intnntout_st treat_wealthindex_intnntout_st treat_nrmpref_intnntout_st treat_lastturnout_intnntout_st  treat_powerful_intnntout_st treat_counting_intnntout_st treat_salience_intnntout_st treat_source_intnntout_st treat_news_intnntout_st"


	* DYAD CONTROLS 
	
	*All, plus
	global controlsplusd "age_ge_plusd_st female_ge_plusd_st educyr_ge_plusd_st wealthindex_ge_plusd_st nrmpref_ge_plusd_st lastturnout_ge_plusd_st  powerful_ge_plusd_st counting_ge_plusd_st salience_ge_plusd_st source_ge_plusd_st news_ge_plusd_st candinfo_ge_plusd_st  pastsupport_ge_plusd_st cogender_ge_plusd_st coethnic_ge_plusd_st"		
	global controlsplusd_treat "treat_age_ge_plusd_st treat_female_ge_plusd_st treat_educyr_ge_plusd_st treat_wealthindex_ge_plusd_st treat_nrmpref_ge_plusd_st treat_lastturnout_ge_plusd_st  treat_powerful_ge_plusd_st treat_counting_ge_plusd_st treat_salience_ge_plusd_st treat_source_ge_plusd_st treat_news_ge_plusd_st treat_candinfo_ge_plusd_st treat_cogender_ge_plusd_st treat_coethnic_ge_plusd_st treat_pastsupport_ge_plusd_st"
	
	*Intend NRM, plus
	global controlsplusnrmd "age_ge_plusnrmd_st female_ge_plusnrmd_st educyr_ge_plusnrmd_st wealthindex_ge_plusnrmd_st nrmpref_ge_plusnrmd_st lastturnout_ge_plusnrmd_st  powerful_ge_plusnrmd_st counting_ge_plusnrmd_st salience_ge_plusnrmd_st source_ge_plusnrmd_st news_ge_plusnrmd_st candinfo_ge_plusnrmd_st  pastsupport_ge_plusnrmd_st cogender_ge_plusnrmd_st coethnic_ge_plusnrmd_st"		
	global controlsplusnrmd_treat "treat_age_ge_plusnrmd_st treat_female_ge_plusnrmd_st treat_educyr_ge_plusnrmd_st treat_wealthindex_ge_plusnrmd_st treat_nrmpref_ge_plusnrmd_st treat_lastturnout_ge_plusnrmd_st  treat_powerful_ge_plusnrmd_st treat_counting_ge_plusnrmd_st treat_salience_ge_plusnrmd_st treat_source_ge_plusnrmd_st treat_news_ge_plusnrmd_st treat_candinfo_ge_plusnrmd_st treat_cogender_ge_plusnrmd_st treat_coethnic_ge_plusnrmd_st treat_pastsupport_ge_plusnrmd_st"
	
	*Intend Opp, plus
	global controlsplusoppd "age_ge_intoppdp_st female_ge_intoppdp_st educyr_ge_intoppdp_st wealthindex_ge_intoppdp_st nrmpref_ge_intoppdp_st lastturnout_ge_intoppdp_st  powerful_ge_intoppdp_st counting_ge_intoppdp_st salience_ge_intoppdp_st source_ge_intoppdp_st news_ge_intoppdp_st candinfo_ge_intoppdp_st  pastsupport_ge_intoppdp_st cogender_ge_intoppdp_st coethnic_ge_intoppdp_st"		
	global controlsplusoppd_treat "treat_age_ge_intoppdp_st treat_female_ge_intoppdp_st treat_educyr_ge_intoppdp_st treat_wealthindex_ge_intoppdp_st treat_nrmpref_ge_intoppdp_st treat_lastturnout_ge_intoppdp_st  treat_powerful_ge_intoppdp_st treat_counting_ge_intoppdp_st treat_salience_ge_intoppdp_st treat_source_ge_intoppdp_st treat_news_ge_intoppdp_st treat_candinfo_ge_intoppdp_st  treat_pastsupport_ge_intoppdp_st treat_cogender_ge_intoppdp_st treat_coethnic_ge_intoppdp_st"		

	local covariates "age female educyr wealthindex nrmpref lastturnout  powerful counting salience source news"
	
	*Keep only the main sample
	
	*Drop attriters:
	drop if attrition == 1
	
	*Drop unfilmed candidates
	keep if filmed == 1
	
	*Recreate unique identifiers
	drop uniquecand uniqueconst uniqueps 
	egen uniquecand = tag(candidateid)
	la var uniquecand "Unique candidate"
	egen uniqueconst = tag(code)
	la var uniqueconst "Unique constituency"
	egen uniqueps = tag(codeps)
	la var uniqueps "Unique PS"
	
	
************************************************
***** Descriptive stats in theory section ******
************************************************

* % of sample believing votes are monitored

	tab monitor if uniqueid == 1



***********************************	
* CANDIDATE KNOWLEDGE AT BASELINE *
***********************************	

* Table 1: Respondents know less about opposition candidates at baseline

		reghdfe heardof  i.party  coethnic incumbent if viable == 1, cl(codeps) a(id codeps)
		eststo a
		reghdfe heardof  i.party  coethnic incumbent if viable == 1  & intendnrm==1, cl(codeps) a(id codeps)
		eststo b
		
		reghdfe candknow i.party  coethnic incumbent if viable == 1  & heardof==1, cl(codeps) a(id codeps)
		eststo c
		reghdfe candknow i.party  coethnic incumbent if viable == 1  & heardof==1 & intendnrm==1, cl(codeps) a(id codeps)
		eststo d

		reghdfe informed  i.party  coethnic incumbent if viable == 1  & heardof==1, cl(codeps) a(id codeps)
		eststo e
		reghdfe informed  i.party  coethnic incumbent if viable == 1  & heardof==1 & intendnrm==1, cl(codeps) a(id codeps)
		eststo f
		
		

		# delimit;
		esttab     a b  c d e f 
		using table1.tex,
				title(Knowledge about candidates by party)
				keep( 2.party 3.party  coethnic incumbent _cons)
				varlabels (
				2.party "Opposition"
				3.party "Independent"
				coethnic "Coethnic"
				incumbent "Incumbent"
				_cons "Constant")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 					
				mtitles( "\shortstack{Heard of\\(all)}"
						"\shortstack{Heard of\\(NRM lean)}"
						"\shortstack{Knowledge\\(all)}"
						"\shortstack{Knowledge\\(NRM lean)}"
						"\shortstack{Informed\\(all)}"
						"\shortstack{Informed\\(NRM lean)}")
				stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;
				
		
		*Descriptives in text
		
		*NRM candidates
		tab heardof if candprty_c == 1
		*Opposition candidates (neither NRM nor independent)
		tab heardof if candprty_c != 1 & candprty_c != 6
		
		*Without incumbents:
		*NRM candidates
		tab heardof if candprty_c == 1 & incumbent == 0
		*Opposition candidates (neither NRM nor independent)
		tab heardof if (candprty_c != 1 & candprty_c != 6 & incumbent == 0)
		

		
****************************************************************************	
* TREATMENT EFFECTS: UPDATING 
****************************************************************************	
		
**UPDATING ABOUT CANDIDATES BY PARTISANSHIP

		*"Plus" sample, i.e. restricted to randomly selected 50% of sample who received additional questions during the endline	
		
		*Figure 2a
		
		areg heardofcand_e  treat##i.party $controlsplusd $controlsplusd_treat if plus==1, absorb(code) cl(codeps)
		margins treat, by(i.party)
		marginsplot, x(party) graphregion(color(white)) recast(scatter) xtitle("Candidate party") ytitle("Heard of candidate, endline") scheme(lean2) title("Heard of candidate, predicted values by party")  legend(order(3 "Control" 4 "Treatment")) note("Showing 95% confidence intervals", size(medsmall)) 
		graph export fg2a.png, replace

		*Figure 2b
		
		areg sectorcorrect_e  treat##i.party $controlsplusd $controlsplusd_treat if plus==1 , absorb(code) cl(codeps)
		margins treat, by(i.party)
		marginsplot, x(party) graphregion(color(white)) recast(scatter) xtitle("Candidate party") scheme(lean2) ytitle("Sector priority correct, endline") title("Priority sector correct, predicted values by party")  legend(order(3 "Control" 4 "Treatment")) note("Showing 95% confidence intervals", size(medsmall)) 
		graph export fg2b.png, replace
		
	
	
**LIKABILITY


		*Figure 3
		
		areg like_e  treat##i.party $controlsplusd $controlsplusd_treat, absorb(code) cl(codeps)
		margins treat, by(i.party)
		marginsplot, x(party) graphregion(color(white)) recast(scatter) xtitle("Candidate party") ytitle("Candidate likability (0-10)") title("") scheme(lean2) legend(order(3 "Control" 4 "Treatment")) note("Showing 95% confidence intervals", size(medsmall))
		graph export fg3.png, replace




	 
****************************************************************************	
* TREATMENT EFFECTS: VOTE CHOICE
****************************************************************************

		*Figure 4
		
		set more off
		foreach x in votedopp votednrm votedindep {
	
		display "GE"
		areg `x' treat $controls $controls_treat if uniqueid == 1 , absorb(code) cl(codeps)
		eststo ge_`x'1
		estadd local control "Yes"
		estadd local fe "Yes"
		lincom treat, level(90)
		
		areg `x' treat if uniqueid == 1 , absorb(code) cl(codeps)
		eststo ge_`x'2
		estadd local control "No"
		estadd local fe "Yes"
		
		display "GE - NRM only"
		areg `x' treat $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1 , absorb(code) cl(codeps)
		eststo genrm_`x'1
		estadd local control "Yes"
		estadd local fe "Yes"
		lincom treat, level(90)
		
		areg `x' treat if uniqueid == 1 & intendnrm == 1 , absorb(code) cl(codeps)
		eststo genrm_`x'2
		estadd local control "No"
		estadd local fe "Yes"	
	}
	
	
# delimit;	 
	coefplot 
		  ge_votednrm1   genrm_votednrm1   || 
		  ge_votedopp1  genrm_votedopp1    || 
		   ge_votedindep1   genrm_votedindep1,
	keep(treat) bycoefs bylabels ( "Voted NRM" "Voted Opposition" "Voted Independent") 
	 xline(0, lcolor(gray)) graphregion(margin(l=1) color(white)) 
	 scheme(lean2)  levels(95 90)
	 legend(order(3 "All" 6 "Lean NRM")) 
	 xtitle(Average treatment effect on voting behavior) ylabel(,labsize(small))  xlabel(,format(%9.2f)) note("Showing 90 and 95% confidence intervals", size(medsmall))
	   mlabel format(%9.3f) mlabposition(12);
	   	 graph export fg4.png, replace	
	

	
	* Table 2: Determinants of voting for the ruling party
		
	foreach x in  votednrm {

			areg `x' treat treat_knowgap2_high  knowgap2_high $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo genrm_know_`x'1
			
			areg `x' treat treat_primcanddropped primcanddropped $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo genrm_prim_`x'1
			
			areg `x' treat treat_nrmpref_low  nrmpref_low $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo genrm_pref_`x'1
			}
			
		
		# delimit;
		esttab    genrm_know_votednrm1 genrm_prim_votednrm1 genrm_pref_votednrm1
		using table2.tex,
				title(Horserace - Intend NRM)
				addnote("Sample restricted to the general election and respondents who state they intend to vote for the NRM at baseline. The unit of observation is the individual voter. Knowledge gap high implies that a respondent's knowledge gap at baseline was above the median, where knowledge gap is defined as share of factual questions answered correctly about the NRM candidate minus the average share of factual questions answered correctly about all viable non-NRM candidates. The model includes constituency fixed effects and covariates. Standard errors are clustered by polling station. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
				keep(treat treat_primcanddropped treat_nrmpref_low treat_knowgap2_high knowgap2_high primcanddropped nrmpref_low _cons)
				varlabels (
				treat "Treat" 
				treat_knowgap2_high "Treat x High knowledge gap"
				treat_primcanddropped "Treat x Primary candidate dropped"
				treat_nrmpref_low "Treat x Open other parties"
				knowgap2_high "High knowledge gap"
				primcanddropped "Primary candidate dropped"
				nrmpref_low "Open other parties"
				_cons "Constant")
						mtitles("\shortstack{Voted NRM}"
						"\shortstack{Voted NRM}"
						"\shortstack{Voted NRM}")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 					
				stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;
			
		
	*Table 3: Voting behavior by feeling monitored
	
	foreach x in  votednrm votedopp turnout {
		
		areg `x' treat treat_monitor_bin monitor_bin $controlsintnrm $controlsintnrm_treat if uniqueid == 1 &intendnrm == 1, absorb(code) cl(codeps)
		eststo `x'_mon1
		
		}
	
	# delimit;
	esttab  votednrm_mon1 votedopp_mon1 turnout_mon1
		using table3.tex,
			stats(N r2 , fmt(0 3) labels (`"Observations"' `"\(R^{2}\)"'))
			title(Heterogeneous effects by feeling monitored, intend NRM)
			keep(treat treat_monitor_bin monitor_bin _cons)
			varlabels (
			treat "Treatment"
			treat_monitor_bin "Treatment * Feel monitored"
			monitor_bin "Feeling monitored"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles("\shortstack{Voted\\NRM}"
					"\shortstack{Voted\\Opposition}"
					"\shortstack{Turnout}")
		 label booktabs nonotes replace;

	
	
	
	
	

************************************************************
************************ APPENDICES ************************
************************************************************



**************************************	
* Appendix E: Descriptive Statistics *
**************************************	

* Table E1: Balance


	global covariates "age female educyr wealthindex nrmpref lastturnout  powerful counting salience source news"
	global dyadcovars "age female  educyr wealthindex nrmpref lastturnout powerful counting salience source news candinfo pastsupport cogender coethnic"
	
	la var age "Age"
	la var educyr "Education (years)"
	la var wealthindex "Wealth index"
	la var nrmpref "Closeness to NRM"
	la var lastturnout "Turnout in 2011 election"
	la var powerful "Ballot not secret" 
	la var counting "Elections free and fair"
	la var salience "Video salience"
	la var source "Videos preferred source"
	la var news "News consumption"
	
	logit treat $covariates i.code if uniqueid == 1, cl(codeps)
	eststo gepub_balance
	logit treat $covariates i.code if uniqueid == 1 & intendnrm == 1, cl(codeps)
	eststo genrm_balance


# delimit;
esttab     gepub_balance genrm_balance 
using balance.tex,
		title(Balance)
		keep(age female educyr wealthindex nrmpref lastturnout  powerful counting salience source news)
		addnote("All models include constituency fixed effects. Standard errors are clustered by polling station. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
		cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
		starlevels(* .10 ** .05 *** .01) 					
		mtitles( "\shortstack{\textbf{All}}"
				"\shortstack{\textbf{Lean NRM}}")
		stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
		fmt(0 3)) label booktabs nonotes replace;

* Table E2: Attrition

	preserve
	
	
	use "mtc_analysis_closethegap.dta", clear


	*Keep only dyads with filmed candidates
	keep if filmed == 1
	drop uniqueid
	egen uniqueid = tag(id)
	
	ttest attrition if uniqueid == 1, by(treat)

	eststo clear
	areg attrition treat if uniqueid == 1, cl(codeps) a(code) 
	eststo att1
	areg attrition treat `covariates' if uniqueid == 1, cl(codeps) a(code) 
	eststo att2

	*Intending to vote NRM
	areg attrition treat if uniqueid == 1 & intendnrm == 1, cl(codeps) a(code) 
	eststo attnrm1
	areg attrition treat `covariates' if uniqueid == 1 & intendnrm == 1, cl(codeps) a(code) 
	eststo attnrm2
	
	
		# delimit;
		esttab  att1 att2 attnrm1 attnrm2 
		using attrition.tex,
			title(Balance in attrition)
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
			fmt(0 3)) label booktabs nonotes replace;
			
			
	restore
	

* Table E3: Respondent characteristics

	*GE
	sum age educyr female lastturnout intendnrm nrmpref nocoethniccand nocoethnicvcand if uniqueid == 1
	sum coethnic 
	
	
	*GE-NRM
	sum age educyr female lastturnout intendnrm  nrmpref nocoethniccand nocoethnicvcand if uniqueid == 1 & intendnrm == 1
	sum coethnic if intendnrm == 1


	 

* Figure E2: Correlates of feeling monitored

		*Impute missing values for knowgap
		cap drop knowgap_i
		gen knowgap_i = knowgap
		qui sum knowgap if uniqueid == 1
		replace knowgap_i = r(mean) if knowgap == .

	areg monitor_bin age female educyr wealthindex news source salience  powerful counting nrmpref knowgap_i intendnrm lastturnout if uniqueid == 1, a(code)
		eststo monitor
		
		# delimit;
		coefplot monitor, keep(age female educyr wealthindex news source salience  powerful counting nrmpref knowgap intendnrm lastturnout) 
		xline(0, lcolor(gray)) graphregion(margin(l=1) color(white))  scheme(lean2)  levels(95 90) 
		coeflabels (age="Age" female="Female" educyr="Education years" wealthindex="Wealth index" nrmpref="NRM preference" lastturnout="Voted in 2011" powerful="Doubt secret ballot" counting="Doubt free & fair elections" salience="Salience of topics covered in video" source="Candidate video is preferred source" news="Frequency of news consumption" knowgap="Knowledge gap between NRM and other parties" intendnrm="Intend to vote for NRM") 
		ylabel(,labsize(small))  xlabel(,format(%9.2f)) mlabel format(%9.3f) mlabposition(12);
		graph export detoffear.png, replace	


* Table E4: Summary statistics for candidates


	bys code: latabstat candprty_c incumbent winner_ge vote intendedvote  tribe_c ratequalified_ep ratequalified_norm candunderstand_ep candunderstand_norm candexpress_ep   candexpress_norm , by(candidate) f(%5.2f)

		
	** Distribution of viable candidates
	
	 tab viable if uniquecand == 1, mi
	 bysort code: tab viable if uniquecand == 1, mi
	 

*Afrobarometer Cross-National Comparisons
*This section uses Afrobarometer merged data from rounds 6 and 7, available at https://www.afrobarometer.org/data/merged-round-7-data-34-countries-2019 and http://afrobarometer.org/data/merged-round-6-data-36-countries-2016, last accessed May 11, 2020.

	
	preserve


* Figure E3
	
	use "r7_merged_data_34ctry.release_excerpt.dta", clear
	
	gen trust_opp = 0 if Q43F == 0 | Q43F == 1
	replace trust_opp = 1 if Q43F == 2 | Q43F == 3
 
	gen trust_ruling = 0 if Q43E == 0 | Q43E == 1
	replace trust_ruling = 1 if Q43E == 2 | Q43E == 3
 
	bys COUNTRY: egen mean_trustopp = mean(trust_opp)
	bys COUNTRY: egen mean_trustruling = mean(trust_ruling)
 
	gen diff_trust = mean_trustruling - mean_trustopp
	duplicates drop COUNTRY, force
	drop if COUNTRY == 17
	
	graph hbar (asis) diff_trust, nofill over(COUNTRY, sort(diff_trust) descending label(labsize(vsmall)) ) ///
	scheme(lean2)  ytitle("") ///
	b1title("Difference in % of respondents who trust ruling party and opposition")

	graph export trust_graph.png, replace	
	  
* Figure E4
	  
	use "merged_r6_data_2016_36countries2_excerpt.dta", clear
	  
	gen viabledk = 0
	 replace viabledk = 1 if Q64 == 9
	 replace viabledk = . if Q64 == -1 | Q64 == 99
	  
	bys COUNTRY: egen mean_viabledk = mean(viabledk)  
	   
	graph hbar (mean) mean_viabledk, nofill over(COUNTRY, sort(mean_viabledk) descending label(labsize(vsmall))) scheme(lean2)  ytitle("") b1title("% of respondents with uncertainty about opposition viability")
	
	graph export viable_opp.png, replace
	
	restore


****************************************************************************	
*Appendix F: Additional Results
****************************************************************************	

***************************
* F.1 Political Knowledge *
***************************


* Table F1: Treatment effect on knowledge

		eststo clear
		
		foreach x in heardofcand_e sectorcorrect_e {
			areg `x'  treat i.party $controlsplusd $controlsplusd_treat if plus==1, absorb(code) cl(codeps)
			eststo `x'1
			areg `x'  treat if plus==1, absorb(code) cl(codeps)
			eststo `x'2
			
			areg `x'  treat i.party $controlsplusnrmd $controlsplusnrmd_treat if plus==1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo `x'_nrm1
			areg `x' treat if plus==1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo `x'_nrm2
			}
			
			
				
		# delimit;
		esttab  heardofcand_e1 heardofcand_e_nrm1 sectorcorrect_e1  sectorcorrect_e_nrm1 
		using know.tex,
			title(Treatment effects on knowledge )
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles( "\shortstack{Heard of\\All}"
					"\shortstack{Heard of\\Lean NRM}"
					"\shortstack{Sector correct\\All}"
					"\shortstack{Sector correct\\Lean NRM}")
			stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
			fmt(0 3)) label booktabs nonotes replace;
		
		



* Table F2: Treatment effect on knowledge, by candidate party (Table corresponding to Figure 2)
		cap drop treat_ind
		gen treat_ind = treat * ind
		
		eststo clear
		foreach x in heardofcand_e sectorcorrect_e {
		areg `x' treat treat_opp treat_ind opp ind $controlsplusd $controlsplusd_treat if plus==1, absorb(code) cl(codeps)
		eststo `x'
		
		areg `x' treat treat_opp treat_ind opp ind  if plus==1, absorb(code) cl(codeps)
		eststo `x'2
		
		areg `x' treat treat_opp treat_ind opp ind  $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1 & plus==1, absorb(code) cl(codeps)
		eststo `x'_nrm
		
		areg `x' treat treat_opp treat_ind opp ind if intendnrm == 1 & plus==1, absorb(code) cl(codeps)
		eststo `x'_nrm2
		
		}
		

	# delimit;
	esttab  heardofcand_e heardofcand_e_nrm sectorcorrect_e sectorcorrect_e_nrm
	using know_party.tex,
		title(Treatment effects on knowledge by candidate party)
		keep(treat treat_opp treat_ind opp ind _cons)
		varlabels (
		treat "Treatment"
		treat_opp "Treatment * Opposition"
		treat_ind "Treatment * Independent" 
		opp "Opposition"
		ind "Independent"
		_cons "Constant")
		cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
		starlevels(* .10 ** .05 *** .01) 					
		mtitles( "\shortstack{Heard of\\All}"
				"\shortstack{Heard of\\Lean NRM}"
				"\shortstack{Sector correct\\All}"
				"\shortstack{Sector correct\\Lean NRM}")
		stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
		fmt(0 3)) label booktabs nonotes replace;
				


****************		
*F.2 Likability*
****************

* Table F3: Treatment effect on candidate likability, by candidate and voter type (Table corresponding to Figure 3)
		
		foreach x in like_e {
		
			areg `x' treat opp ind  $controlsplusd $controlsplusd_treat, absorb(code) cl(codeps)
			eststo ge_`x'
		
			areg `x' treat treat_opp treat_ind opp ind  $controlsplusd $controlsplusd_treat, absorb(code) cl(codeps)
			eststo ge_`x'1
			
			areg `x' treat treat_opp treat_ind opp ind , absorb(code) cl(codeps)
			eststo ge_`x'2
			
			areg `x' treat opp ind  $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1 & plus==1, absorb(code) cl(codeps)
			eststo ge_`x'nrm
			
			areg `x' treat treat_opp treat_ind opp ind  $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1 & plus==1, absorb(code) cl(codeps)
			eststo ge_`x'nrm1
			
			areg `x' treat treat_opp treat_ind opp ind  if intendnrm == 1 & plus==1, absorb(code) cl(codeps)
			eststo ge_`x'nrm2
			}
			
			
		
		# delimit;
		esttab  ge_like_e ge_like_e1 ge_like_enrm ge_like_enrm1
		using like_party.tex,
				title(Likability)
				addnote("The unit of observation is the voter-candidate dyad. The dependent variable is a ten-point likability index (`On 1-10 scale, how much do you like the candidate?') All models include constituency fixed effects. Standard errors are clustered by polling station. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
				keep(treat treat_opp treat_ind opp ind _cons)
				varlabels (
				treat "Treatment"
				treat_opp "Treatment x Opposition candidate"
				opp "Opposition candidate"
				treat_ind "Treatment x Independent candidate"
				ind "Independent candidate"
				_cons "Constant")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 	
				mtitles( "\shortstack{Likeability\\All}"
				"\shortstack{Likeability\\All}"
				"\shortstack{Likeability\\Lean NRM}"
				"\shortstack{Likeability\\Lean NRM}")
				stats(N r2, labels("N" "R$^2$"  fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;
				

*Table F4: Treatment effect on candidate likability by partisanship
		
	foreach x in like_e {
		display "GE - NRM leaning (OPP)"
			areg `x' treat treat_opp opp $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1, absorb(code) cl(codeps)
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_opp
			estadd scalar est = r(estimate)
			test treat + treat_opp = 0
			estadd scalar p = r(p)
			eststo ge_`x'_nrmopp1

			areg `x' treat treat_opp opp if intendnrm == 1, absorb(code) cl(codeps)
			estadd local control "No"
			estadd local fe "Yes"
			lincom treat + treat_opp
			estadd scalar est = r(estimate)
			test treat + treat_opp = 0
			estadd scalar p = r(p)
			eststo ge_`x'_nrmopp2
			
			display "GE - NRM leaning (NRM)"
			areg `x' treat treat_nrm nrm $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1, absorb(code) cl(codeps)
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_nrm
			estadd scalar est2 = r(estimate)
			test treat + treat_nrm = 0
			estadd scalar p2 = r(p)
			eststo ge_`x'_nrmnrm1

			areg `x' treat treat_nrm nrm if intendnrm == 1, absorb(code) cl(codeps)
			estadd local control "No"
			estadd local fe "Yes"
			lincom treat + treat_nrm
			estadd scalar est2 = r(estimate)
			test treat + treat_nrm = 0
			estadd scalar p2 = r(p)
			eststo ge_`x'_nrmnrm2
			
			display "GE - Opp leaning"
			areg `x' treat treat_opp opp  $controlsplusoppd $controlsplusoppd_treat if intendopp == 1, absorb(code) cl(codeps)
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_opp
			estadd scalar est = r(estimate)
			test treat + treat_opp = 0
			estadd scalar p = r(p)
			eststo ge_`x'_oppopp1

			areg `x' treat treat_opp opp if intendopp == 1, absorb(code) cl(codeps)
			estadd local control "No"
			estadd local fe "Yes"
			lincom treat + treat_opp
			estadd scalar est = r(estimate)
			test treat + treat_opp = 0
			estadd scalar p = r(p)
			eststo ge_`x'_oppopp2
			
			display "GE - Opp leaning"
			areg `x' treat treat_nrm nrm  $controlsplusoppd $controlsplusoppd_treat if intendopp == 1, absorb(code) cl(codeps)
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_nrm
			estadd scalar est2 = r(estimate)
			test treat + treat_nrm = 0
			estadd scalar p2 = r(p)
			eststo ge_`x'_oppnrm1

			areg `x' treat treat_nrm nrm if intendopp == 1, absorb(code) cl(codeps)
			estadd local control "No"
			estadd local fe "Yes"
			lincom treat + treat_nrm
			estadd scalar est2 = r(estimate)
			test treat + treat_nrm = 0
			estadd scalar p2 = r(p)
			eststo ge_`x'_oppnrm2
			}


		# delimit;
		esttab  ge_like_e_nrmopp1 ge_like_e_nrmnrm1 ge_like_e_oppopp1 ge_like_e_oppnrm1
		using like_other.tex,
				title(Likability)
				addnote("The unit of observation is the voter-candidate dyad. The dependent variable is a ten-point likability index (`On 1-10 scale, how much do you like the candidate?') All models include constituency fixed effects. Standard errors are clustered by polling station. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
				keep(treat treat_opp opp treat_nrm nrm _cons)
				varlabels (
				treat "Treatment"
				treat_opp "Treatment x Opposition candidate"
				opp "Opposition candidate"
				treat_nrm "Treatment x NRM candidate"
				nrm "NRM candidate"
				_cons "Constant")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 	
				stats(N r2 est p est2 p2, labels("N" "R$^2$" "Coeff (Treat + Treat x Opp)" "p-value (Treat + Treat x Opp)" "Coeff (Treat + Treat x NRM)" "p-value (Treat + Treat x NRM)" fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;

		
***********************
* F.3 Voting behavior *
***********************

* Table F5: Treatment effects on vote choice (Table corresponding to Figure 4)

		set more off
		foreach x in votedopp votednrm votedindep {
		
			display "GE"
			areg `x' treat $controls $controls_treat if uniqueid == 1 , absorb(code) cl(codeps)
			eststo ge_`x'1
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)
			
			areg `x' treat if uniqueid == 1 , absorb(code) cl(codeps)
			eststo ge_`x'2
			estadd local control "No"
			estadd local fe "Yes"
			
			display "GE - NRM only"
			areg `x' treat $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1 , absorb(code) cl(codeps)
			eststo genrm_`x'1
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)
			
			areg `x' treat if uniqueid == 1 & intendnrm == 1 , absorb(code) cl(codeps)
			eststo genrm_`x'2
			estadd local control "No"
			estadd local fe "Yes"	
		}
		
		
		 	# delimit;
			esttab   ge_votednrm1  genrm_votednrm1  ge_votedopp1  genrm_votedopp1  ge_votedindep1  genrm_votedindep1  
			using votechoice_all2.tex,
			stats(N r2 , fmt(0 3) labels (`"Observations"' `"\(R^{2}\)"'))
			title(Treatment effects on vote choice)
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles("\shortstack{Voted NRM\\All}"
					"\shortstack{Voted NRM\\Lean NRM}"
					"\shortstack{Voted Opp\\All}"
					"\shortstack{Voted Opp\\Lean NRM}"
					"\shortstack{Voted Ind\\All}"
					"\shortstack{Voted Ind\\Lean NRM}")
		 label booktabs nonotes replace;
		 
		*Wald test
		
		*All
		reg votednrm treat $controls $controls_treat i.code if uniqueid == 1
		est store m1
		reg votedopp treat $controls $controls_treat i.code if uniqueid == 1
		est store m2
		suest m1 m2, cl(codeps)
		test [m1_mean]treat = [m2_mean]treat
		* Prob > chi2 =    0.0711
		
		*Intend NRM
		reg votednrm treat $controlsintnrm $controlsintnrm_treat i.code if uniqueid == 1 & intendnrm == 1
		est store m3
		reg votedopp treat $controlsintnrm $controlsintnrm_treat i.code if uniqueid == 1 & intendnrm == 1
		est store m4
		suest m3 m4, cl(codeps)
		test [m3_mean]treat = [m4_mean]treat
		*Prob > chi2 =    0.0031
			

		*Table F6: Treatment effects on vote choice, conditional on turnout	 
		set more off
		foreach x in votedopp votednrm votedindep {
		
			display "GE"
			areg `x' treat $controlsgetout $controlsgetout_treat if uniqueid == 1 & turnout==1 , absorb(code) cl(codeps)
			eststo ge_`x'1t
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)
			
			areg `x' treat if uniqueid == 1 & turnout==1, absorb(code) cl(codeps)
			eststo ge_`x'2t
			estadd local control "No"
			estadd local fe "Yes"
			
			display "GE - NRM only"
			areg `x' treat $controlsintnrmtout $controlsintnrmtout_treat if uniqueid == 1 & intendnrm == 1 & turnout==1, absorb(code) cl(codeps)
			eststo genrm_`x'1t
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)
			
			areg `x' treat if uniqueid == 1 & intendnrm == 1 & turnout==1, absorb(code) cl(codeps)
			eststo genrm_`x'2t
			estadd local control "No"
			estadd local fe "Yes"	
		}
		
	
	# delimit;
			esttab   ge_votednrm1t  genrm_votednrm1t  ge_votedopp1t  genrm_votedopp1t  ge_votedindep1t  genrm_votedindep1t 
			using votechoice_all2t.tex,
			stats(N r2 , fmt(0 3) labels (`"Observations"' `"\(R^{2}\)"'))
			title(Treatment effects on vote choice)
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles("\shortstack{Voted NRM\\All}"
					"\shortstack{Voted NRM\\Lean NRM}"
					"\shortstack{Voted Opp\\All}"
					"\shortstack{Voted Opp\\Lean NRM}"
					"\shortstack{Voted Ind\\All}"
					"\shortstack{Voted Ind\\Lean NRM}")
		 label booktabs nonotes replace;
		 
		 
	
* Table F7: Treatment effects on vote choice (incumbent, debate winner)
	
	set more off
	foreach x in votedinc voteddebwinner voteddebwinnerpop {
	
		display "GE"
		areg `x' treat $controls $controls_treat if uniqueid == 1 , absorb(code) cl(codeps)
		eststo ge_`x'1
		estadd local control "Yes"
		estadd local fe "Yes"
		lincom treat, level(90)
		
		
		display "GE - NRM only"
		areg `x' treat $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1 , absorb(code) cl(codeps)
		eststo genrm_`x'1
		estadd local control "Yes"
		estadd local fe "Yes"
		lincom treat, level(90)
		
		display "GE - tunrout"
		areg `x' treat $controlsgetout $controlsgetout_treat if uniqueid == 1 & turnout==1 , absorb(code) cl(codeps)
		eststo ge_`x'1t
		estadd local control "Yes"
		estadd local fe "Yes"
		lincom treat, level(90)
		

		display "GE - NRM only & turnout"
		areg `x' treat $controlsintnrmtout $controlsintnrmtout_treat if uniqueid == 1 & intendnrm == 1 & turnout==1, absorb(code) cl(codeps)
		eststo genrm_`x'1t
		estadd local control "Yes"
		estadd local fe "Yes"
		lincom treat, level(90)
	}
	


		 
	# delimit;
	esttab    ge_votedinc1 genrm_votedinc1  ge_voteddebwinner1 genrm_voteddebwinner1  ge_voteddebwinnerpop1 genrm_voteddebwinnerpop1  
		using votechoice_others_all2.tex,
			stats(N r2 , fmt(0 3) labels (`"Observations"' `"\(R^{2}\)"'))
			title(Treatment effects on vote choice)
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles("\shortstack{Voted Inc\\All}"
					"\shortstack{Voted Inc\\Lean NRM}"
					"\shortstack{Voted best performer (exp)\\All}"
					"\shortstack{Voted best performer (exp)\\Lean NRM}"
					"\shortstack{Voted best performer (pop)\\All}"
					"\shortstack{Voted best performer (pop)\\Lean NRM}")
			label booktabs nonotes replace;
			
	
		
		 
* Table F8: Treatment effects on turnout

	set more off
	foreach x in turnout {
		
			display "GE"
			areg `x' treat $controls $controls_treat if uniqueid == 1 , absorb(code) cl(codeps)
			eststo ge_`x'1
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)
			
			areg `x' treat if uniqueid == 1 , absorb(code) cl(codeps)
			eststo ge_`x'2
			estadd local control "No"
			estadd local fe "Yes"
			
			display "GE - NRM only"
			areg `x' treat $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1 , absorb(code) cl(codeps)
			eststo genrm_`x'1
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)
			
			areg `x' treat if uniqueid == 1 & intendnrm == 1 , absorb(code) cl(codeps)
			eststo genrm_`x'2
			estadd local control "No"
			estadd local fe "Yes"	
		}
		
		
	# delimit;
	esttab   ge_turnout1 ge_turnout2 genrm_turnout1 genrm_turnout2
			using turnout.tex,
			stats(N r2 , fmt(0 3) labels (`"Observations"' `"\(R^{2}\)"'))
			title(Treatment effects on turnout)
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles("\shortstack{Turnout\\(All)}"
					"\shortstack{Turnout\\(All)}"
					"\shortstack{Turnout\\(Lean NRM)}"
					"\shortstack{Turnout\\(Lean NRM)}")
		 label booktabs nonotes replace;
		 

*********************************		
* F.4 Correlates of vote choice *
*********************************	

* Figure F5: Determinants of vote choice in the control group
	
			
	reg vote access effort rateavg informed candparty persohelp offer coethnic corrupt mpdistrictalign mpmoneyalign mpsectoralign i.code if heardof == 1 & treat == 0, cluster(id) 
	eststo m2
		
	coefplot (m2), keep(access effort rateavg candparty corrupt mpdistrictalign  mpsectoralign mpmoneyalign persohelp offer coethnic) xline(0, lcolor(gray)) scheme(lean2) levels(95 90) ciopts(lwidth(medium thick)) grid(none) aseq   note("90% and 95% confidence intervals, n=4,822 voter-candidate dyads")
	graph export DetOfVote2_ge_lean.png, replace
	
	


********************************
* F.5 Alternative explanations *
********************************


* Table F9: Candidate party, winners, video performance, and vote margin in the general election

	sort code
	list code candprty_c if debatewinner == 1 & uniquecand == 1
	list code candprty_c if debate_winner_experts == 1 & uniquecand == 1
	list code candprty_c if winner_ge == 1 & uniquecand == 1
	list code margin if uniqueconst == 1
	

* Table F10: Switching and enumerator perception

	cap drop treat_govsent
	gen treat_govsent = treat * govsent

	areg switch treat treat_govsent  govsent $controls $controls_treat if uniqueid==1, absorb(code) cl(codeps)
	eststo gepub_switch_govsent
	
	areg switch treat treat_govsent govsent $controlsintnrm $controlsintnrm_treat if uniqueid==1 & intendnrm == 1, absorb(code) cl(codeps)
	eststo geint_switch_govsent
	
	areg switchtoopp treat treat_govsent govsent $controls $controls_treat if uniqueid==1 & intendnrm == 1, absorb(code) cl(codeps)
	eststo geint_switchtoopp_govsent




		# delimit;
		esttab    gepub_switch_govsent geint_switch_govsent  geint_switchtoopp_govsent 
		using switchgovsent.tex,
				title(Switching and enumerator perception )
				addnote("All models include constituency fixed effects. Standard errors are clustered by polling station. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
				keep(treat treat_govsent govsent _cons)
				varlabels (
				treat "Treatment" 
				treat_govsent "Treat x Government sent"
				govsent "Government sent"
				_cons "Constant")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 					
				mtitles( "\shortstack{Switch\\All}"
						"\shortstack{Switch \\Lean NRM}"
						"\shortstack{Switch to Opp\\Lean NRM}")
				stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;
	

* Table F11: Candidate response


	
	eststo clear
	foreach x in anycandvisit_e offer_e {
	
	areg `x' treat  $controls $controls_treat if uniqueid == 1, cl(codeps) a(code)
		eststo `x'1
		
	areg `x' treat if uniqueid == 1, cl(codeps) a(code)
		eststo `x'2
		
		}

		
		
		# delimit;
		esttab   anycandvisit_e1 anycandvisit_e2 offer_e1 offer_e2
		using candresponse.tex,
				title(Candidate response )
				addnote("All models include constituency fixed effects. Standard errors are clustered by polling station. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
				keep(treat _cons)
				varlabels (
				treat "Treatment" 
				_cons "Constant")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 					
				mtitles("\shortstack{Any candidate visit}"
						"\shortstack{Any candidate visit}"
						"\shortstack{Any offer}"
						"\shortstack{Any offer}")
				stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;
	


* Table F12: Treatment effects on perceived free and fairness of the election


	eststo clear

	foreach y in freevote_e {
		display "Free and fair"
		
		areg `y' treat $controls $controls_treat if uniqueid == 1, cl(codeps) absorb(code)
		eststo free1
		
		areg `y' treat if uniqueid == 1, cl(codeps) absorb(code)
		eststo free2
		
		areg `y' treat $controlsintnrm $controlsintnrm_treat  if uniqueid == 1 & intendnrm == 1, cl(codeps) absorb(code)
		eststo free3
		
		areg `y' treat if uniqueid == 1 & intendnrm == 1, cl(codeps) absorb(code)
		eststo free4
		
		}
	
	

	# delimit;
	esttab   free1 free2 free3 free4 
	using freevote_e.tex,
			title(Treatment effects on perceived free and fairness of elections)
			addnote("The unit of observation is the individual voter. The model includes constituency fixed effects and--as indicated--covariates. Standard errors are clustered by polling station. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
			keep(treat _cons)
			varlabels (treat "Treatment"  
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			stats(N r2 , labels("N" "R$^2$" fmt(%5)) 	
			fmt(0 3)) label booktabs nonotes replace;
			
			# delimit cr
		


*******************************
* F.6 Polling station results *
*******************************

* Table F13: Polling station results

	
	foreach x in age female educyr wealthindex nrmpref lastturnout  powerful counting salience source news  {
		egen `x'_psavg = mean(`x'), by(codeps)
		egen `x'_st_psavg = std(`x'_psavg) if uniqueps == 1
		egen t = mean(`x'_st_psavg), by(codeps)
		replace `x'_st_psavg = t if `x'_st_psavg == .
		drop `x'_psavg t
		gen treat_`x'_st_psavg = treat * `x'_st_psavg
		}
		
	global controls2ps "age_st_psavg female_st_psavg educyr_st_psavg wealthindex_st_psavg nrmpref_st_psavg lastturnout_st_psavg powerful_st_psavg counting_st_psavg salience_st_psavg source_st_psavg news_st_psavg"
	global treat_controls2ps "treat_age_st_psavg treat_female_st_psavg treat_educyr_st_psavg treat_wealthindex_st_psavg treat_nrmpref_st_psavg treat_lastturnout_st_psavg treat_powerful_st_psavg treat_counting_st_psavg treat_salience_st_psavg treat_source_st_psavg treat_news_st_psavg"
	
	sum $controls2ps $treat_controls2ps if uniqueps == 1
	
	foreach x in turnout_ps votesharenrm_ps voteshareopp_ps voteshareind_ps  {
		egen t = mean(`x'), by(codeps)
		replace `x' = t if `x' == .
		drop t
		}


	foreach x in turnout_ps votesharenrm_ps voteshareopp_ps voteshareind_ps  {
		areg `x' treat $controls2ps $treat_controls2ps if uniqueps == 1, a(code)
		eststo `x'
		}

	# delimit;
			esttab  turnout_ps votesharenrm_ps voteshareopp_ps voteshareind_ps
			using "ge_ps.tex",
					title(Polling station results)
					addnote("Dependent variables from official polling station results. The unit of observation is the polling station. All models include constituency fixed effects and covariates. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
					keep(treat _cons)
					varlabels (
					treat "Treatment" _cons "Constant")
					cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
					starlevels(* .10 ** .05 *** .01) 					
					mtitles("\shortstack{\textbf{Turnout}}"
							"\shortstack{\textbf{NRM}}"
							"\shortstack{\textbf{Opposition}}"
							"\shortstack{\textbf{Independent}}")
					stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
					fmt(0 3)) label booktabs nonotes replace;
					

*********************************
* Appendix G: Robustness Checks *
*********************************


* Table G1: Treatment effect on knowledge – No covariates

		*Overall
		eststo clear
		
		foreach x in heardofcand_e sectorcorrect_e {

			areg `x'  treat if plus==1, absorb(code) cl(codeps)
			eststo `x'2
			
			areg `x' treat if plus==1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo `x'_nrm2
			}
			

		# delimit;
		esttab  heardofcand_e2 heardofcand_e_nrm2 sectorcorrect_e2  sectorcorrect_e_nrm2 
		using know_nocov.tex,
			title(Treatment effects on knowledge )
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles( "\shortstack{Heard of\\All}"
					"\shortstack{Heard of\\Lean NRM}"
					"\shortstack{Sector correct\\All}"
					"\shortstack{Sector correct\\Lean NRM}")
			stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
			fmt(0 3)) label booktabs nonotes replace;
			
			
* Table G2: Treatment effect on knowledge – Standard errors clustered by respondent
	
		eststo clear
		
		foreach x in heardofcand_e sectorcorrect_e {
			areg `x'  treat i.party $controlsplusd $controlsplusd_treat if plus==1, absorb(code) cl(id)
			eststo `x'1
			
			areg `x'  treat i.party $controlsplusnrmd $controlsplusnrmd_treat if plus==1 & intendnrm == 1, absorb(code) cl(id)
			eststo `x'_nrm1
			}
			
			
				
		# delimit;
		esttab  heardofcand_e1 heardofcand_e_nrm1 sectorcorrect_e1  sectorcorrect_e_nrm1 
		using know_idcl.tex,
			title(Treatment effects on knowledge )
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles( "\shortstack{Heard of\\All}"
					"\shortstack{Heard of\\Lean NRM}"
					"\shortstack{Sector correct\\All}"
					"\shortstack{Sector correct\\Lean NRM}")
			stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
			fmt(0 3)) label booktabs nonotes replace;
			
* Table G3: see below

* Table G4: Treatment effect on knowledge, by candidate party – No covariates
		
		cap drop treat_ind
		gen treat_ind = treat * ind
		
		eststo clear
		foreach x in heardofcand_e sectorcorrect_e {
		
		areg `x' treat treat_opp treat_ind opp ind  if plus==1, absorb(code) cl(codeps)
		eststo `x'2
		
		areg `x' treat treat_opp treat_ind opp ind if intendnrm == 1 & plus==1, absorb(code) cl(codeps)
		eststo `x'_nrm2
		
		}
		

		*No cov
		# delimit;
		esttab  heardofcand_e2 heardofcand_e_nrm2 sectorcorrect_e2 sectorcorrect_e_nrm2
		using know_party_nocov.tex,
			title(Treatment effects on knowledge by candidate party)
			keep(treat treat_opp treat_ind opp ind _cons)
			varlabels (
			treat "Treatment"
			treat_opp "Treatment * Opposition"
			treat_ind "Treatment * Independent" 
			opp "Opposition"
			ind "Independent"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles( "\shortstack{Heard of\\All}"
					"\shortstack{Heard of\\Lean NRM}"
					"\shortstack{Sector correct\\All}"
					"\shortstack{Sector correct\\Lean NRM}")
			stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
			fmt(0 3)) label booktabs nonotes replace;
				
	
* Table G5: Treatment effect on knowledge, by candidate party – Standard errors clustered by respondent	
	
		eststo clear
		foreach x in heardofcand_e sectorcorrect_e {
		areg `x' treat treat_opp treat_ind opp ind $controlsplusd $controlsplusd_treat if plus==1, absorb(code) cl(id)
		eststo `x'

		areg `x' treat treat_opp treat_ind opp ind  $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1 & plus==1, absorb(code) cl(id)
		eststo `x'_nrm
		}
		

	# delimit;
	esttab  heardofcand_e heardofcand_e_nrm sectorcorrect_e sectorcorrect_e_nrm
	using know_party_idcl.tex,
		title(Treatment effects on knowledge by candidate party)
		keep(treat treat_opp treat_ind opp ind _cons)
		varlabels (
		treat "Treatment"
		treat_opp "Treatment * Opposition"
		treat_ind "Treatment * Independent" 
		opp "Opposition"
		ind "Independent"
		_cons "Constant")
		cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
		starlevels(* .10 ** .05 *** .01) 					
		mtitles( "\shortstack{Heard of\\All}"
				"\shortstack{Heard of\\Lean NRM}"
				"\shortstack{Sector correct\\All}"
				"\shortstack{Sector correct\\Lean NRM}")
		stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
		fmt(0 3)) label booktabs nonotes replace;
		
* Table G6: See below
				
* Table G7: Treatment effect on candidate likability -- No covariates
	
	foreach x in like_e {

			areg `x' treat treat_opp opp if intendnrm == 1, absorb(code) cl(codeps)
			estadd local control "No"
			estadd local fe "Yes"
			lincom treat + treat_opp
			estadd scalar est = r(estimate)
			test treat + treat_opp = 0
			estadd scalar p = r(p)
			eststo ge_`x'_nrmopp2
			
			areg `x' treat treat_nrm nrm if intendnrm == 1, absorb(code) cl(codeps)
			estadd local control "No"
			estadd local fe "Yes"
			lincom treat + treat_nrm
			estadd scalar est2 = r(estimate)
			test treat + treat_nrm = 0
			estadd scalar p2 = r(p)
			eststo ge_`x'_nrmnrm2

			areg `x' treat treat_opp opp if intendopp == 1, absorb(code) cl(codeps)
			estadd local control "No"
			estadd local fe "Yes"
			lincom treat + treat_opp
			estadd scalar est = r(estimate)
			test treat + treat_opp = 0
			estadd scalar p = r(p)
			eststo ge_`x'_oppopp2
			
			areg `x' treat treat_nrm nrm if intendopp == 1, absorb(code) cl(codeps)
			estadd local control "No"
			estadd local fe "Yes"
			lincom treat + treat_nrm
			estadd scalar est2 = r(estimate)
			test treat + treat_nrm = 0
			estadd scalar p2 = r(p)
			eststo ge_`x'_oppnrm2
			}
			

		# delimit;
		esttab  ge_like_e_nrmopp2 ge_like_e_nrmnrm2 ge_like_e_oppopp2 ge_like_e_oppnrm2
		using like_other_nocov.tex,
				title(Likability)
				addnote("The unit of observation is the voter-candidate dyad. The dependent variable is a ten-point likability index (`On 1-10 scale, how much do you like the 	candidate?') All models include constituency fixed effects. Standard errors are clustered by polling station. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
				keep(treat treat_opp opp treat_nrm nrm _cons)
				varlabels (
				treat "Treatment"
				treat_opp "Treatment x Opposition candidate"
				opp "Opposition candidate"
				treat_nrm "Treatment x NRM candidate"
				nrm "NRM candidate"
				_cons "Constant")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 	
				stats(N r2 est p est2 p2, labels("N" "R$^2$" "Coeff (Treat + Treat x Opp)" "p-value (Treat + Treat x Opp)" "Coeff (Treat + Treat x NRM)" "p-value (Treat + Treat x NRM)" fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;
			
* Table G8: Treatment effect on candidate likability – Standard errors clustered by respondent

		foreach x in like_e {
		display "GE - NRM leaning (OPP)"
			areg `x' treat treat_opp opp $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1, absorb(code) cl(id)
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_opp
			estadd scalar est = r(estimate)
			test treat + treat_opp = 0
			estadd scalar p = r(p)
			eststo ge_`x'_nrmopp1
			
			display "GE - NRM leaning (NRM)"
			areg `x' treat treat_nrm nrm $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1, absorb(code) cl(id)
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_nrm
			estadd scalar est2 = r(estimate)
			test treat + treat_nrm = 0
			estadd scalar p2 = r(p)
			eststo ge_`x'_nrmnrm1
			
			display "GE - Opp leaning"
			areg `x' treat treat_opp opp  $controlsplusoppd $controlsplusoppd_treat if intendopp == 1, absorb(code) cl(id)
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_opp
			estadd scalar est = r(estimate)
			test treat + treat_opp = 0
			estadd scalar p = r(p)
			eststo ge_`x'_oppopp1
			
			display "GE - Opp leaning"
			areg `x' treat treat_nrm nrm  $controlsplusoppd $controlsplusoppd_treat if intendopp == 1, absorb(code) cl(id)
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_nrm
			estadd scalar est2 = r(estimate)
			test treat + treat_nrm = 0
			estadd scalar p2 = r(p)
			eststo ge_`x'_oppnrm1

			}


		# delimit;
		esttab  ge_like_e_nrmopp1 ge_like_e_nrmnrm1 ge_like_e_oppopp1 ge_like_e_oppnrm1
		using like_other_idcl.tex,
				title(Likability)
				addnote("The unit of observation is the voter-candidate dyad. The dependent variable is a ten-point likability index (`On 1-10 scale, how much do you like the candidate?') All models include constituency fixed effects. Standard errors are clustered by respondent. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
				keep(treat treat_opp opp treat_nrm nrm _cons)
				varlabels (
				treat "Treatment"
				treat_opp "Treatment x Opposition candidate"
				opp "Opposition candidate"
				treat_nrm "Treatment x NRM candidate"
				nrm "NRM candidate"
				_cons "Constant")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 	
				stats(N r2 est p est2 p2, labels("N" "R$^2$" "Coeff (Treat + Treat x Opp)" "p-value (Treat + Treat x Opp)" "Coeff (Treat + Treat x NRM)" "p-value (Treat + Treat x NRM)" fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;

* Tables G9-12: See below

***********************
* G2: Voting behavior *
***********************

* Table G13: Treatment effects on vote choice – No covariates
	
		set more off
		foreach x in votedopp votednrm votedindep {
		
			display "GE"
			areg `x' treat if uniqueid == 1 , absorb(code) cl(codeps)
			eststo ge_`x'2
			estadd local control "No"
			estadd local fe "Yes"
			
			display "GE - NRM only"			
			areg `x' treat if uniqueid == 1 & intendnrm == 1 , absorb(code) cl(codeps)
			eststo genrm_`x'2
			estadd local control "No"
			estadd local fe "Yes"	
		}
		
		# delimit;
		esttab   ge_votednrm2  genrm_votednrm2   ge_votedopp2  genrm_votedopp2  ge_votedindep2  genrm_votedindep2 
				using votechoice_all2_nocov.tex,
				stats(N r2 , fmt(0 3) labels (`"Observations"' `"\(R^{2}\)"'))
				title(Treatment effects on vote choice, no covariates)
				keep(treat _cons)
				varlabels (
				treat "Treatment"
				_cons "Constant")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 					
				mtitles("\shortstack{Voted NRM\\All}"
						"\shortstack{Voted NRM\\Lean NRM}"
						"\shortstack{Voted Opp\\All}"
						"\shortstack{Voted Opp\\Lean NRM}"
						"\shortstack{Voted Ind\\All}"
						"\shortstack{Voted Ind\\Lean NRM}")
			 label booktabs nonotes replace;
			 
* Table G14: Treatment effects on vote choice – Turnout at face value
	
		set more off
		foreach x in votedopp_e votednrm_e votedindep_e {
		
			display "GE"
			areg `x' treat $controls $controls_treat if uniqueid == 1 , absorb(code) cl(codeps)
			eststo ge_`x'1
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)
			
			display "GE - NRM only"
			areg `x' treat $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1 , absorb(code) cl(codeps)
			eststo genrm_`x'1
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)
		}

		foreach x in turnout_e {
		
			display "GE"
			areg `x' treat $controls $controls_treat if uniqueid == 1 , absorb(code) cl(codeps)
			eststo ge_`x'1
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)
			
			display "GE - NRM only"
			areg `x' treat $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1 , absorb(code) cl(codeps)
			eststo genrm_`x'1
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)	
		}
		
		
		
	# delimit;
	esttab  ge_votednrm_e1  genrm_votednrm_e1    ge_votedopp_e1  genrm_votedopp_e1  ge_votedindep_e1  genrm_votedindep_e1   ge_turnout_e1  genrm_turnout_e1
			using votechoice_all2_turnout_face.tex,
			stats(N r2 , fmt(0 3) labels (`"Observations"' `"\(R^{2}\)"'))
			title(Treatment effects on vote choice)
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles("\shortstack{Voted NRM\\All}"
					"\shortstack{Voted NRM\\Lean NRM}"
					"\shortstack{Voted Opp\\All}"
					"\shortstack{Voted Opp\\Lean NRM}"
					"\shortstack{Voted Ind\\All}"
					"\shortstack{Voted Ind\\Lean NRM}"
					"\shortstack{Turnout\\All}"
					"\shortstack{Turnout\\Lean NRM}")
		 label booktabs nonotes replace;
		 
* Tables G15-G16: See below

* Table G17: Treatment effects on switching

		 
	eststo clear	 
	set more off
	foreach x in switch {
	
		display "All"
		areg `x' treat $controls $controls_treat if uniqueid == 1 , absorb(code) cl(codeps)
		eststo ge_`x'1
		
		display "NRM only"
		areg `x' treat $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1 , absorb(code) cl(codeps)
		eststo genrm_`x'1
		
		display "NON NRM only"
		areg `x' treat $controlsintnn $controlsintnn_treat if uniqueid == 1 & intendnrm != 1 , absorb(code) cl(codeps)
		eststo genn_`x'1
	
		
	}
	
	
	# delimit;
		esttab  ge_switch1 genrm_switch1 genn_switch1
		using switch.tex,
				title(Switching)
				addnote("The unit of observation is the individual voter. All models includes constituency fixed effects and covariates. Standard errors are clustered by polling station. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
				keep(treat _cons)
				varlabels (
				treat "Treat" 
				_cons "Constant")
						mtitles("\shortstack{All}"
						"\shortstack{Lean NRM}"
						"\shortstack{Don't lean NRM}")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 					
				stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;
		
******************		 
* G3: Mechanisms *
******************


* Table G18: Determinants of voting for the ruling party – No covariates
		
	foreach x in votednrm {

			areg `x' treat treat_primcanddropped  primcanddropped if uniqueid == 1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo genrm_prim_`x'2
			
			areg `x' treat treat_knowgap2_high  knowgap2_high if uniqueid == 1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo genrm_know_`x'2
			
			areg `x' treat treat_nrmpref_low  nrmpref_low if uniqueid == 1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo genrm_pref_`x'2
			}
			
		
		# delimit;
		esttab    genrm_know_votednrm2 genrm_prim_votednrm2 genrm_pref_votednrm2 
		using horserace_votednrm_nocov.tex,
				title(Horserace - Intend NRM)
				addnote("Sample restricted to the general election and respondents who state they intend to vote for the NRM at baseline. The unit of observation is the individual voter. Knowledge gap high implies that a respondent's knowledge gap at baseline was above the median, where knowledge gap is defined as share of factual questions answered correctly about the NRM candidate minus the average share of factual questions answered correctly about all viable non-NRM candidates. The model includes constituency fixed effects and covariates. Standard errors are clustered by polling station. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
				keep(treat treat_primcanddropped treat_nrmpref_low treat_knowgap2_high knowgap2_high primcanddropped nrmpref_low _cons)
				varlabels (
				treat "Treat" 
				treat_knowgap2_high "Treat x High knowledge gap"
				treat_primcanddropped "Treat x Primary candidate dropped"
				treat_nrmpref_low "Treat x Open other parties"
				knowgap2_high "High knowledge gap"
				primcanddropped "Primary candidate dropped"
				nrmpref_low "Open other parties"
				_cons "Constant")
						mtitles("\shortstack{Voted NRM}"
						"\shortstack{Voted NRM}"
						"\shortstack{Voted NRM}")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 					
				stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;
	

* Table G19: Determinants of voting for the ruling party – Standard errors clustered by respondent

	foreach x in votednrm {

			areg `x' treat treat_knowgap2_high  knowgap2_high $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1, absorb(code) cl(id)
			eststo genrm_know_`x'1
			
			areg `x' treat treat_primcanddropped primcanddropped $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1, absorb(code) cl(id)
			eststo genrm_prim_`x'1
			
			areg `x' treat treat_nrmpref_low  nrmpref_low $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1, absorb(code) cl(id)
			eststo genrm_pref_`x'1
			}
			
		
		# delimit;
		esttab    genrm_know_votednrm1 genrm_prim_votednrm1 genrm_pref_votednrm1
		using horserace_votednrm_idcl.tex,
				title(Horserace - Intend NRM)
				addnote("Sample restricted to the general election and respondents who state they intend to vote for the NRM at baseline. The unit of observation is the individual voter. Knowledge gap high implies that a respondent's knowledge gap at baseline was above the median, where knowledge gap is defined as share of factual questions answered correctly about the NRM candidate minus the average share of factual questions answered correctly about all viable non-NRM candidates. The model includes constituency fixed effects and covariates. Standard errors are clustered by respondent. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
				keep(treat treat_primcanddropped treat_nrmpref_low treat_knowgap2_high knowgap2_high primcanddropped nrmpref_low _cons)
				varlabels (
				treat "Treat" 
				treat_knowgap2_high "Treat x High knowledge gap"
				treat_primcanddropped "Treat x Primary candidate dropped"
				treat_nrmpref_low "Treat x Open other parties"
				knowgap2_high "High knowledge gap"
				primcanddropped "Primary candidate dropped"
				nrmpref_low "Open other parties"
				_cons "Constant")
						mtitles("\shortstack{Voted NRM}"
						"\shortstack{Voted NRM}"
						"\shortstack{Voted NRM}")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 					
				stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;
			
		
* Table G20: Voting behavior by feeling monitored – No covariates
	
	foreach x in  votednrm votedopp turnout {
		
		areg `x' treat treat_monitor_bin monitor_bin if uniqueid == 1 &intendnrm == 1, absorb(code) cl(codeps)
		eststo `x'_mon2
		
		}
	
	# delimit;
	esttab  votednrm_mon2 votedopp_mon2 turnout_mon2
		using het_monitor_nrm_nocov.tex,
			stats(N r2 , fmt(0 3) labels (`"Observations"' `"\(R^{2}\)"'))
			title(Heterogeneous effects by feeling monitored, intend NRM)
			keep(treat treat_monitor_bin monitor_bin _cons)
			varlabels (
			treat "Treatment"
			treat_monitor_bin "Treatment * Feel monitored"
			monitor_bin "Feeling monitored"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles("\shortstack{Voted\\NRM}"
					"\shortstack{Voted\\Opposition}"
					"\shortstack{Turnout}")
		 label booktabs nonotes replace;
		 
* Table G21: Voting behavior by feeling monitored – Standard errors clustered by respondent

		foreach x in  votednrm votedopp turnout {
		
		areg `x' treat treat_monitor_bin monitor_bin $controlsintnrm $controlsintnrm_treat if uniqueid == 1 &intendnrm == 1, absorb(code) cl(id)
		eststo `x'_mon1
		
		}
	
	# delimit;
	esttab  votednrm_mon1 votedopp_mon1 turnout_mon1
		using het_monitor_nrm_idcl.tex,
			stats(N r2 , fmt(0 3) labels (`"Observations"' `"\(R^{2}\)"'))
			title(Heterogeneous effects by feeling monitored, intend NRM)
			keep(treat treat_monitor_bin monitor_bin _cons)
			varlabels (
			treat "Treatment"
			treat_monitor_bin "Treatment * Feel monitored"
			monitor_bin "Feeling monitored"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles("\shortstack{Voted\\NRM}"
					"\shortstack{Voted\\Opposition}"
					"\shortstack{Turnout}")
		 label booktabs nonotes replace;

	


			
****************************************************************************************
* Robustness check for intend NRM specs, due to imbalanced attrition in this subsample *
****************************************************************************************

*** Use the expanded sample to calculate inverse probability weights and Manski bounds

	
	use "mtc_analysis_closethegap.dta", clear


	*Keep only dyads with filmed candidates
	keep if filmed == 1
	drop uniqueid
	egen uniqueid = tag(id)
	
	*Calculate inverse probability weights
	
	cap drop pid
	cap drop ipwd
	cap drop pi
	cap drop ipw
	
	local covariates "age female educyr wealthindex nrmpref lastturnout  powerful counting salience source news"
	
	foreach x in `covariates' {
		cap drop `x'_t
		gen `x'_t = `x' * treat
		}
		
	local cov_t "age_t female_t educyr_t wealthindex_t nrmpref_t lastturnout_t powerful_t counting_t salience_t source_t news_t"
	
	*Weights calculated at the dyad level
	logit attrition `cov' `cov_t' i.code if intendnrm == 1 & filmed == 1, cl(codeps)
	predict pid, p
	gen ipwd = 1/(1-pid) 
	
	*Weights calculated at the individual level
	logit attrition `cov' `cov_t' i.code if intendnrm == 1 & uniqueid == 1, cl(codeps)
	predict pi, p
	gen ipw = 1/(1-pi) 
	
	
	*Set up for Manski bounds
	
	*Plus was defined at the time of the endline survey (random half of respondents got the extra knowledge module). Thus, assign plus = 1 to random half of attriters. 
	set seed 12345
	gen rand = runiform()
	replace plus = 1 if rand >= 0.5 & attrition == 1
	replace plus = 0 if rand < 0.5 & attrition == 1

	*Replace missing values in DVs with highest possible (upper bound) and lowest possible value (lower bound)
	
	foreach x in heardofcand_e sectorcorrect_e like_e votedopp votednrm votedindep turnout {
		gen `x'_ub = `x'
		gen `x'_lb = `x'
		}
	
	foreach x in heardofcand_e sectorcorrect_e votedopp votednrm votedindep turnout {
		replace `x'_ub = 1 if `x' == .
		replace `x'_lb = 0 if `x' == .
		}
	
	replace like_e_ub = 10 if like_e == .
	replace like_e_lb =  0 if like_e == .
	
	*Generate additional standardized covariates
	
	*Knowledge and likability
	*plus == 1 & intendnrm == 1 & filmed == 1
	
		foreach x in age female educyr wealthindex nrmpref lastturnout powerful counting salience source news candinfo pastsupport cogender coethnic {
			cap drop `x'_t_st
			cap drop treat_`x'_t_st
			qui sum `x' if  plus == 1 & intendnrm == 1 
			gen `x'_t_st = (`x' - r(mean))/r(sd) if  plus == 1 & intendnrm == 1 
			gen treat_`x'_t_st = `x'_t_st * treat	
			}
	
		global controls_t "age_t_st female_t_st educyr_t_st wealthindex_t_st nrmpref_t_st lastturnout_t_st  powerful_t_st counting_t_st salience_t_st source_t_st news_t_st"
		global controls_t_treat "treat_age_t_st treat_female_t_st treat_educyr_t_st treat_wealthindex_t_st treat_nrmpref_t_st treat_lastturnout_t_st treat_powerful_t_st treat_counting_t_st treat_salience_t_st treat_source_t_st treat_news_t_st"
		

	*Vote choice and turnout
	*if uniqueid == 1 & intendnrm == 1
	
		foreach x in age female educyr wealthindex nrmpref lastturnout powerful counting salience source news candinfo pastsupport cogender coethnic {
			cap drop `x'_t2_st
			cap drop treat_`x'_t2_st
			qui sum `x' if uniqueid == 1 & intendnrm == 1
			gen `x'_t2_st = (`x' - r(mean))/r(sd) if uniqueid == 1 & intendnrm == 1
			gen treat_`x'_t2_st = `x'_t2_st * treat	
			}
	
		global controls_t2 "age_t2_st female_t2_st educyr_t2_st wealthindex_t2_st nrmpref_t2_st lastturnout_t2_st  powerful_t2_st counting_t2_st salience_t2_st source_t2_st news_t2_st"
		global controls_t2_treat "treat_age_t2_st treat_female_t2_st treat_educyr_t2_st treat_wealthindex_t2_st treat_nrmpref_t2_st treat_lastturnout_t2_st treat_powerful_t2_st treat_counting_t2_st treat_salience_t2_st treat_source_t2_st treat_news_t2_st"
		
	
	
	
* Table G10: Treatment effect on knowledge and likability – Inverse probability weights Heard of Sector correct

	cap drop treat_ind 
	gen treat_ind = treat * ind

	eststo clear
	foreach x in heardofcand_e sectorcorrect_e like_e {
			
			areg `x'  treat opp ind $controlsplusnrmd $controlsplusnrmd_treat if plus==1 & intendnrm == 1 & filmed == 1 [pw=ipwd], absorb(code) cl(codeps)
			eststo `x'_nrm1
			
			areg `x' treat treat_opp treat_ind opp ind  $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1 & plus==1 & filmed == 1 [pw=ipwd], absorb(code) cl(codeps)
			eststo `x'_prtynrm1
			
			}
		
			
	
		# delimit;
		esttab  heardofcand_e_nrm1 heardofcand_e_prtynrm1 sectorcorrect_e_nrm1 sectorcorrect_e_prtynrm1 like_e_nrm1 like_e_prtynrm1
		using know_like_ipw.tex,
			title(Treatment effects on knowledge and likability )
			keep(treat treat_opp treat_ind opp ind _cons)
			varlabels (
			treat "Treatment"
			treat_opp "Treatment x Opposition candidate"
			opp "Opposition candidate"
			treat_ind "Treatment x Independent candidate"
			ind "Independent candidate"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
			fmt(0 3)) label booktabs nonotes replace;
			
			

	
* Table G11: Treatment effect on knowledge and likability – Manski bounds

	
		eststo clear
		foreach x in heardofcand_e_ub heardofcand_e_lb sectorcorrect_e_ub sectorcorrect_e_lb like_e_ub like_e_lb {
			
			areg `x'  treat opp ind $controls_t $controls_t_treat if plus == 1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo `x'_nrm1
			
			}
			
		# delimit;
		esttab  heardofcand_e_ub_nrm1 heardofcand_e_lb_nrm1 sectorcorrect_e_ub_nrm1 sectorcorrect_e_lb_nrm1 like_e_ub_nrm1 like_e_lb_nrm1
		using know_like_manski.tex,
			title(Treatment effects on knowledge and likability -- Manski bounds)
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
			fmt(0 3)) label booktabs nonotes replace;
			
			

			
* Table G12: Treatment effect on knowledge and likability, by candidate party – Manski bound

		eststo clear
		foreach x in heardofcand_e_ub heardofcand_e_lb sectorcorrect_e_ub sectorcorrect_e_lb like_e_ub like_e_lb {
			
			areg `x' treat treat_opp treat_ind opp ind  $controls_t $controls_t_treat if intendnrm == 1 & plus==1, absorb(code) cl(codeps)
			eststo `x'_prtynrm1
			
			}
			
				
		# delimit;
		esttab  heardofcand_e_ub_prtynrm1 heardofcand_e_lb_prtynrm1 sectorcorrect_e_ub_prtynrm1 sectorcorrect_e_lb_prtynrm1 like_e_ub_prtynrm1 like_e_lb_prtynrm1
		using know_like_party_manski.tex,
			title(Treatment effects on knowledge and likability, by candidate party -- Manski bounds)
			keep(treat treat_opp treat_ind opp ind _cons)
			varlabels (
			treat "Treatment"
			treat_opp "Treatment x Opposition candidate"
			opp "Opposition candidate"
			treat_ind "Treatment x Independent candidate"
			ind "Independent candidate"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
			fmt(0 3)) label booktabs nonotes replace;
			
			


* Table G15: Treatment effects on vote choice and turnout – Inverse probability weights

		set more off
		foreach x in votedopp votednrm votedindep turnout {
		
		
			display "GE - NRM only"
			areg `x' treat $controlsintnrm $controlsintnrm_treat if uniqueid == 1 & intendnrm == 1 [pw=ipw], absorb(code) cl(codeps)
			eststo genrm_`x'1
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)
		}
		

		
		 	# delimit;
			esttab    genrm_votednrm1   genrm_votedopp1  genrm_votedindep1   genrm_turnout1
			using votechoice_turnout_ipw.tex,
			stats(N r2 , fmt(0 3) labels (`"Observations"' `"\(R^{2}\)"'))
			title(Treatment effects on vote choice)
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			label booktabs nonotes replace;
		 
		 
	

* Table G16: Treatment effects on vote choice and turnout – Manski bounds

		eststo clear
		foreach x in votedopp_ub votedopp_lb votednrm_ub votednrm_lb votedindep_ub votedindep_lb turnout_ub turnout_lb {
		
		
			display "GE - NRM only"
			areg `x' treat $controls_t2 $controls_t2_treat if uniqueid == 1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo genrm_`x'1
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat, level(90)
		}
		

		
		 	# delimit;
			esttab    genrm_votednrm_ub1 genrm_votednrm_lb1   genrm_votedopp_ub1 genrm_votedopp_lb1  genrm_votedindep_ub1 genrm_votedindep_lb1   genrm_turnout_ub1 genrm_turnout_lb1
			using votechoice_turnout_manski.tex,
			stats(N r2 , fmt(0 3) labels (`"Observations"' `"\(R^{2}\)"'))
			title(Treatment effects on vote choice and turnout -- Manski bounds)
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			label booktabs nonotes replace;


***********************
* Unfilmed candidates *
***********************

	use "mtc_analysis_closethegap.dta", clear

	*Keep only the relevant data
	drop if attrition == 1
	drop uniqueid
	egen uniqueid = tag(id)


	foreach x in ratequalified_norm  candunderstand_norm candexpress_norm {
		egen `x'_avg = mean(`x'), by(candidateid)
		}
	
* Table E4: Summary statistics for candidates

		gsort code -vote
		br code candidate candprty_c incumbent winner_ge voteshare tribe_c ratequalified_ep ratequalified_norm_avg candunderstand_ep candunderstand_norm_avg candexpress_ep candexpress_norm_avg if uniquecand == 1
		
		
	*Robustness check: ATE on knowledge, likability including unfilmed candidates here
	
	*Baseline values
	bys party: tab heardof 
	bys party: tab heardof if  incumbent==1
	bys party: tab heardof if incumbent==0
	bys party: tab heardof if intendnrm==1

	
* Table G3: Treatment effect on knowledge – Including unfilmed candidates
	
	eststo clear
		
		foreach x in heardofcand_e sectorcorrect_e {
			areg `x'  treat i.party $controlsplusd $controlsplusd_treat if plus==1, absorb(code) cl(codeps)
			eststo `x'1
			
			areg `x'  treat i.party $controlsplusnrmd $controlsplusnrmd_treat if plus==1 & intendnrm == 1, absorb(code) cl(codeps)
			eststo `x'_nrm1
			}
		

		# delimit;
		esttab  heardofcand_e1 heardofcand_e_nrm1 sectorcorrect_e1  sectorcorrect_e_nrm1 
		using know_allcand.tex,
			title(Treatment effects on knowledge, including unfilmed candidates )
			keep(treat _cons)
			varlabels (
			treat "Treatment"
			_cons "Constant")
			cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
			starlevels(* .10 ** .05 *** .01) 					
			mtitles( "\shortstack{Heard of\\All}"
					"\shortstack{Heard of\\Lean NRM}"
					"\shortstack{Sector correct\\All}"
					"\shortstack{Sector correct\\Lean NRM}")
			stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
			fmt(0 3)) label booktabs nonotes replace;
			
			
	
* Table G6: Treatment effect on knowledge, by candidate party – Including unfilmed candidates

		cap drop treat_ind
		gen treat_ind = treat * ind
		
		eststo clear
		foreach x in heardofcand_e sectorcorrect_e {
			areg `x' treat treat_opp treat_ind opp ind $controlsplusd $controlsplusd_treat if plus==1, absorb(code) cl(codeps)
			eststo `x'
			
			areg `x' treat treat_opp treat_ind opp ind  $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1 & plus==1, absorb(code) cl(codeps)
			eststo `x'_nrm
		
		}
		
	
	# delimit;
	esttab  heardofcand_e heardofcand_e_nrm sectorcorrect_e sectorcorrect_e_nrm
	using know_party_allcand.tex,
		title(Treatment effects on knowledge by candidate party)
		keep(treat treat_opp treat_ind opp ind _cons)
		varlabels (
		treat "Treatment"
		treat_opp "Treatment * Opposition"
		treat_ind "Treatment * Independent" 
		opp "Opposition"
		ind "Independent"
		_cons "Constant")
		cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
		starlevels(* .10 ** .05 *** .01) 					
		mtitles( "\shortstack{Heard of\\All}"
				"\shortstack{Heard of\\Lean NRM}"
				"\shortstack{Sector correct\\All}"
				"\shortstack{Sector correct\\Lean NRM}")
		stats(N r2, labels("N" "R$^2$" fmt(%5)) 	
		fmt(0 3)) label booktabs nonotes replace;
	
	
* Table G9: Treatment effect on candidate likability – Including unfilmed candidates

	foreach x in like_e {
		display "GE - NRM leaning (OPP)"
			areg `x' treat treat_opp opp $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1, absorb(code) cl(codeps)
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_opp
			estadd scalar est = r(estimate)
			test treat + treat_opp = 0
			estadd scalar p = r(p)
			eststo ge_`x'_nrmopp1
			
			display "GE - NRM leaning (NRM)"
			areg `x' treat treat_nrm nrm $controlsplusnrmd $controlsplusnrmd_treat if intendnrm == 1, absorb(code) cl(codeps) 
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_nrm
			estadd scalar est2 = r(estimate)
			test treat + treat_nrm = 0
			estadd scalar p2 = r(p)
			eststo ge_`x'_nrmnrm1
			
			display "GE - Opp leaning"
			areg `x' treat treat_opp opp  $controlsplusoppd $controlsplusoppd_treat if intendopp == 1, absorb(code) cl(codeps)
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_opp
			estadd scalar est = r(estimate)
			test treat + treat_opp = 0
			estadd scalar p = r(p)
			eststo ge_`x'_oppopp1

			
			display "GE - Opp leaning"
			areg `x' treat treat_nrm nrm  $controlsplusoppd $controlsplusoppd_treat if intendopp == 1, absorb(code) cl(codeps)
			estadd local control "Yes"
			estadd local fe "Yes"
			lincom treat + treat_nrm
			estadd scalar est2 = r(estimate)
			test treat + treat_nrm = 0
			estadd scalar p2 = r(p)
			eststo ge_`x'_oppnrm1

			}

		
		# delimit;
		esttab  ge_like_e_nrmopp1 ge_like_e_nrmnrm1 ge_like_e_oppopp1 ge_like_e_oppnrm1
		using like_other_allcand.tex,
				title(Likability (including unfilmed candidates))
				addnote("The unit of observation is the voter-candidate dyad. The dependent variable is a ten-point likability index (`On 1-10 scale, how much do you like the candidate?') All models include constituency fixed effects. Standard errors are clustered by polling station. *** p$<$0.01; ** p$<$0.05; * p$<$0.10")
				keep(treat treat_opp opp treat_nrm nrm _cons)
				varlabels (
				treat "Treatment"
				treat_opp "Treatment x Opposition candidate"
				opp "Opposition candidate"
				treat_nrm "Treatment x NRM candidate"
				nrm "NRM candidate"
				_cons "Constant")
				cells(b(fmt(%5.3f) star) se(fmt(%5.3f) par))
				starlevels(* .10 ** .05 *** .01) 	
				stats(N r2 est p est2 p2, labels("N" "R$^2$" "Coeff (Treat + Treat x Opp)" "p-value (Treat + Treat x Opp)" "Coeff (Treat + Treat x NRM)" "p-value (Treat + Treat x NRM)" fmt(%5)) 	
				fmt(0 3)) label booktabs nonotes replace;

	
	

	
