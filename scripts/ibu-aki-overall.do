* ==============================================================================
*  IBU-AKI Overall effect of IBU on AKI 
* ==============================================================================
//Base settings
	clear
	set more off
	version 14.2

********************************************************************************
**  Load file with balance weights
********************************************************************************
//open file ibu-aki-overall.dta from data directory
	cd "/Users/haedi/Library/CloudStorage/Box-Box/Repos/Ibuprofen-AKI-HTE/data"
	use ibu-aki-overall.dta

************************************************************************	
** ATT
************************************************************************	

//Unadjusted
	poisson kEver pain, exposure(pTime1000)
	poisson kEver pain, exposure(pTime1000) irr

// Set up excel
	putexcel set ../results/ibu-aki-overall.xlsx, replace
		putexcel A1 = "Effect Modifier" B1 = "Oxycodone" C1 ="LB" D1= "UB" ///
				E1 = "Ibuprofen" F1= "LB" G1 = "UB" 						///
				H1= "Rate Difference" I1="LB" J1= "UB"						///	
				K1= "IRR" L1 = "LB" M1 = "UB"				
		putexcel A2 = "Overall" 
		
// Adjusted
	poisson kEver i.pain [pweight = ATTwts], exposure(pTime1000) irr
		matrix result = r(table)
				putexcel K2 = matrix(result[1,2]) L2 = matrix(result[5,2]) M2 = matrix(result[6,2])
				
	// Aditive scale
	margins pain, predict(ir)
		matrix result = r(table)
				putexcel B2 = matrix(result[1,1]) C2 = matrix(result[5,1]) D2 = matrix(result[6,1])
				putexcel E2 = matrix(result[1,2]) F2 = matrix(result[5,2]) G2 = matrix(result[6,2])

	margins pain, predict(ir) contrast(effects)
		matrix result = r(table)
			putexcel H2 = matrix(result[1,1]) I2 = matrix(result[5,1]) J2 = matrix(result[6,1])
			
	putexcel close
************************************************************************	
** Secondary Outcome: Stage 2 or 3 AKI
************************************************************************	

// Generate new variable for AKI stage 2 or 3
	generate kStage23 = (kStage == 2 | kStage == 3)
	tab kStage kStage23, missing
	

// Set up excel
	putexcel set ../results/ibu-aki-kStage23.xlsx, replace
		putexcel A1 = "Effect Modifier" B1 = "Oxycodone" C1 ="LB" D1= "UB" ///
				E1 = "Ibuprofen" F1= "LB" G1 = "UB" 						///
				H1= "Rate Difference" I1="LB" J1= "UB"						///	
				K1= "IRR" L1 = "LB" M1 = "UB"				
		putexcel A2 = "ATT" 

// Adjusted
	poisson kStage23 i.pain [pweight = ATTwts], exposure(pTime1000) irr
		matrix result = r(table)
				putexcel K2 = matrix(result[1,2]) L2 = matrix(result[5,2]) M2 = matrix(result[6,2])
				
	// Aditive scale
	margins pain, predict(ir)
		matrix result = r(table)
				putexcel B2 = matrix(result[1,1]) C2 = matrix(result[5,1]) D2 = matrix(result[6,1])
				putexcel E2 = matrix(result[1,2]) F2 = matrix(result[5,2]) G2 = matrix(result[6,2])

	margins pain, predict(ir) contrast(effects)
		matrix result = r(table)
			putexcel H2 = matrix(result[1,1]) I2 = matrix(result[5,1]) J2 = matrix(result[6,1])
	
	putexcel close
	
************************************************************************	
** Secondary Outcome: RRT
************************************************************************	

// Set up excel
	putexcel set ../results/ibu-aki-rrt.xlsx, replace
		putexcel A1 = "Effect Modifier" B1 = "Oxycodone" C1 ="LB" D1= "UB" ///
				E1 = "Ibuprofen" F1= "LB" G1 = "UB" 						///
				H1= "Rate Difference" I1="LB" J1= "UB"						///	
				K1= "IRR" L1 = "LB" M1 = "UB"				
		putexcel A2 = "ATT" 

// Adjusted
	poisson rrt i.pain [pweight = ATTwts], exposure(pTime1000) irr
		matrix result = r(table)
				putexcel K2 = matrix(result[1,2]) L2 = matrix(result[5,2]) M2 = matrix(result[6,2])
				
	// Aditive scale
	margins pain, predict(ir)
		matrix result = r(table)
				putexcel B2 = matrix(result[1,1]) C2 = matrix(result[5,1]) D2 = matrix(result[6,1])
				putexcel E2 = matrix(result[1,2]) F2 = matrix(result[5,2]) G2 = matrix(result[6,2])

	margins pain, predict(ir) contrast(effects)
		matrix result = r(table)
			putexcel H2 = matrix(result[1,1]) I2 = matrix(result[5,1]) J2 = matrix(result[6,1])

			putexcel close			
			
************************************************************************		
*** Supplementary Analyses	
************************************************************************

************************************************************************	
** Outcome over 5 days of followup after expousre ends
************************************************************************	

** ATT
//Unadjusted
	poisson kEver5 pain, exposure(p5Time1000)
	poisson kEver5 pain, exposure(p5Time1000) irr

// Set up excel
	putexcel set ../results/ibu-aki-overall5.xlsx, replace
		putexcel A1 = "Effect Modifier" B1 = "Oxycodone" C1 ="LB" D1= "UB" ///
				E1 = "Ibuprofen" F1= "LB" G1 = "UB" 						///
				H1= "Rate Difference" I1="LB" J1= "UB"						///	
				K1= "IRR" L1 = "LB" M1 = "UB"				
		putexcel A2 = "ATT" 
		
// Adjusted
	poisson kEver5 i.pain [pweight = ATTwts], exposure(p5Time1000) irr
		matrix result = r(table)
				putexcel K2 = matrix(result[1,2]) L2 = matrix(result[5,2]) M2 = matrix(result[6,2])
				
	// Aditive scale
	margins pain, predict(ir)
		matrix result = r(table)
				putexcel B2 = matrix(result[1,1]) C2 = matrix(result[5,1]) D2 = matrix(result[6,1])
				putexcel E2 = matrix(result[1,2]) F2 = matrix(result[5,2]) G2 = matrix(result[6,2])

	margins pain, predict(ir) contrast(effects)
		matrix result = r(table)
			putexcel H2 = matrix(result[1,1]) I2 = matrix(result[5,1]) J2 = matrix(result[6,1])
	
	putexcel close
************************************************************************	
** Secondary Outcome: Stage 2 or 3 AKI with 5 dyas followup after expousre ends
************************************************************************	

// Generate new variable for AKI stage 2 or 3
	generate kStage523 = (kStage5 == 2 | kStage5 == 3)
	tab kStage5 kStage523, missing
	
// Unadjusted 
	poisson kStage523 i.pain, exposure(p5Time1000) irr


// Set up excel
	putexcel set ../results/ibu-aki-kStage523.xlsx, replace
		putexcel A1 = "Effect Modifier" B1 = "Oxycodone" C1 ="LB" D1= "UB" ///
				E1 = "Ibuprofen" F1= "LB" G1 = "UB" 						///
				H1= "Rate Difference" I1="LB" J1= "UB"						///	
				K1= "IRR" L1 = "LB" M1 = "UB"				
		putexcel A2 = "ATT" 

// Adjusted
	poisson kStage523 i.pain [pweight = ATTwts], exposure(p5Time1000) irr
		matrix result = r(table)
				putexcel K2 = matrix(result[1,2]) L2 = matrix(result[5,2]) M2 = matrix(result[6,2])
				
	// Aditive scale
	margins pain, predict(ir)
		matrix result = r(table)
				putexcel B2 = matrix(result[1,1]) C2 = matrix(result[5,1]) D2 = matrix(result[6,1])
				putexcel E2 = matrix(result[1,2]) F2 = matrix(result[5,2]) G2 = matrix(result[6,2])

	margins pain, predict(ir) contrast(effects)
		matrix result = r(table)
			putexcel H2 = matrix(result[1,1]) I2 = matrix(result[5,1]) J2 = matrix(result[6,1])
	
	putexcel close
************************************************************************	
** Secondary Outcome: RRT with 5 days followup after expousre ends
************************************************************************	
// Unadjusted
	poisson rrt5 i.pain, exposure(p5Time1000) irr


// Set up excel
	putexcel set ../results/ibu-aki-rrt5.xlsx, replace
		putexcel A1 = "Effect Modifier" B1 = "Oxycodone" C1 ="LB" D1= "UB" ///
				E1 = "Ibuprofen" F1= "LB" G1 = "UB" 						///
				H1= "Rate Difference" I1="LB" J1= "UB"						///	
				K1= "IRR" L1 = "LB" M1 = "UB"				
		putexcel A2 = "ATT" 

// Adjusted
	poisson rrt5 i.pain [pweight = ATTwts], exposure(p5Time1000) irr
		matrix result = r(table)
				putexcel K2 = matrix(result[1,2]) L2 = matrix(result[5,2]) M2 = matrix(result[6,2])
				
	// Aditive scale
	margins pain, predict(ir)
		matrix result = r(table)
				putexcel B2 = matrix(result[1,1]) C2 = matrix(result[5,1]) D2 = matrix(result[6,1])
				putexcel E2 = matrix(result[1,2]) F2 = matrix(result[5,2]) G2 = matrix(result[6,2])

	margins pain, predict(ir) contrast(effects)
		matrix result = r(table)
			putexcel H2 = matrix(result[1,1]) I2 = matrix(result[5,1]) J2 = matrix(result[6,1])

	putexcel close		

************************************************************************
** 	Opioid Exposure in the Baseline Period
************************************************************************
clear
use ibu-aki-overall-opBase.dta 

// Set up excel
	putexcel set ../results/ibu-aki-overall-opBase.xlsx, replace
		putexcel A1 = "overall" B1 = "Oxycodone" C1 ="LB" D1= "UB" ///
				E1 = "Ibuprofen" F1= "LB" G1 = "UB" 						///
				H1= "Rate Difference" I1="LB" J1= "UB"						///	
				K1= "IRR" L1 = "LB" M1 = "UB"				
		putexcel A2 = "Overall" 

// Adjusted
	poisson kEver i.pain [pweight = ATTwts_omeCat], exposure(pTime1000) irr
		matrix result = r(table)
				putexcel K2 = matrix(result[1,2]) L2 = matrix(result[5,2]) M2 = matrix(result[6,2])
				
	// Aditive scale
	margins pain, predict(ir)
		matrix result = r(table)
				putexcel B2 = matrix(result[1,1]) C2 = matrix(result[5,1]) D2 = matrix(result[6,1])
				putexcel E2 = matrix(result[1,2]) F2 = matrix(result[5,2]) G2 = matrix(result[6,2])

	margins pain, predict(ir) contrast(effects)
		matrix result = r(table)
			putexcel H2 = matrix(result[1,1]) I2 = matrix(result[5,1]) J2 = matrix(result[6,1])
			
	putexcel close
	
************************************************************************	
**  exlcusion of patietns who received non-oral opioids (IV/PCA/GTT) in baseline period
************************************************************************
clear
use ibu-aki-overall-opBasePOonly.dta

// Set up excel
	putexcel set ../results/ibu-aki-overall-opBasePOonly.xlsx, replace
		putexcel A1 = "Effect Modifier" B1 = "Oxycodone" C1 ="LB" D1= "UB" ///
				E1 = "Ibuprofen" F1= "LB" G1 = "UB" 						///
				H1= "Rate Difference" I1="LB" J1= "UB"						///	
				K1= "IRR" L1 = "LB" M1 = "UB"				
		putexcel A2 = "Overall" 

// Adjusted
	poisson kEver i.pain [pweight = ATTwts_opBasePOonly], exposure(pTime1000) irr
		matrix result = r(table)
				putexcel K2 = matrix(result[1,2]) L2 = matrix(result[5,2]) M2 = matrix(result[6,2])
				
	// Aditive scale
	margins pain, predict(ir)
		matrix result = r(table)
				putexcel B2 = matrix(result[1,1]) C2 = matrix(result[5,1]) D2 = matrix(result[6,1])
				putexcel E2 = matrix(result[1,2]) F2 = matrix(result[5,2]) G2 = matrix(result[6,2])

	margins pain, predict(ir) contrast(effects)
		matrix result = r(table)
			putexcel H2 = matrix(result[1,1]) I2 = matrix(result[5,1]) J2 = matrix(result[6,1])
			
	putexcel close
