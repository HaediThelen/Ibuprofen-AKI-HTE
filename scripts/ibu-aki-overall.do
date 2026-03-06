* ==============================================================================
*  IBU-AKI Overall effect of IBU on AKI 
* ==============================================================================
//Base settings
	clear
	set more off
	version 14.2

********************************************************************************
** Step 1: Load file with balance weights
********************************************************************************
//open file ibu-aki-overall.dta from data directory
	cd "/Users/haedi/Library/CloudStorage/Box-Box/Data/NSAID-AKI/data"
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
		putexcel A2 = "ATT" 
		
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
			