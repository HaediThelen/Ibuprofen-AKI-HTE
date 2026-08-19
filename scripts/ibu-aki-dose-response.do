* ==============================================================================
*  IBU-AKI Dose Response Analysis
* ==============================================================================
//Base settings
	clear
	set more off
	version 14.2

********************************************************************************
** Step 1: Load file with balance weights
********************************************************************************
//open file from data directory
	use ../data/ibu-aki-dose-response.dta
	
************************************************************************	
** ADose Response - ATE
************************************************************************	

//Unadjusted
	poisson kEver i.doseCat, exposure(pTime1000)
	poisson kEver i.doseCat, exposure(pTime1000) irr

// Set up excel
	putexcel set ../results/ibu-aki-overall-dose-response.xlsx, replace
		putexcel A1 = "Dose-Response" 		B1 = "IR" C1 ="LB" D1= "UB" E1 = "IRR vs Oxycodone" F1 = "IRR LB" G1 = "IRR UB" ///
				 A2 = "Oxycodone"	///
				 A3 = "Low-Dose Ibuprofen" ///
				 A4 = "Medium-Dose Ibuprofen" ///
				 A5 = "High-Dose Ibuprofen" ///
		
// Adjusted
	poisson kEver i.doseCat [pweight = ATE_doseCat_wts], exposure(pTime1000) irr
		matrix result = r(table)
				putexcel E3 = matrix(result[1,2]) F3 = matrix(result[5,2]) G3 = matrix(result[6,2]) 
				putexcel E4 = matrix(result[1,3]) F4 = matrix(result[5,3]) G4 = matrix(result[6,3]) 
				putexcel E5 = matrix(result[1,4]) F5 = matrix(result[5,4]) G5 = matrix(result[6,4]) 
				
	// Aditive scale
	margins doseCat , predict(ir)
		matrix result = r(table)
				putexcel B2 = matrix(result[1,1]) C2 = matrix(result[5,1]) D2 = matrix(result[6,1])
				putexcel B3 = matrix(result[1,2]) C3 = matrix(result[5,2]) D3 = matrix(result[6,2])
				putexcel B4 = matrix(result[1,3]) C4 = matrix(result[5,3]) D4 = matrix(result[6,3])
				putexcel B5 = matrix(result[1,4]) C5 = matrix(result[5,4]) D5 = matrix(result[6,4])


	margins doseCat, predict(ir) contrast(effects)
		matrix result = r(table)

			
	putexcel close
