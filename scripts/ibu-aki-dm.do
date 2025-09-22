* ==============================================================================
* IBU Opioid AKI effect of IBU on AKI by DM (Binary)
* ==============================================================================
//Base settings
	clear
	set more off
	version 14.2

********************************************************************************
** Step 1: Load in Data
** dm_bin is the dm  binary variable to use
********************************************************************************	
//open file
	cd "/Users/haedi/Library/CloudStorage/Box-Box/Data/NSAID-AKI/data"
	use ibu-aki-dm.dta
************************************************************************	
** ATT
************************************************************************		

// Store the results in a new .dta file

//Unadjusted
	poisson kEver pain, exposure(pTime1000)
	poisson kEver pain, exposure(pTime1000) irr
	
// Adjusted
	poisson kEver i.pain [pweight = ATTwts], exposure(pTime1000) irr
	
	// Aditive scale
	margins pain, predict(ir)
	margins pain, predict(ir) contrast(effects)
		
// Set up excel sheet
	putexcel set ../results/ibu-aki-dm-bin-ATT.xlsx, replace
		putexcel A1 = "Effect Modifier" B1 = "Oxycodone" C1 ="LB" D1= "UB" ///
				E1 = "Ibuprofen" F1= "LB" G1 = "UB" 						///
				H1= "Risk Difference" I1="LB" J1= "UB"						///	
				K1= "Difference in Differences" L1 = "LB" M1 = "UB"			///
				N1 = "IRR" O1 = "LB" P1="UB"								///
				Q1 = "Ratio of IRR" R1 = "LB" S1= "UB"					
		putexcel A2 = "No DM" A3 = "DM"
	
//Fit interaction model
	poisson kEver i.dm_bin##i.pain [pweight = ATTwts], exposure(pTime1000) irr
		matrix result = r(table)
			putexcel N2 = matrix(result[1,4]) O2 = matrix(result[5,4]) P2 = matrix(result[6,4])
			putexcel Q3 = matrix(result[1,8]) R3 = matrix(result[5,8]) S3 = matrix(result[6,8])

// Multiplicative Interaction
	// Estimate the IRR for each level of DM
		lincom 1.pain + 2.dm_bin#1.pain, eform
			matrix result = r(estimate) , r(lb) , r(ub)
				putexcel N3 = matrix(result[1,1]) O3 = matrix(result[1,2]) P3 = matrix(result[1,3])

// Additive Interaction
	// Effects of pain at each bmi level
		margins r.pain@dm_bin, predict(ir)
			matrix result = r(table)
				putexcel H2 = matrix(result[1,1]) I2 = matrix(result[5,1]) J2 = matrix(result[6,1])
				putexcel H3 = matrix(result[1,2]) I3 = matrix(result[5,2]) J3 = matrix(result[6,2])

	// IR in each of the 2 groups (for 2x2 table)
		margins pain#dm_bin, predict(ir)
			matrix result = r(table)
				putexcel B2 = matrix(result[1,1]) C2 = matrix(result[5,1]) D2 = matrix(result[6,1])
				putexcel B3 = matrix(result[1,2]) C3 = matrix(result[5,2]) D3 = matrix(result[6,2])
				putexcel E2 = matrix(result[1,3]) F2 = matrix(result[5,3]) G2 = matrix(result[6,3])
				putexcel E3 = matrix(result[1,4]) F3 = matrix(result[5,4]) G3 = matrix(result[6,4])
				
	// Difference in difference estimate
		margins pain#i.dm_bin, predict(ir) contrast(effects)
			matrix result = r(table)
				putexcel K3 = matrix(result[1,1]) L3 = matrix(result[5,1]) M3 = matrix(result[6,1])
	
	putexcel close
