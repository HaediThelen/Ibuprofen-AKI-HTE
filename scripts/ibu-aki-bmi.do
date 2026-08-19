* ==============================================================================
* IBU Opioid AKI effect of IBU on AKI by BMI continuous ixn
* Summer Rotation 8/17/23 &	categorical class		
* ==============================================================================
//Base settings
	clear
	set more off
	version 14.2

********************************************************************************
** Step 1: Load in Data
********************************************************************************	
//open file ibu-aki-age.dta from data directory
	cd "/Users/haedi/Library/CloudStorage/Box-Box/Repos/Ibuprofen-AKI-HTE/data"
	use ibu-aki-bmi.dta
					
************************************************************************	
** Step 3: calculate effect modification using bmi (Continuous) with splines used
** in the BW steps (splines calculated in R) 
************************************************************************	
	
// no spline model
	poisson kEver i.pain##c.bmi [pweight = ATTwts], exposure(pTime1000) irr
	estat ic
	
// Test margins	(update spline values as appropriate)
	// original code: 
	margins pain, at(bmi=(15(5)50)) noatlegend predict(ir)

// Set up excel sheet
	putexcel set ../results/ibu-aki-bmi-ATT-spline.xlsx, replace
		putexcel A1 = "BMI" B1 = "Pain" C1 ="Margin" D1= "std.err." 				///
				E1 = "LB" F1 = "UB" G1 = "Ixn p value" 								/// 				
				A2 = "15" A3 = "15" A4 = "20" A5 = "20" A6 = "25" A7 = "25" 		///	
				A8 = "30" A9 = "30" A10 = "35" A11 = "35" A12 = "40" A13 = "40" 	///
				A14 = "45" A15 = "45" A16 = "50" A17 = "50" 						///
				B2 = "0" B3 = "1"  B4 = "0" B5 = "1"  B6 = "0" B7 = "1" 			///
				B8 = "0" B9 = "1"  B10 = "0" B11 = "1"  B12 = "0" B13 = "1"			///
				B14 = "0" B15 = "1"  B16 = "0" B17 = "1" 
		
// Fit interaction model with continuous bmi splines from R
poisson kEver i.pain c.bmi1 c.bmi2 c.bmi3  ///
    c.bmi1#i.pain c.bmi2#i.pain c.bmi3#i.pain  ///
    [pweight=ATTwts], exposure(pTime1000) irr	
	estat ic
	
	// Get interaction p value
	testparm c.bmi1#i.pain c.bmi2#i.pain c.bmi3#i.pain 

	local ixn_p = r(p)
	
	// Insert interaction p value
	putexcel G2 = `ixn_p'

	///bmi = 15
	margins 	pain, 			at(												///
									bmi1=(-0.03619266)							///	
									bmi2=(0.07797069)							///
									bmi3=(-0.041690539)) 						///
									predict(ir)								
	matrix result = r(table)
		putexcel C2 = matrix(result[1,1]) D2 = matrix(result[2,1]) E2 = matrix(result[5,1]) F2 = matrix(result[6,1]) ///
				 C3 = matrix(result[1,2]) D3 = matrix(result[2,2]) E3 = matrix(result[5,2]) F3 = matrix(result[6,2])
																	
	///bmi = 20
	margins 	pain, 			at(												///
									bmi1=(-0.18311739)							///	
									bmi2=(0.43493703 )							///
									bmi3=(-0.232558662)) 						///
									predict(ir)										
	matrix result = r(table)
		putexcel C4 = matrix(result[1,1]) D4 = matrix(result[2,1]) E4 = matrix(result[5,1]) F4 = matrix(result[6,1]) ///
				 C5 = matrix(result[1,2]) D5 = matrix(result[2,2]) E5 = matrix(result[5,2]) F5 = matrix(result[6,2])
	
	///bmi = 25
	margins 	pain, 			at(												///
									bmi1=(-0.17792710)							///	
									bmi2=(0.63790036)							///
									bmi3=(-0.341082146)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C6 = matrix(result[1,1]) D6 = matrix(result[2,1]) E6 = matrix(result[5,1]) F6 = matrix(result[6,1]) ///
				 C7 = matrix(result[1,2]) D7 = matrix(result[2,2]) E7 = matrix(result[5,2]) F7 = matrix(result[6,2])
	
	
	///bmi = 30
	margins 	pain, 			at(												///
									bmi1=(0.05013006)							///	
									bmi2=(0.60638057)							///
									bmi3=(-0.320090400)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C8 = matrix(result[1,1]) D8 = matrix(result[2,1]) E8 = matrix(result[5,1]) F8 = matrix(result[6,1]) ///
				 C9 = matrix(result[1,2]) D9 = matrix(result[2,2]) E9 = matrix(result[5,2]) F9 = matrix(result[6,2])
	
	///bmi = 35
	margins 	pain, 			at(												///
									bmi1=(0.28107517)							///	
									bmi2=(0.51036524)							///
									bmi3=(-0.235822737)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C10 = matrix(result[1,1]) D10 = matrix(result[2,1]) E10 = matrix(result[5,1]) F10 = matrix(result[6,1]) ///
				 C11 = matrix(result[1,2]) D11 = matrix(result[2,2]) E11 = matrix(result[5,2]) F11 = matrix(result[6,2])
	
	///bmi = 40 
	margins 	pain, 			at(												///
									bmi1=(0.40922559)							///	
									bmi2=(0.43971317)							///
									bmi3=(-0.128313749)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C12 = matrix(result[1,1]) D12 = matrix(result[2,1]) E12 = matrix(result[5,1]) F12 = matrix(result[6,1]) ///
				 C13 = matrix(result[1,2]) D13 = matrix(result[2,2]) E13 = matrix(result[5,2]) F13 = matrix(result[6,2])
	
	///bmi = 45
	margins 	pain, 			at(												///
									bmi1=(0.44835921)							///	
									bmi2=(0.39153146)							///
									bmi3=(-0.001186329)) 							///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C14 = matrix(result[1,1]) D14 = matrix(result[2,1]) E14 = matrix(result[5,1]) F14 = matrix(result[6,1]) ///
				 C15 = matrix(result[1,2]) D15 = matrix(result[2,2]) E15 = matrix(result[5,2]) F15 = matrix(result[6,2])
	
	//bmi = 50

	margins 	pain, 			at(												///						
									bmi1=(0.41337029)							///	
									bmi2=(0.36206039)							///
									bmi3=(0.142276976)) 							///
									predict(ir)
	matrix result = r(table)
		putexcel C16 = matrix(result[1,1]) D16 = matrix(result[2,1]) E16 = matrix(result[5,1]) F16 = matrix(result[6,1]) ///
				 C17 = matrix(result[1,2]) D17 = matrix(result[2,2]) E17 = matrix(result[5,2]) F17 = matrix(result[6,2])
	
	
	putexcel close
