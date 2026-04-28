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
	cd "/Users/haedi/Library/CloudStorage/Box-Box/Data/NSAID-AKI/data"
	use ibu-aki-bmi.dta

************************************************************************	
** Step 2: calculate effect modification using bmi (Continuous)
************************************************************************	
// Check linearity of kEver with bmi
	//twoway lowess kEver bmi	
	histogram bmi

// Fit interaction model with continuous indexGFR
	poisson kEver i.pain##c.bmi [pweight = ATTwts], exposure(pTime1000) irr

// Predict aki incidence rate at a range of indexGFR values in both pain groups
	margins pain, at(bmi=(15(5)50)) atmeans noatlegend predict(ir)
	
	// Figure 5A Display prediccted IRs 
		marginsplot, x(bmi) title(AKI Incidence Rate) ytitle(Predicted Incidence Rate) ///
				name(kever1, replace) legend(order(1 "Oxycodone" 2 "Ibuprofen") pos(6)) xtitle(Baseline BMI)
   
   // Figure 5B Difference in predicted IRs
		margins, dydx(pain) at(bmi=(15(5)50)) atmeans noatlegend predict(ir)
			matrix result = r(table)
			matrix list result
		// Set up Excel
			putexcel set ../results/ibu-aki-bmi-ATT-IRD.xlsx, replace
			putexcel A1 = "bmi" B1 = "IRD" C1 = "std.err." D1 = "LB" E1 = "UB" ///
					A2 = "15" A3 = "20" A4 = "25" A5 = "30" A6 = "35" A7 = "40"         ///
					A8 = "45" A9 = "50" 
					
			// Write margins results row by row
			putexcel B2 = matrix(result[1,9])  C2 = matrix(result[2,9])  D2 = matrix(result[5,9])  E2 = matrix(result[6,9]) ///
					B3 = matrix(result[1,10])  C3 = matrix(result[2,10])  D3 = matrix(result[5,10])  E3 = matrix(result[6,10]) ///
					B4 = matrix(result[1,11])  C4 = matrix(result[2,11])  D4 = matrix(result[5,11])  E4 = matrix(result[6,11]) ///
					B5 = matrix(result[1,12])  C5 = matrix(result[2,12])  D5 = matrix(result[5,12])  E5 = matrix(result[6,12]) ///
					B6 = matrix(result[1,13])  C6 = matrix(result[2,13])  D6 = matrix(result[5,13])  E6 = matrix(result[6,13]) ///
					B7 = matrix(result[1,14])  C7 = matrix(result[2,14])  D7 = matrix(result[5,14])  E7 = matrix(result[6,14]) ///
					B8 = matrix(result[1,15])  C8 = matrix(result[2,15])  D8 = matrix(result[5,15])  E8 = matrix(result[6,15]) ///
					B9 = matrix(result[1,16])  C9 = matrix(result[2,16])  D9 = matrix(result[5,16])  E9 = matrix(result[6,16])

			putexcel close
	
		marginsplot, x(bmi) recast(line) recastci(rarea) ciopt(color(green%50)) ///
				title(Difference in IRR) ytitle(Difference in IRR) ///
				yline(0) name(diff, replace) xtitle(Baseline BMI) plot1opts(lcolor(green))

	// Figure 5C Ratio of predicted IRs
		
		// Initialize an empty matrix to store the results
		matrix results = J(30, 4, .)
		forval i = 1/8 {
			matrix results[`i', 1] = 15 + (`i' - 1) * 5
		}
		matrix colnames results = BMI EST LB UB
		matrix list results


		// Loop through different values of XX from 20 to 320 by 10
		forval xx = 1/8 {
			local value = 15 + (`xx' -1) *5
			// Perform the Poisson regression
				quietly poisson kEver i.pain##c.bmi [pweight = ATTwts] if pTime1000, exposure(pTime1000) irr

			// Calculate the linear combination and store it in the second 
				// column of the results matrix, 
				// store 95% CI in col 3 and 4
			lincom 1.pain + `value'*c.bmi#1.pain, eform
			matrix results[`xx', 2] = r(estimate)
			matrix results[`xx', 3] = r(lb)
			matrix results[`xx', 4] = r(ub)

			// Clear the stored estimates
			ereturn clear
		}

			matrix list results
			svmat results, names(col)
			
			// Set up Excel
			putexcel set ../results/ibu-aki-bmi-ATT-IRR.xlsx, replace
			putexcel A1 = "bmi" B1 = "IRR" C1 = "LB" D1 = "UB" ///
					A2 = "15" A3 = "20" A4 = "25" A5 = "30" A6 = "35" A7 = "40"         ///
					A8 = "45" A9 = "50" 
					
			// Write margins results row by row
			putexcel B2 = matrix(results[1,2])  C2 = matrix(results[1,3])  D2 = matrix(results[1,4])  ///
					B3 = matrix(results[2,2])  C3 = matrix(results[2,3])  D3 = matrix(results[2,4])   ///
					B4 = matrix(results[3,2])  C4 = matrix(results[3,3])  D4 = matrix(results[3,4])   ///
					B5 = matrix(results[4,2])  C5 = matrix(results[4,3])  D5 = matrix(results[4,4])   ///
					B6 = matrix(results[5,2])  C6 = matrix(results[5,3])  D6 = matrix(results[5,4])   ///
					B7 = matrix(results[6,2])  C7 = matrix(results[6,3])  D7 = matrix(results[6,4])   ///
					B8 = matrix(results[7,2])  C8 = matrix(results[7,3])  D8 = matrix(results[7,4])   ///
					B9 = matrix(results[8,2])  C9 = matrix(results[8,3])  D9 = matrix(results[8,4])   ///
					B10 = matrix(results[9,2]) C10 = matrix(results[9,3]) D10 = matrix(results[9,4])  ///

			putexcel close
			
			
		// Plot Figure 5C
			twoway (line EST BMI, lc(navy))  ///
				   (rarea UB LB BMI, lc(navy) fc(navy%50)), ///
					yline(1) ///
					title(Ratio of IRR) ytitle(Ratio of IRR) ///
					xtitle(Baseline BMI) legend(off) xlabel(15[5]50) name(IRR, replace)
					
	// No spline model
	poisson kEver i.pain##c.bmi [pweight = ATTwts], exposure(pTime1000) irr
	estat ic
	
	// get interaction p value
	test 1.pain#c.bmi
	local ixn_p = r(p)
	

	// Set up Excel sheet
	putexcel set ../results/ibu-aki-bmi-ATT-no-spline.xlsx, replace
		putexcel A1 = "BMI" B1 = "Pain" C1 ="Margin" D1= "std.err."                ///
				E1 = "LB" F1 = "UB" G1 = "Ixn p value"                                               ///
				A2 = "15" A3 = "15" A4 = "20" A5 = "20" A6 = "25" A7 = "25"       ///
				A8 = "30" A9 = "30" A10 = "35" A11 = "35" A12 = "40" A13 = "40"   ///
				A14 = "45" A15 = "45" A16 = "50" A17 = "50"                       ///
				B2 = "0" B3 = "1"  B4 = "0" B5 = "1"  B6 = "0" B7 = "1"           ///
				B8 = "0" B9 = "1"  B10 = "0" B11 = "1"  B12 = "0" B13 = "1"       ///
				B14 = "0" B15 = "1"  B16 = "0" B17 = "1" 
				
	// insert interaction p value
		putexcel G2 = `ixn_p'

	// Test margins
	margins pain, at(bmi=(15(5)50)) noatlegend predict(ir)
				
	// Grab margins output and write to Excel
	matrix result = r(table)
		putexcel C2 = matrix(result[1,1]) D2 = matrix(result[2,1]) E2 = matrix(result[5,1]) F2 = matrix(result[6,1]) ///
				 C3 = matrix(result[1,2]) D3 = matrix(result[2,2]) E3 = matrix(result[5,2]) F3 = matrix(result[6,2]) ///
				 C4 = matrix(result[1,3]) D4 = matrix(result[2,3]) E4 = matrix(result[5,3]) F4 = matrix(result[6,3]) ///
				 C5 = matrix(result[1,4]) D5 = matrix(result[2,4]) E5 = matrix(result[5,4]) F5 = matrix(result[6,4]) ///
				 C6 = matrix(result[1,5]) D6 = matrix(result[2,5]) E6 = matrix(result[5,5]) F6 = matrix(result[6,5]) ///
				 C7 = matrix(result[1,6]) D7 = matrix(result[2,6]) E7 = matrix(result[5,6]) F7 = matrix(result[6,6]) ///
				 C8 = matrix(result[1,7]) D8 = matrix(result[2,7]) E8 = matrix(result[5,7]) F8 = matrix(result[6,7]) ///
				 C9 = matrix(result[1,8]) D9 = matrix(result[2,8]) E9 = matrix(result[5,8]) F9 = matrix(result[6,8]) ///
				 C10 = matrix(result[1,9]) D10 = matrix(result[2,9]) E10 = matrix(result[5,9]) F10 = matrix(result[6,9]) ///
				 C11 = matrix(result[1,10]) D11 = matrix(result[2,10]) E11 = matrix(result[5,10]) F11 = matrix(result[6,10]) ///
				 C12 = matrix(result[1,11]) D12 = matrix(result[2,11]) E12 = matrix(result[5,11]) F12 = matrix(result[6,11]) ///
				 C13 = matrix(result[1,12]) D13 = matrix(result[2,12]) E13 = matrix(result[5,12]) F13 = matrix(result[6,12]) ///
				 C14 = matrix(result[1,13]) D14 = matrix(result[2,13]) E14 = matrix(result[5,13]) F14 = matrix(result[6,13]) ///
				 C15 = matrix(result[1,14]) D15 = matrix(result[2,14]) E15 = matrix(result[5,14]) F15 = matrix(result[6,14]) ///
				 C16 = matrix(result[1,15]) D16 = matrix(result[2,15]) E16 = matrix(result[5,15]) F16 = matrix(result[6,15]) ///
				 C17 = matrix(result[1,16]) D17 = matrix(result[2,16]) E17 = matrix(result[5,16]) F17 = matrix(result[6,16]) 

	putexcel close			
					
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
