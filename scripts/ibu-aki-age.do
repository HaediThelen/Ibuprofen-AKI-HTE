* ==============================================================================
* IBU Opioid AKI effect of IBU on AKI by Age Check continuous ixn							  	
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
	use ibu-aki-age.dta

************************************************************************	
** Step 2: calculate effect modification using age (Continuous)
** Target ATT
************************************************************************
// Check linearity of kEver with age
	//twoway lowess kEver age	
	histogram age, width (2)

// Fit interaction model with continuous age
	poisson kEver i.pain##c.age[pweight = ATTwts], exposure(pTime1000) irr

// Predict aki incidence rate at a range of age values in both pain groups
	margins pain, at(age=(20(10)90)) atmeans noatlegend predict(ir)

	
	
	// Figure 5A Display prediccted IRs 
		marginsplot, x(age) title(AKI Incidence Rate) ytitle(Predicted Incidence Rate) ///
				name(kever1, replace) legend(order(1 "Oxycodone" 2 "Ibuprofen") pos(6)) xtitle(Age)
   
   // Figure 5B Difference in predicted IRs
		margins, dydx(pain) at(age=(20(10)90)) atmeans noatlegend predict(ir)
			matrix result = r(table)
			matrix list result
		// Set up Excel
			putexcel set ../results/ibu-aki-age-ATT-IRD.xlsx, replace
			putexcel A1 = "age" B1 = "IRD" C1 = "std.err." D1 = "LB" E1 = "UB" ///
					A2 = "20" A3 = "30" A4 = "40" A5 = "50" A6 = "60" A7 = "70"         ///
					A8 = "80" A9 = "90" 
					
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
			
			//Plot
		marginsplot, x(age) recast(line) recastci(rarea) ciopt(color(green%50)) ///
				title(Difference in IRR) ytitle(Difference in IRR) ///
				yline(0) name(diff, replace) xtitle(Age) plot1opts(lcolor(green))

	// Figure 5C Ratio of predicted IRs
		
		// Initialize an empty matrix to store the results
		matrix results = J(30, 4, .)
		forval i = 1/8 {
			matrix results[`i', 1] = 20 + (`i' - 1) * 10
		}
		matrix colnames results = AGE EST LB UB
		matrix list results


		// Loop through different values of XX from 20 to 320 by 10
		forval xx = 1/8 {
			local value = 20 + (`xx' -1) *10
			// Perform the Poisson regression
				quietly poisson kEver i.pain##c.age [pweight = ATTwts] if pTime1000, exposure(pTime1000) irr

			// Calculate the linear combination and store it in the second 
				// column of the results matrix, 
				// store 95% CI in col 3 and 4
			lincom 1.pain + `value'*c.age#1.pain, eform
			matrix results[`xx', 2] = r(estimate)
			matrix results[`xx', 3] = r(lb)
			matrix results[`xx', 4] = r(ub)

			// Clear the stored estimates
			ereturn clear
		}

			matrix list results
			svmat results, names(col)
			
			// Set up Excel
			putexcel set ../results/ibu-aki-age-ATT-IRR.xlsx, replace
			putexcel A1 = "age" B1 = "IRR" C1 = "LB" D1 = "UB"    ///
					A2 = "20" A3 = "30" A4 = "40" A5 = "50" A6 = "60" A7 = "70"   ///
					A8 = "80" A9 = "90" 
					
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
			twoway (line EST AGE, lc(navy))  ///
				   (rarea UB LB AGE, lc(navy) fc(navy%50)), ///
					yline(1) ///
					title(Ratio of IRR) ytitle(Ratio of IRR) ///
					xtitle(Age) legend(off) xlabel(20[10]90) name(IRR, replace)
					
	
// no spline model
	poisson kEver i.pain##c.age [pweight = ATTwts], exposure(pTime1000) irr
	estat ic
	
	// get interaction p value
	test 1.pain#c.age
	local ixn_p = r(p)

// Set up excel sheet
	putexcel set ../results/ibu-aki-age-ATT-no-spline.xlsx, replace
		putexcel A1 = "Age" B1 = "Pain" C1 ="Margin" D1= "std.err." 				///
				E1 = "LB" F1 = "UB" G1 = "Ixn p value" 								/// 				
				A2 = "20" A3 = "20" A4 = "30" A5 = "30" A6 = "40" A7 = "40" 		///	
				A8 = "50" A9 = "50" A10 = "60" A11 = "60" A12 = "70" A13 = "70" 	///
				A14 = "80" A15 = "80" A16 = "90" A17 = "90" A18 = "100" A19 = "100" ///
				A20 = "110" A21 = "110" 					///
				B2 = "0" B3 = "1"  B4 = "0" B5 = "1"  B6 = "0" B7 = "1" 			///
				B8 = "0" B9 = "1"  B10 = "0" B11 = "1"  B12 = "0" B13 = "1"			///
				B14 = "0" B15 = "1"  B16 = "0" B17 = "1"  B18 = "0" B19 = "1" 	 	///
				B20 = "0" B21 = "1" 
							
	// insert interaction p value
		putexcel G2 = `ixn_p'
	
// Test margins	(update spline values as appropriate)
	// original code: 
	margins pain, at(age=(20(10)110)) noatlegend predict(ir)	

// Age = 20
margins pain, at(age=20) predict(ir)
matrix result = r(table)
    putexcel C2 = matrix(result[1,1]) D2 = matrix(result[2,1]) E2 = matrix(result[5,1]) F2 = matrix(result[6,1]) ///
             C3 = matrix(result[1,2]) D3 = matrix(result[2,2]) E3 = matrix(result[5,2]) F3 = matrix(result[6,2])

// Age = 30
margins pain, at(age=30) predict(ir)
matrix result = r(table)
    putexcel C4 = matrix(result[1,1]) D4 = matrix(result[2,1]) E4 = matrix(result[5,1]) F4 = matrix(result[6,1]) ///
             C5 = matrix(result[1,2]) D5 = matrix(result[2,2]) E5 = matrix(result[5,2]) F5 = matrix(result[6,2])

// Age = 40
margins pain, at(age=40) predict(ir)
matrix result = r(table)
    putexcel C6 = matrix(result[1,1]) D6 = matrix(result[2,1]) E6 = matrix(result[5,1]) F6 = matrix(result[6,1]) ///
             C7 = matrix(result[1,2]) D7 = matrix(result[2,2]) E7 = matrix(result[5,2]) F7 = matrix(result[6,2])

// Age = 50
margins pain, at(age=50) predict(ir)
matrix result = r(table)
    putexcel C8 = matrix(result[1,1]) D8 = matrix(result[2,1]) E8 = matrix(result[5,1]) F8 = matrix(result[6,1]) ///
             C9 = matrix(result[1,2]) D9 = matrix(result[2,2]) E9 = matrix(result[5,2]) F9 = matrix(result[6,2])

// Age = 60
margins pain, at(age=60) predict(ir)
matrix result = r(table)
    putexcel C10 = matrix(result[1,1]) D10 = matrix(result[2,1]) E10 = matrix(result[5,1]) F10 = matrix(result[6,1]) ///
             C11 = matrix(result[1,2]) D11 = matrix(result[2,2]) E11 = matrix(result[5,2]) F11 = matrix(result[6,2])

// Age = 70
margins pain, at(age=70) predict(ir)
matrix result = r(table)
    putexcel C12 = matrix(result[1,1]) D12 = matrix(result[2,1]) E12 = matrix(result[5,1]) F12 = matrix(result[6,1]) ///
             C13 = matrix(result[1,2]) D13 = matrix(result[2,2]) E13 = matrix(result[5,2]) F13 = matrix(result[6,2])

// Age = 80
margins pain, at(age=80) predict(ir)
matrix result = r(table)
    putexcel C14 = matrix(result[1,1]) D14 = matrix(result[2,1]) E14 = matrix(result[5,1]) F14 = matrix(result[6,1]) ///
             C15 = matrix(result[1,2]) D15 = matrix(result[2,2]) E15 = matrix(result[5,2]) F15 = matrix(result[6,2])

// Age = 90
margins pain, at(age=90) predict(ir)
matrix result = r(table)
    putexcel C16 = matrix(result[1,1]) D16 = matrix(result[2,1]) E16 = matrix(result[5,1]) F16 = matrix(result[6,1]) ///
             C17 = matrix(result[1,2]) D17 = matrix(result[2,2]) E17 = matrix(result[5,2]) F17 = matrix(result[6,2])

// Age = 100
margins pain, at(age=100) predict(ir)
matrix result = r(table)
    putexcel C18 = matrix(result[1,1]) D18 = matrix(result[2,1]) E18 = matrix(result[5,1]) F18 = matrix(result[6,1]) ///
             C19 = matrix(result[1,2]) D19 = matrix(result[2,2]) E19 = matrix(result[5,2]) F19 = matrix(result[6,2])

// Age = 110
margins pain, at(age=110) predict(ir)
matrix result = r(table)
    putexcel C20 = matrix(result[1,1]) D20 = matrix(result[2,1]) E20 = matrix(result[5,1]) F20 = matrix(result[6,1]) ///
             C21 = matrix(result[1,2]) D21 = matrix(result[2,2]) E21 = matrix(result[5,2]) F21 = matrix(result[6,2])

putexcel close	
					
					
************************************************************************	
** Step 3: calculate effect modification using age (Continuous) with splines used
** in the BW steps (splines calculated in R) 
** Target ATT
************************************************************************	
// no spline model
	poisson kEver i.pain##c.age [pweight = ATTwts], exposure(pTime1000) irr
	estat ic
	
// Test margins	(update spline values as appropriate)
	// original code: 
	margins pain, at(age=(20(10)110)) noatlegend predict(ir)

// Set up excel sheet
	putexcel set ../results/ibu-aki-age-ATT-spline.xlsx, replace
		putexcel A1 = "Age" B1 = "Pain" C1 ="Margin" D1= "std.err." 				///
				E1 = "LB" F1 = "UB" G1 = "Ixn p value" 								/// 				
				A2 = "20" A3 = "20" A4 = "30" A5 = "30" A6 = "40" A7 = "40" 		///	
				A8 = "50" A9 = "50" A10 = "60" A11 = "60" A12 = "70" A13 = "70" 	///
				A14 = "80" A15 = "80" A16 = "90" A17 = "90" A18 = "100" A19 = "100" ///
				A20 = "110" A21 = "110" 					///
				B2 = "0" B3 = "1"  B4 = "0" B5 = "1"  B6 = "0" B7 = "1" 			///
				B8 = "0" B9 = "1"  B10 = "0" B11 = "1"  B12 = "0" B13 = "1"			///
				B14 = "0" B15 = "1"  B16 = "0" B17 = "1"  B18 = "0" B19 = "1" 	 	///
				B20 = "0" B21 = "1" 
		
// Fit interaction model with continuous age splines from R
poisson kEver i.pain c.age1 c.age2 c.age3  ///
    c.age1#i.pain c.age2#i.pain c.age3#i.pain  ///
    [pweight=ATTwts], exposure(pTime1000) irr
	estat ic
	
	// Get interaction p value
	testparm c.age1#i.pain c.age2#i.pain c.age3#i.pain 

	local ixn_p = r(p)
	
	// Insert interaction p value
	putexcel G2 = `ixn_p'
	
	///Age = 20
	margins 	pain, 			at(												///
									age1=(-0.02425470)							///	
									age2=(0.0563582)							///
									age3=(-0.03204682)) 						///
									predict(ir)								
	matrix result = r(table)
		putexcel C2 = matrix(result[1,1]) D2 = matrix(result[2,1]) E2 = matrix(result[5,1]) F2 = matrix(result[6,1]) ///
				 C3 = matrix(result[1,2]) D3 = matrix(result[2,2]) E3 = matrix(result[5,2]) F3 = matrix(result[6,2])
																	
	///Age = 30
	margins 	pain, 			at(												///
									age1=(-0.12683436 )							///	
									age2=(0.3224068)							///
									age3=(-0.18332934)) 						///
									predict(ir)										
	matrix result = r(table)
		putexcel C4 = matrix(result[1,1]) D4 = matrix(result[2,1]) E4 = matrix(result[5,1]) F4 = matrix(result[6,1]) ///
				 C5 = matrix(result[1,2]) D5 = matrix(result[2,2]) E5 = matrix(result[5,2]) F5 = matrix(result[6,2])
	
	///Age = 40
	margins 	pain, 			at(												///
									age1=(-0.14929761)							///	
									age2=(0.5209880)							///
									age3=(-0.29624805)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C6 = matrix(result[1,1]) D6 = matrix(result[2,1]) E6 = matrix(result[5,1]) F6 = matrix(result[6,1]) ///
				 C7 = matrix(result[1,2]) D7 = matrix(result[2,2]) E7 = matrix(result[5,2]) F7 = matrix(result[6,2])
	
	
	///Age = 50
	margins 	pain, 			at(												///
									age1=(-0.02488079)							///	
									age2=(0.5958789)							///
									age3=(-0.33883309)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C8 = matrix(result[1,1]) D8 = matrix(result[2,1]) E8 = matrix(result[5,1]) F8 = matrix(result[6,1]) ///
				 C9 = matrix(result[1,2]) D9 = matrix(result[2,2]) E9 = matrix(result[5,2]) F9 = matrix(result[6,2])
	
	///Age = 60
	margins 	pain, 			at(												///
									age1=( 0.26494855)							///	
									age2=(0.5191939)							///
									age3=(-0.28642610)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C10 = matrix(result[1,1]) D10 = matrix(result[2,1]) E10 = matrix(result[5,1]) F10 = matrix(result[6,1]) ///
				 C11 = matrix(result[1,2]) D11 = matrix(result[2,2]) E11 = matrix(result[5,2]) F11 = matrix(result[6,2])
	
	///Age = 70 
	margins 	pain, 			at(												///
									age1=(0.48587566)							///	
									age2=(0.4123343)							///
									age3=(-0.15363987)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C12 = matrix(result[1,1]) D12 = matrix(result[2,1]) E12 = matrix(result[5,1]) F12 = matrix(result[6,1]) ///
				 C13 = matrix(result[1,2]) D13 = matrix(result[2,2]) E13 = matrix(result[5,2]) F13 = matrix(result[6,2])
	
	///Age = 80
	margins 	pain, 			at(												///
									age1=(0.50036175)							///	
									age2=( 0.3566874 )							///
									age3=(0.03793088)) 							///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C14 = matrix(result[1,1]) D14 = matrix(result[2,1]) E14 = matrix(result[5,1]) F14 = matrix(result[6,1]) ///
				 C15 = matrix(result[1,2]) D15 = matrix(result[2,2]) E15 = matrix(result[5,2]) F15 = matrix(result[6,2])
	
	//Age = 90

	margins 	pain, 			at(												///						
									age1=(0.35537323 )							///	
									age2=(0.3420830)							///
									age3=(0.27300864)) 							///
									predict(ir)
	matrix result = r(table)
		putexcel C16 = matrix(result[1,1]) D16 = matrix(result[2,1]) E16 = matrix(result[5,1]) F16 = matrix(result[6,1]) ///
				 C17 = matrix(result[1,2]) D17 = matrix(result[2,2]) E17 = matrix(result[5,2]) F17 = matrix(result[6,2])
	
	///Age = 100
	margins 	pain, 			at(												///
									age1=(0.10590135)							///	
									age2=(0.3543685)							///
									age3=(0.53659101)) 							///
									predict(ir)									
	
	matrix result = r(table)
		putexcel C18 = matrix(result[1,1]) D18 = matrix(result[2,1]) E18 = matrix(result[5,1]) F18 = matrix(result[6,1]) ///
				 C19 = matrix(result[1,2]) D19 = matrix(result[2,2]) E19 = matrix(result[5,2]) F19 = matrix(result[6,2])
	
	///Age = 110
	margins 	pain, 			at(												///
									age1=(-0.19307183)							///	
									age2=(0.3793938)							///
									age3=(0.81367804)) 							///
									predict(ir)		
	matrix result = r(table)
		putexcel C20 = matrix(result[1,1]) D20 = matrix(result[2,1]) E20 = matrix(result[5,1]) F20 = matrix(result[6,1]) ///
				 C21 = matrix(result[1,2]) D21 = matrix(result[2,2]) E21 = matrix(result[5,2]) F21 = matrix(result[6,2])
		
	putexcel close
	
