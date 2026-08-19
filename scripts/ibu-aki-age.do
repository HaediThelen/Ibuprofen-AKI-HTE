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
	use ../data/ibu-aki-age.dta	
					
************************************************************************	
** Step 2: calculate effect modification using age (Continuous) with splines used
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
	
