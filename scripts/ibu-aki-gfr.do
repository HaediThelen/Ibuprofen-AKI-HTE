* ==============================================================================
* IBU Opioid AKI effect of IBU on AKI by EGFR Check continuous ixn							  	
* ==============================================================================

//Base settings
	clear
	set more off
	version 14.2

********************************************************************************
** Step 1: Load in Data
********************************************************************************	
//open file ibu-aki-indexGFR.dta from data directory
	cd "/Users/haedi/Library/CloudStorage/Box-Box/Repos/Ibuprofen-AKI-HTE/data"
	use ibu-aki-indexGFR.dta

************************************************************************	
** Step 3: calculate effect modification using eGFR (Continuous) with splines
** in the BW steps (splines calculated in R) 
************************************************************************	
	
// no spline model
	poisson kEver i.pain##c.indexGFR [pweight = ATTwts], exposure(pTime1000) irr
	estat ic
	
// Test margins	(update spline values as appropriate)
	// original code: 
	margins pain, at(indexGFR=(20(10)150)) noatlegend predict(ir) 

// Set up excel sheet
	putexcel set ../results/ibu-aki-GFR-ATT-spline.xlsx, replace
		putexcel A1 = "eGFR" B1 = "Pain" C1 ="Margin" D1= "std.err." 				///
				E1 = "LB" F1 = "UB" G1 = "Ixn p value" 								/// 				
				A2 = "20" A3 = "20" A4 = "30" A5 = "30" A6 = "40" A7 = "40" 		///	
				A8 = "50" A9 = "50" A10 = "60" A11 = "60" A12 = "70" A13 = "70" 	///
				A14 = "80" A15 = "80" A16 = "90" A17 = "90" A18 = "100" A19 = "100" ///
				A20 = "110" A21 = "110" A22 = "120" A23 = "120" A24 = "130" A25 = "130" ///
				A26 = "140" A27 = "140" A28 = "150" A29 = "150" 					///
				B2 = "0" B3 = "1"  B4 = "0" B5 = "1"  B6 = "0" B7 = "1" 			///
				B8 = "0" B9 = "1"  B10 = "0" B11 = "1"  B12 = "0" B13 = "1"			///
				B14 = "0" B15 = "1"  B16 = "0" B17 = "1"  B18 = "0" B19 = "1" 	 	///
				B20 = "0" B21 = "1" B22 = "0" B23 = "1" B24 = "0" B25 = "1"  		///
				B26 = "0" B27 = "1" B28 = "0" B29 = "1"    
					
// Fit interaction model with continuous indexGFR splines from R
poisson kEver i.pain c.indexGFR1 c.indexGFR2 c.indexGFR3 c.indexGFR4 ///
    c.indexGFR1#i.pain c.indexGFR2#i.pain c.indexGFR3#i.pain c.indexGFR4#i.pain ///
    [pweight=ATTwts], exposure(pTime1000) irr	
	estat ic
	
	// Get interaction p value
	testparm c.indexGFR1#i.pain c.indexGFR2#i.pain c.indexGFR3#i.pain c.indexGFR4#i.pain

	local ixn_p = r(p)
	
	// Insert interaction p value
	putexcel G2 = `ixn_p'
	
	///eGFR = 20
	margins 	pain, 			at(														///
									indexGFR1=(0)										///	
									indexGFR2=(0.0144448668)							///
									indexGFR3=(-0.03002308) 							///
									indexGFR4=( 0.01557821)) 							///
									predict(ir)									
	matrix result = r(table)
		putexcel C2 = matrix(result[1,1]) D2 = matrix(result[2,1]) E2 = matrix(result[5,1]) F2 = matrix(result[6,1]) ///
				 C3 = matrix(result[1,2]) D3 = matrix(result[2,2]) E3 = matrix(result[5,2]) F3 = matrix(result[6,2])
																	
	///eGFR = 30
	margins 	pain, 			at(													///
									indexGFR1=(0.002021788)							///	
									indexGFR2=(-0.0752223609)						///
									indexGFR3=(0.15634667) 							///
									indexGFR4=(-0.08112431)) 						///
									predict(ir)										
	matrix result = r(table)
		putexcel C4 = matrix(result[1,1]) D4 = matrix(result[2,1]) E4 = matrix(result[5,1]) F4 = matrix(result[6,1]) ///
				 C5 = matrix(result[1,2]) D5 = matrix(result[2,2]) E5 = matrix(result[5,2]) F5 = matrix(result[6,2])
	
	///eGFR = 40
	margins 	pain, 			at(													///
									indexGFR1=(0.021203468 )						///	
									indexGFR2=(-0.1542772893)						///
									indexGFR3=(0.32065918 ) 						///
									indexGFR4=(-0.16638189)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C6 = matrix(result[1,1]) D6 = matrix(result[2,1]) E6 = matrix(result[5,1]) F6 = matrix(result[6,1]) ///
				 C7 = matrix(result[1,2]) D7 = matrix(result[2,2]) E7 = matrix(result[5,2]) F7 = matrix(result[6,2])
	
	
	///eGFR = 50
	margins 	pain, 			at(													///
									indexGFR1=(0.077916425)							///	
									indexGFR2=(-0.2101215158 )						///
									indexGFR3=(0.43672917) 							///
									indexGFR4=(-0.22660766)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C8 = matrix(result[1,1]) D8 = matrix(result[2,1]) E8 = matrix(result[5,1]) F8 = matrix(result[6,1]) ///
				 C9 = matrix(result[1,2]) D9 = matrix(result[2,2]) E9 = matrix(result[5,2]) F9 = matrix(result[6,2])
	
	///eGFR = 60
	margins 	pain, 			at(													///
									indexGFR1=(0.192545671)							///	
									indexGFR2=(-0.2301482110)						///
									indexGFR3=(0.47835386 ) 						///
									indexGFR4=(-0.24820565)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C10 = matrix(result[1,1]) D10 = matrix(result[2,1]) E10 = matrix(result[5,1]) F10 = matrix(result[6,1]) ///
				 C11 = matrix(result[1,2]) D11 = matrix(result[2,2]) E11 = matrix(result[5,2]) F11 = matrix(result[6,2])
	
	///eGFR = 70 
	margins 	pain, 			at(													///
									indexGFR1=(0.385476216)							///	
									indexGFR2=(-0.2017505454)						///
									indexGFR3=(0.41933045) 							///
									indexGFR4=(-0.21757990)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C12 = matrix(result[1,1]) D12 = matrix(result[2,1]) E12 = matrix(result[5,1]) F12 = matrix(result[6,1]) ///
				 C13 = matrix(result[1,2]) D13 = matrix(result[2,2]) E13 = matrix(result[5,2]) F13 = matrix(result[6,2])
	
	///eGFR = 80
	margins 	pain, 			at(													///
									indexGFR1=( 0.657269098)						///	
									indexGFR2=(-0.1169308917)						///
									indexGFR3=(0.25036818) 							///
									indexGFR4=(-0.12990968)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C14 = matrix(result[1,1]) D14 = matrix(result[2,1]) E14 = matrix(result[5,1]) F14 = matrix(result[6,1]) ///
				 C15 = matrix(result[1,2]) D15 = matrix(result[2,2]) E15 = matrix(result[5,2]) F15 = matrix(result[6,2])
	
	//eGFR = 90

	margins 	pain, 			at(													///						
									indexGFR1=(.8658574)							///	
									indexGFR2=(-.0008267)							///
									indexGFR3=(.0838487) 							///
									indexGFR4=(-.043507))							///
									predict(ir)
	matrix result = r(table)
		putexcel C16 = matrix(result[1,1]) D16 = matrix(result[2,1]) E16 = matrix(result[5,1]) F16 = matrix(result[6,1]) ///
				 C17 = matrix(result[1,2]) D17 = matrix(result[2,2]) E17 = matrix(result[5,2]) F17 = matrix(result[6,2])	
	
	///eGFR = 100
	margins 	pain, 			at(													///
									indexGFR1=(0.859676865)							///	
									indexGFR2=(0.1116086667)						///
									indexGFR3=(0.04731530) 							///
									indexGFR4=(-0.02395982)) 						///
									predict(ir)									
	
	matrix result = r(table)
		putexcel C18 = matrix(result[1,1]) D18 = matrix(result[2,1]) E18 = matrix(result[5,1]) F18 = matrix(result[6,1]) ///
				 C19 = matrix(result[1,2]) D19 = matrix(result[2,2]) E19 = matrix(result[5,2]) F19 = matrix(result[6,2])
	
	///eGFR = 110
	margins 	pain, 			at(													///
									indexGFR1=(0.746161336  )						///	
									indexGFR2=(0.2093978058)						///
									indexGFR3=(0.08151156) 							///
									indexGFR4=(-0.03707071)) 						///
									predict(ir)		
	matrix result = r(table)
		putexcel C20 = matrix(result[1,1]) D20 = matrix(result[2,1]) E20 = matrix(result[5,1]) F20 = matrix(result[6,1]) ///
				 C21 = matrix(result[1,2]) D21 = matrix(result[2,2]) E21 = matrix(result[5,2]) F21 = matrix(result[6,2])
	
	//eGFR = 120
	margins 	pain, 			at(													///
									indexGFR1=( 0.636842717 )						///	
									indexGFR2=(0.2905920514)						///
									indexGFR3=(0.11683880) 							///
									indexGFR4=(-0.04427357)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C22 = matrix(result[1,1]) D22 = matrix(result[2,1]) E22 = matrix(result[5,1]) F22 = matrix(result[6,1]) ///
				 C23 = matrix(result[1,2]) D23 = matrix(result[2,2]) E23 = matrix(result[5,2]) F23 = matrix(result[6,2])
	
	///eGFR = 130
	margins 	pain, 			at(													///
									indexGFR1=(0.538759552)							///	
									indexGFR2=(0.3558654921)						///
									indexGFR3=(0.14872349 ) 						///
									indexGFR4=(-0.04334853)) 						///
									predict(ir)		
	matrix result = r(table)
		putexcel C24 = matrix(result[1,1]) D24 = matrix(result[2,1]) E24 = matrix(result[5,1]) F24 = matrix(result[6,1]) ///
				 C25 = matrix(result[1,2]) D25 = matrix(result[2,2]) E25 = matrix(result[5,2]) F25 = matrix(result[6,2])
	
	///eGFR = 140
	margins 	pain, 			at(													///
									indexGFR1=(0.451302592)							///	
									indexGFR2=(0.4060814414 )						///
									indexGFR3=(0.17735231) 							///
									indexGFR4=(-0.03473634)) 						///
									predict(ir)										
	matrix result = r(table)
		putexcel C26 = matrix(result[1,1]) D26 = matrix(result[2,1]) E26 = matrix(result[5,1]) F26 = matrix(result[6,1]) ///
				 C27 = matrix(result[1,2]) D27 = matrix(result[2,2]) E27 = matrix(result[5,2]) F27 = matrix(result[6,2])
		
	
	///eGFR = 150
	margins 	pain, 			at(													///
									indexGFR1=(0.373862590)							///	
									indexGFR2=(0.4421032131)						///
									indexGFR3=(0.20291193) 							///
									indexGFR4=(-0.01887773)) 						///
									predict(ir)	
	
	matrix result = r(table)
		putexcel C28 = matrix(result[1,1]) D28 = matrix(result[2,1]) E28 = matrix(result[5,1]) F28 = matrix(result[6,1]) ///
				 C29 = matrix(result[1,2]) D29 = matrix(result[2,2]) E29 = matrix(result[5,2]) F29 = matrix(result[6,2])
	
	putexcel close
	