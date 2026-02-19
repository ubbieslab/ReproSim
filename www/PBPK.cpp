//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//    https://www.gnu.org/licenses/
//
//    This program comes with ABSOLUTELY NO WARRANTY;
//    This is free software, and you are welcome to redistribute it
//    under certain conditions;
//
//  PBPK.cpp
//
//  model specification file
//
//
//
// ------------------------------------------------------------------------------------------
// See section 3.2. code blocks for examples on how to edit model code to include annotation
//	http://mrgsolve.github.io/user_guide/model-specification.html
//
//  use with parameters that will be overwritten by values in table - NA_real_
//
// 
//
// Units: conc [mg/L], rate constants [/hr], weight [kg], cardiac output [L/hr]
//
// Assume 1L == 1kg
//
// Densities of different body matter
// 	Blood - 1.0428 g/cm^3
// 	Bone - 1.75 g/cm^3
// 	Fat - 0.9094 g/cm^3
// 	Muscle - 1.0599 g/cm^3
//
// Converting ng/g to ng/ml should include a measure of the density
// Currently know density of blood plasma = 1.06g/cm^3
// Also know 1mL = 1cm^3
// 	So ng/g * 1.06g/ml = ng/ml
// Currently going to assume ng/g == ng/ml   1ng/ml = 0.001mg/L
//
// ------------------------------------------------------------------------------------------


$GLOBAL
// define total concentration in lumen, epi and stroma tissue
#define C_totLUMEN 	(LUMEN_SOLUTION +  LUMEN_PRECIP) 
#define C_totEPI	(EPITHELIUM )
#define C_totSTROMA (STROMA_TISSUE)

#define mass_balance ((LUMEN_SOLUTION+LUMEN_PRECIP)*V_lumen+(EPITHELIUM)*V_Epithelium+(STROMA_TISSUE)*V_Stroma_Tissue+STROMA_BLOOD*V_Stroma_Blood+VEINS*V_Veins+LUNGS*V_Lungs+ARTERIES*V_Arteries+ADIPOSE*V_Adipose+BODY*V_Body+LIVER*V_Liver+KIDNEYS*V_Kidneys+CERVIX_FLUID*V_CVfluid+CERVIX_EPITHELIUM*V_Cervical_Epithelium+CERVIX_MYOMETRIUM*V_Cervical_Myometrium+UTERUS_FLUID*V_Ufluid+UTERUS_ENDOMETRIUM*V_Uterine_Endometrium+UTERUS_MYOMETRIUM*V_Uterine_Myometrium+GUT*V_Gut+RENAL_EXC+HEPATIC_EXC+LUMEN_EXC+LUMEN_PRECIP_EXC - RELEASE_1 - RELEASE_2 - RELEASE_3)

//================

$SET delta = 0.01

//================

$PARAM


dose = 0.0
F = 1.0

// Fraction of dose in each dosing phase
f1 = 1.0
f2 = 0.0
f3 = 0.0

// Duration of each dosing phase
dur1 = 1.0
dur2 = 0.0
dur3 = 0.0


// First order rate constants (k), units: [1/hr]
// Current values based on previous dapivirine model, requires updating

/// Vaginal Tissue
// k_LE: Luminal fluid to epithelium
k_LE = 5

// k_EL: Epithelium to luminal fluid
k_EL = 0.01

// k_EVS: Epithelium to vaginal stroma
k_EVS = 0.43

// Active transport from epithelium to stroma (ionized drug only)
k_activeEtoS = 0

// Vaginal stroma to epithelium
k_VSE = 0.01

// Active transport from stroma tissue to blood (ionized drug only)
k_active_StrTtoB = 0

// Vaginal stroma tissue to blood
k_VS_TtoB = 0.15

// Blood to vaginal stroma tissue
k_VS_BtoT = 0.2

/// Vaginal-Cervical
// Luminal Fluid/Cervical Mucus
k_L_CF = 0.0001
k_CF_L = 0.0001

// Epithelium
k_VE_CE = 0.001
k_CE_VE = 0.0001

// Stroma/Myometrium
k_VS_CM = 0.0001
k_CM_VS = 0.0001

/// Cervical Tissue
// Mucosa - Cervical Endometrium
k_CF_CE = 2
k_CE_CF = 0.212

// Cervical Endometrium - Myometrium
k_CE_CM = 0.043
k_CM_CE = 0.212

// Cervical Myometrium - Blood
k_CM_TtoB = 0.043
k_CM_BtoT = 1

/// Cervical-Uterine
// Cervical Mucus/Uterine Lining
k_CF_UF = 0.0001
k_UF_CF = 0.0001

// Cervical Endometrium/Uterine Endometrium
k_CE_UE = 0.01
k_UE_CE = 0.0001

// Cervical Myometrium/Uterine Myometrium
k_CM_UM = 0.0001
k_UM_CM = 0.0001

///Uterine Tissue
// Endometrial Fluid - Uterine Endometrium
k_UF_UE = 2
k_UE_UF = 0.148

// Uterine Endometrium - Myometrium
k_UE_UM = 0.076
k_UM_UE = 0.097

// Uterine Myometrium - Blood
k_UM_TtoB = 0.076
k_UM_BtoT = 3

// Drug Clearance (CL), units: [L/hr]
CL_H = 7.06
CL_R = 0
CL_L = 2.5e-5

// Partition coefficients (P), units: dimensionless

P_H = 2.1           // Liver
P_B = 0.6         	// Body
P_Lungs = 2.5      // Lungs
P_VS = 0.01        	// Vaginal Stroma 
P_CM = 0.01			// Cervical Myometrium 
P_UM = 0.01			// Uterine Myometrium 
P_R = 1.9			// Kidneys 
P_AD = 5			// Adipose Tissue
P_G =  4    //GI track
P_S = 1.1	//Spleen
BP = 1.07

// pH
pH_Lumen = 4.4
pH_Body = 7.0

// Weight [kg]
// Body mass of reference woman = 58 kg
weight = 58.0

// Organ and tissue mass fractions [fraction of total body weight]
// From Brown et al.; Toxicology and Industrial Health, Vol. 13, No. 4, 1997,
// and Report of the Task Group on Reference Man; International Commission on Radiological Protection, No. 23, 1974
// Values not available in the above sources were back-calculated from tissue volume, assuming a density of 1 L/kg
F_Lungs = 0.008
F_Liver = 0.026
F_Kidneys = 0.004
F_Adipose = 0.327
F_Arteries = 0.019
F_Veins = 0.056
F_Uterus = 1.38e-3
F_Cervix = 4.31e-4
F_Vag_Lumen =  8.62e-6
F_Vag_Epithelium = 1.51e-4
F_Vag_Stroma = 1.81e-4
F_Uterine_Fluid =  1.72e-6
F_CVfluid = 8.62e-6
F_Gut = 0.0165
F_Spleen = 2.6e-3


//pKa - use to find ionisation rate
pKa = 14	

// Solubility - based on in aqueous solubility from IB, units: mg/L
solubility = 2.05 // Aqueous solubility
gamma = 5.0	// Hill eqn exponent. For gamma >= ~5, eqn behaves as an on-off switch


// Protein Binding
f_unbound = 0.02

// Flags and rate constants for dosing equations
// Sequential (0) vs Simultaneous (1)
Simult = 1
// Route of administration
G_Flag = 0.0
U_Flag = 0.0
V_Flag = 0.0
// Zero order
ZO1 = 0.0
ZO2 = 0.0
ZO3 = 0.0
// First order
FO1 = 0.0
FO2 = 0.0
FO3 = 0.0
// Higuchi (square root time)
SQ1 = 0.0
SQ2 = 0.0
SQ3 = 0.0
// Time dependence
TD1 = 0.0
TD2 = 0.0
TD3 = 0.0
// Lag time
lag1 = 0.0
lag2 = 0.0
lag3 = 0.0

//================

$CMT

LUMEN_SOLUTION
LUMEN_PRECIP
EPITHELIUM
STROMA_TISSUE
STROMA_BLOOD
VEINS
LUNGS
ARTERIES
BODY
LIVER

ELIM_DRUG


CERVIX_FLUID
CERVIX_EPITHELIUM
CERVIX_MYOMETRIUM

UTERUS_FLUID
UTERUS_ENDOMETRIUM
UTERUS_MYOMETRIUM

KIDNEYS
ADIPOSE
GUT

G_DEPOT // CMT 21

V_DEPOT // CMT 22
U_DEPOT // CMT 23

RELEASE_1
RELEASE_2
RELEASE_3



RENAL_EXC
HEPATIC_EXC
LUMEN_EXC
LUMEN_PRECIP_EXC

SPLEEN


//================

$MAIN

double pH_Epi = pH_Body;
double pH_Stroma = pH_Body;



// Blood flow rates [L/hr]
double Q_CO = 4.9 * 60;			// [L/min] *60 = [L/hr]
double Q_VS = 2.44;
double Q_H  = 0.1 * Q_CO;
double Q_B  = Q_CO - Q_VS - Q_H - Q_R - Q_AD - Q_G - Q_S;
double Q_R = 0.15 * Q_CO;
double Q_AD = 0.11 * Q_CO;
double Q_G = 0.14*Q_CO;   //XL from brown 1997
double Q_S = 0.03*Q_CO;



// Calculated mass fractions
double F_Stroma_Blood = 2.59e-3;
// Calculate rest of body through mass fraction balance
double F_Body = 1.0 - F_Lungs - F_Liver - F_Kidneys - F_Adipose - F_Spleen - F_Arteries - F_Veins - F_Uterus - F_Cervix - F_Vag_Lumen - F_Vag_Epithelium - F_Vag_Stroma - F_Stroma_Blood - F_Uterine_Fluid - F_CVfluid - F_Gut ;


// To incorporate IIV, tissue volumes are calculated from body mass fraction
// Assumes tissue density of 1 kg/L, with the exception of adipose tissue density = 0.916 kg/L
double V_Stroma_Blood = F_Stroma_Blood * weight;
double V_lumen = F_Vag_Lumen * weight; 				
double V_Epithelium = F_Vag_Epithelium * weight;
double V_Stroma_Tissue = F_Vag_Stroma * weight;
double V_Uterus = F_Uterus * weight;
double V_Ufluid = F_Uterine_Fluid * weight;
double V_Uterine_Endometrium = 0.6/1.6 * V_Uterus;
double V_Uterine_Myometrium = 1.0/1.6 * V_Uterus;
double V_Cervix = F_Cervix * weight;
double V_CVfluid = F_CVfluid * weight; //change the F from F_Van_Lumen to F_CVfluid XL
double V_Cervical_Epithelium = 1/2.2 * V_Cervix; 
double V_Cervical_Myometrium = V_Cervix - V_Cervical_Epithelium;	
double V_Lungs = F_Lungs * weight;
double V_Liver = F_Liver * weight;
double V_Body = F_Body * weight;
double V_Veins = F_Veins * weight;
double V_Arteries = F_Arteries * weight;
double V_Kidneys = F_Kidneys * weight;
double V_Adipose = F_Adipose * weight/0.916; 			// Assume adipose tissue density of 0.916 kg/L
double V_Gut = F_Gut*weight;
double V_Spleen = F_Spleen*weight;


// Separate total dose into dosing phases
double dose1 = dose*F*f1;
double dose2 = dose*F*f2;
double dose3 = dose*F*f3;


// Solubility and Ionization
// Based on Henderson-Hasselbalch equations
double f_uni = 1;// 1 / (1+pow(10, pH_Lumen - pKa ))
double f_unibo = 1; // 1 / (1+pow(10, pH_Body - pKa ))

// Sinusoidal function for solubility. By setting gamma >= ~5, it functions like an on-off switch for movement
// of drug from suspension to solution. Decrease gamma for a more gradual transition.
double SolubilityLumen = pow((LUMEN_SOLUTION +  LUMEN_PRECIP), gamma) / (pow(solubility/2.0,gamma) + pow((LUMEN_SOLUTION +  LUMEN_PRECIP),gamma));


//================

//$OMEGA

//================

$ODE


double SQRT_Term1 = SQ1/(2.0*sqrt(SOLVERTIME));
double SQRT_Term2 = SQ2/(2.0*sqrt(SOLVERTIME));
double SQRT_Term3 = SQ3/(2.0*sqrt(SOLVERTIME));

if (SOLVERTIME <= 0 ) {
	SQRT_Term1 = 0;
	SQRT_Term2 = 0;
	SQRT_Term3 = 0;
}


double Input1 = ZO1+((G_DEPOT + V_DEPOT + U_DEPOT)*f1)*FO1 + SQRT_Term1; //+SQ1/(2.0*sqrt(SOLVERTIME));
double Input2 = ZO2+((G_DEPOT + V_DEPOT + U_DEPOT)*f2)*FO2 + SQRT_Term2; //+SQ2/(2.0*sqrt(SOLVERTIME));
double Input3 = ZO3+((G_DEPOT + V_DEPOT + U_DEPOT)*f3)*FO3 + SQRT_Term3; //+SQ3/(2.0*sqrt(SOLVERTIME));


// Turn release phases off when the specified fraction of total dose is released
if(RELEASE_1 >= dose1) Input1 = 0;
if(RELEASE_2 >= dose2) Input2 = 0;
if(RELEASE_3 >= dose3) Input3 = 0;


// Incorporate time dependency
if (TD1 != 0) Input1 = Input1/(TD1+SOLVERTIME);
if (TD2 != 0) Input2 = Input2/(TD2+SOLVERTIME);
if (TD3 != 0) Input3 = Input3/(TD3+SOLVERTIME);

// Mass flow rates, units: ug/h
//Vaginal and Body Compartments
double Flux1 = (k_LE * LUMEN_SOLUTION * f_unbound * f_uni) * V_lumen - (k_EL * EPITHELIUM * f_unibo) * V_Epithelium;	// Vaginal Lumen Solution <-> Epithelium
double Flux2 = (k_EVS * EPITHELIUM * f_unibo) * V_Epithelium - (k_VSE * STROMA_TISSUE * f_unibo) * V_Stroma_Tissue;		// Vaginal Epithelium <-> Stroma
double Flux3 = k_VS_TtoB * BP * STROMA_TISSUE * f_unibo /P_VS * V_Stroma_Tissue - k_VS_BtoT * STROMA_BLOOD * V_Stroma_Blood;		// Vaginal Stroma <-> Stroma Blood
double Flux4 = Q_VS * STROMA_BLOOD;																	// Stroma Blood <-> Venous Blood
double Flux5 = Q_CO * VEINS;																		// Venous Blood
double Flux6 = (Q_CO * BP* LUNGS / P_Lungs);															// Lungs
double Flux7 = Q_VS * ARTERIES;																		// Arterial Blood <-> Stroma Blood
double Flux8 = Q_B * ARTERIES;																		// Arterial Blood
double Flux9 = (Q_B * BP * BODY/P_B);																	// Rest of Body
double Flux10 = Q_H * ARTERIES;																		// Arterial Blood <-> Liver
double Flux11 = ((Q_H+Q_G+Q_S) * BP* LIVER/P_H);																	// Liver
double Flux12 = CL_H * BP * (LIVER/P_H);																	// Hepatic Clearance



double Flux16 = (k_activeEtoS * EPITHELIUM*(1-f_unibo)) * V_Epithelium;										// Active uptake from Vaginal Epithelium to Stroma
double Flux17 = (k_active_StrTtoB * STROMA_TISSUE*(1-f_unibo)) * V_Stroma_Tissue;							// Active uptake from Vaginal Stroma to Stroma Blood



// GI track //XL
double Flux39 = Q_G*ARTERIES;
double Flux40 = Q_G*GUT*BP/P_G;
// SPLEEN 
double Flux42 = Q_S*ARTERIES;
double Flux43 = Q_S*BP*SPLEEN/P_S;

// Vaginal Lumen Solution Clearance //XL
double Flux37 = CL_L*LUMEN_SOLUTION;
// Vaginal Luemn Precip Clearance //XL
double Flux38 = CL_L*LUMEN_PRECIP;
// Vaginal Lumen ionized drug clearance //XL
double Flux41 = CL_L*LUMEN_ION;


//Cervical Compartments
double Flux19 = k_L_CF * LUMEN_SOLUTION * V_lumen * f_unbound- k_CF_L * CERVIX_FLUID * V_CVfluid;			 // vaginal luminal <-> cervical luminal		
double Flux20 = k_VE_CE * EPITHELIUM * V_Epithelium - k_CE_VE * CERVIX_EPITHELIUM * V_Cervical_Epithelium;				 // vaginal epi <-> cervical epi
double Flux21 = k_VS_CM * STROMA_TISSUE * V_Stroma_Tissue - k_CM_VS * CERVIX_MYOMETRIUM * V_Cervical_Myometrium;		 // vaginal stroma <-> cervical myometrium
double Flux22 = k_CF_CE * CERVIX_FLUID * V_CVfluid - k_CE_CF * CERVIX_EPITHELIUM * V_Cervical_Epithelium;				 // cervical luminal <-> cervical epi
double Flux23 = k_CE_CM * CERVIX_EPITHELIUM * V_Cervical_Epithelium - k_CM_CE * CERVIX_MYOMETRIUM *V_Cervical_Myometrium;// cervical epi <-> cervical myometrium
double Flux24 = k_CM_TtoB * BP * CERVIX_MYOMETRIUM/P_CM * V_Cervical_Myometrium - k_CM_BtoT * STROMA_BLOOD * V_Stroma_Blood;				 // cervical myo <-> stroma blood

// Uterine Compartments
double Flux25 = k_CF_UF * CERVIX_FLUID * V_CVfluid - k_UF_CF * UTERUS_FLUID * V_Ufluid*f_unbound;										 // cervical luminal fluid <-> uterine luminal fluid
double Flux26 = k_CE_UE * CERVIX_EPITHELIUM * V_Cervical_Epithelium - k_UE_CE * UTERUS_ENDOMETRIUM * V_Uterine_Endometrium;  // cervical epi <-> uterine epi
double Flux27 = k_CM_UM * CERVIX_MYOMETRIUM * V_Cervical_Myometrium - k_UM_CM * UTERUS_MYOMETRIUM * V_Uterine_Myometrium;	 // cervical myo <-> uterine myo
double Flux28 = k_UF_UE * UTERUS_FLUID * V_Ufluid *f_unbound - k_UE_UF * UTERUS_ENDOMETRIUM * V_Uterine_Endometrium;					 // uterine luminal <-> uterine endo
double Flux29 = k_UE_UM * UTERUS_ENDOMETRIUM * V_Uterine_Endometrium - k_UM_UE * UTERUS_MYOMETRIUM * V_Uterine_Myometrium;	 // uterine endo <-> uterine myo
double Flux30 = k_UM_TtoB * BP * UTERUS_MYOMETRIUM/P_UM * V_Uterine_Myometrium - k_UM_BtoT * STROMA_BLOOD * V_Stroma_Blood;					 // uterine myo <-> stroma blood



// Kidneys
double Flux32 = Q_R * ARTERIES;
double Flux33 = Q_R * BP* KIDNEYS/P_R;
double Flux34 = CL_R * (KIDNEYS/P_R);

// Adipose
double Flux35 = Q_AD * ARTERIES;
double Flux36 = Q_AD *BP* ADIPOSE/P_AD;


//double Flux32 = ka*GUT;

dxdt_LUMEN_SOLUTION		= (1 / V_lumen) * ((-Flux1-Flux19-Flux37) + V_Flag*(Input1+Input2+Input3)*(1-SolubilityLumen));  
dxdt_LUMEN_PRECIP		= (1 / V_lumen) * ( V_Flag*(Input1+Input2+Input3)*SolubilityLumen - Flux38);
dxdt_EPITHELIUM 		= (1 / V_Epithelium) * (Flux1 - Flux2 - Flux16 - Flux20);
dxdt_STROMA_TISSUE 		= (1 / V_Stroma_Tissue) * (Flux2 + Flux16 - Flux3 - Flux17 - Flux21);
dxdt_STROMA_BLOOD 		= (1 / V_Stroma_Blood) * (Flux3 - Flux4 + Flux7 + Flux17 + Flux24 + Flux30);
dxdt_VEINS 				= (1 / V_Veins) * (Flux4 - Flux5 + Flux9 + Flux11 + Flux33 + Flux36);
dxdt_LUNGS 				= (1 / V_Lungs) * (Flux5 - Flux6);
dxdt_ARTERIES 			= (1 / V_Arteries) * (Flux6 - Flux7 - Flux8 - Flux10 - Flux32 - Flux35 - Flux39 - Flux42);
dxdt_ADIPOSE			= (1 / V_Adipose) * (Flux35 - Flux36);
dxdt_BODY 				= (1 / V_Body) * (Flux8 - Flux9);
dxdt_LIVER 				= (1 / V_Liver) * ((Flux10-Flux11-Flux12)+Flux40 + Flux43);
dxdt_KIDNEYS			= (1 / V_Kidneys) * (Flux32 - Flux33 - Flux34);

dxdt_ELIM_DRUG 			= Flux12 + Flux34 + Flux37 + Flux38;

dxdt_CERVIX_FLUID		= (1 / V_CVfluid) * (Flux19 - Flux22 - Flux25);
dxdt_CERVIX_EPITHELIUM	= (1 / V_Cervical_Epithelium) * (Flux20 + Flux22 - Flux23 - Flux26);
dxdt_CERVIX_MYOMETRIUM	= (1 / V_Cervical_Myometrium) * (Flux21 + Flux23 - Flux24 - Flux27);

dxdt_UTERUS_FLUID		= (1 / V_Ufluid) * ((Flux25 - Flux28) + U_Flag*(Input1+Input2+Input3));
dxdt_UTERUS_ENDOMETRIUM	= (1 / V_Uterine_Endometrium) * (Flux26 + Flux28 - Flux29);
dxdt_UTERUS_MYOMETRIUM	= (1 / V_Uterine_Myometrium) * (Flux27 + Flux29 - Flux30);

dxdt_GUT 				= (1 / V_Gut) * (G_Flag*(Input1+Input2+Input3) + Flux39 - Flux40);
dxdt_SPLEEN				= (1/V_Spleen)* (Flux42 - Flux43);


dxdt_G_DEPOT = -G_Flag*(Input1+Input2+Input3);
dxdt_V_DEPOT = -V_Flag*(Input1+Input2+Input3);
dxdt_U_DEPOT = -U_Flag*(Input1+Input2+Input3);

dxdt_RELEASE_1 = Input1;
dxdt_RELEASE_2 = Input2;
dxdt_RELEASE_3 = Input3;

dxdt_RENAL_EXC = Flux34;
dxdt_HEPATIC_EXC = Flux12;
dxdt_LUMEN_EXC = Flux37; //XL
dxdt_LUMEN_PRECIP_EXC = Flux38; //XL

double LUMEN_BOUND = LUMEN_SOLUTION * (1 - f_unbound);
double LUMEN_UNBOUND = LUMEN_SOLUTION * f_unbound ;
double CLUMENPRECIP = LUMEN_PRECIP;
double LUMEN_ION = LUMEN_SOLUTION * (1 - f_uni);
double EPITHELIUM_ION = EPITHELIUM * (1 - f_unibo);
double STROMA_TISSUE_ION = STROMA_TISSUE * (1 - f_unibo);

//================

$TABLE
double TOTAL_DEPOT = G_DEPOT + V_DEPOT + U_DEPOT;

//================

$CAPTURE SolubilityLumen C_totLUMEN C_totEPI C_totSTROMA LUMEN_BOUND LUMEN_UNBOUND LUMEN_ION EPITHELIUM_ION STROMA_TISSUE_ION CLUMENPRECIP Input1 Input2 Input3 TOTAL_DEPOT dose1 dose2 dose3 TD1 TD2 TD3 G_Flag V_Flag U_Flag ZO1 ZO2 ZO3 FO1 FO2 FO3 SQ1 SQ2 SQ3 SQRT_Term1 SQRT_Term2 SQRT_Term3 mass_balance

//================

