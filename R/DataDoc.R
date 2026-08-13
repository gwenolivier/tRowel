#' Example soil dataset
#'
#' An example dataset of soil measurements used in examples.
#'
#' @format A data frame of of 60 rows and 23 columns
#' \itemize{
#'   \item SampleID: ID for soil sample.
#'   \item ProfileID: ID for soil profile.
#'   \item Depth_cm_Start: Sample starting depth in centimeters.
#'   \item Depth_cm_End: Sample ending depth in centimeters.
#'   \item Munsell: Munsell color code.
#'   \item Sand_pct: Percent sand.
#'   \item Silt_pct: Percent silt.
#'   \item Clay_pct: Percent clay.
#'   \item OrganicC_pct: Percent organics cabon.
#'   \item Carbonate_pct: Percent carbonate.
#'   \item d13C: delta thirteen c value.
#'   \item d15N: delta fifteen n value.
#'   \item Xlf: Mass-specific low-field AC susceptibility value.
#'   \item Ca: Calcium value.
#'   \item Al: Alumninum value.
#'   \item Si: Silicon value.
#'   \item K: Potassium value.
#'   \item Mn: Manganese value.
#'   \item P: Phosphorus value.
#'   \item Fe: Iron value.
#'   \item Sr: Strontium value.
#'   \item Ti: Titanium value.
#'   \item Rb: Rubidium value.
#'   \item Mg: Magnesium value.
#' }
"testPit"

#' Example elevation dataset
#'
#' An example dataset of soil measurements used in examples.
#'
#' @format A data frame of of 5 rows and 2 columns
#' \itemize{
#'   \item ProfileID: ID for soil profile.
#'   \item surfaceElevM: Surface elevation in meters for the profile ID.
#' }
"testElevation"


#' Example qualitative soil dataset
#'
#' An example dataset of qualitative soil measurements used in examples.
#'
#' @format A data frame of of 22 rows and 12 columns
#' \itemize{
#'   \item ProfileID: ID for soil profile.
#'   \item Depth_cm_Start: Sample starting depth in centimeters.
#'   \item Depth_cm_End: Sample ending depth in centimeters.
#'   \item Texture: Categorical classification of soil texture.
#'   \item Structure: Categorical classification of soil structure.
#'   \item Size: Categorical classification of soil particle size.
#'   \item Consistency: Categorical classification of soil Consistency.
#'   \item Grade: Categorical classification of soil grade.
#'   \item Carbonates: Categorical classification of soil carbonates.
#'   \item Slickensides: Categorical classification of soil slickensides.
#'   \item Roots_rootlets: Categorical classification of soil root/rootlet presence.
#'   \item Insects_worm_burrows: Categorical classification of insect or worm burrows in soil.
#' }
"testQual"


#' Amazonian Dark Earths soil dataset
#'
#' Amazonian Dark Earths soil dataset from Demetrio et al. (2021).
#'
#' @format A data frame of of 450 rows and 36 columns
#' \itemize{
#'   \item ProfileID: Soil profile ID
#'   \item DepthStart: Sample starting depth in centimeters.
#'   \item DepthEnd: Sample ending depth in centimeters.
#'   \item pH1: pH tested using CaCl2.
#'   \item pH2: pH-SMP.
#'   \item Alplus3:  Exchangeable aluminium (cmolc kg-1).
#'   \item HplusAl3plus: Exchangeable acidity (cmolc kg-1).
#'   \item Ca2plus: Exchangeable calcium (cmolc kg-1).
#'   \item Mg2plus: Exchangeable magnesium (cmolc kg-1).
#'   \item K:  Exchangeable potassium (cmolc kg-1).
#'   \item SB: Sum of bases (sum of Ca2, Mg2+, K+; cmolc kg-1).
#'   \item CEC: Cation Exchange Capacity (sum of Al+H, Ca2+, Mg2+,K+; cmolc kg-1).
#'   \item P:  Phosphorus avaliable extracted by Mehlich-1 (mg kg-1).
#'   \item TotalNitrogen: Total nitrogen.
#'   \item TotalCarbon: Total carbon.
#'   \item BaseSaturation: sum of base cations (Ca2+, Mg2+, K+, and Na+).
#'   \item Ba: Barium content mg kg-1 (pseudototal).
#'   \item Cd: Cadmium content mg kg-1 (pseudototal).
#'   \item Cu1: Copper content mg kg-1 (pseudototal).
#'   \item Fe: Iron content mg kg-1 (pseudototal).
#'   \item Ni1: Nickel content mg kg-1 (pseudototal).
#'   \item Pb: Lead content mg kg-1 (pseudototal).
#'   \item Se: Selenium content mg kg-1 (pseudototal).
#'   \item Zn: Zinc content mg kg-1 (pseudototal).
#'   \item Fe_Available: Available Iron mg kg-1.
#'   \item Zn_Available: Available Zinc mg kg-1.
#'   \item Mn_Available: Available Manganese mg kg-1.
#'   \item Cu_Available: Available Copper mg kg-1.
#'   \item Ni_Available: Available Nickel mg kg-1.
#'   \item Clay: Clay size (<0.002 mm) particle content (g kg-1).
#'   \item CoarseSand: Coarse sand size (<2 mm-0.2 mm) particle content (g kg-1).
#'   \item FineSand: Fine sand size (<0.2 mm-0.053 mm) particle content (g kg-1). 
#'   \item TotalSand: Total sand (CoarseSand+FineSand).
#'   \item Silt: Silt size (<0.053 mm-0.002 mm) particle content (g kg-1).
#'   \item MagneticSusceptibility: Magnetic susceptibility of soil.
#'   \item ApparentElectricalConductivity: Electircal conductivity of soil.
#' }
#' @references{
#' \itemize{
#'   \item Demetrio WC, Conrado AC, Acioli ANS, Ferreira AC, Bartz MLC, James SW, da Silva E, Maia LS, Martins GC, Macedo RS, Stanton DWG, Lavelle P, Velasquez E, Zangerle A, Barbosa R, Tapia-Coral SC, Muniz AW, Santos A, Ferreira T, Segalla RF, Decaens T, Nadolny HS, Pena-Venegas CP, Maia C, Pasini A, Mota AF, Taube Junior PS, Silva TAC, Rebellato L, de Oliveira Junior RC, Neves EG, Lima HP, Feitosa RM, Vidal Torrado P, McKey D, Clement CR, Shock MP, Teixeira WG, Motta ACV, Melo VF, Dieckow J, Garrastazu MC, Chubatsu LS, Network TPI, Kille P, Brown GG, and Cunha L. 2021. A "Dirty" Footprint: Macroinvertebrate diversity in Amazonian Anthropic Soils. Glob Chang Biol 27:4575–4591. 10.1111/gcb.15752
#'   }
#' }
"ADE"