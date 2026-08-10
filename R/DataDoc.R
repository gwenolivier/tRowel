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