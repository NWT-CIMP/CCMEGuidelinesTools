#Functions to create guidelines


#' Calculate CCME Water Quality Guidelines for the Protection of Aquatic Life
#' For the Manganese,  Zinc, Cadmium, Nickel, Copper, Lead, and Ammonia, water
#' quality guidelines depend on the hardness of the water, or other factors.
#' This function calculates those Guideline values for specific conditions.
#' Currently accepts only single values
#' @param Parameter Options include "Manganese", "Zinc", "Cadmium", "Nickel",
#'   "Copper", "Lead"
#' @param Hardness Required for all (mg/L)
#' @param pH Required for Manganese and Ammonia
#' @param DOC Required for Zinc (mg/L)
#' @param Temp Required for Ammonia
#' @param CaCO3 Required for Manganese
#'
#' @return Returns a vectors with the short-term, long-term limits and whether
#'   the limit if for Dissolved or Total
#' @export
#'
#' @examples
calc_water_guidelines<- function (Parameter= c("Manganese", "Zinc", "Cadmium", "Nickel", "Copper", "Lead", "Ammonia"),
                                  Hardness=NA, pH=NA, DOC=NA, Temp= NA, CaCO3=NA){
  if(Parameter %in% c("Manganese", "Zinc", "Cadmium", "Nickel", "Copper", "Lead") & is.na(Hardness)){
    message("Must provide hardness")
  }
  if(Parameter %in% c("Manganese", "Ammonia") & is.na(pH)){
    message("Must provide pH")
  }
  if(Parameter %in% c("Zinc") & is.na(DOC)){
    message("Must provide DOC")
  }
  if(Parameter %in% c("Ammonia") & is.na(Temp)){
    message("Must provide Temp")
  }
  if(Parameter %in% c("Manganese") & is.na(CaCO3)){
    message("Must provide Temp")
  }

  if(Parameter=="Ammonia"){
    short=NA
    long= table_ammonia$Value[table_ammonia$Temp== seq(0, 30,5)[which.min(seq(0, 30,5)-Temp)]  &
                                table_ammonia$pH== unique(table_ammonia$pH)[which.min(unique(table_ammonia$pH)-pH)]]
    Type= "Total"

    if(length(long)==0){
      long=NA
    }
  }

  if(Parameter=="Manganese"){
    short= exp(0.878*log(Hardness) + 4.76)
    long = table_manganese$Value[CaCO3>=table_manganese$CaCO3 &
                                   (CaCO3<table_manganese$CaCO3_upper )&
                                   table_manganese$pH== unique(table_manganese$pH)[which.min(unique(table_manganese$pH)-pH)]]
    Type="Dissolved"
    if(length(long)==0){
      long=NA
    }

  }

  if(Parameter=="Zinc"){
    short= exp(0.833*log(Hardness) + 0.240*log(DOC) + 0.526)
    long = exp(0.947*log(Hardness) - 0.8158*pH + 0.398*log(DOC) + 4.625)
    Type="Dissolved"
  }
  if(Parameter=="Cadmium"){
    short= 10^(1.016*log10(Hardness) - 1.71)
    long = 10^(0.83*log10(Hardness) - 2.46)
    Type="Total"
  }

  if(Parameter=="Lead"){
    long= ifelse(is.na(Hardness)| Hardness<60, 1,
                 ifelse(Hardness>60 & Hardness<= 180,exp(1.273*log(Hardness)-4.705), 7))
    short = NA
    Type="Total"
  }

  if(Parameter=="Copper"){
    long= ifelse(is.na(Hardness)| Hardness<60, 2,
                 ifelse(Hardness>60 & Hardness<= 180,0.2* exp(0.8545*log(Hardness)-1.465), 4))
    short = NA
    Type="Total"
  }

  if(Parameter=="Nickel"){
    long= ifelse(is.na(Hardness)| Hardness<60, 25,
                 ifelse(Hardness>60 & Hardness<= 180,exp(0.76*log(Hardness)+1.06), 150))
    short = NA
    Type="Total"
  }

return(c(short, long, Type))
}


check_against_guideline<- function(Parameter, shortTerm, longTerm ){
  short_exceeds <- ifelse(Parameter>shortTerm, "Exceeds", NA)
  long_exceeds <- ifelse(Parameter>longTerm, "Exceeds", NA)

  return(cbind(short_exceeds, long_exceeds))
}


lookup_water_guidelines <- function(Parameter,
                                    Hardness=NA, pH=NA, DOC=NA, Temp= NA, CaCO3=NA){



  if(Parameter %in% c("Manganese", "Zinc", "Cadmium", "Nickel", "Copper", "Lead", "Ammonia")){

        return(calc_water_guidelines(Parameter= Parameter,
                                 Hardness=Hardness, pH=pH, DOC=DOC, Temp= Temp, CaCO3=CaCO3))

  } else {
    short = ccme_water$CCMEwater_short [Parameter==ccme_water$Parameter]
    long=ccme_water$CCMEwater_long [Parameter==ccme_water$Parameter]
    Type=ccme_water$DorT [Parameter==ccme_water$Parameter]
    return(c(as.numeric(short), as.numeric(long), Type))

  }

}


#' Calculate Water Hardness
#'
#' @param Calcium Calcium concentration (mg/L)
#' @param Magnesium Total magnesium concentration (mg/L)
#'
#' @return
#' @export
#'
#' @examples
calc_hardness <- function(Calcium, Magnesium){
  Hardness = 2.497*Calcium + 4.118 * Magnesium
  return(Hardness)
}



#' Look up CCME Sediment Quality Guidelines for the Protection of Aquatic Life
#' All guidelines provided are for freshwater. Currently only metals and PACs
#' are implemented. No calculated guidelines for sediment are conditional on
#' other values so no required inputs, except the parameter of interest.
#' @param Parameter
#'
#' @return Vector of Interim sediment quality guidelines(ISQG) and probably effect levels (PEL)
#' @export
#'
#' @examples
lookup_sediment_guidelines <- function(Parameter){

  ISQG = ccme_sediment$CCMEsediment_ISQG [Parameter==ccme_sediment$Parameter]
  PEL=ccme_sediment$CCMEsediment_PEL[Parameter==ccme_sediment$Parameter]
  return(c(as.numeric(ISQG), as.numeric(PEL)))

}
