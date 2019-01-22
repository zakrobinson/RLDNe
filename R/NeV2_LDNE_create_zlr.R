#'Write LDNe Parameter File
#'
#' This function writes a parameter for LD Method in NeEstmator V2.1
#'
#' @param input_file Genepop format file containing genotypic data
#' @param param_file Desired name of parameter file produced
#' @param Ne_out_file Desired name of output from LDNe
#' @param matingsystem 0: Random mating, 1: Monogamy (LD method)
#' @param crit_vals minimum allele frequency cutoff. Defaults to c(0.02,0.05,0.1).
#' @author Zak Robinson, Contact: zachary.robinson(at)umontana.com
#' @return Path of created LDNe param file
#' @export




NeV2_LDNe_create<-function(input_file, param_file,NE_out_file, matingsystem=1,crit_vals= c(0.02,0.05,0.1)){


  ncrits<-length(crit_vals) #ncrits number of critical values
  method=1 #Method 1: LD method; refer to manual for more methods
  sink(file = param_file)
  cat(paste(as.character(method),"\t* LD Method"))
  cat("\n")
  cat(paste(as.character(ncrits),"\t* number of critical values"))
  cat("\n")
  cat(paste(as.character(crit_vals)))
  cat("\t* critical allele frequency values")
  cat("\n")
  cat("1\t*tabular output")
  cat("\n")
  cat("1\t* confidence intervals")
  cat("\n")
  cat(paste(as.character(matingsystem),"\t* 0: Random mating, 1: Monogamy (LD method)"))
  cat("\n")
  cat("0\t* max individual to be processed per pop, 0 for no limit")
  cat("\n")
  cat("0\t* Pop. range to run, given in pair. No limit if the first = 0")
  cat("\n")
  cat("0\t* Loc. ranges to run, given in pairs. No limit if the 1st = 0")
  cat("\n")
  cat(paste0(NE_out_file,"\t* output file name"))
  cat("\n")
  cat(paste(input_file,"\t* input file"))
  cat("\n*")

  sink()

  closeAllConnections()

  return(param_file)

}


