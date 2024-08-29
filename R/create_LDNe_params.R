#'Write LDNe Parameter File
#'
#' This function writes a parameter for LD Method in NeEstmator V2.1
#'
#' @param x RLDNe_data object or genepop file name
#' @param param_file Desired name of parameter file produced
#' @param NE_out_file Desired name of output file from LDNe
#' @param matingsystem 0: Random mating, 1: Monogamy (LD method). Defaults to Random Mating.
#' @param crit_vals minimum allele frequency cutoff. Defaults to c(0.02,0.05,0.1).
#' @author Zak Robinson, Contact: zachary.robinson(at)umontana.com
#' @return Path of created LDNe param file
#' @export




create_LDNe_params <- function(x, param_file="LDNe_params.txt",NE_out_file="LDNEresults.txt", matingsystem=0,crit_vals= c(0.02,0.05,0.1)){
  if(class(x)=="RLDNe_data"){
    input_file=x$GP_filename
    classy=T
  }else if(is.character(x)&length(x)==1){
    input_file=x
  }else{
    stop("x should be a RLDNe_data object or a file path")
  }
  ncrits <- length(crit_vals) #ncrits number of critical values
  method = 1 #Method 1: LD method; refer to manual for more methods
  outLines <- c(
          paste(as.character(method),"\t* LD Method"),
          paste(as.character(ncrits),"\t* number of critical values"),
          paste0(paste(as.character(crit_vals),collapse = " "),"\t* critical allele frequency values"),
          "1\t* tabular output",
          "1\t* confidence intervals",
          paste(as.character(matingsystem),"\t* 0: Random mating, 1: Monogamy (LD method)"),
          "0\t* max individual to be processed per pop, 0 for no limit",
          "0\t* Pop. range to run, given in pair. No limit if the first = 0",
          "0\t* Loc. ranges to run, given in pairs. No limit if the 1st = 0",
          paste0(NE_out_file,"\t* output file name"),
          paste(input_file,"\t* input file")
)


if(length(grep(x = NE_out_file,pattern = "\\.txt$|\\.out$|\\.csv$)"))>0){
  theout<-gsub(x = NE_out_file,pattern = "\\.txt$|\\.out$|\\.csv$)",replacement = "xLD.txt")
}else{
    theout<-paste0(NE_out_file,"xLD.txt")
}
writeLines(outLines,con = param_file)
if(classy){
  x$LDNe_paramfile <- param_file
  x$LDNeOutFile <- theout
}else{
x <- list(GP_filename=input_file,Pop_Decode=NULL,Locus_Decode=NULL,LDNe_paramfile=param_file,LDNeOutFile=theout)
class(x) <- "RLDNe_data"

}
  return(x)
}







