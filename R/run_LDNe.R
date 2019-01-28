#' Function to Run LDNe from NeEstimator 2.1
#' @param LDNe_params LDNe parameter file, best to use output from NEV2_LDNE_create_zlr
#' @author Zak Robinson, Contact: zachary.robinson(at)umontana.com
#' @export


run_LDNe<-function(LDNe_params){
  if(Sys.info()["sysname"]=="Windows"){
    Neestimator<-paste0(.libPaths()[length(.libPaths())],"/RLDNe/bin/Windows/Ne2-1.exe")
  }

  else{
    if(Sys.info()["sysname"]=="Linux"){
      Neestimator<-paste0(.libPaths()[length(.libPaths())],"/RLDNe/bin/Linux/Ne2-1L")
    }

  else{
    Neestimator<-paste0(.libPaths()[length(.libPaths())],"/RLDNe/bin/Mac/Ne2-1M")
  }
  }
  system(paste0(Neestimator," ", "c:", LDNe_params))
  return("output file location was specified in param file")
}
