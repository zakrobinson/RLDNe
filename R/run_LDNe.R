#' Function to Run LDNe from NeEstimator 2.1
#' @param LDNe_params LDNe parameter file, best to use output from NEV2_LDNE_create_zlr
#' @author Zak Robinson, Contact: zachary.robinson(at)umontana.com
#' @export


run_LDNe<-function(LDNe_params){
  if(Sys.info()["sysname"]=="Windows"){
    for(i in .libPaths()){
      if(file.exists(paste0(i,"/RLDNe/bin/windows64/Ne2-1.exe"))){
        Neestimator<-paste0(i,"/RLDNe/bin/windows64/Ne2-1.exe")
      }
    }
  }

  else{
    if(Sys.info()["sysname"]=="Linux"){
      for(i in .libPaths()){
        if(file.exists(paste0(i,"/RLDNe/bin/linux/Ne2-1L"))){
          Neestimator<-paste0(i,"/RLDNe/bin/linux/Ne2-1L")
        }
      }
    }

  else{
    for(i in .libPaths()){
      if(file.exists(paste0(i,"/RLDNe/bin/mac/Ne2-1M"))){
        Neestimator<-paste0(i,"/RLDNe/bin/mac/Ne2-1M")
      }
    }
  }
  }
  system(paste0(Neestimator," ", "c:", LDNe_params))
  return("output file location was specified in param file")
}
