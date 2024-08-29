#' Function to Run LDNe from NeEstimator 2.1 using 64-bit executables
#' @param x An RLDNe object or file path to parameter file
#' @author Zak Robinson, Contact: <zrobinson@critfc.org>
#' @export


run_LDNe<-function(x){
  if(class(x)=="RLDNe_data"){
    if(is.null(x$LDNe_paramfile)){
      stop("LDNe_paramfile is NULL; have you ran create_LDNe_params() ?")
    }else{
    LDNe_params=x$LDNe_paramfile}
  }else if(is.character(x)&length(x)==1){
    LDNe_params=x
  }else{
    stop("x should be a RLDNe_data object or a file path")
  }

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
  out <- system2(command = Neestimator,args = paste0("c:", LDNe_params),stdout = TRUE)
  return(out)
}


#' Function to Run LDNe from NeEstimator 2.1 using 32-bit executables
#' @param x An RLDNe object or file path to parameter file
#' @author Zak Robinson, Contact: <zrobinson@critfc.org>
#' @export


run_LDNe_32bit <-function(x){

  if(class(x)=="RLDNe_data"){
    if(is.null(x$LDNe_paramfile)){
      stop("LDNe_paramfile is NULL; have you ran create_LDNe_params() ?")
    }else{
      LDNe_params=x$LDNe_paramfile}
  }else if(is.character(x)&length(x)==1){
    LDNe_params=x
  }else{
    stop("x should be a RLDNe_data object or a file path")
  }

  if(Sys.info()["sysname"]=="Windows"){
    for(i in .libPaths()){
      if(file.exists(paste0(i,"/RLDNe/bin/windows32bit/Ne2-1.exe"))){
        Neestimator<-paste0(i,"/RLDNe/bin/windows32bit/Ne2-1.exe")
      }
    }
  }

  else{
    if(Sys.info()["sysname"]=="Linux"){
      for(i in .libPaths()){
        if(file.exists(paste0(i,"/RLDNe/bin/linux32bit/Ne2-1L"))){
          Neestimator<-paste0(i,"/RLDNe/bin/linux32bit/Ne2-1L")
        }
      }
    }

    else{
      for(i in .libPaths()){
        if(file.exists(paste0(i,"/RLDNe/bin/mac32bit/Ne2-1M"))){
          Neestimator<-paste0(i,"/RLDNe/bin/mac32bit/Ne2-1M")
        }
      }
    }
  }
  out <- system2(command = Neestimator,args = paste0("c:", LDNe_params),stdout = TRUE)
  return(out)
}
