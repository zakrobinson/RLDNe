#' A function that will generate or execute a system command that changes file permissions of NeEstimator V2.1 executables on Linux or Mac OSs.
#' @param execute Logical (Default: FALSE), if true a system command will be executed to add execuatable permissions to distributed executable files. If False, the necessary command is provided to the user.
#' @author Zak Robinson, Contact: zachary.robinson(at)umontana.com
#' @export


fix_permissions_RLDNe<-function(execute=F){

  if(Sys.info()["sysname"]=="Windows"){
    stop("This function generates Linux and Mac commands only")
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

  if(execute){
  system(paste0("chmod +x"," ",Neestimator))
    x<-paste0("This command was executed: chmod +x"," ",Neestimator)
  }else{
    x<-paste0("Run this command in commandline: chmod +x"," ",Neestimator)
  }

  return(x)
}


