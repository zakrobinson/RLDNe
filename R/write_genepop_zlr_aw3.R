#'Write Genepop file
#'
#' This function writes a genepop file from hierfstat format data.
#'
#' @param loci data.frame of genotypes in hierfstat format
#' @param ind.ids vector of individual Identifiers ( must be equal in length to rows of genotypes)
#' @param pops vector of (equal length to rows of genotypes) designating populations for each individual
#' @param folder dir for saving genepop output file
#' @param filepath file name for output file also the description at top of genepop file
#' @param missingVal specifies the value of missing data in loci object
#' @param ncode 2 x number of characters per allele (i.e. 22 = ncode = 2, since 1 character per allele going into the function * 2)
#' @param diploid logical for whether the data is diploid or not
#' @author Zak Robinson, Contact: zachary.robinson(at)umontana.com
#' @return returns a list of length 2. The output contains the filepath of created genepop file and information for identifying populations following genepop analysis.
#' @examples
#' data("writegenepop_examp")
#' write_genepop_zlr(loci = wgp_example[,3:ncol(wgp_example)],pops = wgp_example$pop,ind.ids = wgp_example$ind_id,folder = "",filepath ="genepop_output.txt",missingVal = NA,ncode = 2,diploid = T)
#' @export
#'
#'
write_genepop_zlr<-function(loci,pops,ind.ids,folder,filepath,missingVal=NA,ncode=2,diploid=T){

## Note: if ncode is 2 and diploid is true then the alleles will be recoded to 2 digits


  if(length(unlist(strsplit(filepath,split = "\\s",perl = T)))>1){
    warning("genepop doesn't like spaces in file names")
  }


  d2_snp_fix<-function(string,missingval=missingVal){
    if(is.na(missingval)){
      if(!is.na(string)){
        ns<-paste0("0",substring(text = string,1,1),"0",substring(text = string,2,2))
        return(ns)
      }
      else{return(string)}
    }
    else{
      if(string!=missingval){
        ns<-paste0("0",substring(text = string,1,1),"0",substring(text = string,2,2))
        return(ns)
      }
      else{return(string)}
    }
  }

  if(ncode==2 & diploid==T){

    loci<-apply(loci,MARGIN = c(1,2),FUN = d2_snp_fix)
    ncode<-4
  }



  recode_missing_gp<-function(x,ncod=ncode, missingval=missingVal){
    if(!is.na(missingval)){
      if(x==missingval){
        x<-paste0(rep(0,times=ncod),collapse = "")
      }
    }

    if(is.na(x)){
      x<-paste0(rep(0,times=ncod),collapse = "")
    }
    return(x)
  }

  bacon<-function(z,fin=lastid){# applied across rows to produce genotypes
    if(z["pops"]==pop_levels[i]){
      cat(paste(z["ind.ids"],","," "))
      cat(paste(z[colnames(loci)]))
      if(z["ind.ids"]==fin){
        cat("  ")
      }
      else(cat("\n"))
    }
  }



  loci<-as.data.frame(apply(X = loci,MARGIN = c(1,2),FUN = recode_missing_gp))



    pop_levels<-unique(pops)

    #####open file connection
    sink(file = paste0(folder,filepath),append = F)
    # write header of file
    cat(filepath,sep = "\n")
    x<-paste("Locus",1:ncol(loci),sep = "-")
    y<-vector()
    for(i in 1:length(x)){
      if(i<length(x)){
        y[i]<-paste0(x[i],","," ")
      }
    }
    x<-c(y,x[length(x)])
    cat(x,sep ="")
    cat("\n")



    ######write individual.ids and genotypes#######

    x<-cbind(pops,ind.ids,loci)
    lastid<-x[nrow(x),"ind.ids"]
    popdecode<-data.frame(popname=rep(NA,length(pop_levels)),gp_popname=rep(NA,length(pop_levels)))
    for(i in 1:length(pop_levels)){
      cat("Pop\n")
      apply(X = x,MARGIN=1,FUN =bacon)

      popdecode$popname[i]<-as.character(pop_levels[i])
      tmp<-as.character(x[which(x$pops==pop_levels[i]),"ind.ids"])
      popdecode$gp_popname[i]<-tmp[length(tmp)]
<<<<<<< HEAD
      popdecode$ld_popname[i]<-tmp[1]
=======
>>>>>>> 5de65dab29be2c1b2abd83229982e55a81adb579
    }

    sink()


    output<-list(paste0(folder,filepath),popdecode)
    names(output)<-c("Output_File","Pop_decode")
    return(output)
    closeAllConnections()
  }

#### end write genepop function
