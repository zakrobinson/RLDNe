#'Write Genepop file
#'
#' This function writes a genepop file from hierfstat format data.
#'
#' @param loci Data frame of genotypes in hierfstat format.
#' @param ind.ids Vector of individual Identifiers ( must be equal in length to rows of genotypes)
#' @param pops Vector of (equal length to rows of genotypes) designating populations for each individual
#' @param folder Directory for saving genepop output file
#' @param filename File name for output file also the description at top of genepop file
#' @param missingVal specifies the value of missing data in loci object. Defaults to NA.
#' @param ncode 2 x number of characters per allele (i.e. 22 = ncode = 2, since 1 character per allele going into the function * 2)
#' @param diploid logical for whether the data is diploid or not
#' @author Zak Robinson, Contact: zachary.robinson(at)umontana.com
#' @return returns a list of length 2. The output contains the filename of created genepop file and information for identifying populations following Genepop and NeEstimator Analysis.
#' @examples
#' data("wgp_example")
#'
#' write_genepop_zlr(loci = wgp_example[,3:ncol(wgp_example)],pops = wgp_example$pop,
#' ind.ids = wgp_example$ind_id,folder = "",filename ="genepop_output.txt",missingVal = NA,
#' ncode = 2,diploid = TRUE)
#'
#' @export




write_genepop_zlr<-function(loci,pops,ind.ids,folder=getwd(),filename,missingVal=NA,ncode=2,diploid=T){




  if(length(unlist(strsplit(filename,split = "\\s",perl = T)))>1){
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
    on.exit(expr = suppressWarnings(sink())) #don't leave connections open during catastrophic failure
    #####open file connection
    sink(file = paste0(folder,filename),append = F)
    # write header of file
    cat(filename,sep = "\n")
    x<-paste0("L",1:ncol(loci))
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
      popdecode$ld_popname[i]<-tmp[1]
    }
    sink()

    output<-list(paste0(folder,filename),popdecode)
    names(output)<-c("Output_File","Pop_decode")
    return(output)

  }

#### end write genepop function
