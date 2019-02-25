#' Convert Alleles to Genotypes (hierfstat format)
#'
#' A function that will return a data frame in genotype per column format from a data frame in allele per column format.
#'
#'
#'@param df Data frame that contains genotypic data and metadata
#'@param allele_cols Numeric column identifiers of loci
#'@param allelesAsIntegers For alleles with zeros use FALSE. If FALSE this will eliminate the loss of leading zeros, but will not be usable in 'hierfstat'. When TRUE the genotypes are converted to integers for 'hierfstat' package.
#'@author Zak Robinson, Contact: zachary.robinson(at)umontana.com
#'@return Data frame in genotype per column format
#'@examples
#' data("wgp_example_2col")
#' genotypes<-alleles2genotypes(df=wgp_example_2col,allele_cols = 3:ncol(wgp_example_2col),
#' allelesAsIntegers=TRUE)
#'@export




alleles2genotypes<-function(df,allele_cols,allelesAsIntegers=TRUE){



  meta_data_cols<-which(!(1:ncol(df) %in% allele_cols))
  alleles<-df[,allele_cols]


### Error Checks
  if(length(allele_cols) %% 2){stop("Uneven Number of Alleles Submitted")}
  if(length(unique(unique(apply(X = alleles,MARGIN = 2,nchar)[1,])))>1){
    warning("Alleles are different character lengths: NA encoding other than NA ?")}
###

  group<-matrix(data=allele_cols,byrow=TRUE,ncol=2)

  locinames<-colnames(df)[group[,1]]

  #group
  group<-split(group,rep(1:nrow(group)))


  make_loci<-function(x){
    paste0(as.character(df[,x[1]]),as.character(df[,x[2]]))
  }

  loc_list<-lapply(group,FUN =make_loci)

  loci_mat<-matrix(unlist(loc_list),nrow = dim(alleles)[1],ncol = dim(alleles)[2]/2)


  out_df<-as.data.frame(matrix(ncol=(ncol(loci_mat)+length(meta_data_cols)),nrow = nrow(loci_mat)),stringsAsFactors=FALSE)

  out_df[,min(allele_cols):((length(allele_cols)/2)+(min(allele_cols)-1))]<-loci_mat
  colnames(out_df)[min(allele_cols):((length(allele_cols)/2)+(min(allele_cols)-1))]<-locinames

  out_df

  for(i in meta_data_cols){
    if(i>min(allele_cols)){
      col_correct<-i-(length(allele_cols)/2)
      out_df[,col_correct]<-df[,i]
      colnames(out_df)[col_correct]<-colnames(df)[i]
    }
    else{
      out_df[,i]<-df[,i]
      colnames(out_df)[i]<-colnames(df)[i]

    }
  }


if(allelesAsIntegers){
suppressWarnings(out_df[,min(allele_cols):((length(allele_cols)/2)+(min(allele_cols)-1))]<-apply(out_df[,min(allele_cols):((length(allele_cols)/2)+(min(allele_cols)-1))],c(1,2),as.integer))
}


  return(out_df)
}


