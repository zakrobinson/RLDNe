#' Convert wider format Alleles to wide format Genotypes (hierfstat format)
#'
#' A function that will return a data frame in genotype per column format from a data frame in allele per column format.
#'
#'
#'@param df Data frame that contains genotypic data and metadata
#'@param alleles Numeric column positions or names of loci
#'@param name_sep The locus-allele seperator in column names (e.g., "_" in Loc1_A1,Loc1_A2...LocN_A1,LocN_A2).
#'@param name_pattern The locus-allele identifiers in column names specified by 2 regex capture groups (e.g., "(.+)_(A.$) performs the same as name_sep="_"). This is useful when the locus-allele separator string is repeated in column names (e.g, Loc_1_A1). This argument takes precedent over name_sep.
#'@param suffix Logical that allele identifier is a suffix (e.g., Loc1_A1 instead of A1_Loc1). Default=T.
#'@param Adelim Desired delimiter string between alleles in output. The default="" is not ideal when allele character lengths vary.
#'@author Zak Robinson, Contact: <zrobinson@critfc.org>
#'@return Data frame in genotype per column format
#'@details This function is meant to conveniently wrap tidyr and dplyr operations to convert data between two common wide formats. At the moment it works for diploid data when there is a common locus-allele identifier. If the input varies (e.g., Loc1.a1 Loc2_A1) you will want to gsub your way to uniformity or write your own solution.
#'@seealso \code{\link{genotypes2alleles}}
#'@examples
#' data("wgp_example_2col")
#' genotypes<-alleles2genotypes(wgp_example_2col,alleles = grep("_\\d$",colnames(wgp_example_2col)),name_pattern = "(.+)_(.$)",suffix = T,Adelim = ":")
#' @import dplyr
#' @import tidyr
#'@export


alleles2genotypes <- function(df,alleles,name_sep=NULL,name_pattern=NULL,suffix=T,Adelim=""){

  if(is.null(name_pattern)&is.null(name_sep))stop("name_sep or name_pattern must be specified")
  if(!is.null(name_pattern)&!is.null(name_sep))warning("name_sep and name_pattern are both specified. name_pattern takes precedent")
  if(is.null(name_pattern)){
    seps <- unlist(strsplit(name_sep,split = ""))
    sepsEsc <- ifelse(seps %in% c(".", "*", "+", "?", "^", "$", "(", ")", "[", "]", "{", "}", "|"),paste0('\\',seps),seps)
    name_sep <- paste(sepsEsc,collapse = "")
    name_pattern=paste0("(.+)",name_sep,"(.+)")
  }
if(length(alleles)%%2!=0)stop("Odd number of alleles provided, we're dealing with diploid data here?")
if(suffix){
df_genos <- df %>%
  pivot_longer(
    cols = all_of(alleles),
    names_to = c("locus", "allele"),
    names_pattern = name_pattern,
    values_to = "genotype"
  ) %>%
  pivot_wider(
    names_from = allele,
    values_from = genotype,
    names_sort = FALSE
  ) %>%
  rename_with(.cols = c(ncol(.)-1,ncol(.)),.fn = function(x){return(c("A1","A2"))}) %>%
  mutate(genotype=if_else(is.na(A1)|is.na(A2),
                          NA,
                          paste(A1,A2,sep = Adelim))) %>%
  select(-A1,-A2) %>%
  pivot_wider(names_from = locus,values_from = genotype)
} else{
  df_genos <- df %>%
    pivot_longer(
      cols = all_of(alleles),
      names_to = c("allele","locus"),
      names_pattern = name_pattern,
      values_to = "genotype"
    ) %>%
    pivot_wider(
      names_from = allele,
      values_from = genotype,
      names_sort = FALSE
    ) %>%
    rename_with(.cols = c(ncol(.)-1,ncol(.)),.fn = function(x){return(c("A1","A2"))}) %>%
    mutate(genotype=if_else(is.na(A1)|is.na(A2),
                            NA,
                            paste(A1,A2,sep = Adelim))) %>%
    select(-A1,-A2) %>%
    pivot_wider(names_from = locus,values_from = genotype)
}
return(df_genos)
}


