#' Convert wide format Genotypes to wider format Alleles
#'
#' A function that will return a data frame in allele per column format from genotype per column format.
#'
#'
#'@param df Data frame that contains genotypic data and metadata
#'@param loci Numeric column positions or names of loci
#'@param fixed_A The number of characters of alleles in input. A diploid genotype "AT" has a fixed_A=1. Only use when the delimiter between alleles is "".
#'@param delim A non-zero length character string separating alleles in input. For example, ":" for "A:T" or "-" 114-116"
#'@param name_sep The desired locus allele name separator for output column names e.g., "_" for Loc1_A1. Defaults to "."
#'@author Zak Robinson, Contact: <zrobinson@critfc.org>
#'@return Data frame in allele per column format
#'@details This function is meant to conveniently wrap tidyr and dplyr operations to convert data between two common wide formats. At present, it's just set up for diploid data.
#'@seealso \code{\link{alleles2genotypes}}
#'@examples
#' data("wgp_example")
#' alleles <- genotypes2alleles(wgp_example,loci = grep("locus",colnames(wgp_example)),fixed_A = 1)
#' @import dplyr
#' @import tidyr
#'@export



genotypes2alleles <-function(df,loci,fixed_A=NULL,delim=NULL,name_sep="."){

  if(is.null(delim)&is.null(fixed_A)) stop("fixed_A or delim must be specified")
  if(!is.null(delim)&!is.null(fixed_A)) message("delim takes precedent over fixed_A")


  if(!is.null(delim)){
    NAtest <- df %>% select(all_of(loci)) %>% reframe(across(everything(),~sum(grepl(delim,.x))/sum(!is.na(.x))))
    if(any(NAtest<1)) message("Data suggests some non-missing genotypes do not contain delimiter")
    alleles_df <-  df %>%
      tidyr::pivot_longer(cols = loci,
                          names_to = "locus",
                          values_to = "genotype") %>%
      tidyr::separate_wider_delim(genotype,
                                  delim = delim,
                                  names = c("A1","A2")) %>%
      tidyr::pivot_longer(cols = c(A1,A2),
                          names_to = "allele") %>%
      tidyr::pivot_wider(names_from = c(locus,allele),
                         values_from = value,
                         names_sort = F)
  }else{
    Nchartest <- df %>% select(loci) %>% reframe(across(everything(),~sum(nchar(na.omit(.x))==(2*fixed_A))/sum(!is.na(.x))))
    if(any(Nchartest<1)) message("Data suggests some non-missing genotypes have lengths that do not equal 2*fixed_A")
    alleles_df <-  df %>%
      tidyr::pivot_longer(cols = loci,
                          names_to = "locus",
                          values_to = "genotype") %>%
      tidyr::separate_wider_position(genotype,
                                     widths = c(A1=fixed_A,A2=fixed_A)) %>%
      tidyr::pivot_longer(cols = c(A1,A2),
                          names_to = "allele") %>%
      tidyr::pivot_wider(names_from = c(locus,allele),
                         values_from = value,names_sort = F,names_sep = name_sep)
  }

  return(alleles_df)
}



