#'Write Genepop file and create an RLDNe list object
#'
#'
#' @param x an EFGLdata object
#' @param filename the name of the file to write
#' @param header a string to use as the header line of the genepop file
#' @param pops a vector of pops to include. If not specified,
#'   all pops are used.
#' @param loci a vector of loci to include. If not specified,
#'   all loci are used.
#' @param useIndNames TRUE to use individual names as sample identifiers. Otherwise,
#'   population names are used
#' @return writes a genepop-style file and returns a RLDNe_data object, which is just a list of length 5.
#'  The RLDNe_data object merely streamlines the use of subsequent functions by keeping track of file names and population/loci names.
#' @examples
#'
#' data("wgp_example_2col")
#' colnames(wgp_example_2col) <- gsub("_(\\d)$",replacement = "\\.A\\1",colnames(wgp_example_2col))
#' efgl <- readInData(wgp_example_2col,genotypeStart = 3,pedigreeColumn = 1,nameColumn = 2)
#' rldne <- exportGenePop_RLDNe(EFGLdata = efgl)
#'
#'@import EFGLmh
#' @export



exportGenePop_RLDNe <- function(EFGLdata, filename="genepop.gen", header = "genePopfile", pops = NULL,
                                loci = NULL, useIndNames = FALSE){
  if (ncol(EFGLdata$genotypes) < 3)
    stop("no genotypes")
  if (is.null(loci))
    loci <- getLoci(EFGLdata)
  if (is.null(pops))
    pops <- getPops(EFGLdata)
  if (any(!pops %in% getPops(EFGLdata)))
    stop("not all pops are in this EFGLdata object")
  loci2 <- c(paste0(loci, ".A1"), paste0(loci, ".A2"))
  l <- colnames(EFGLdata$genotypes)[3:ncol(EFGLdata$genotypes)]
  if (any(!loci2 %in% l))
    stop("one or more loci were not found in input")
  g <- EFGLdata$genotypes %>% filter(Pop %in% pops)
  if (useIndNames) {
    gp <- g %>% select(Pop, Ind) %>% mutate(Ind = paste0(Ind,
                                                         ","))
    popdecode <- full_join(gp %>% group_by(Pop) %>% slice_tail(n=1) %>% rename(GP_NAME=Ind) %>% ungroup(),
                           gp %>% group_by(Pop) %>% slice_head(n=1) %>% rename(LD_NAME=Ind) %>% ungroup(),
                           join_by(Pop)) %>%
                 mutate(across(everything(),~gsub(",$","",.x))) %>%
                 mutate(LD_NAME=substr(LD_NAME,nchar(LD_NAME)-10+1,nchar(LD_NAME)))
  }
  else {
    gp <- g %>% select(Pop) %>% mutate(Ind = paste0(Pop,
                                                    ","))
    if(max(nchar(pops))>10){
      newpops <- paste0("pop",sprintf("%05d",1:length(pops)))
      popdecode <- tibble(Pop=pops,GP_NAME=newpops,LD_NAME=newpops)
      gp <- gp %>% mutate(Ind=popdecode$GP_NAME[match(gsub(",$","",Ind),popdecode$Pop)])
    }else{
      popdecode<-NULL}

  }
  to_remove <- c()
  for (i in loci) {
    a <- g %>% select(paste0(i, ".A1"), paste0(i, ".A2"))
    alleleIndex <- a %>% tidyr::gather(locus, allele, 1:2) %>%
      filter(!is.na(allele)) %>% pull(allele) %>% unique
    if (length(alleleIndex) < 1) {
      warning("all missing data for locus", i, ". Skipping this locus.")
      to_remove <- c(to_remove, i)
      next
    }
    if (length(alleleIndex) > 99)
      stop("More than 99 alleles at locus ", i)
    alleleIndex <- tibble(allele = alleleIndex, index = gsub(" ",
                                                             "0", format(1:length(alleleIndex), width = 2)))
    a1 <- a %>% pull(1)
    a2 <- a %>% pull(2)
    a1 <- alleleIndex$index[match(a1, alleleIndex$allele)]
    a2 <- alleleIndex$index[match(a2, alleleIndex$allele)]
    a1[is.na(a1)] <- "00"
    a2[is.na(a2)] <- "00"
    gp <- gp %>% tibble::add_column(`:=`(!!i, paste0(a1,
                                                     a2)))
  }
  loci <- loci[!loci %in% to_remove]
  newLoci <- paste0("loc",sprintf("%05d",1:length(loci)))
  loci_decode <- tibble(LocusName=loci,GP_NAME=newLoci)
  cat(header, "\n", file = filename, append = FALSE, sep = "")
  for (i in newLoci){ cat(i, "\n", sep = "", file = filename, append = TRUE)}
  for (p in pops) {
    cat("POP\n", file = filename, append = TRUE)
    write.table(filter(gp, Pop == p) %>% select(-Pop), file = filename,
                sep = " ", append = TRUE, col.names = FALSE, row.names = FALSE,
                quote = FALSE)
  }
  RLDNe_data <- list(GP_filename=filename,Pop_Decode=popdecode,Locus_Decode=loci_decode,LDNe_paramfile=NULL,LDNeOutFile=NULL)
  class(RLDNe_data) <- "RLDNe_data"
  return(RLDNe_data)
}





#' print method for RLDNe_data
#' @param x an RLDNe_data object
#' @param ... ignored
#' @export


print.RLDNe_data <- function(x,...){
  cat("##################\nRLDNe List Object\n##################\n\n")
  cat("$GP_filename: ",x$GP_filename,"\n\n")
  if(is.null(x$Pop_Decode)){
    cat("$Pop_Decode: <empty> \n\n")
  }else{
    cat("$Pop_Decode: tibble object to restore original population names to output\n\n")
  }

  if(is.null(x$Locus_Decode)){
   cat("$Locus_Decode: <empty> \n\n")
  }else{
  cat("$Locus_Decode: tibble object to restore original locus names to genepop output\n\n")
  }
   if(is.null(x$LDNe_paramfile)){
    cat("$LDNe_paramfile: <empty> ; Run create_LDNe_params() to generate\n\n")
  }else{
    cat("$LDNe_paramfile: ",x$LDNe_paramfile, "\n\n")
  }
  if(is.null(x$LDNeOutFile)){
    cat("$LDNeOutFile: <empty> ; Run create_LDNe_params() and run_LDNe() to generate\n\n")
  }else if(!file.exists(x$LDNeOutFile)){
   cat("$LDNeOutFile: ", x$LDNeOutFile ," : but file does not exist run_LDNe() to generate\n\n")
  }else{
    cat("$LDNeOutFile: ",x$LDNeOutFile)
  }
cat("\n")
}






















