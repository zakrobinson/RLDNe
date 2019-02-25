# RLDNe


## Description:
An R-package that conveniently interfaces with NeEstimator 2.1 (Do <i>et al.</i> 2014). NeEstimator V2.1 executables are distributed with this package freely, for non-commericial, educational purposes only. Visit http://www.molecularfisherieslaboratory.com.au/neestimator-software/ for more information on NeEstimator V2.1. Currently, this package is set up for the LD-method only.

## Install directions:
devtools::install_github(repo="zakrobinson/RLDNe")


## Example function sequence for the LD-method in NeEstimator 
library(RLDNe)

data("wgp_example")


gp_file<-write_genepop_zlr(loci = wgp_example[,3:ncol(wgp_example)],pops = wgp_example$pop,ind.ids = wgp_example$ind_id,folder = "",filename ="genepop_output.txt",missingVal = NA,ncode = 2,diploid = T)


param_files<- NeV2_LDNe_create(input_file = gp_file$Output_File ,param_file = "Ne_params.txt" ,NE_out_file = "Ne_out.txt")


run_LDNe(LDNe_params = param_files$param_file)

Ne_estimates<-readLDNe_tab(path = param_files$Ne_out_tab)

#### Example of how to convert from alleles per column format to genotypes per column format
data("wgp_example_2col")
genotype_examp<-alleles2genotypes(df = wgp_example_2col,allele_cols = 3:ncol(wgp_example_2col),allelesAsIntegers = TRUE)


## Citation for NeEstimator

Do, C., Waples, R. S., Peel, D., Macbeth, G. M., Tillett, B. J. & Ovenden, J. R. (2014). NeEstimator V2: re-implementation of software for the estimation of contemporary effective population size (Ne) from genetic data. Molecular Ecology Resources. 14, 209-214.
