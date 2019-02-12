# RLDNe


## Description:
An R-package that conveniently interfaces with NeEstimator 2.1 (Do <i>et al.</i> 2014). NeEstimator V2.1 executables are distributed with this package freely, for non-commericial, educational purposes only. Visit http://www.molecularfisherieslaboratory.com.au/neestimator-software/ for more information on NeEstimator V2.1. Currently, this package is set up for the LD-method only.

## Install directions:
devtools::install_github(repo="zakrobinson/RLDNe")


## Example function sequence for the LD-method in NeEstimator 
data("writegenpop_examp")


x<-write_genepop_zlr(loci = wgp_example[,3:ncol(wgp_example)],pops = wgp_example$pop,ind.ids = wgp_example$ind_id,folder = "",filepath ="genepop_output.txt",missingVal = NA,ncode = 2,diploid = T)[1]


y<- NeV2_LDNe_create(input_file = x ,param_file = "params.txt" ,NE_out_file = "Neout.txt")


run_LDNe(LDNe_params = y)

readLDNe_tab(path = "NeoutxLD.txt")

## Citation for NeEstimator

Do, C., Waples, R. S., Peel, D., Macbeth, G. M., Tillett, B. J. & Ovenden, J. R. (2014). NeEstimator V2: re-implementation of software for the estimation of contemporary effective population size (Ne) from genetic data. Molecular Ecology Resources. 14, 209-214.
