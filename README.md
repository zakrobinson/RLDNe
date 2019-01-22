# RLDNe
## Description:
An R-package that conveniently interfaces with NeEstimator 2.1

## Install directions:
devtools::install_github(repo="zakrobinson/RLDNe")


## Example function sequence for the LDNe-method 
data("writegenepop_examp")


x<-write_genepop_zlr(loci = wgp_example[,3:ncol(wgp_example)],pops = wgp_example$pop,ind.ids = wgp_example$ind_id,folder = "",filepath ="genepop_output.txt",missingVal = NA,ncode = 2,diploid = T)[1]


y<- NeV2_LDNe_create(input_file = x ,param_file = "params.txt" ,NE_out_file = "Neout.txt")


run_LDNe(LDNe_params = y)
