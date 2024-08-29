# RLDNe

## Description:

An R-package that conveniently interfaces with NeEstimator 2.1 (Do <i>et al.</i> 2014). NeEstimator V2.1 executables are distributed with this package freely, for non-commericial, educational purposes only. Visit <http://www.molecularfisherieslaboratory.com.au/neestimator-software/> for more information on NeEstimator V2.1. Currently, this package is set up for the LD-method only. Given that NeEstimator is quite straightfoward to run, a common use case for this package is when you want to simulate/downsample and run many iterations of the LD-method via R.

## Install directions:

devtools::install_github(repo="zakrobinson/RLDNe")

## Example function sequence for the LD-method in NeEstimator

Note that the newest verison makes use of the EFGLmh package for the initial data object and exportGenepop_RLDNe is just a repurposed version of EFGLmh::exportGenepop. More information is available at: <https://github.com/delomast/EFGLmh>.

```         
library(RLDNe)

data("wgp_example_2col")

colnames(wgp_example_2col) <- gsub("_(\\d)$",replacement = "\\.A\\1",colnames(wgp_example_2col))

efgl <- readInData(wgp_example_2col,genotypeStart = 3,pedigreeColumn = 1,nameColumn = 2)
rldne <- exportGenePop_RLDNe(EFGLdata = efgl)
rldne <- create_LDNe_params(rldne)
std_out <- run_LDNe(rldne)
df <- read_LDNeOutFile(rldne)
```

#### Preprocessing data and converting among alleles per column format and genotypes per column format

```         
data("wgp_example_2col")

genotypes <- alleles2genotypes(wgp_example_2col,
alleles = grep("_\\d$",colnames(wgp_example_2col)),
name_pattern = "(.+)_(.$)",
suffix = T,
Adelim = "")

data("wgp_example")

alleles <- genotypes2alleles(wgp_example,
loci = grep("locus",colnames(wgp_example)),
fixed_A = 1)
```

## Citation for NeEstimator

Do, C., Waples, R. S., Peel, D., Macbeth, G. M., Tillett, B. J. & Ovenden, J. R. (2014). NeEstimator V2: re-implementation of software for the estimation of contemporary effective population size (Ne) from genetic data. Molecular Ecology Resources. 14, 209-214.
