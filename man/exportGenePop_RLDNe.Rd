% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportGenepop_RLDNe.R
\name{exportGenePop_RLDNe}
\alias{exportGenePop_RLDNe}
\title{Write Genepop file and create an RLDNe list object}
\usage{
exportGenePop_RLDNe(
  EFGLdata,
  filename = "genepop.gen",
  header = "genePopfile",
  pops = NULL,
  loci = NULL,
  useIndNames = FALSE
)
}
\arguments{
\item{filename}{the name of the file to write}

\item{header}{a string to use as the header line of the genepop file}

\item{pops}{a vector of pops to include. If not specified,
all pops are used.}

\item{loci}{a vector of loci to include. If not specified,
all loci are used.}

\item{useIndNames}{TRUE to use individual names as sample identifiers. Otherwise,
population names are used}

\item{x}{an EFGLdata object}
}
\value{
writes a genepop-style file and returns a RLDNe_data object, which is just a list of length 5.
 The RLDNe_data object merely streamlines the use of subsequent functions by keeping track of file names and population/loci names.
}
\description{
Write Genepop file and create an RLDNe list object
}
\examples{

data("wgp_example_2col")
colnames(wgp_example_2col) <- gsub("_(\\\\d)$",replacement = "\\\\.A\\\\1",colnames(wgp_example_2col))
efgl <- readInData(wgp_example_2col,genotypeStart = 3,pedigreeColumn = 1,nameColumn = 2)
rldne <- exportGenePop_RLDNe(EFGLdata = efgl)

}
