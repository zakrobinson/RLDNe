% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_LDNe_params.R
\name{create_LDNe_params}
\alias{create_LDNe_params}
\title{Write LDNe Parameter File}
\usage{
create_LDNe_params(
  x,
  param_file = "LDNe_params.txt",
  NE_out_file = "LDNEresults.txt",
  matingsystem = 0,
  crit_vals = c(0.02, 0.05, 0.1)
)
}
\arguments{
\item{x}{RLDNe_data object or genepop file name}

\item{param_file}{Desired name of parameter file produced}

\item{NE_out_file}{Desired name of output file from LDNe}

\item{matingsystem}{0: Random mating, 1: Monogamy (LD method). Defaults to Random Mating.}

\item{crit_vals}{minimum allele frequency cutoff. Defaults to c(0.02,0.05,0.1).}
}
\value{
Path of created LDNe param file
}
\description{
This function writes a parameter for LD Method in NeEstmator V2.1
}
\author{
Zak Robinson, Contact: zachary.robinson(at)umontana.com
}
