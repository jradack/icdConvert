% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping.R
\name{map_stage}
\alias{map_stage}
\title{Map Stage}
\usage{
map_stage(
  codes,
  icdVer_dest,
  code_type = c("dg", "pc"),
  direction = c("forward", "backward")
)
}
\arguments{
\item{codes}{A vector of ICD diagnosis or procedure codes.}

\item{icdVer_dest}{A number, either 9 or 10, indicating the destination ICD version.}

\item{code_type}{A string, either "dg" or "pc," indicating the codes are diagnosis or procedure, respectively.}

\item{direction}{A string, either "forward" or "backward", to indicate the direction of the mapping.}
}
\value{
A dataframe with the source code, the matching destination code, and additional columns for cases where multiple codes represent a single source code.
}
\description{
Performs a single stage of ICD code mapping, either the forward map or the
backward map. The forward map takes the source codes and maps to the destination
codes using the GEM, while the backward map finds all the codes mapped to the
source codes in the GEM of the opposite direction.
}
