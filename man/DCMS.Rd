% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distanceFunctions.R
\name{DCMS}
\alias{DCMS}
\title{De-correlated Composite of Multiple Signals (DCMS)}
\usage{
DCMS(dfv, column.nums = 1:ncol(dfv), subset = 1:nrow(dfv), S = NULL, dfp,
  column.nums.p = 1:ncol(dfp))
}
\arguments{
\item{dfv}{a data frame containing observations in rows and statistics in columns.}

\item{column.nums}{indexes the columns of the data frame that will be used to
calculate DCMS (all other columns are ignored). Only columns containing raw statistics should be used in the DCMS calculation, and not those containing p-values generated from raw statistics.}

\item{subset}{index the rows of the data frame that will be used to calculate the covariance matrix S (unless specified manually).}

\item{S}{the covariance matrix used to account for correlation between observations in the DCMS calculation. Leave as NULL to use the ordinary covariance matrix calculated using cov(dfv[subset,column.nums],use="pairwise.complete.obs").}

\item{pvalue.method}{an integer between 1 and 3 giving the method used to calculate p-values based on the description above.}
}
\description{
Calculates the DCMS for each row (locus, SNP) in the data frame. Data are subset prior to calculating distances (see details).
}
\details{
The selected columns of the input data frame (i.e. the columns specified by column.nums) are assumed to contain the raw test statistics, which are then converted into p-values in one of several ways. The following methods are available: 1) calculate p-values directly from the ranking of the statistics, 2) calculate p-values assuming the test statistic follows a normal distribution, 3) calculate p-values using the distribution of robust points as a null distribution. The covariance matrix used in the DCMS calculation can be specified directly through the argument S, or if S=NULL then this matrix is calculated directly from selected rows and columns of dfv.
}
\author{
Robert Verity \email{r.verity@imperial.ac.uk}
}
