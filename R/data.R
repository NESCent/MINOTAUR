

########################################################################

###################
## DOCUMENTATION ##
###################

#' Example Dataset: Human GWAS
#'
#' This dataset contains an example of output returned
#' from a human Genome-Wide Association Study (GWAS).
#'
#'
#' @format A data frame with 17,624 Single-Nucleotide Polymorphisms (SNPs)
#'  in the rows and 9 variables in the columns.
#'
#' @details \itemize{
#' \item \code{Chr} \itemize{\item A factor containing the chromosome on which each SNP locus is located.}
#' \item \code{SNP} \itemize{\item A ...}
#' \item \code{BP} \itemize{\item A ...}
#' \item \code{Trait1_Beta} \itemize{\item A ...}
#' \item \code{Trait1_P} \itemize{\item A ...}
#' \item \code{Trait2_Beta} \itemize{\item A ...}
#' \item \code{Trait2_P} \itemize{\item A ...}
#' \item \code{Trait3_Beta} \itemize{\item A ...}
#' \item \code{Trait3_P} \itemize{\item A ...}
#' }
#'
#'
#' @author Liuyang Wang \email{wallacewly@@gmail.com}
#' @author Caitlin Collins \email{caitlin.collins12@@imperial.ac.uk}
#'
#' @source Unpublished human GWAS analysis.
#'
#' @references Robert Verity*, Caitlin Collins*,
#' Daren C. Card, Sara M. Schaal, Liuyang Wang, & Katie E. Lotterhos.
#' MINOTAUR: an R package for visualizing and
#' calculating multivariate outliers in genomic datasets.
#' In Review.
#'
#'
#' @keywords datasets
#'

"HumanGWAS"

########################################################################
# f
# #' \describe{
# #'    \item{Chr}{Chromosome}
# #' }
# #' @value{
# #'    \item{Chr}{A factor containing the chromosome of each individual site.}
# #' }
# #' @item Chr A factor containing the chromosome of each individual site.
########################################################################

###################
## DOCUMENTATION ##
###################

#' Example Dataset: Simulated Expansion from Two Refugia
#'
#' This dataset contains population genetic data simulated
#' under a model of expansion from two refugia.
#'
#'
#' @format A data frame with 10,000 Single-Nucleotide Polymorphisms (SNPs)
#' in the rows and 15 variables in the columns.
#'
#' @details \itemize{
#' \item \code{SNPnames} \itemize{\item A factor containing the unique names of each SNP locus.}
#' \item \code{Corr.all} \itemize{\item A ...}
#' \item \code{FST.All} \itemize{\item A ...}
#' \item \code{s_high} \itemize{\item A ...}
#' \item \code{SNPIncluded} \itemize{\item A ...}
#' \item \code{He.LS} \itemize{\item A ...}
#' \item \code{He_samp} \itemize{\item A ...}
#' \item \code{log.bf} \itemize{\item A ...}
#' \item \code{rho} \itemize{\item A ...}
#' \item \code{xtx} \itemize{\item A ...}
#' \item \code{TW.Zscore} \itemize{\item A ...}
#' \item \code{Md} \itemize{\item A ...}
#' \item \code{Hd} \itemize{\item A ...}
#' \item \code{Kd} \itemize{\item A ...}
#' \item \code{Nd} \itemize{\item A ...}
#' }
#'
#'
#' @author Kathleen Lotterhos \email{k.lotterhos@@neu.edu}
#' @author Caitlin Collins \email{caitlin.collins12@@imperial.ac.uk}
#'
#' @source Simulated population genetic dataset.
#'
#' @references Lotterhos KE, Whitlock MC (2015)
#' The relative power of genome scans to detect local adaptation
#' depends on sampling design and statistical method.
#' Molecular Ecology, 24, 1031–1046.
#'
#' @references Robert Verity*, Caitlin Collins*,
#' Daren C. Card, Sara M. Schaal, Liuyang Wang, & Katie E. Lotterhos.
#' MINOTAUR: an R package for visualizing and
#' calculating multivariate outliers in genomic datasets.
#' In Review.
#'
#' @keywords datasets
#'

"TwoRefSim"

########################################################################




########################################################################

###################
## DOCUMENTATION ##
###################

#' Example Dataset: Nonparametric Inverse
#'
#' This is a simple, nonparametric two-variable dataset
#' with an inverse relationship between variables.
#'
#'
#' @format A data frame with 1,002 observations in the rows
#'  and 2 variables in the columns.
#'
#'
#' @details \itemize{
#' \item \code{x} \itemize{\item A simulated numeric variable.}
#' \item \code{y} \itemize{\item A simulated numeric variable.}
#' }
#'
#' @author Robert Verity \email{r.verity@@imperial.ac.uk}
#' @author Caitlin Collins \email{caitlin.collins12@@imperial.ac.uk}
#'
#' @source Simulated nonparametric dataset.
#'
#' @references Robert Verity*, Caitlin Collins*,
#' Daren C. Card, Sara M. Schaal, Liuyang Wang, & Katie E. Lotterhos.
#' MINOTAUR: an R package for visualizing and
#' calculating multivariate outliers in genomic datasets.
#' In Review.
#'
#' @keywords datasets
#'

"NonParametricInverse"

########################################################################



########################################################################

###################
## DOCUMENTATION ##
###################

#' Example Dataset: Nonparametric Multimodal
#'
#' This is a simple, nonparametric two-variable dataset
#' that is highly multimodal.
#'
#'
#' @format A data frame with 1,000 observations in the rows
#'  and 2 variables in the columns.
#'
#' @details \itemize{
#' \item \code{x} \itemize{\item A simulated numeric variable.}
#' \item \code{y} \itemize{\item A simulated numeric variable.}
#' }
#'
#' @author Robert Verity \email{r.verity@@imperial.ac.uk}
#' @author Caitlin Collins \email{caitlin.collins12@@imperial.ac.uk}
#'
#' @source Simulated nonparametric dataset; unpublished.
#'
#' @references Robert Verity*, Caitlin Collins*,
#' Daren C. Card, Sara M. Schaal, Liuyang Wang, & Katie E. Lotterhos.
#' MINOTAUR: an R package for visualizing and
#' calculating multivariate outliers in genomic datasets.
#' In Review.
#'
#' @keywords datasets
#'

"NonParametricMultimodal"

########################################################################

# f
# #' @return{
# #'  \item{x}{A numeric variable.}
# #'  \item{y}{A numeric variable.}
# #' }



