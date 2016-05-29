

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
#' \item \code{SNP} \itemize{\item SNP (single nucleotide polymorphism) ID}
#' \item \code{BP} \itemize{\item Base Pair (location on the chromosome)}
#' \item \code{Trait1_Beta} \itemize{\item Slope from a GWAS analysis with Trait 1}
#' \item \code{Trait1_P} \itemize{\item P-value from a GWAS analysis with Trait 1}
#' \item \code{Trait2_Beta} \itemize{\item Slope from a GWAS analysis with Trait 2}
#' \item \code{Trait2_P} \itemize{\item P-value from a GWAS analysis with Trait 2}
#' \item \code{Trait3_Beta} \itemize{\item Slope from a GWAS analysis with Trait 3}
#' \item \code{Trait3_P} \itemize{\item P-value from a GWAS analysis with Trait 3}
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
#' \item \code{Corr.all} \itemize{\item Correlation between allele frequencies and the environment, based on all 129600 demes on the landscape and infinite sampling of individuals}
#' \item \code{FST.All} \itemize{\item FST for the SNP based on all 129600 demes on the landscape and infinite sampling of individuals}
#' \item \code{s_high} \itemize{\item The selection coefficient for the SNP (0 = Neutral)}
#' \item \code{SNPIncluded} \itemize{\item Was the SNP included in the analysis? (SNPs with He < 0.05 were removed)}
#' \item \code{He.LS} \itemize{\item Expected heterozygosity (He) for the SNP based on all 129600 demes on the landscape and infinite sampling of individuals}
#' \item \code{He_samp} \itemize{\item Expected heterozygosity (He) for the SNP based on the sample of finite populations and finite individuals}
#' \item \code{log.bf} \itemize{\item The log-Bayes Factor from the genetic-environment association output by Bayenv2}
#' \item \code{rho} \itemize{\item Spearman's rho corrected by population structure output by Bayenv2}
#' \item \code{xtx} \itemize{\item XTX (an FST analog) output by Bayenv2}
#' \item \code{TW.Zscore} \itemize{\item Z-score output by LFMM for the genetic-environment association}
#' \item \code{Md} \itemize{\item Mahalanobis distance based on log.bf, rho, xtx, and TW.Zscore}
#' \item \code{Hd} \itemize{\item Harmonic mean distance based on log.bf, rho, xtx, and TW.Zscore}
#' \item \code{Kd} \itemize{\item Kernel density deviance based on log.bf, rho, xtx, and TW.Zscore}
#' \item \code{Nd} \itemize{\item Nearest neighbor distance based on log.bf, rho, xtx, and TW.Zscore}
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



