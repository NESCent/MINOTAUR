# MINOTAUR: an R package for the detection and visualization of outliers in multivariate space.


[![Travis-CI Build Status](https://travis-ci.org/thierrygosselin/MINOTAUR.svg?branch=master)](https://travis-ci.org/thierrygosselin/MINOTAUR)
[![DOI](https://zenodo.org/badge/23897/NESCent/MINOTAUR.svg)](https://zenodo.org/badge/latestdoi/23897/NESCent/MINOTAUR)

## INSTALL
```
install.packages("devtools", dependencies = TRUE)

devtools::install_github("rstudio/DT", ref="24d71f2") # temporary fix (required for some datatables)

devtools::install_github("NESCent/MINOTAUR", build_vignettes = TRUE)

library(MINOTAUR)

MINOTAUR()
```

## ADDITIONAL NOTES
Users may need to install additional development tools for their operating system to properly compile MINOTAUR. See here for details: [https://www.rstudio.com/products/rpackages/devtools/](https://www.rstudio.com/products/rpackages/devtools/).

Windows users should install the appropriate version of Rtools in order to properly build and install packages. See the following link for download options: [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/). Mac users should install Xcode command line tools. And Linux users should install r-devel or r-base-dev. 

In case additional dependencies fail to install properly in Windows, and installation errors arise (a problem we are currently addressing), installing these dependencies by issuing the following command prior to installing MINOTAUR should alleviate this issue:
```
install.packages(c("ade4", "ape", "dplyr", "seqinr", "httpuv", "spdep", "vegan", "chron", "spam", "maps", "miniUI"), dependencies = TRUE)
```

If additional issues are encountered, please [open a new issue](https://github.com/NESCent/MINOTAUR/issues/new).
