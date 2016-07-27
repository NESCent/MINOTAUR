# MINOTAUR

[![Travis-CI Build Status](https://travis-ci.org/thierrygosselin/MINOTAUR.svg?branch=master)](https://travis-ci.org/thierrygosselin/MINOTAUR)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thierrygosselin/MINOTAUR?branch=master&svg=true)](https://ci.appveyor.com/project/thierrygosselin/MINOTAUR)

## INSTALL
```
install.packages("devtools", dependencies = TRUE)

library(devtools)

install_github("NESCent/MINOTAUR", build_vignettes=TRUE)

library(MINOTAUR)

MINOTAUR()
```

## ADDITIONAL NOTES FOR WINDOWS USERS
Windows users must also install the appropriate version of Rtools in order to properly build and install packages. See the following link for download options: [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/).

Additional dependencies also do not appear to be installing properly in Windows, leading to MINOTAUR installation errors (a problem we are currently addressing). Installing these dependencies by issuing the following command prior to installing MINOTAUR should alleviate this issue.
```
install.packages(c("ade4", "ape", "dplyr", "seqinr", "httpuv", "spdep", "vegan", "chron", "spam", "maps", "miniUI"), dependencies = TRUE)
```

If additional issues are encountered, please [open a new issue](https://github.com/NESCent/MINOTAUR/issues/new).
