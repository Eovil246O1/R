# check if RTools available
install.packages('devtools')
install.packages('pkgbuild')
library(pkgbuild)
library(devtools)
pkgbuild::find_rtools(T)

devtools:::version_info

# install LightGBM
# more details: https://github.com/Microsoft/LightGBM/issues/912
devtools::install_github("Laurae2/lgbdl", force = TRUE)

library(lgbdl)

lgb.dl(commit = "master",
       compiler = "vs",
       repo = "https://github.com/Microsoft/LightGBM")

library(lightgbm)
