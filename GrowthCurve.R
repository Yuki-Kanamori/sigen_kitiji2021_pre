# source("http://www.rforge.net/FSA/InstallFSA.R") # with errot

## Install all non-"base" depends packages that are not already installed
tmp <- c("plotrix","plyr")
tmp <- tmp[!(tmp %in% library()$results[,"Package"])]
if (length(tmp)>0) install.packages(tmp)

## Install all suggests packages that are not already installed
tmp <- c("car","dplyr","fishmethods","gplots","Hmisc","knitr","lmtest","marked","nlstools","popbio","Rcapture","sciplot","testthat")
tmp <- tmp[!(tmp %in% library()$results[,"Package"])]
if (length(tmp)>0) install.packages(tmp)

# Install many of the dependencies of the other packages that are not already installed
#   all dependencies to the best of my knowledge on 2-Feb-15
tmp <- c("acepack","assertthat","BB","BH","bitops","boot","bootstrap","caTools","cluster","coda","colorspace","DBI","dfoptim","dichromat","digest","evaluate","foreign","formatR","Formula","ggplot2","gtable","highr","htmltools","KernSmooth","labeling","latticeExtra","lazyeval","lme4","magrittr","Matrix","memoise","mgcv","mime","minqa","munsell","mvtnorm","nlme","nloptr","nnet","numDeriv","optextras","optimx","parallel","pbkrtest","plyr","proto","quadprog","quantreg" ,"R2admb","R6","Rcgmin","RColorBrewer","Rcpp","RcppEigen","reshape2","rmarkdown","rpart","Rvmmin","sandwich","scales","setRNG","SparseM","splines","stringr","survival","svUnit","TH.data","truncnorm","ucminf","yaml","zoo")
tmp <- tmp[!(tmp %in% library()$results[,"Package"])]
if (length(tmp)>0) install.packages(tmp)

## Install FSA and FSAdata from RForge.net
utils::install.packages("FSA",repos="http://www.rforge.net/",type="source")
utils::install.packages("FSAdata",repos="http://www.rforge.net/",type="source")

## Clean up
rm(tmp)

remotes::install_github('droglenc/FSA')
if (!require('remotes')) install.packages('remotes'); require('remotes')
remotes::install_github('droglenc/FSA')

require(devtools)
install_github('droglenc/FSA')
