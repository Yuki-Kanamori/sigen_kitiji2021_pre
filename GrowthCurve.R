# source("http://www.rforge.net/FSA/InstallFSA.R") # with errot

# ## Install all non-"base" depends packages that are not already installed
# tmp <- c("plotrix","plyr")
# tmp <- tmp[!(tmp %in% library()$results[,"Package"])]
# if (length(tmp)>0) install.packages(tmp)
# 
# ## Install all suggests packages that are not already installed
# tmp <- c("car","dplyr","fishmethods","gplots","Hmisc","knitr","lmtest","marked","nlstools","popbio","Rcapture","sciplot","testthat")
# tmp <- tmp[!(tmp %in% library()$results[,"Package"])]
# if (length(tmp)>0) install.packages(tmp)
# 
# # Install many of the dependencies of the other packages that are not already installed
# #   all dependencies to the best of my knowledge on 2-Feb-15
# tmp <- c("acepack","assertthat","BB","BH","bitops","boot","bootstrap","caTools","cluster","coda","colorspace","DBI","dfoptim","dichromat","digest","evaluate","foreign","formatR","Formula","ggplot2","gtable","highr","htmltools","KernSmooth","labeling","latticeExtra","lazyeval","lme4","magrittr","Matrix","memoise","mgcv","mime","minqa","munsell","mvtnorm","nlme","nloptr","nnet","numDeriv","optextras","optimx","parallel","pbkrtest","plyr","proto","quadprog","quantreg" ,"R2admb","R6","Rcgmin","RColorBrewer","Rcpp","RcppEigen","reshape2","rmarkdown","rpart","Rvmmin","sandwich","scales","setRNG","SparseM","splines","stringr","survival","svUnit","TH.data","truncnorm","ucminf","yaml","zoo")
# tmp <- tmp[!(tmp %in% library()$results[,"Package"])]
# if (length(tmp)>0) install.packages(tmp)
# 
# ## Install FSA and FSAdata from RForge.net
# utils::install.packages("FSA",repos="http://www.rforge.net/",type="source")
# utils::install.packages("FSAdata",repos="http://www.rforge.net/",type="source")
# 
# ## Clean up
# rm(tmp)
# 
# remotes::install_github('droglenc/FSA')
# if (!require('remotes')) install.packages('remotes'); require('remotes')
# remotes::install_github('droglenc/FSA')

# できた
# require(devtools)
# install_github('droglenc/FSA')

# load the packages -------------------------------------------------------
# require(xlsx)
require(openxlsx)
require(readxl)
# require(tidyr)
# require(dplyr)
# require(plyr)
# require(ggplot2)
# require(investr)
# require(stringr)
# require(abind)
# require(gridExtra)
# require(ggrepel)
require(FSA)
require(tidyverse)

# set working directory -----------------------------------------------------------
# please change here
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021")
options(warn = -1)

# -------------------------------------------------------------------------
# 2-1  成長曲線の作成，ALKの作成，および年齢別資源微数の算出 
#      (引き継ぎ資料の2-1部分)
# -------------------------------------------------------------------------
sheets = excel_sheets("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021/ALdata.xlsx") #シート名の取得
temp = NULL
for(i in 1:length(sheets)){
  df = read.xlsx("ALdata.xlsx", sheet = sheets[i]) %>% filter(pick == 1) %>% select(label, SL, age)
  df = df %>% mutate(length_mm = SL, age_num = as.numeric(as.character(age)), year = paste0(sheets[i])) #10+, 10++, and ? turned NA
  summary(df)
  df2 = na.omit(df)
  summary(df2)
  
  temp = rbind(temp, df)
}

temp = na.omit(temp)

# 初期値の設定　FSAパッケージより
start.full <- vbStarts(length_mm ~ age_num, data = temp, type="typical")
start.full
# $Linf
# [1] 247.7722
# 
# $K
# [1] 0.06315588
# 
# $t0
# [1] -2.974303
