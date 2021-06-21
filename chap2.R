# -------------------------------------------------------------------------
# 2-0  パッケージの読み込みとディレクトリの設定 
# -------------------------------------------------------------------------

# load the packages -------------------------------------------------------
require(xlsx)
require(openxlsx)
require(tidyr)
require(dplyr)
require(plyr)
require(ggplot2)
require(investr)
require(stringr)
require(abind)
require(gridExtra)
require(ggrepel)

# set working directory -----------------------------------------------------------
# please change here
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021")


# -------------------------------------------------------------------------
# 2-1  成長曲線の作成，ALKの作成，および年齢別資源微数の算出 
#      (引き継ぎ資料の2-1部分)
# -------------------------------------------------------------------------
df = read.xlsx("ALdata.xlsx", sheet = "2020") %>% filter(pick == 1) %>% select(label, SL, age)
summary(df)
mode(df$age)

# step 1; remove the data that age is 10+, 10++, and ? --------------------
df = df %>% mutate(length_mm = SL, age_num = as.numeric(as.character(age))) #10+, 10++, and ? turned NA
summary(df)
df2 = na.omit(df)
summary(df2)

# step 2; fit the von Bertalanffy growth curve and estimate params --------
# Lt = L_max*(1-e^(-K(t-t0)))
mode(df2$age_num)
mode(df2$length_mm)

Lmax = 320
fit = nls(length_mm ~ Lmax*(1-exp(-K*((age_num+0.5) - t0))), data = df2, start = c(K = 0.01, t0 = -3), trace = TRUE)
summary(fit)

# plot (https://stackoverflow.com/questions/33305620/plotting-nls-fits-with-overlapping-prediction-intervals-in-a-single-figure)
plotFit(fit, interval = "prediction", ylim = c(0, 250), pch = 19, col.pred = 'light blue', shade=T)


# step 3; make the tables of number at age (NAA)
# !!note!!  use 10+ and 10++
head(df)
df3 = df %>% select(SL, age) %>% dplyr::rename(length_mm = SL)
summary(df3)
head(df3)
unique(df3$age)
df3 = df3 %>% mutate(fumei = ifelse(df3$age == "?", 100, as.character(df3$age)))

df3 = df3 %>% mutate(fumei = ifelse(df3$age == "?", 100, as.character(df3$age)),
                     age2 = ifelse(df3$age == "10+", 10, ifelse(df3$age > 9, 10, as.character(df3$age)))) %>% filter(fumei != 100) %>% select(-fumei) %>% mutate(count = 1)
unique(df3$age2)
df3$age2 = as.numeric(df3$age2)
summary(df3$age2)
df3$age3 = ifelse(df3$age2 > 10, 10, df3$age2)
df3 = na.omit(df3)
summary(df3)
naa = ddply(df3, .(length_mm, age3), summarize, number = sum(count))

length_mm = rep(seq(min(df3$length_mm), max(df3$length_mm)), length(unique(df3$age3))+1) #1761rows
age_num = rep(0:max(df3$age3), each = length(unique(length_mm)))
tag = data_frame(length_mm = length_mm, age_num = age_num)
tag = tag %>% mutate(length_cate = ifelse(length_mm < 100, str_sub(tag$length_mm, 1, 1), str_sub(tag$length_mm, 1, 2)))

head(naa)
head(tag)

naa = naa %>% dplyr::rename(age = age3)
tag = tag %>% dplyr::rename(age = age_num)

naa2 = merge(naa, tag, by = c("length_mm", "age"), all = T)
naa2$number = ifelse(is.na(naa2$number), 0, naa2$number)
summary(naa2)
NAA = ddply(naa2, .(age, length_cate), summarize, number = sum(number))
NAA$length_cate = as.numeric(NAA$length_cate)
summary(NAA)

# add the data that NAA does not have
add = NAA %>% filter(length_cate < min(NAA$length_cate)*2-1)
add = add %>% mutate(length_cate = rep(1:(min(add$length_cate)-1)), number = 0)
NAA = rbind(add, NAA)
NAA = NAA %>% arrange(length_cate, age) 
sum = ddply(NAA, .(length_cate), summarize, sum = sum(number))

NAA2 = NAA %>% tidyr::spread(key = length_cate, value = number)
sum2 = sum %>% tidyr::spread(key = length_cate, value = sum) %>% mutate(age = "total")
number_at_age = rbind(NAA2, sum2)
write.csv(number_at_age, "number_at_age.csv", fileEncoding = "CP932")
#write.csv(number_at_age, "number_at_age_freq.csv", fileEncoding = "CP932")

# step 4; make the tables of age composition (AC)
AC = left_join(NAA, sum, by = "length_cate") %>% mutate(freq = ifelse(sum > 0, number/sum, 0))
AC = AC %>% select(length_cate, age, freq)
a_sum = ddply(AC, .(length_cate), summarize, sum = sum(freq))

age_composition = AC %>% tidyr::spread(key = length_cate, value = freq)
a_sum2 = a_sum %>% tidyr::spread(key = length_cate, value = sum) %>% mutate(age = "total")
age_composition = rbind(age_composition, a_sum2)
# write.csv(age_composition, "age_composition.csv", fileEncoding = "CP932")
write.csv(age_composition, "number_at_age_freq.csv", fileEncoding = "CP932")
# freq at age?
