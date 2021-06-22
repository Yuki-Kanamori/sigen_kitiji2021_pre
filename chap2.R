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


# step 5; calculate the number at age ---------------------------------------------
# get survey data and make dataframe
len_num = read.csv("survey_N_at_length.csv", fileEncoding = "CP932") %>% mutate(year = as.numeric(str_sub(調査種類名称, 1, 4))) %>% filter(year == 2020) %>% select(-year)
len_num = len_num[, 16:ncol(len_num)] %>% mutate(site = c("N", "S"))
len_num = len_num %>% gather(key = age_j, value = number, 1:(ncol(len_num)-1)) %>% na.omit()
summary(len_num)
# len_num2 = ddply(NatL, .(age_j), summarize, number = sum(number))
len_num2 = len_num %>% dplyr::group_by(age_j) %>% dplyr::summarize(number = sum(number)) %>% mutate(length_cate = as.numeric(str_sub(age_j, 3, 4))) %>% select(-age_j)

summary(len_num2)
AC2 = left_join(AC, len_num2, by = "length_cate") %>% mutate(bisu = freq*number)
num_ac2 = ddply(AC2, .(length_cate), summarize, total = mean(number))

number_at_age2 = AC2 %>% select(length_cate, age, bisu) %>% tidyr::spread(key = length_cate, value = bisu)
num_ac2 = num_ac2 %>% tidyr::spread(key = length_cate, value = total) %>% mutate(age = "total")

number_at_age2 = rbind(number_at_age2, num_ac2)
# x = number_at_age2[1:(nrow(number_at_age2)-1), 2:ncol(number_at_age2)]
# apply(x, 2, sum) - number_at_age2[nrow(number_at_age2), 2:ncol(number_at_age2)]

number_at_age2[2,5] = number_at_age2[nrow(number_at_age2), 5]
write.csv(number_at_age2, "number_at_age_exp.csv")





# -------------------------------------------------------------------------
# 2-2  年齢別資源体重の算出
#      (引き継ぎ資料の2-2部分) 
# -------------------------------------------------------------------------

# step 1; calculate the weight at age, or at length -----------------------
number_at_age3 = number_at_age2[-nrow(number_at_age2), ] %>% gather(key = length, value = number, 2:ncol(number_at_age2))
number_at_age3 = number_at_age2 %>% gather(key = length, value = number, 2:ncol(number_at_age2)) %>% filter(age != "total")
summary(number_at_age3)
mode(number_at_age3$age)
length = number_at_age3 %>% mutate(sum_length = (as.numeric(as.character(as.factor(length))) + 0.5)*number)

s_length_age = ddply(length, .(age), summarize, sum_l = sum(sum_length))
s_number_age = ddply(length, .(age), summarize, sum_n = sum(number))

mean_length_weight_at_age = left_join(s_length_age, s_number_age, by = "age") %>% mutate(mean_cm = sum_l/sum_n) %>% select(age, mean_cm) %>% mutate(mean_mm = mean_cm*10) %>% mutate(weight = (1.86739*10^(-5))*(mean_mm^3.06825547)) 
write.csv(mean_length_weight_at_age, "mean_length_weight_at_age.csv")



# -------------------------------------------------------------------------
# 2-3  漁獲量まとめ
#      (引き継ぎ資料の2-3部分)
# -------------------------------------------------------------------------

# step 1; summary ---------------------------------------------------------
okisoko = read.xlsx("okisoko_after2019.xlsx", sheet = "2020")
summary(okisoko$魚種名)
colnames(okisoko)
summary(okisoko)

okisoko = okisoko %>% mutate(method = ifelse(漁法 == 102, "2そう曳き", ifelse(漁法 == 103, "トロール", "かけ廻し"))) %>%
  mutate(pref = ifelse(県コード == 13, "青森", ifelse(県コード == 14, "岩手", ifelse(県コード == 15, "宮城", ifelse(県コード == 18, "茨城", "福島"))))) %>% select(漁区名, method, pref, 漁獲量の合計, 網数の合計) %>% dplyr::rename(area = 漁区名, catch = 漁獲量の合計, effort = 網数の合計) %>% mutate(cpue = catch/effort)

catch_t1 = ddply(okisoko, .(pref, method, area), summarize, sum = sum(catch)) %>% tidyr::spread(key = area, value = sum)
catch_t1[is.na(catch_t1)] = 0

catch_t2 = ddply(okisoko, .(area), summarize, sum = sum(catch))
catch_t2[is.na(catch_t2)] = 0

catch_t3 = ddply(okisoko, .(method, area), summarize, sum = sum(catch)) %>% tidyr::spread(key = method, value = sum)
catch_t3[is.na(catch_t3)] = 0

effort_t1 = ddply(okisoko, .(method, area), summarize, sum = sum(effort)) %>% tidyr::spread(key = method, value = sum)
effort_t1[is.na(effort_t1)] = 0

effort_t2 = ddply(okisoko, .(pref, method, area), summarize, sum = sum(effort)) %>% tidyr::spread(key = area, value = sum)
effort_t2[is.na(effort_t2)] = 0

write.csv(catch_t1, "catch_t1.csv", fileEncoding = "CP932")
write.csv(catch_t2, "catch_t2.csv", fileEncoding = "CP932")
write.csv(catch_t3, "catch_t3.csv", fileEncoding = "CP932")
write.csv(effort_t1, "effort_t1.csv", fileEncoding = "CP932")


# step 2; summary of the data derived from prefectures --------------------
### aomori
ao = read.xlsx("catch_pref.xlsx", sheet = "ao") %>% select(漁法, 数量kg) %>% dplyr::rename( method = 漁法, catch_kg = 数量kg)
summary(ao)
ao_sum = ddply(ao, .(method), summarize, sum_temp = sum(catch_kg))
ao_sum$method
ao_sum$method2 = c("その他", "その他", "沖底", "小底")
ao_sum = ao_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))


### iwate
iwa = read.xlsx("catch_pref.xlsx", sheet = "iwa") %>% select(漁業種名, 合計) %>% dplyr::rename(method = 漁業種名, sum_temp = 合計) %>% dplyr::group_by(method) %>% dplyr::summarize(sum_temp = sum(sum_temp))
iwa_sum = iwa
iwa_sum$method
iwa_sum$method2 = c("延縄", "沖底", "延縄", "刺網", "延縄")
iwa_sum = iwa_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))


### iwate
iwa = read.xlsx("catch_pref.xlsx", sheet = "iwa") %>% select(漁業種名, "合計") %>% dplyr::rename(method = 漁業種名, sum_temp = 合計) %>% dplyr::group_by(method) %>% dplyr::summarize(sum_temp = sum(sum_temp))
iwa_sum = iwa
iwa_sum$method
# iwa_sum$method2 = c("延縄", "沖底", "延縄", "刺網", "延縄")
iwa_sum$method2 = c("その他", "延縄", "沖底", "延縄", "刺網")
iwa_sum = iwa_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))


### miyagi
miya = read.xlsx("catch_pref.xlsx", sheet = "miya", startRow = 4)
miya = miya[, c(1,2,ncol(miya))]
miya_l = miya %>% filter(魚種コード == "きちじ") %>% select(漁業種コード, 総計) %>% dplyr::rename(method = 漁業種コード, sum = 総計)
miya_s = miya %>% filter(魚種コード == "こきちじ") %>% select(漁業種コード, 総計) %>% dplyr::rename(method = 漁業種コード, sum = 総計)

miya2 = left_join(miya_l, miya_s, by = "method")
miya2[is.na(miya2)] = 0
miya2 = miya2 %>% mutate(sum_temp = sum.x+sum.y) %>% select(method, sum_temp)
# miya_sum = miya2 %>% filter(method != "その他漁業種") %>% filter(method != "その他漁業種・全漁法2") %>% filter(method != "沿岸小漁")
# miya_sum$method
# miya_sum$method2 = c("沖底", "刺網", "延縄") #沿岸小漁=その他
# miya_sum = miya_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))
miya2$method
miya2$method2 = c("沖底", "その他", "その他", "延縄") #沿岸小漁=その他
miya_sum = miya2 %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))


### fukusima
fuku = read.xlsx("catch_pref.xlsx", sheet = "fuku", startRow = 2) %>% 
  select(沖合底びき網) %>% 
  mutate(method = paste0("沖合底びき網")) %>% 
  dplyr::rename(catch_kg = 沖合底びき網) %>% 
  na.omit %>% 
  dplyr::group_by(method) %>% 
  dplyr::summarize(sum = sum(catch_kg))
fuku_sum = fuku
fuku_sum$method
fuku_sum$method2 = c("沖底")
fuku_sum = fuku_sum %>% select(-method)


### ibaraki
iba = read.xlsx("catch_pref.xlsx", sheet = "iba", startRow = 4)
iba = iba[13, ] 
iba = iba %>% mutate(method2 = "沖底")
iba = iba[, 3:4]
colnames(iba)[1] = "sum"
# iba = iba %>% dplyr::rename(method = 漁法) %>% mutate(num = as.numeric(as.character(as.factor(iba$年計))))
# iba = iba %>% filter(method != "小計") %>% dplyr::group_by(method) %>% dplyr::summarize(sum_temp = sum(num))
# iba_sum = iba
# iba_sum$method
# iba_sum$method2 = c("その他", "延縄", "沖底", "小底", "小底")
# iba_sum = iba_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))
iba_sum = iba


merge = ao_sum %>% dplyr::full_join(iwa_sum, by = "method2") %>% dplyr::full_join(miya_sum, by = "method2") %>% dplyr::full_join(fuku_sum, by = "method2") %>% dplyr::full_join(iba_sum, by = "method2")
colnames(merge) = c("漁業種", "青森", "岩手", "宮城", "福島", "茨城")
merge[is.na(merge)] = 0
write.csv(merge, "catch_by_method&pref.csv", fileEncoding = "CP932")



# -------------------------------------------------------------------------
# 2-4  資源量計算とABCの算定
#      (引き継ぎ資料の2-4部分)
# -------------------------------------------------------------------------

# step 1; catch trend -----------------------------------------------------
catch_old = read.csv("catchdata_old.csv", fileEncoding = "CP932") %>% na.omit()
catch_old = catch_old[, c(1, 3:5)]
catch_old = catch_old %>% tidyr::gather(key = method, value = sum, 2:4) %>% dplyr::rename(year = 年)
catch_old = catch_old %>% mutate(method2 = ifelse(str_detect(catch_old$method, pattern = "以外"), "沖底・小底以外", catch_old$method)) %>% select(-method) %>% dplyr::rename(method = method2)
summary(catch_old)

c19 = read.csv("catch2019.csv", fileEncoding = "CP932") %>% select(-X)

catch_new = rbind(ao_sum, iwa_sum, miya_sum, fuku_sum, iba_sum) %>% mutate(年 = 2020)
catch_new = catch_new %>% mutate(method = ifelse(str_detect(catch_new$method2, pattern = "沖底"), "沖底", ifelse(str_detect(catch_new$method2, pattern = "小底"), "小底", "沖底・小底以外"))) %>% select(-method2) %>% dplyr::rename(year = 年) %>% dplyr::rename(catch_kg = sum) %>% mutate(sum = catch_kg/1000)
summary(catch_new)
write.csv(catch_new, "catch2020.csv", fileEncoding = "CP932")

colnames(catch_new)
colnames(catch_old)
catch = rbind(catch_old, c19 %>% select(-catch_kg), catch_new %>% select(-catch_kg))
summary(catch)
catch = catch %>% dplyr::group_by(method, year) %>% dplyr::summarize(catch_t = sum(sum))

unique(catch$method)
levels(catch$method) 
catch$method = factor(catch$method, levels = c("沖底・小底以外", "小底", "沖底"))

g = ggplot(catch, aes(x = year, y = catch_t, fill = method))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "年", y = "漁獲量 (トン)", fill = "漁業種")
col_catch = c("grey50", "white", "grey0")
c = scale_fill_manual(values = col_catch)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.85, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig5 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(1975, 2020, by = 3))+scale_y_continuous(expand = c(0,0),limits = c(0, 4000))
ggsave(file = "fig5.png", plot = fig5, units = "in", width = 11.69, height = 8.27)



# step 2; effort trend ----------------------------------------------------
# ~2018まで
eff_old = read.csv("effortdata_old.csv", fileEncoding = "CP932")
eff_old = ddply(eff_old, .(method, year), summarize, sum = sum(effort))

#2019
oki2019 = read.xlsx("okisoko_after2019.xlsx", sheet = "2019")
oki2019 = oki2019 %>% mutate(method = ifelse(漁法 == 102, "2そう曳き", ifelse(漁法 == 103, "トロール", "かけ廻し"))) %>%
  mutate(pref = ifelse(県コード == 13, "青森", ifelse(県コード == 14, "岩手", ifelse(県コード == 15, "宮城", ifelse(県コード == 18, "茨城", "福島"))))) %>% select(漁区名, method, pref, 漁獲量の合計, 網数の合計) %>% dplyr::rename(area = 漁区名, catch = 漁獲量の合計, effort = 網数の合計) %>% mutate(cpue = catch/effort)

eff19 = ddply(oki2019, .(method), summarize, sum = sum(effort))
eff19$year = 2019

#2020; 上の方で使っている
eff = ddply(okisoko, .(method), summarize, sum = sum(effort))
eff$year = 2020

eff = rbind(eff_old, eff19, eff)
eff = eff %>% mutate(label = ifelse(eff$method == "かけ廻し", "尻屋崎〜岩手沖のかけ廻し", ifelse(eff$method == "トロール", "金華山~房総のトロール", "岩手沖の2そう曳き")))

unique(eff$label)
levels(eff$label)
eff$label = factor(eff$label, levels = c("尻屋崎〜岩手沖のかけ廻し", "岩手沖の2そう曳き", "金華山~房総のトロール"))

g = ggplot(eff, aes(x = year, y = sum/1000, shape = label, linetype = label, fill = label))
p = geom_point(size = 5)
l = geom_line(size = 1)
lab = labs(x = "年", y = "有漁網数 (千)", shape = "漁業種")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.8, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
# fig6 = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0, 0.5), breaks=seq(1972, 2019, by = 2))+scale_y_continuous(expand = c(0,0),limits = c(0, 30))+scale_shape_manual(values = c(22, 17, 18))+scale_fill_manual(values = c('white','black','black'))+scale_size_manual(values = c(3,3,4))+scale_linetype_manual(values = c("dotted", "solid", "dotted"))
fig6 = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0, 0.5), breaks=seq(1972, 2020, by = 2))+scale_y_continuous(expand = c(0,0),limits = c(0, 30))+scale_linetype_manual(values = c("dotted", "solid", "dotted"),)+ guides(linetype=FALSE, fill = FALSE)+scale_shape_manual(values = c(22, 17, 18))+scale_fill_manual(values = c('white','black','black'))+scale_size_manual(values = c(3,3,4))
ggsave(file = "fig6.png", plot = fig6, units = "in", width = 11.69, height = 8.27)
