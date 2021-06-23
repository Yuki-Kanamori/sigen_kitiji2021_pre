# -------------------------------------------------------------------------
# 2-0  パッケージの読み込みとディレクトリの設定 
# -------------------------------------------------------------------------

# load the packages -------------------------------------------------------
require(xlsx)
require(openxlsx)
require(readxl)
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

catch_t1 = ddply(okisoko, .(pref, method, area), summarize, sum_kg = sum(catch)) %>% tidyr::spread(key = area, value = sum)
catch_t1[is.na(catch_t1)] = 0

catch_t2 = ddply(okisoko, .(area), summarize, sum_kg = sum(catch))
catch_t2[is.na(catch_t2)] = 0

catch_t3 = ddply(okisoko, .(method, area), summarize, sum_kg = sum(catch)) %>% tidyr::spread(key = method, value = sum)
catch_t3[is.na(catch_t3)] = 0

effort_t1 = ddply(okisoko, .(method, area), summarize, sum= sum(effort)) %>% tidyr::spread(key = method, value = sum)
effort_t1[is.na(effort_t1)] = 0

effort_t2 = ddply(okisoko, .(pref, method, area), summarize, sum_kg = sum(effort)) %>% tidyr::spread(key = area, value = sum)
effort_t2[is.na(effort_t2)] = 0

write.csv(catch_t1, "catch_t1.csv", fileEncoding = "CP932")
write.csv(catch_t2, "catch_t2.csv", fileEncoding = "CP932")
write.csv(catch_t3, "table2.csv", fileEncoding = "CP932")
write.csv(effort_t1, "table3.csv", fileEncoding = "CP932")


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
write.csv(merge, "table1.csv", fileEncoding = "CP932")



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


# step 3; CPUE trend ----------------------------------------------------------
# -2018
gyo_old = read.csv("okisoko_old.csv", fileEncoding = "CP932")
unique(gyo_old$method)

# 2019-
sheets = excel_sheets("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021/okisoko_after2019.xlsx") #シート名の取得
cpue = NULL
for(i in 1:length(sheets)){
  data = read.xlsx("okisoko_after2019.xlsx", sheet = sheets[i])
  data2 = data %>% mutate(method = ifelse(漁法 == 102, "2そう曳き", ifelse(漁法 == 103, "トロール", "かけ廻し"))) %>%
    mutate(pref = ifelse(県コード == 13, "青森", ifelse(県コード == 14, "岩手", ifelse(県コード == 15, "宮城", ifelse(県コード == 18, "茨城", "福島"))))) %>% select(漁区名, method, pref, 漁獲量の合計, 網数の合計) %>% filter(漁区名 != "襟裳西")
  
  temp_cpue = data2 %>% group_by(method) %>% dplyr::summarize(effort = sum(網数の合計), catch = sum(漁獲量の合計)) %>% mutate(year = as.numeric(paste0(sheets[i])))
  cpue = rbind(cpue, temp_cpue)
}

cpue2 = rbind(gyo_old, cpue) %>% mutate(cpue = catch/effort)
mean_cpue = ddply(cpue2, .(method), summarize, m_cpue = mean(cpue))
cpue2 = left_join(cpue2, mean_cpue, by = "method") %>% mutate(cpue2 = cpue/m_cpue) %>% mutate(bunsi = catch*cpue2)

y_cpue = ddply(cpue2, .(year), summarize, bunsi = sum(bunsi))
y_catch = ddply(cpue2, .(year), summarize, total_catch = sum(catch))
w_cpue = left_join(y_cpue, y_catch, by = "year") %>% mutate(weighted_cpue = bunsi/total_catch)

# cpue2 = cpue2 %>% mutate(label = ifelse(cpue2$method == "かけ廻し", "尻屋崎〜岩手沖のかけ廻し", ifelse(cpue2$method == "トロール", "金華山~房総のトロール", "岩手沖の2そう曳き")))
cpue2$label = ifelse(cpue2$method == "かけ廻し", "尻屋崎〜岩手沖のかけ廻し", ifelse(cpue2$method == "トロール", "金華山~房総のトロール", "岩手沖の2そう曳き"))
unique(cpue2$label)
levels(cpue2$label)
cpue2$label = factor(cpue2$label, levels = c("尻屋崎〜岩手沖のかけ廻し", "岩手沖の2そう曳き", "金華山~房総のトロール"))


table4 = data2  %>% group_by(method,漁区名) %>% dplyr::summarize(effort = sum(網数の合計), catch = sum(漁獲量の合計))
table4$cpue = table4$catch/table4$effort
table4
write.csv(table4, "table4.csv")


### かけ廻し
g = ggplot(cpue2 %>% filter(method == "かけ廻し"), aes(x = year, y = cpue, shape = label, fill = label))
p = geom_point(shape = 22, size = 4, fill = "white")
l = geom_line(linetype = "dotted", size = 1)
lab = labs(x = "年", y = "CPUE  (kg/網)", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_blank(),
           # axis.title.y = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(1.6)),
           legend.title = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
kake = g+l+p+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1972, 2020, by = 2), expand=c(0, 0.5))+scale_y_continuous(limits = c(0, 60))

### 2そう
g = ggplot(cpue2 %>% filter(method == "2そう曳き"), aes(x = year, y = cpue, shape = label))
p = geom_point(shape = 17, size = 5)
l = geom_line(linetype = "solid", size = 1)
lab = labs(x = "年", y = "CPUE  (kg/網)", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_blank(),
           # axis.title.y = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(1.6)),
           legend.title = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
niso = g+p+l+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1972, 2020, by = 2), expand = c(0, 0.5))+scale_y_continuous(limits = c(0, 300))

### トロール
g = ggplot(cpue2 %>% filter(method == "トロール"), aes(x = year, y = cpue, shape = label))
p = geom_point(shape = 18, size = 6)
l = geom_line(linetype = "dotted", size = 1)
lab = labs(x = "年", y = "CPUE  (kg/網)", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_blank(),
           # axis.title.y = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(1.6)),
           legend.title = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
tra = g+p+l+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1972, 2020, by = 2), expand=c(0, 0.5))+scale_y_continuous(limits = c(0, 120))

### weighted CPUE
w_cpue$label = "太平洋北部"
g = ggplot(w_cpue, aes(x = year, y = weighted_cpue))
p = geom_point(shape = 20, size = 6)
l = geom_line(size = 0.6, linetype = "solid")
lab = labs(x = "年", y = "重み付CPUE \n（相対値）", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           # axis.title.x = element_text(size = rel(2)),
           # axis.title.y = element_text(size = rel(2)),
           axis.title.x = element_text(size = rel(1.6)),
           axis.title.y = element_text(size = rel(1.6)),
           legend.title = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
w = g+p+l+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(min(w_cpue$year), max(w_cpue$year), by = 2), expand=c(0, 0.5))+scale_y_continuous(limits = c(0, 3))

fig8 = grid.arrange(kake, niso, tra, w, ncol = 1)
ggsave(file = "fig8.png", plot = fig8, units = "in", width = 9.5, height = 11.69)



# step 4; estimation of stock abundance (number & biomass) ---------------------------------------------------------
# -2018
olddata = read.csv("olddata_trawl.csv") 

# combine the catch data from the trawl surveys
old_trawl = olddata %>% filter(data == 'trawl') %>% gather(key = year_tag, value = number, 2:(ncol(olddata)-1)) %>% mutate(year = as.numeric(str_sub(year_tag, 2, 5))) %>% select(-year_tag, -data)
summary(old_trawl)

# 2019-
sheets = excel_sheets("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021/ALdata.xlsx") #シート名の取得
number_at_age_table = NULL
mean_length_weight_at_age_table = NULL
for(i in 1:length(sheets)){
  df = read.xlsx("ALdata.xlsx", sheet = sheets[i]) %>% filter(pick == 1) %>% select(label, SL, age)
  summary(df)
  mode(df$age)
  
  # step 1; remove the data that age is 10+, 10++, and ? --------------------
  df = df %>% mutate(length_mm = SL, age_num = as.numeric(as.character(age))) #10+, 10++, and ? turned NA
  summary(df)
  
  # step 2; make the tables of number at age (NAA)
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
  #write.csv(number_at_age, "number_at_age.csv", fileEncoding = "CP932")
  
  # step 3; make the tables of age composition (AC)
  AC = left_join(NAA, sum, by = "length_cate") %>% mutate(freq = ifelse(sum > 0, number/sum, 0))
  AC = AC %>% select(length_cate, age, freq)
  a_sum = ddply(AC, .(length_cate), summarize, sum = sum(freq))
  
  age_composition = AC %>% tidyr::spread(key = length_cate, value = freq)
  a_sum2 = a_sum %>% tidyr::spread(key = length_cate, value = sum) %>% mutate(age = "total")
  age_composition = rbind(age_composition, a_sum2)
  #write.csv(age_composition, "number_at_age_freq.csv", fileEncoding = "CP932")
  # freq at age?
  
  
  # step 4; calculate the number at age ---------------------------------------------
  # get survey data and make dataframe
  len_num = read.csv("survey_N_at_length.csv", fileEncoding = "CP932") %>% mutate(year = as.numeric(str_sub(調査種類名称, 1, 4))) %>% filter(year == sheets[i]) %>% select(-year)
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
  # write.csv(number_at_age2, "number_at_age_exp.csv")
  
  
  # step 5; calculate the weight at age, or at length -----------------------
  number_at_age3 = number_at_age2[-nrow(number_at_age2), ] %>% gather(key = length, value = number, 2:ncol(number_at_age2))
  number_at_age3 = number_at_age2 %>% gather(key = length, value = number, 2:ncol(number_at_age2)) %>% filter(age != "total")
  summary(number_at_age3)
  mode(number_at_age3$age)
  length = number_at_age3 %>% mutate(sum_length = (as.numeric(as.character(as.factor(length))) + 0.5)*number)
  
  s_length_age = ddply(length, .(age), summarize, sum_l = sum(sum_length))
  s_number_age = ddply(length, .(age), summarize, sum_n = sum(number))
  
  mean_length_weight_at_age = left_join(s_length_age, s_number_age, by = "age") %>% mutate(mean_cm = sum_l/sum_n) %>% select(age, mean_cm) %>% mutate(mean_mm = mean_cm*10) %>% mutate(weight = (1.86739*10^(-5))*(mean_mm^3.06825547)) 
  # write.csv(mean_length_weight_at_age, "mean_length_weight_at_age.csv")
  
  #output
  mean_length_weight_at_age$year = as.numeric(paste0(sheets[i]))
  number_at_age2$year = as.numeric(paste0(sheets[i]))
  # if(i = max(sheets)){
  #   print(mean_length_weight_at_age)
  #   print(number_at_age2)
  # }
  
  mean_length_weight_at_age_table = rbind(mean_length_weight_at_age_table, mean_length_weight_at_age)
  number_at_age_table = rbind(number_at_age_table, number_at_age2)
}

temp = NULL
for(i in min(number_at_age_table$year):max(number_at_age_table$year)){
  naa = number_at_age_table %>% filter(year == i)
  # naa = number_at_age_table %>% filter(year == 2019)
  # naa = number_at_age_table %>% filter(year == 2020)
  naa = naa[1:(nrow(naa)-1), 3:ncol(naa)]
  naa = apply(naa, 1, sum)
  naa = naa %>% data.frame() %>% mutate(age = 0:10) %>% filter(age != 0)
  colnames(naa) = c('number', 'age')
  naa$year = as.numeric(paste0(i))
  temp = rbind(temp, naa)
}
trawl = rbind(old_trawl, temp)
summary(trawl)


# combine the length data
old_length = olddata %>% filter(data == 'length') %>% gather(key = year_tag, value = mean_mm, 2:(ncol(olddata)-1)) %>% mutate(year = as.numeric(str_sub(year_tag, 2, 5))) %>% select(-year_tag, -data)
summary(old_length)

temp_length = NULL
for(i in 1:length(sheets)){
  length = mean_length_weight_at_age_table %>% filter(year == sheets[i]) %>% select(age, mean_mm) %>% mutate(age = as.numeric(age), year = as.numeric(paste0(sheets[i]))) %>% filter(age > 1)
  length2 = rbind(length[2:nrow(length),], length[1, ]) #ageが数値なので10->2->3...となっているのを直す
  temp_length = rbind(temp_length, length2)
}
length = rbind(old_length, temp_length)
summary(length)

# combine the catch data from the fishing
new_catchF = NULL
for(i in 1:length(sheets)){
  okisoko = read.xlsx("okisoko_after2019.xlsx", sheet = sheets[i])
  temp = data.frame(catch = sum(okisoko$漁獲量の合計)/1000, year = as.numeric(paste0(sheets[i])))
  new_catchF = rbind(new_catchF, temp)
}

old_catchF = olddata %>% filter(data == 'catch_fisheries') %>% gather(key = year_tag, value = catch, 2:(ncol(olddata)-1)) %>% mutate(year = as.numeric(str_sub(year_tag, 2, 5))) %>% select(-year_tag, -data,-age)
catchF = rbind(old_catchF, new_catchF)
summary(catchF)



### survival rate at age
survival = NULL
for(i in min(trawl$year):(max(trawl$year)-1)){
  # i = min(trawl$year)
  data_lastyr = trawl %>% filter(year == i)
  data_thisyr = trawl %>% filter(year == (i+1))
  data = left_join(data_lastyr, data_thisyr, by = 'age') %>% arrange(age)
  surv = matrix(NA, ncol = 1, nrow = 9)
  
  if(i < 2006){
    for(j in 2:5){
      if(j < 5){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = data$number.y[(j)]/(data$number.x[j]+data$number.x[j-1])
      }
    }
  }
  
  if(i == 2006){
    for(j in 2:5){
      if(j < 5){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = (data$number.y[(j)]+data$number.y[(j+1)]+data$number.y[(j+2)]+data$number.y[(j+3)]+data$number.y[(j+4)]+data$number.y[(j+5)])/(data$number.x[j]+data$number.x[j-1])
      }
    }
  }
  
  if(i > 2006){
    for(j in 2:10){
      if(j < 10){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = data$number.y[(j)]/(data$number.x[j]+data$number.x[j-1])
      }
    }
  }
  survival = rbind(survival, surv)
}
survival = data.frame(surv = survival, year = rep(1996:2020, each = 9), age = rep(2:10))


### selectivity at age
a = 1524.581
b = 0.082366
c = 0.738107

q = NULL
for(i in min(length$year):max(length$year)){
  # i = max(length$year)-1
  data = length %>% filter(year == i) %>% arrange(age)
  temp_q = matrix(NA, ncol = 1, nrow = 9)
  
  for(j in 1:9){
    temp_q[j, 1] = c/{1+a*exp(-b*data$mean_mm[j])}
  }
  temp_q2 = data.frame(q = temp_q[,1], year = mean(data$year), age = 2:10)
  q = rbind(q, temp_q2)
}  
summary(q)



### weight at age
weight = NULL
for(i in min(length$year):max(length$year)){
  # i = min(length$year)
  data = length %>% filter(year == i) %>% arrange(age)
  temp_w = matrix(NA, ncol = 1, nrow = 9)
  
  for(j in 1:9){
    temp_w[j, 1] = (1.86739*10^(-5))*data$mean_mm[j]^(3.06825547)
  }
  temp_w2 = data.frame(weight = temp_w[,1], year = mean(data$year), age = 2:10)
  weight = rbind(weight, temp_w2)
}
summary(weight)



### number at age when selectivity changes at age
abund_oct_sel = NULL
for(i in min(trawl$year):max(trawl$year)){
  # i = min(trawl$year)
  data_trawl = trawl %>% filter(year == i)
  data_q = q %>% filter(year == i)
  data_weight = weight %>% filter(year == i)
  data = left_join(data_trawl, data_q, by = c("age", "year")) %>% filter(age > 1) %>% arrange(age)
  data = left_join(data, data_weight,  by = c("age", "year")) %>% filter(age > 1) %>% arrange(age)
  
  temp_naa_sel = matrix(NA, ncol = 1, nrow = 9)
  temp_baa_sel = matrix(NA, ncol = 1, nrow = 9)
  
  for(j in 1:9){
    #j = 9
    temp_naa_sel[j, 1] = data$number[j]/data$q[j]
  }
  
  for(k in 1:9){
    temp_baa_sel[k, 1] = temp_naa_sel[k, 1]*data$weight[k]*(0.001)^2
  }
  
  temp_abund_oct = data.frame(number_sel = temp_naa_sel[, 1], biomass_sel = temp_baa_sel[, 1], year = mean(data$year), age = 2:10)
  abund_oct_sel = rbind(abund_oct_sel, temp_abund_oct)
}



### fishing rate, F, Z, and survival rate within 2 month
M = 2.5/20 #fixed
abund_jan_forF_notneeded = NULL
fishing_rate = NULL
Z = NULL
survival_2month = NULL

for(i in (min(abund_oct_sel$year)+1):max(abund_oct_sel$year)){
  # i = min(abund_oct_sel$year)+1
  data_oct_sel_last = abund_oct_sel %>% filter(year == (i-1)) %>% na.omit()
  data_catchF_last = catchF %>% filter(year == (i-1))
  data_catchF_this = catchF %>% filter(year == i)
  
  temp_abund_jan = sum(data_oct_sel_last$biomass_sel)*exp(-2/12*0.125)-data_catchF_last$catch/6*exp(-2/12*0.125)
  temp_fishing_rate = data_catchF_this$catch/temp_abund_jan
  temp_f = -log(1-(temp_fishing_rate/exp(-M/2)))
  temp_Z = temp_f + M
  temp_survival_2month = exp(-temp_Z/6)
  
  abund_jan_forF_notneeded_pre = data.frame(biomass = temp_abund_jan, year = i)
  fishing_rate_pre = data.frame(f = temp_f, year = i)
  Z_pre = data.frame(z = temp_Z, year = i)
  survival_2month_pre = data.frame(surv = temp_survival_2month, year = i)
  
  abund_jan_forF_notneeded = rbind(abund_jan_forF_notneeded, abund_jan_forF_notneeded_pre)
  fishing_rate = rbind(fishing_rate, fishing_rate_pre)
  Z = rbind(Z, Z_pre)
  survival_2month = rbind(survival_2month, survival_2month_pre)
}



### abundance in January
est = NULL
for(i in (min(abund_oct_sel$year)+1):(max(abund_oct_sel$year)+1)){
  # i = max(abund_oct_sel$year) #1995
  
  if(i < max(abund_oct_sel$year)+1){
    data_survival = survival_2month %>% filter(year == i)
    data_abund_oct_sel = abund_oct_sel %>% filter(year == (i-1)) %>% arrange(age)
    data_weight = weight %>% filter(year == (i-1)) %>% arrange(age)
    
    temp_number = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    temp_biomass = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    
    for(j in 1:nrow(data_abund_oct_sel)){
      temp_number[j, 1] = data_survival$surv*data_abund_oct_sel$number_sel[j]
    }
    
    for(k in 1:nrow(data_abund_oct_sel)){
      temp_biomass[k, 1] = temp_number[k, 1]*data_weight$weight[k]*(0.001)^2
    }
    
    temp_est = data.frame(number = temp_number[, 1], biomass = temp_biomass[, 1], year = i, age = 2:10)
    est = rbind(est, temp_est)
  }
  
  if(i == max(abund_oct_sel$year)+1){
    data_survival = survival_2month %>% filter(year == (i-1))
    data_abund_oct_sel = abund_oct_sel %>% filter(year == (i-1)) %>% arrange(age)
    data_weight = weight %>% filter(year == (i-1)) %>% arrange(age)
    
    temp_number = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    temp_biomass = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    
    for(j in 1:nrow(data_abund_oct_sel)){
      temp_number[j, 1] = data_survival$surv*data_abund_oct_sel$number_sel[j]
    }
    
    for(k in 1:nrow(data_abund_oct_sel)){
      temp_biomass[k, 1] = temp_number[k, 1]*data_weight$weight[k]*(0.001)^2
    }
    
    temp_est = data.frame(number = temp_number[, 1], biomass = temp_biomass[, 1], year = i, age = 2:10)
    est = rbind(est, temp_est)
  }
}



### catch rate
trend = est %>% select(year, biomass) %>% na.omit() %>% dplyr::group_by(year) %>% dplyr::summarize(total = sum(biomass))
catch_rate = left_join(catchF, trend, by = "year") %>% mutate(rate = catch/total*100)
fishing_trend = left_join(catch_rate, fishing_rate, by = "year") %>% select(year, rate, f) %>% gather(key = data, value = value, 2:3) %>% mutate(data2 = ifelse(data == "f", "F値", "漁獲割合"))

mean = fishing_trend %>% filter(data == "rate") %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-1)-3)) %>% select(value)
(mean_fishing_trend = mean(mean$value))


### figures 
### year trend of stock biomass (fig. 10)
trend = est %>% select(year, biomass) %>% na.omit() %>% dplyr::group_by(year) %>% dplyr::summarize(total = sum(biomass))
low = (max(trend$total)-min(trend$total))*1/3+min(trend$total)
high = max(trend$total)-(max(trend$total)-min(trend$total))*1/3

g = ggplot(trend, aes(x = year, y = total/1000))
p = geom_point(shape = 20, size = 6)
l = geom_line(size = 0.6, linetype = "solid")
lab = labs(x = "年", y = "資源量（トン）", shape = "")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
level_l = geom_hline(yintercept = low/1000, linetype = "dashed", color = "gray50")
level_h = geom_hline(yintercept = high/1000, linetype = "dashed", color = "gray50")
fig10 = g+p+l+lab+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand = c(0.03, 0.03))+level_l+level_h
ggsave(file = "fig10.png", plot = fig10, units = "in", width = 11.69, height = 8.27)


# check_trend = trend %>% filter(year > 2015)
check_trend = trend %>% filter(year > 2016)
summary(lm(total/1000 ~ year, data = check_trend))

### year trend of stock number (fig. 11)
est = est %>% mutate(age2 = ifelse(age > 4, "5歳以上", "2-4歳"))
summary(est)

est2 = est
est2[is.na(est2)] = 0
est2 = ddply(est2, .(year, age2), summarize, total = sum(number))
summary(est2)

levels(est2$age2) 
unique(est$age2)
est2$age2 = factor(est2$age2, levels = c("5歳以上", "2-4歳"))
levels(est2$age2)

g = ggplot(est2, aes(x = year, y = total/1000000, fill = age2))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "年", y = "資源尾数（百万尾）", legend = NULL)
col_age = c("black", "white")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
c = scale_fill_manual(values =  c("black", "white"))
fig11 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 150))
ggsave(file = "fig11.png", plot = fig11, units = "in", width = 11.69, height = 8.27)



### year trend of fishing (fig. 13)
# F values
g = ggplot(fishing_trend %>% filter(data == "f") %>% na.omit(), aes(x = year, y = value))
p = geom_point(size = 5)
l = geom_line(size = 1)
lab = labs(x = "", y = "F値")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
trend_f = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand = c(0, 0.5))


# catch rate
g = ggplot(fishing_trend %>% filter(data == "rate") %>% na.omit(), aes(x = year, y = value))
p = geom_point(size = 5)
l = geom_line(size = 1)
lab = labs(x = "年", y = "漁獲割合（%）")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
trend_catch_rate = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand = c(0, 0.5))

fig12 = grid.arrange(trend_f, trend_catch_rate, ncol = 1)
ggsave(file = "fig12.png", plot = fig12, units = "in", width = 11.69, height = 8.27)




# step 5; estimation of stock abundance in North and South  (number & biomass) ---------------------------------------------------------
ns = read.csv("trawl_N_at_length_ns.csv", fileEncoding = "CP932")
summary(ns)
ns[is.na(ns)] = 0
ns = ns %>% dplyr::rename(year = 年, area = 南北) %>% gather(key = size_class, value = number, -c("year", "area"))
ns = ns %>% mutate(size_class = as.numeric(str_sub(ns$size_class, 2,3)))
summary(ns)

net_eff = data.frame(size = seq(15, 315, 10)) 
net_eff = net_eff %>% mutate(q = 0.738/(1+1525*exp(-0.0824*net_eff$size)), size_class = rep(1:nrow(net_eff)))
summary(net_eff)

ns = left_join(ns, net_eff, by = "size_class")
summary(ns)
ns = ns %>% mutate(number_sel = ns$number/ns$q)

# ns2 = ddply(ns, .(year, area), summarize, total = sum(number_sel))
# ns2 = ns2 %>% spread(key = area, value = total)
# summary(ns2)
# ns2$n_rate = ns2$北部/(ns2$北部+ns2$南部)

ns = ns %>% mutate(weight = 1.867*10^(-5)*ns$size^(3.068))
ns = ns %>% mutate(biomass_sel = ns$number_sel*ns$weight)

ns3 = ddply(ns, .(year, area), summarize, total_number = sum(number_sel), total_biomass = sum(biomass_sel))
ns4 = left_join(ns3 %>% select(-total_biomass) %>% spread(key = area, value = total_number), ns3 %>% select(-total_number) %>% spread(key = area, value = total_biomass), by = "year") 
ns4 = ns4 %>% mutate(n_rate_number = ns4$北部.x/(ns4$南部.x+ns4$北部.x), n_rate_biomass = ns4$北部.y/(ns4$南部.y+ns4$北部.y))

ns4_2 = ns4 %>% mutate(year = year + 1)

head(trend)  
trend_ns = left_join(trend, ns4_2 %>% select(year, n_rate_biomass), by = "year")
trend_ns = trend_ns %>% mutate(total_n = (trend_ns$total)/1000*trend_ns$n_rate_biomass, total_s = (trend_ns$total)/1000*(1-trend_ns$n_rate_biomass)) %>% select(year, total_n, total_s) %>% gather(key = data, value = biomass_sel, -year) %>% mutate(area = rep(c("北部", "南部"), each = length(unique(trend_ns$year)))) %>% na.omit()



### fig. A3-3
levels(trend_ns$area)
trend_ns$area = factor(trend_ns$area, levels = c("北部", "南部"))

g = ggplot(trend_ns, aes(x = year, y = biomass_sel, fill = area))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "年", y = "漁獲量（千トン）", legend = NULL)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
c = scale_fill_manual(values =  c("white", "black"))
fig_a33 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand= c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 15))
ggsave(file = "fig_a33.png", plot = fig_a33, units = "in", width = 11.69, height = 8.27)



# step 6; get ABC ----------------------------------------------------------
f_current = fishing_rate %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-1)-3)) %>% summarize(mean(f))

# s_pre = Z %>% filter(year == (as.numeric(str_sub(Sys.Date(), 1, 4))-1))
# s_current = exp(-s_pre$z)
s_current = exp(-(Z %>% filter(year == (as.numeric(str_sub(Sys.Date(), 1, 4))-1)) %>% select(z)))

# s1_pre = survival %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-1)-3), age == 2)
# s1_current = mean(s1_pre$surv)
s1_current = survival %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-1)-3), age == 2) %>% summarize(mean(surv))

number_2old_oct_last = trawl %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-1, age == 1) %>% select(number)/1000 * s1_current
number_2old_jan_this = number_2old_oct_last*survival_2month %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-1) %>% select(surv)

number_2old_jan_this_sel = number_2old_jan_this/q %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-1, age == 2) %>% select(q)

# the estimated abundance in step 4
abund_abc = est %>% filter(year == (as.numeric(str_sub(Sys.Date(), 1, 4))-1)) %>% select(number, biomass, year, age) %>% dplyr::rename(number_est = number, biomass_est = biomass)
abund_abc = left_join(abund_abc, weight %>% filter(year == (as.numeric(str_sub(Sys.Date(), 1, 4))-1)), by = c("year", "age"))

next_year = NULL
for(i in 1:(length(abund_abc$age)-1)){
  #i=1
  if(i == 1){
    temp = number_2old_jan_this_sel
    next_year = rbind(next_year, temp)
  }
  if(i < (length(abund_abc$age)-1)){
    temp = abund_abc$number_est[i]*s_current$z
    next_year = rbind(next_year, temp)
  }
  if(i == (length(abund_abc$age)-1)){
    temp = (abund_abc$number_est[i]+abund_abc$number_est[i+1])*s_current$z
    next_year = rbind(next_year, temp)
  }
}

abund_abc = abund_abc %>% mutate(next_year_number = next_year$number/1000) %>% mutate(next_year_biomass = next_year_number*weight/1000)

(total_number_next = sum(abund_abc$next_year_number))
(total_biomass_next = sum(abund_abc$next_year_biomass))

f_limit = 0.058
f_target = f_limit*0.8
z_abc = f_limit+M

abc_limit = (f_limit*(1-exp(-z_abc)))/z_abc*total_biomass_next
abc_target = (f_target*(1-exp(-z_abc)))/z_abc*total_biomass_next

# re-estimation of ABC
total_biomass_this = sum(abund_abc$biomass_est)
(re_abc_limit = (f_limit*(1-exp(-z_abc)))/z_abc*total_biomass_this)
(re_abc_target = (f_target*(1-exp(-z_abc)))/z_abc*total_biomass_this)



# step 6; spawner-recruitment relationship ----------------------
summary(ns)
# test = ddply(ns, .(year, size_class, size), summarize, number = sum(number)/0.3)

ns_rec = ddply(ns, .(year, size_class, size), summarize, number = sum(number))
# 
# net_eff = data.frame(size = seq(15, 315, 10)) %>% mutate(size_class = rep(1:nrow(net_eff)))
# net_eff = net_eff %>% mutate(q = 0.738/(1+1525*exp(-0.0824*net_eff$size_class)))
# 
net_eff = data.frame(size = seq(15, 315, 10)) 
net_eff = net_eff %>% mutate(q = 0.738/(1+1525*exp(-0.0824*net_eff$size)), size_class = rep(1:nrow(net_eff)))
ns_rec = left_join(ns_rec, net_eff, by = c("size", "size_class"))
ns_rec = ns_rec %>% mutate(number_sel = number/q)

survival_2month2 = survival_2month %>% mutate(year = year-1)
survival_2month2_latest = abind(survival_2month2 %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-2) %>% select(surv), as.numeric(str_sub(Sys.Date(), 1, 4))-1) %>% data.frame
survival_2month2 = abind(survival_2month2, survival_2month2_latest, along = 1) %>% data.frame() %>% dplyr::rename(year = V2)

ns_rec = left_join(ns_rec, survival_2month2, by = "year") %>% mutate(weight = 1.867*10^(-5)*((ns_rec$size_class+0.5)*10)^(3.068))

ns_rec2 = ns_rec %>% mutate(number_sel2 = number_sel*surv, year2 = year+1, maturity = 100/(1+exp(-1.967*((size_class+0.5)-15.309)))) %>% mutate(number_adult = number_sel2*maturity*0.01) %>% mutate(biomass_adult = number_adult*weight)

biomass_female = ddply(ns_rec2, .(year2), summarize, biomass = sum(biomass_adult)/2)

summary(est)
rec_number = est %>% select(number, year, age) %>% mutate(year2 = year-3) %>% filter(age == 2)
srr = left_join(biomass_female, rec_number, by = "year2")
srr = srr %>% mutate(rps = number/(biomass*0.001))



### figures 
g = ggplot(srr %>% na.omit(),  aes(x = year2, y = rps))
b = geom_bar(stat = "identity", width = 0.5, colour = "black", fill = "black")
lab = labs(x = "年級", y = "RPS（尾/kg）", legend = NULL)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.8)),
           axis.title.y = element_text(size = rel(1.8)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig13 = g+b+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand= c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 60))
ggsave(file = "fig13.png", plot = fig13, units = "in", width = 11.69, height = 8.27)



g = ggplot(srr %>% na.omit(), aes(x = year2, y = number/1000000))
p = geom_point(size = 5)
l = geom_line(size = 1)
lab = labs(x = "", y = "2歳魚尾数（百万尾）")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.8)),
           axis.title.y = element_text(size = rel(1.8)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
ko = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2018, by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 100))

g = ggplot(srr %>% na.omit(), aes(x = year2, y = biomass/1000000))
p = geom_point(size = 5)
l = geom_line(size = 1)
lab = labs(x = "年級", y = "雌親魚量（トン）")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.8)),
           axis.title.y = element_text(size = rel(1.8)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
oya = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2018, by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 6000))

fig14 = grid.arrange(ko, oya, ncol = 1)
ggsave(file = "fig14.png", plot = fig14, units = "in", width = 11.69, height = 8.27)



srr2 = srr %>% na.omit() %>% mutate(year3 = ifelse(year2 == 1996, 1996, ifelse(year2 == 2018, 2018, NA)))

g = ggplot(srr2, aes(x = biomass/1000000, y = number/1000000, label = year3))
p = geom_point(size = 5)
l = geom_line(size = 1)
pa = geom_path()
lab = labs(x = "雌親魚量（トン）", y = "2歳魚尾数（百万尾）")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.8)),
           axis.title.y = element_text(size = rel(1.8)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
# fig15 = g+p+pa+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0),limits = c(0, 6000))+scale_y_continuous(expand = c(0,0),limits = c(0, 100))+geom_label_repel()

# g+p+pa+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0),limits = c(0, 6000))+scale_y_continuous(expand = c(0,0),limits = c(0, 100))+geom_text(aes(x = biomass/1000000+1, label = year3), size = rel(2.8), hjust = 2)

fig15 = g+p+pa+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0),limits = c(0, 5800))+scale_y_continuous(expand = c(0,0),limits = c(0, 100))+annotate("text", x = 350, y = 3, label = "1996", size = 6)+annotate("text", x = 5000, y = 5, label = "2018", size = 6)


# +geom_text_repel(size = 5)
# +geom_label_repel()
# +geom_text(aes(label = year2), data = srr %>% filter(year %in% c(1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017)), nudge_x = -250)

ggsave(file = "fig15.png", plot = fig15, units = "in", width = 11.69, height = 8.27, scale = 0.9)





# -------------------------------------------------------------------------
# 2-5  その他の図表 
# -------------------------------------------------------------------------
# step 1; yearly trend of the survival rate at age 1
surv_fig = survival %>% filter(age == 2)
surv_fig = surv_fig %>% mutate(temp = as.numeric(str_sub(surv_fig$year, 3, 4))-1) %>% mutate(temp2 = temp+1) 
surv_fig = surv_fig %>% mutate(temp3 = ifelse(surv_fig$temp == -1, "99", ifelse(surv_fig$temp < 10, formatC(surv_fig$temp, width = 2, flag = "0"), surv_fig$temp))) %>% mutate(temp4 = ifelse(temp2 < 10, formatC(surv_fig$temp2, width = 2, flag = "0"), ifelse(temp2 == 100, "00", temp2))) %>% mutate(xtitle = paste0(temp3, "→", temp4)) %>% select(surv, xtitle, year)

surv_fig$
  unique(surv_fig$xtitle)


g = ggplot(surv_fig, aes(x = year, y = surv))
p = geom_point(size = 5)
l = geom_line(size = 1)
pa = geom_path()
lab = labs(x = "当年", y = "前年1歳魚尾数に対する当年2歳魚尾数の比率")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.8)),
           axis.title.y = element_text(size = rel(1.8)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
figa41 = g+p+pa+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand = c(0.03, 0.03))+scale_y_continuous(expand = c(0,0),limits = c(0, 12))
ggsave(file = "figa41.png", plot = figa41, units = "in", width = 11.69, height = 8.27, scale = 0.9)
