
# SUMMARY -----------------------------------------------------------------
# 3-1  図4; 漁場の空間分布 
#      (引き継ぎ資料の3-1部分)
# step 1 成長曲線の前処理（成長曲線に不必要な10+, 10++, and ?のデータを除去する）
# step 2 von Bertalanffy growth curveにfittingし，パラメータ（k and t0）の推定を行う
# step 3 ALKの作成 (number at age)    ※表がcsvで出てきます
# step 4 ALKの作成 (age composition)  ※表がcsvで出てきます
# step 5 年齢別資源微数の算出
# 
# 
# 3-2  図9; 漁獲物の体長組成 
#      (引き継ぎ資料の3-2部分)
#      
#      
# 3-3  図17; F-YPRとF-%SPRの関係 
#      (引き継ぎ資料の3-3部分)
#      
#      
# 3-5  補足図3-1; 調査地点，密度分布，及び体長組成 
#      (引き継ぎ資料の3-5部分)     
# 
# 
# 3-6  補足図3-2; 年齢別体長組成
#      (引き継ぎ資料の3-6部分)
# 
# 
# 3-7  補足表3-1; 調査から得られた資源量と資源微数の経年変化
#      (引き継ぎ資料の3-7部分)



# load the packages -------------------------------------------------------
require(xlsx)
require(openxlsx)
require(readxl)
require(tidyr)
require(dplyr)
require(plyr)
require(ggplot2)
require(maps)
require(mapdata)
require(cowplot)
require(mapdata)
require(investr)
require(stringr)
require(gridExtra)
require(ggrepel)

# please change here -----------------------------------------------------------
# set working directory
# setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/森川さん由来/R01d_キチジ資源評価")

# # how many years ago
# # e.g. wanna analyze the data of 2018 and now is 2020, then n = 2
# n = 2


# 3-1 図4: 漁場の空間分布 --------------------------------------------------------------
# please change here -----------------------------------------------------------
# set working directory
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021")

# lonlat = read.delim("Map.txt", header=T)
# lonlat2 = read.table("キアンコウ緯度経度.txt", header=T, fileEncoding = "CP932")
# 
# gyoseki_2years_ago = read.table("kitiji_gyoseki2018.txt", header=T)

###10進法で、LONGとLATのみ。列名もLONGとLATにしてください###
x <- read.delim("Map.txt", header=T)


###地図の入ったテキストファイル###
z <- read.table("キアンコウ緯度経度.txt", header=T, fileEncoding = "CP932")
z2 = read.xlsx("okisoko_after2019.xlsx", sheet = "2020")
# z2 <- read.table("kitiji_gyoseki2018.txt", header=T)

head(z2)
head(z)

for(i in 1 : nrow(z2)) {
  z2[i, 10] <- as.numeric(z[which(z2[i, 4]==z[, 2]), 5])
  z2[i, 11] <- as.numeric(z[which(z2[i, 4]==z[, 2]), 6])
}
y <- z2 
colnames(y) <- c(colnames(y)[1:9], "緯度", "経度")

y[, 12] <- as.numeric(as.character(paste(as.numeric(as.character(substr(y$緯度, 1, 2)))+as.numeric(as.character(substr(y$緯度, 4, 5)))/60)))
y[, 13] <- as.numeric(as.character(paste(as.numeric(as.character(substr(y$経度, 1, 3)))+as.numeric(as.character(substr(y$経度, 5, 6)))/60)))
y <- data.frame(AREA = tapply(y$漁区, y$漁区, mean), 
                LAT = tapply(y[, 12], y$漁区, mean), 
                LONG = tapply(y[, 13], y$漁区, mean), 
                catch = tapply(y$漁獲量の合計, y$漁区, sum))


quartzFonts(HiraKaku = quartzFont(rep("HiraginoSans-W3", 4)))
theme_set(theme_cowplot(font_family = "HiraKaku")) 

map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
region2 = subset(world_map, world_map$region == "Japan")
local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + coord_map(xlim = c(140.5, 145.5), ylim = c(35, 43))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.8)),
           axis.title.y = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           plot.title = element_text(size = rel(1.8)),
           legend.title = element_text(size = rel(1.8)),
           legend.text = element_text(size = rel(1.8)))
# th = theme(panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            axis.text.x = element_text(size = rel(1.2), angle = 90),
#            axis.text.y = element_text(size = rel(1.5)),
#            axis.title.x = element_text(size = rel(1.5)),
#            axis.title.y = element_text(size = rel(1.5)),
#            legend.title = element_blank(),
#            legend.text = element_text(size = rel(1.2)),
#            strip.text.x = element_text(size = rel(1.5)),
#            legend.position = c(0.85, 0.8),
#            legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
p = geom_point(data = y, aes(x = LONG, y = LAT, colour = catch/1000), shape = 15, size = 4)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
labs = labs(x = "経度", y = "緯度", colour = "漁獲量（トン）")
fig4 = local_map+theme_bw(base_family = "HiraKakuPro-W3")+th+p+c+labs+
  geom_hline(yintercept = 40.5, colour="black", linetype = "dashed")+
  geom_hline(yintercept = 39, colour="black", linetype = "dashed")+
  geom_hline(yintercept = 38, colour="black", linetype = "dashed")+
  geom_hline(yintercept = 36.5, colour="black", linetype = "dashed")+
  annotate("text",label="尻屋崎", x=144.5, y=41, family="HiraKaku", size = 6)+
  annotate("text",label="岩手", x=144.3, y=39.5, family="HiraKaku", size = 6)+
  annotate("text",label="金華山", x=144.4, y=38.5, family="HiraKaku", size = 6)+
  annotate("text",label="房総", x=144.3, y=37, family="HiraKaku", size = 6) 
ggsave(file = "fig4.png", plot = fig4, units = "in", width = 8.27, height = 11.69)




# ---------------------------------------------------------------
# 3-5  補足図3-1; 調査地点，密度分布，及び体長組成 ---------------------------------
# ---------------------------------------------------------------
trawl_length = read.xlsx("q_魚種別体長別資源量2020.xlsx") %>% filter(和名 == "キチジ")
# trawl_length = read.csv("trawl_ns_length2.csv", fileEncoding = "CP932")
trawl_length1 = trawl_length[, c(6,10,11,15)]

colnames(trawl_length1)
colnames(trawl_length1) = c("NS", "station_code", "depth", "total_number")
summary(trawl_length1)
number_at_depth = ddply(trawl_length1, .(station_code, depth), summarize, total = sum(total_number))
# number_at_depth$depth2 = as.factor(number_at_depth$depth)
unique(number_at_depth$depth)
number_at_depth$depth2 = factor(number_at_depth$depth, levels = c("150", "250", "350", "450", "550", "650", "750", "900"))

g = ggplot(number_at_depth, aes(x = depth2, y = total/1000))
b = geom_bar(stat = "identity", width = 1, colour = "grey50")
lab = labs(x = "水深（m）", y = "資源密度", title = "(B)")
f = facet_wrap(~ station_code, ncol = 2)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.8)),
           axis.title.y = element_text(size = rel(1.8)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           plot.title = element_text(size = rel(1.8)))
figa31b = g+b+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_y_continuous(expand = c(0,0),limits = c(0, 25))
# ggsave(file = "figa31b.png", plot = figa31b, units = "in", width = 11.69, height = 8.27)
# bquote('Assimilation ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')
ggsave(file = "figA31B.png", plot = figa31b, units = "in", width = 8.27, height = 11.69)
ggsave(file = "figA31B_2.png", plot = figa31b, units = "in", width = 11.69, height = 8.27)





trawl_length2 = trawl_length[, c(6,16:ncol(trawl_length))]
colnames(trawl_length2)
summary(trawl_length2)
trawl_length2 = trawl_length2 %>% tidyr::gather(key = temp, value = extention_number, 2:ncol(trawl_length2)) 
trawl_length2 = trawl_length2 %>% mutate(size_class = as.numeric(str_sub(trawl_length2$temp, 3, 4))) %>% filter(size_class < 32)
summary(trawl_length2)
colnames(trawl_length2)[1] = "NS"
trawl_length2$NS2 = ifelse(trawl_length2$NS == "N", "北部", "南部")
levels(trawl_length2$NS2)
trawl_length2$NS2 = factor(trawl_length2$NS2, levels = c("北部", "南部"))
trawl_length2 = ddply(trawl_length2, .(size_class, NS2), summarize, total = sum(extention_number))
summary(trawl_length2)

g = ggplot(trawl_length2, aes(x = size_class, y = total/1000, fill = NS2))
b = geom_bar(stat = "identity", width = 0.8, colour = "black", position = "dodge")
lab = labs(x = "体長（cm）", y = "資源尾数 (千尾)", title = "(C)")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.8)),
           axis.title.y = element_text(size = rel(1.8)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           plot.title = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
c = scale_fill_manual(values =  c("black", "white"))
figa31c = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(0, max(trawl_length2$size_class), by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, (max(trawl_length2$total/1000))+2))

ggsave(file = "figA31C.png", plot = figa31c, units = "in", width = 11.69, height = 8.27)



# ---------------------------------------------------------------
# 3-56  補足図3-2; 年齢別体長組成（調査） ---------------------------------
# ---------------------------------------------------------------
sheets = excel_sheets("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021/ALdata.xlsx") #シート名の取得
freq_at_age_table = NULL
for(i in 1:length(sheets)){
  options(warn=-1)
  df = read.xlsx("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021/ALdata.xlsx", sheet = sheets[i]) %>% filter(pick == 1) %>% select(label, SL, age)
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
  age_comp = age_composition
  age_comp = age_comp %>% gather(key = l, value = freq, 2:ncol(age_comp))
  age_comp = age_comp %>% filter(age != "total") %>% mutate(size_class = as.numeric(str_sub(l, 1,2))) %>% select(-l) %>% mutate(year = sheets[i])
  
  len_num = read.csv("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021/survey_N_at_length.csv", fileEncoding = "CP932") %>% mutate(year = as.numeric(str_sub(調査種類名称, 1, 4))) %>% filter(year == as.numeric(sheets[i])) %>% select(-year)
  len_num = len_num[, 16:ncol(len_num)] %>% mutate(site = c("N", "S"))
  len_num = len_num %>% gather(key = age_j, value = number, 1:(ncol(len_num)-1)) %>% na.omit() %>% mutate(size_class = as.numeric(str_sub(age_j, 3, 4)))
  surv_n_total = ddply(len_num, .(size_class), summarize, n_total = sum(number))
  
  age_comp = full_join(age_comp, surv_n_total, by = "size_class")
  age_comp = age_comp %>% mutate(number = freq*n_total) %>% filter(age > 0)
  
  freq_at_age_table = rbind(freq_at_age_table, age_comp)
}





# age_comp = read.csv("number_at_age_freq.csv")
# age_comp = age_comp[-nrow(age_comp), -1]
# 
# 
# len_num = read.csv("survey_N_at_length.csv", fileEncoding = "CP932") %>% mutate(year = as.numeric(str_sub(調査種類名称, 1, 4))) %>% filter(year == 2020) %>% select(-year)
# len_num = len_num[, 16:ncol(len_num)] %>% mutate(site = c("N", "S"))
# len_num = len_num %>% gather(key = age_j, value = number, 1:(ncol(len_num)-1)) %>% na.omit() %>% mutate(size_class = as.numeric(str_sub(age_j, 3, 4)))
# surv_n_total = ddply(len_num, .(size_class), summarize, n_total = sum(number))
# 
# head(age_comp)
# head(surv_n_total)
# age_comp = full_join(age_comp, surv_n_total, by = "size_class")
# age_comp = age_comp %>% mutate(number = freq*n_total) %>% filter(age > 0)


old = read.csv("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021/survey_N_at_age.csv")
old = old %>% gather(key = l, value = number, 3:ncol(old))
old = old %>% mutate(size_class = as.numeric(str_sub(old$l, 2, 4))) %>% select(-l)

head(old)
head(age_comp)
# ここで2019と2020の両方のデータを生成する必要がある．多分chap2の資源量推定のところでも2019の年齢別漁獲量を算出しているから，そこのコードが使えるはず
new = freq_at_age_table %>% mutate(Age = ifelse(age == 10, "10+", age), Year = year) %>% select(Age, Year, size_class, number) 

all = rbind(old, new)


#figure
levels(all$Age)
unique(all$Age)
all$age = factor(all$Age, levels = c("1", "2", "3", "4", "5", "5+", "6", "7", "8", "9", "10+"))
mode(all$size_class)
summary(all)

g = ggplot(all, aes(x = size_class, y = number/1000000, fill = age))
b = geom_bar(stat = "identity", width = 0.5, colour = "black", size = 0.5)
lab = labs(x = "体長 (cm)", y = "資源尾数 (百万尾)", fill = "年齢")
col_catch = c("red1", "red1", "darkorange", "goldenrod1", "goldenrod4", "grey60", "palegreen3", "palegreen4", "steelblue3", "steelblue4", "grey60")
c = scale_fill_manual(values = col_catch)
f = facet_wrap(~ Year, ncol = 5)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
fig_a32 = g+b+lab+c+f+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(0, 36, by = 5))+scale_y_continuous(expand = c(0,0))
ggsave(file = "fig_A32.png", plot = figa32, units = "in", width = 11.69, height = 8.27)
