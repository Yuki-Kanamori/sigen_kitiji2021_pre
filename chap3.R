
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
# load the packages -------------------------------------------------------
require(tidyr)
require(dplyr)
require(plyr)
require(ggplot2)
require(maps)
require(mapdata)
require(stringr)
require(cowplot)


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
