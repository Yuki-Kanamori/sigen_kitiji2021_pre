require(tidyverse)
require(maps)
require(mapdata)

setwd("/Users/Yuki/Dropbox/業務/キチジゲノム")
df = read.csv("loc_DNAsample.csv", fileEncoding = "CP932") %>% filter(PA == "P") %>% mutate(lon = lon1+lon2/60, lat = lat1+lat2/60) %>% select(lon, lat)
summary(df)

# dir = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021/"
# tohoku <- data.frame(read.csv(paste0(dir, "marmap_coord.csv"), fileEncoding = "CP932"))
# colnames(tohoku) <- c("long","lat","depth")
# check = tohoku[tohoku$depth<0 & tohoku$depth>=-1300,]
# summary(check)
# 
# japan <- map_data("japan")
# japan2 <- japan
# japan2$long <- japan$long-0.01


map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
region2 = subset(world_map, world_map$region == "Japan")
local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "gray50", fill = "gray50") + coord_map(xlim = c(140.5, 146), ylim = c(36, 45.1))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           strip.text.x = element_blank(),
           plot.title = element_text(size = rel(1.8)),
           legend.title = element_text(size = rel(1.8)),
           legend.text = element_text(size = rel(1.8)))
p = geom_point(data = df, aes(x = lon, y = lat), shape = 1, size = 5)
# c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
labs = labs(x = "経度", y = "緯度", colour = "漁獲量（トン）")
fig = local_map+theme_bw(base_family = "HiraKakuPro-W3")+th+p+labs

ggsave(file = "DNAsample.png", plot = fig, unit = "cm", width = 6.88, height = 11.49)
