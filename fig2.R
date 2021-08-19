age = rep(1:10)
weight = c(4,8,12,18,25,33,42,52,63,75)
length = c(5.6, 6.8, 7.9, 8.9, 9.9, 10.8, 11.7, 12.6, 13.4, 14.2)

df = data.frame(age, weight, length)

g = ggplot(df, aes(x = age, y = length))
p = geom_point(shape = 16, size = 3)
l = geom_line(size = 0.6, linetype = "solid")
lab = labs(x = "年齢", y = "● 体長（cm）", shape = "")
th = theme(aspect.ratio = 6.88/11.49,
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = 8, angle = 90, colour = "black"),
           axis.text.y = element_text(size = 8, colour = "black"),
           axis.title.x = element_text(size = 11),
           axis.title.y = element_text(size = 11),
           legend.title = element_text(size = 11),
           strip.text.x = element_text(size = 11))
base = g+p+l+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_y_continuous(breaks = seq(0, 20, by = 2), sec.axis = sec_axis(~ ., name = "○ 体重（g）"), limits = c(0, 20))+scale_x_continuous(breaks = seq(1, 10, by = 1)) 

g = ggplot(df, aes(x = age, y = weight))
p = geom_point(shape = 1, size = 3)
l = geom_line(size = 0.6, linetype = "solid")
lab = labs(x = "年齢", y = "体重（g）", shape = "")
th = theme(aspect.ratio = 6.88/11.49,
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = 8, angle = 90, colour = "black"),
           axis.text.y = element_text(size = 8, colour = "black"),
           axis.title.x = element_text(size = 11),
           axis.title.y = element_text(size = 11),
           legend.title = element_text(size = 11),
           strip.text.x = element_text(size = 11))
add = g+p+l+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_y_continuous(breaks = seq(0, 80, by = 10), sec.axis = sec_axis(~ ., name = "not use", breaks = seq(0, 80, by = 10)), limits = c(0, 80))+scale_x_continuous(breaks = seq(1, 10, by = 1))

base_gtable <- ggplotGrob(base)
additional_gtable <- ggplotGrob(add)

#gtableの構造をチェック
additional_gtable #パネルはc(7,5), 右軸はc(7,6)

#グラフ (点と線) を抜き出す
additional_gtable$grobs[[6]]$children
additional_line_and_points <- additional_gtable$grobs[[6]]$children[3:4] 
# 右軸を抜き出す
additional_right_y_axis <- additional_gtable$grobs[[9]]
#重ねられるグラフ（base）の右軸を見えなくする
base_gtable$grobs[[9]]$gp <- gpar(alpha = 0) #gparはgridパッケージの関数
# 重ねる
fig2 <- base_gtable %>% 
  gtable_add_grob(additional_line_and_points, 7, 5, name = c("a", "b")) %>% 
  gtable_add_grob(additional_right_y_axis, 7, 6) #gtable_add_grobはgtableパッケージの関数