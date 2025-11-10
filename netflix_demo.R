setwd("E:/學/研究所/netflix")
df =read.csv("netflix_techblog_articles_detailed.csv")
df <- df[ , !(names(df) %in% "comment_count")]
df$title[df$article_url == "https://netflixtechblog.com/consoleme-a-central-control-plane-for-aws-permissions-and-access-fd09afdd60a8"] <- 
  "ConsoleMe: A Central Control Plane for AWS Permissions and Access"

df$read_time <- as.numeric(gsub(" min read", "", df$read_time))
library(lubridate)

# lubridate 可以自動解析這類格式
df$publish_date <- mdy(df$publish_date)  # month-day-year
names(df)[names(df) == "publish_date"] <- "publish_time"

# 拆成年、月、日
df$publish_Y <- as.integer(format(df$publish_time, "%Y"))
df$publish_M <- as.integer(format(df$publish_time, "%m"))
df$publish_D <- as.integer(format(df$publish_time, "%d"))

########################
library(dplyr)
library(ggplot2)
library(lubridate)

# 若 publish_time 尚未轉為 Date，可執行：
# df$publish_time <- as.Date(df$publish_time)

df_yearly <- df %>%
  group_by(publish_Y) %>%
  summarise(article_count = n())


# 時間序列折線圖
ggplot(df_yearly, aes(x = publish_Y, y = article_count)) +
  geom_line(color = "red1", size = 1) +
  geom_point(color = "red1") +
  labs(
    title = "Netflix Tech Blog 每年發文數變化趨勢",
    x = "發佈月份",
    y = "文章數量"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background  = element_rect(fill = "#222222", color = NA),  # 整體背景
    panel.background = element_rect(fill = "#333333", color = NA),  # 圖表內部
    panel.grid.major = element_line(color = "#444444", size = 0.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "white"),
    axis.title       = element_text(color = "white"),
    plot.title       = element_text(color = "white", face = "bold", size = 16),
    plot.subtitle    = element_text(color = "gray80"),
    legend.background = element_rect(fill = "#222222", color = NA),
    legend.text      = element_text(color = "white"),
    legend.title     = element_text(color = "white")
  )
######################
library(dplyr)
library(ggplot2)
library(scales)

# 取 log1p 以處理極端值（log(1+x)）
df <- df %>%
  mutate(
    log_clap = log1p(clap_count),
    log_read = log1p(read_time),
    z_clap = scale(log_clap)[,1],
    z_read = scale(read_time)[,1]
  )
summary(df)
ggplot(df, aes(x = z_read, y = z_clap)) +
  geom_bin2d(bins = 30) +
  scale_fill_viridis_c(option = "magma", trans = "sqrt", labels = comma) +
  labs(
    title = "拍手數 × 閱讀時間 熱圖（自然對數的標準化後）",
    subtitle = "顏色越亮代表該區域文章密集度越高",
    x = "閱讀時間（標準化）",
    y = "拍手數（自然對數的標準化）",
    fill = "文章數"
  ) +
  theme_minimal(base_size = 28) +
  theme(
    plot.background  = element_rect(fill = "#222222", color = NA),  # 整體背景
    panel.background = element_rect(fill = "#333333", color = NA),  # 圖表內部
    panel.grid.major = element_line(color = "#444444", size = 0.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "white"),
    axis.title       = element_text(color = "white"),
    plot.title       = element_text(color = "white", face = "bold", size = 16),
    plot.subtitle    = element_text(color = "gray80"),
    legend.background = element_rect(fill = "#222222", color = NA),
    legend.text      = element_text(color = "white"),
    legend.title     = element_text(color = "white")
  )
ggplot(df, aes(x = z_read, y = z_clap)) +
  geom_bin2d(bins = 30) +
  geom_density_2d(color = "white", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red2", se = FALSE, size = 1) +
  scale_fill_viridis_c(option = "magma", trans = "sqrt") +
  labs(
    title = "拍手數與閱讀時間的熱圖＋趨勢線",
    x = "閱讀時間（標準化）",
    y = "拍手數（自然對數的標準化）"
  ) +
  theme_minimal(base_size = 28) +
  theme(
    plot.background  = element_rect(fill = "#222222", color = NA),  # 整體背景
    panel.background = element_rect(fill = "#333333", color = NA),  # 圖表內部
    panel.grid.major = element_line(color = "#444444", size = 0.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "white"),
    axis.title       = element_text(color = "white"),
    plot.title       = element_text(color = "white", face = "bold", size = 16),
    plot.subtitle    = element_text(color = "gray80"),
    legend.background = element_rect(fill = "#222222", color = NA),
    legend.text      = element_text(color = "white"),
    legend.title     = element_text(color = "white")
  )
######################
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)

# 每篇文章拆成長表格
df_long <- df %>% separate_rows(tags, sep = ",")
#####
df_long %>%
  count(tags, sort = TRUE) %>%
  top_n(20, n) %>%  # 前 20 熱門
  ggplot(aes(x = reorder(tags, n), y = n)) +
  geom_col(fill = "red2") +
  coord_flip() +
  labs(title = "熱門 TAG 排序", x = "TAG", y = "出現次數") +
  theme_minimal()+
theme_minimal(base_size = 28) +
  theme(
    plot.background  = element_rect(fill = "#222222", color = NA),  # 整體背景
    panel.background = element_rect(fill = "#333333", color = NA),  # 圖表內部
    panel.grid.major = element_line(color = "#444444", size = 0.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color = "white"),
    axis.title       = element_text(color = "white"),
    plot.title       = element_text(color = "white", face = "bold", size = 16),
    plot.subtitle    = element_text(color = "gray80"),
    legend.background = element_rect(fill = "#222222", color = NA),
    legend.text      = element_text(color = "white"),
    legend.title     = element_text(color = "white")
  )

#########