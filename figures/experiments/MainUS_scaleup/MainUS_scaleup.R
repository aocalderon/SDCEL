library(tidyverse)
setwd("/home/and/Documents/Papers/SDCEL/figures/experiments/MainUS_scaleup/")

data0 = enframe(read_lines("MainUS_scaleup_v01.txt"), value="line")

data1 = data0 %>%
  filter(str_detect(line, 'TIME')) 

fields1 = c("ts","start","appId","time","tag","stage","data")
fields2 = c("dataset", "tolerance", "overlay_method", "overlay_level", "partitions", "nodes", "run")
fields3 = c(NA, NA, NA, "size")
data2 = data1 %>% 
  separate(sep = "\\|", col = "line", into = fields1, extra = "drop") %>%
  separate(sep = "_"  , col = "data", into = fields2, extra = "drop") %>%
  separate(sep = "/"  , col = "dataset", into = fields3, extra = "drop") %>%
  filter(stage == "layer1" | stage == "layer2" | stage == "overlay") %>%
  select(time, stage, size) %>%
  mutate(time = as.numeric(time) / 1000.0, size = as.factor(size)) %>%
  mutate(size = recode_factor(size, S0 = "8M", S1 = "16M", S2 = "24M", S3 = "32M"))

size_labels = c("8M", "16M", "24M", "32M")
data3 = data2 %>%
  group_by(size, stage) %>% summarise(time = mean(time))  %>%
  mutate(size = fct_relevel(size, size_labels))

write_tsv(data3, "MainUS_scaleup.tsv")

p = ggplot(data3, aes(x = size, y = time)) + 
  geom_col(width = 0.7, position="dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Size [number of edges]", y="Time [s]", title=paste0("Scale up for MainUS dataset")) +
  facet_wrap(~ stage)
plot(p)

W = as.numeric(Sys.getenv("R_WIDTH"))
H = as.numeric(Sys.getenv("R_HEIGHT"))
ggsave(paste0("MainUS_scaleup.pdf"), width = 5, height = 3)