library(tidyverse)
setwd("~/RIDIR/Data/MainUS/")

data0a = enframe(read_lines("MainUS_v02.txt"), value="line")
data0b = enframe(read_lines("MainUS_v03.txt"), value="line")

data0 = bind_rows(data0a, data0b)

data1 = data0 %>%
  filter(str_detect(line, 'TIME')) 

fields1 = c("ts","start","appId","time","tag","stage","data")
fields2 = c("partitions", "dataset", "tolerance", "run")
data2 = data1 %>% 
  separate(sep = "\\|", col = "line", into = fields1, extra = "drop") %>%
  separate(sep = "_"  , col = "data", into = fields2, extra = "drop") %>%
  filter(stage == "layer1" | stage == "layer2" | stage == "overlay") %>%
  select(time, stage, partitions, appId) %>%
  mutate(time = as.numeric(time) / 1000.0) %>%
  mutate(partitions = fct_relevel(partitions, "1000", "2000", "3000", "4000", "5000", "6000", "7000", "8000", "9000", "10000", "11000", "12000", "13000", "14000", "15000")) %>%
  add_column(size = "MainUS")

data3 = data2 %>%
  group_by(partitions, stage, size) %>% summarise(time = mean(time)) 

write_tsv(data3, "MainUS.tsv")

p = ggplot(data3, aes(x = partitions, y = time, fill = stage)) + 
  geom_col(width = 0.7, position="dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Number of partitions", y="Time [s]", title=paste0("Performance MainUS dataset (36M edges) for SDCEL computation"))
plot(p)

ggsave(paste0("MainUS.pdf"), width = 6, height = 4)