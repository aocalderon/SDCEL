library(tidyverse)
setwd("/home/and/Documents/Papers/SDCEL/figures/experiments/CA_speedup/")

data0 = enframe(read_lines("CA_speedup_v01.txt"), value="line")

data1 = data0 %>%
  filter(str_detect(line, 'TIME')) 

fields1 = c("ts","start","appId","time","tag","stage","data")
fields2 = c("dataset", "tolerance", "overlay_method", "overlay_level", "partitions", "nodes", "run")
data2 = data1 %>% 
  separate(sep = "\\|", col = "line", into = fields1, extra = "drop") %>%
  separate(sep = "_"  , col = "data", into = fields2, extra = "drop") %>%
  filter(stage == "layer1" | stage == "layer2" | stage == "overlay") %>%
  select(time, stage, nodes) %>%
  mutate(time = as.numeric(time) / 1000.0, nodes = as.factor(nodes))

nodes_labels = c("3", "6", "9", "12")
data3 = data2 %>%
  group_by(nodes, stage) %>% summarise(time = mean(time))  %>%
  mutate(nodes = fct_relevel(nodes, nodes_labels))

write_tsv(data3, "CA_speedup.tsv")

p = ggplot(data3, aes(x = nodes, y = time)) + 
  geom_col(width = 0.7, position="dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Number of nodes", y="Time [s]", title=paste0("Speed up for CA dataset")) +
  facet_wrap(~ stage)
plot(p)

W = as.numeric(Sys.getenv("R_WIDTH"))
H = as.numeric(Sys.getenv("R_HEIGHT"))
ggsave(paste0("CA_speedup.pdf"), width = W, height = H)
