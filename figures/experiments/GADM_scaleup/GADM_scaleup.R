library(tidyverse)
setwd("/home/and/Documents/Papers/SDCEL/figures/experiments/GADM_scaleup/")

data0 = enframe(read_lines("GADM_scaleup_v01.txt"), value="line")

data1 = data0 %>%
  filter(str_detect(line, 'TIME')) 

fields1 = c("ts","start","appId","time","tag","stage","data")
fields2 = c("dataset", "tolerance", "overlay_method", "overlay_level", "partitions", "nodes", "run")
fields3 = c(NA, NA, "size")
data2 = data1 %>% 
  separate(sep = "\\|", col = "line", into = fields1, extra = "drop") %>%
  separate(sep = "_"  , col = "data", into = fields2, extra = "drop") %>%
  separate(sep = "/"  , col = "dataset", into = fields3, extra = "drop") %>%
  filter(stage == "layer1" | stage == "layer2" | stage == "overlay") %>%
  select(time, stage, size) %>%
  mutate(time = as.numeric(time) / 1000.0, size = as.factor(size)) %>%
  mutate(size = recode_factor(size, S0 = "15M", S1 = "30M", S2 = "45M", S3 = "60M"))

size_labels = c("15M", "30M", "45M", "60M")
data3 = data2 %>%
  group_by(size, stage) %>% summarise(time = mean(time))  %>%
  mutate(size = fct_relevel(size, size_labels))

write_tsv(data3, "GADM_scaleup.tsv")

p = ggplot(data3, aes(x = size, y = time)) + 
  geom_col(width = 0.7, position="dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Size [number of edges]", y="Time [s]", title=paste0("Scale up for GADM dataset")) +
  facet_wrap(~ stage)
plot(p)

W = as.numeric(Sys.getenv("R_WIDTH"))
H = as.numeric(Sys.getenv("R_HEIGHT"))
ggsave(paste0("GADM_scaleup.pdf"), width = W, height = H)