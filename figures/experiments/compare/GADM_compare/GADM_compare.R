library(tidyverse)
setwd("~/RIDIR/Data/GADM_compare/")

data0a = enframe(read_lines("GADM_compare_T0_v01.txt"), value="line")
data0b = enframe(read_lines("GADM_compare_T1_v01.txt"), value="line")

data0 = bind_rows(data0a, data0b)

data1 = data0 %>%
  filter(str_detect(line, '\\|TIME\\|')) 

fields1 = c("ts","start","appId","time","tag","stage","data")
fields2 = c("dataset", "tolerance", "overlay_method", "overlay_level", "partitions", "nodes", "run")
fields3 = c(NA, NA, "l1l2")
data2 = data1 %>% 
  separate(sep = "\\|", col = "line", into = fields1, extra = "drop") %>%
  separate(sep = "_"  , col = "data", into = fields2, extra = "drop") %>%
  separate(sep = "/"  , col = "dataset", into = fields3, extra = "drop") %>%
  filter(stage == "overlay") %>%
  select(time, stage, l1l2) %>%
  mutate(time = as.numeric(time) / 1000.0, l1l2 = as.factor(l1l2)) %>%
  mutate(l1l2 = recode_factor(l1l2, T0 = "L1 >> L2", T1 = "L1 << L2"))

data3 = data2 %>%
  group_by(stage, l1l2) %>% summarise(time = mean(time))  

write_tsv(data3, "GADM_compare.tsv")

p = ggplot(data3, aes(x = stage, y = time, fill = l1l2)) + 
  geom_col(width = 0.7, position="dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Stage", y="Time [s]",
       title=paste0("Comparisson by size of the layers for MainUS dataset")) + 
  guides(fill=guide_legend(title="Size relation"))
plot(p)

W = as.numeric(Sys.getenv("R_WIDTH"))
H = as.numeric(Sys.getenv("R_HEIGHT"))
ggsave(paste0("MainUS_compare.pdf"), width = W, height = H)
