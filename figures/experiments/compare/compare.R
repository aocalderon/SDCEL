library(tidyverse)
setwd("/home/and/Documents/Papers/SDCEL/figures/experiments/compare/")

gadm   = read_tsv("GADM_compare/GADM_compare.tsv") %>%
  add_column(dataset = "GADM")
mainus = read_tsv("MainUS_compare/MainUS_compare.tsv") %>%
  add_column(dataset = "MainUS")

data = bind_rows(gadm, mainus)

p = ggplot(data, aes(x = dataset, y = time, fill = l1l2)) + 
  geom_col(width = 0.7, position="dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Dataset", y="Time [s]",
       title=paste0("Comparisson by size of the layers during overlay operation")) + 
  guides(fill=guide_legend(title="Size relation"))
plot(p)

W = as.numeric(Sys.getenv("R_WIDTH"))
H = as.numeric(Sys.getenv("R_HEIGHT"))
ggsave(paste0("compare.pdf"), width = W, height = H)
