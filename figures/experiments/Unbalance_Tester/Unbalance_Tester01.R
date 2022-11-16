library(tidyverse)
data0 = enframe(read_lines("RangeTester3.txt"), value="line")

data1 = data0 %>% filter(str_detect(line, 'INFO')) %>% filter(str_detect(line, 'END')) 

fields = c("tag","ts","runId","size1","size2","method","stage","time")
data2 = data1 %>% 
  separate(sep = "\\t", col = "line", into = fields, extra = "drop") %>%
  select(method, size2, time) %>%
  mutate(time = as.numeric(time) / 1000.0) %>%
  mutate(method = str_replace(method, "Sweeping", "Filter by sweep"))
  
data3 = data2 %>% group_by(method, size2) %>% summarise(time = mean(time)) 
size2_order = factor(data3$size2, level = c('6K','9K','12K','15K','18K','21K'))
#size2_order = factor(data3$size2, level = c('25K', '67K', '104K', '133K', '158K'))

p = ggplot(data3, aes(x = size2_order, y = time, fill = method)) + 
  geom_col(width = 0.7, position="dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Dataset Size", y="Time [s]",
       title=paste0("Traditional vs Alternative approach during a biased overlay")) + 
  guides(fill=guide_legend(title="Method"))
plot(p)

W = 6
H = 4
ggsave(paste0("RangeTester3.pdf"), width = W, height = H)