require(tidyverse)
require(lubridate)
library(cowplot)

paramsPattern = "input1"
getParams <- function(command){
  params = str_trim(str_split(command, "--")[[1]])
  params = params[grepl(paramsPattern, params)]
  return(paste(params, collapse = " "))
}

log = enframe(readLines("ca.txt"))
spark = log %>% filter(grepl(value, pattern = "SparkSubmit ")) %>% 
  separate(value, into = c("time", "duration", "appId", "command"), sep = "\\|")
spark$params = spark$command %>% map(getParams)
spark = spark %>% separate(params, into = c(NA,"input"), sep = " ") %>%
  select(appId, input) %>%
  separate(input, into = c(NA, "capacity", NA, NA), sep = "/")

START = "Getting LDCELs for B"
END   = "Merging DCELs"
pattern = paste0(START,"|",END)
fields = c("time", "duration", "appId", "phase")
sdcel0 = log %>% filter(grepl(value, pattern = pattern)) %>%
  separate(value, fields, sep = "\\|") %>%
  mutate(phase = str_trim(phase), duration = as.numeric(duration) / 1000.0) %>%
  mutate(time = parse_date_time(str_replace(time,",","."), "%Y-%m-%d %H:%M:%OS")) %>%
  select(appId, phase, time, duration) 

sdcel1 = sdcel0 %>% select(appId, phase, time) %>% pivot_wider(names_from = phase, values_from = time)
names(sdcel1) = c("appId", "read", "merge")
sdcel = sdcel1 %>% mutate(time = merge - read)

pattern = "Time for A: |Time for B: |Time for overlay: "
cgal = enframe(readLines("~/RIDIR/Code/CGAL/DCEL/cgal.txt")) %>% filter(grepl(value, pattern = pattern)) %>%
  separate(value, into = c(NA, NA, NA, NA, "time", NA), sep = " ") %>%
  mutate(time = as.numeric(time)) %>%
  mutate(run = rep(1:5, each = 3), capacity = "") %>% # Modify for more runs...
  group_by(capacity, run) %>% summarise(time = sum(time)) %>% ungroup() %>%
  group_by(capacity) %>% summarise(time = as.duration(mean(time))) %>%
  mutate(method = "CGAL", partitions = "")

data = spark %>% inner_join(sdcel, by = "appId") %>% select(capacity, time) %>%
  group_by(capacity) %>% summarise(time = mean(time)) %>% 
  mutate(method = "SDCEL") # %>% union(cgal)
data$capacity = factor(data$capacity, levels = c("1", "2", "5", "10", "25", "50", "100", "200", "500", "1000"))
data$partitions = recode(data$capacity, "1"="15", "2"="12.5", "5"="10", "10"="7", "25"="4", "50"="2", "100"="1", "200"="0.5", "500"="0.2", "1000"="0.1")

bar1 = ggplot(data = data, aes(x = partitions, y = time)) +
  geom_bar(stat="identity", width = 0.75) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_grid(~method) +
  scale_y_continuous(breaks = seq(0, 800, 200)) + coord_cartesian(ylim = c(0, 800)) + 
  labs(x="Number of partitions (x1000)", y="Time [sec]", title="Execution time by number of partitions compared with CGAL") 
bar2 = ggplot(data = cgal, aes(x = partitions, y = time)) +
  geom_bar(stat="identity", width = 0.75) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks.x = element_blank()) + facet_grid(~method) +
  scale_y_continuous(breaks = seq(0, 800, 200)) + coord_cartesian(ylim = c(0, 800)) + 
  labs(x="", y="", title="") 
plot_grid(bar1, bar2, align = "h", ncol = 2, rel_widths = c(6/10, 1/10))
ggsave(paste0("ca.pdf"), width = 12, height = 8, device = "pdf")

sample = c("1", "2", "5", "10", "25", "50")
p = ggplot(data = data %>% filter(capacity %in% sample), aes(x = partitions, y = time)) +
 geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.7) + 
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
 labs(x="Number of partitions (x1000)", y="Time [sec]", title="Execution time by number of partitions") 

ggsave(paste0("ca_sample.pdf"), width = 12, height = 8, device = "pdf")