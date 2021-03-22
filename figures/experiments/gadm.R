require(tidyverse)
require(lubridate)
library(cowplot)

paramsPattern = "input1"
getParams <- function(command){
  params = str_trim(str_split(command, "--")[[1]])
  params = params[grepl(paramsPattern, params)]
  return(paste(params, collapse = " "))
}
extractPartitions <- function(input){
  return(str_replace(str_replace(input, "gadm/edges_P", ""), "K/edgesA", ""))
}

log = enframe(readLines("gadm.txt"))
spark = log %>% filter(grepl(value, pattern = "SparkSubmit ")) %>% 
  separate(value, into = c("time", "appId", "command"), sep = "\\|")
spark$params = spark$command %>% map(getParams)
spark = spark %>% separate(params, into = c(NA,"input"), sep = " ") %>%
  select(appId, input) %>%
  mutate(partitions = extractPartitions(input))

START = "Reading data"
END   = "Merging DCELs"
pattern = paste0(START,"|",END)
fields = c("time", "appId", "phase")
sdcel0 = log %>% filter(grepl(value, pattern = pattern)) %>%
  separate(value, fields, sep = "\\|") %>%
  mutate(phase = str_trim(phase)) %>%
  mutate(time = parse_date_time(str_replace(time,",","."), "%Y-%m-%d %H:%M:%OS")) %>%
  select(appId, phase, time) 

sdcel1 = sdcel0 %>% pivot_wider(names_from = phase, values_from = time)
names(sdcel1) = c("appId", "read", "merge")
sdcel = sdcel1 %>% mutate(time = merge - read)

data = spark %>% inner_join(sdcel, by = "appId") %>% select(partitions, time) %>%
  group_by(partitions) %>% summarise(time = mean(time)) %>% mutate(method = "Cluster")
data$partitions = factor(data$partitions, levels = c("2", "4", "6", "8", "10", "12", "14"))

fields = c("time", "duration","appId", "phase")
local0 = enframe(readLines("local.txt")) %>% 
  filter(grepl(value, pattern = pattern)) %>%
  separate(value, fields, sep = "\\|") %>%
  mutate(phase = str_trim(phase)) %>%
  mutate(time = parse_date_time(str_replace(time,",","."), "%Y-%m-%d %H:%M:%OS")) %>%
  select(appId, phase, time) %>% 
  pivot_wider(names_from = phase, values_from = time) %>%
  mutate(method = "1-core", partitions = "")
names(local0) = c("appId", "read", "merge", "method", "partitions")
local = local0 %>% mutate(time = difftime(merge, read, units = "min")) %>% 
  select(method, partitions, time)

ymin = 0
ymax = 350
ytic = 100
p = ggplot(data = data, aes(x = partitions, y = time)) +
  geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_grid(~method) +
  scale_y_continuous(breaks = seq(ymin, ymax, ytic)) + coord_cartesian(ylim = c(ymin, ymax)) + 
  labs(x="Partitions (x1000)", y="Time [min]", title="Execution time by number of partitions") 
q = ggplot(data = local, aes(x = partitions, y = time)) +
  geom_bar(stat="identity", width = 0.75) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks.x = element_blank()) + 
  facet_grid(~method) +
  scale_y_continuous(breaks = seq(ymin, ymax, ytic)) + coord_cartesian(ylim = c(ymin, ymax)) + 
  labs(x="", y="", title="") 
plot_grid(p, q, align = "h", ncol = 2, rel_widths = c(6/10, 1/10))
ggsave(paste0("gadm.pdf"), width = 12, height = 8, device = "pdf")

p = ggplot(data = data, aes(x = partitions, y = time)) +
  geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x="Partitions (x1000)", y="Time [min]", title="Execution time by number of partitions") 
plot(p)
ggsave(paste0("gadm_sample.pdf"), width = 12, height = 8, device = "pdf")
