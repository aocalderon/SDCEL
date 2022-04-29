require(tidyverse)
require(lubridate)

paramsPattern = "num-executors|filter"
getParams <- function(command){
  params = str_trim(str_split(command, "--")[[1]])
  params = params[grepl(paramsPattern, params)]
  return(paste(params, collapse = " "))
}

log = enframe(readLines("scaleup.txt"))
spark = log %>% filter(grepl(value, pattern = "SparkSubmit ")) %>% 
  separate(value, into = c("time", "duration", "appId", "command"), sep = "\\|")
spark$params = spark$command %>% map(getParams)
spark = spark %>% separate(params, into = c(NA,"nodes", NA, "filter"), sep = " ") %>%
  mutate(load = rep(1:4, 10)) %>%
  select(appId, nodes, filter, load) 

START = "Getting LDCELs for B"
END   = "Merging DCELs"
pattern = paste0(START,"|",END)
fields = c("time", "duration", "appId", "phase")
sdcel0 = log %>% filter(grepl(value, pattern = pattern)) %>%
  separate(value, fields, sep = "\\|") %>%
  mutate(phase = str_trim(phase), duration = as.numeric(duration) / 1000.0) %>%
  mutate(time = parse_date_time(str_replace(time,",","."), "%Y-%m-%d %H:%M:%OS")) %>%
  select(appId, phase, time, duration) 

## Pivot by time (results in minutes)...
sdcel1 = sdcel0 %>% select(appId, phase, time) %>% pivot_wider(names_from = phase, values_from = time)
names(sdcel1) = c("appId", "read", "merge")
sdcel = sdcel1 %>% mutate(time = merge - read)

## Pivot by duration (results in seconds)...
##sdcel2 = sdcel0 %>% select(appId, phase, duration) %>% pivot_wider(names_from = phase, values_from = duration)
##names(sdcel2) = c("appId", "read", "merge")
##sdcel = sdcel2 %>% mutate(time = merge - read)

data = spark %>% inner_join(sdcel, by = "appId") %>% select(nodes, time) %>%
  group_by(nodes) %>% summarise(time = mean(time))
data$nodes = factor(data$nodes, levels = c("3", "6", "9", "12"))

p = ggplot(data = data, aes(x = nodes, y = time)) +
  geom_bar(stat="identity", position=position_dodge(width = 0.75), width = 0.7) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x="Nodes", y="Time [min]", title="Execution time by number of nodes") 
plot(p)

ggsave(paste0("scaleup.pdf"), width = 12, height = 8, device = "pdf")