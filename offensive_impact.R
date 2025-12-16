library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes)
source("totals_collect.R") # totals_collect.R must be run!
df_ = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
gpl_df = df %>% group_by(Year) %>% summarise(.groups = "drop", max_G = max(G)) %>% mutate(gpl = ifelse(max_G > 50,.75*max_G,.6*max_G)) %>% select(-max_G)
menu_map = function(input){
  map = read.csv("Complete Data/menu_options.csv")[,-1]
  return(map$col_name[which(map$display_name == input)])
}
psearch = function(input){
  new = df %>% filter(grepl(input,Player))
  return(new$Player %>% unique())
}

imp_df = df %>% filter(Year == "2025-2026") %>% mutate(score_imp = round((vaPTSv + PTSAdd)/G,2)) %>% arrange(desc(score_imp))
imp_df %>% select(score_imp,everything()) %>% datatable()