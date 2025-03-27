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

mdf_24 = df_ %>% filter(Year %in% c("2023-2024"))
mdf_25 = df_ %>% filter(Year %in% c("2024-2025")) %>% inner_join(gpl_df,by = join_by(Year)) %>% filter(G > gpl) %>% select(-gpl)
mdf = mdf_24 %>% rbind.data.frame(mdf_25)

players = mdf %>% group_by(Player) %>% summarise(.groups = "drop", count = n()) %>% filter(count > 1)
df = mdf %>% inner_join(players,by = join_by(Player)) %>% arrange(Player,Year) %>% mutate(vaPG = valueAdd/G,diff = 0)

for (i in 1:nrow(df)){
  if (i %% 2 == 0){
    df$diff[i] = paste0(round(df$vaPG[i]-df$vaPG[i-1],2)," (",round(df$vaPG[i],2)," | ",round(df$vaPG[i-1],2),")")
  }
}
df %>% filter(Year == "2024-2025") %>% arrange(desc(diff)) %>% transmute(Player,Age,Team,Pos,PTS/G,TRB/G,AST/G,STL/G,BLK/G,diff) %>% head(20)


