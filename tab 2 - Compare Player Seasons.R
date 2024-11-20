library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes)
source("totals_collect.R") # totals_collect.R must be run!
df = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
gpl_df = df %>% group_by(Year) %>% summarise(.groups = "drop", max_G = max(G)) %>% mutate(gpl = ifelse(max_G > 50,.75*max_G,.6*max_G)) %>% select(-max_G)
menu_map = function(input){
  map = read.csv("Complete Data/menu_options.csv")[,-1]
  return(map$col_name[which(map$display_name == input)])
}
psearch = function(input){
  new = df %>% filter(grepl(input,Player))
  return(new$Player %>% unique())
}

  pi1 = psearch("Anthony Edwards")#"Michael Jordan (jordami01)"
  pi2 = psearch("Dwyane Wade")#"LeBron James (jamesle01)"

player_df = df %>% filter(Player %in% c(pi1,pi2))

# specific statistic - across seasons ("By Statistic")
  stat_input = "Value Added"
  stat_col = menu_map(stat_input)
  per_game = "Per Game"
  #per_game = "Total"
  pg_factor = ifelse(per_game == "Per Game",T,F)

p_static = player_df[,c("Player","Team", "Year", "Hex","G",stat_col)]
names(p_static)[ncol(p_static)] = "Stat"
p1_ = p_static %>% filter(Player == pi1) %>% data.frame(Season = 1:length(which(p_static == pi1))) %>% as_tibble()
p2_ = p_static %>% filter(Player == pi2) %>% data.frame(Season = 1:length(which(p_static == pi2))) %>% as_tibble()
p_static = p1_ %>% rbind.data.frame(p2_) %>% as_tibble(); p_static = p_static %>% arrange(Year)
if (pg_factor){
  p_static = p_static %>% mutate(Stat = Stat/G)
  p_static = p_static %>% inner_join(gpl_df, by = "Year")
  p_static$Year = factor(p_static$Year, levels = rev(unique(p_static$Year)))
  p_static = p_static %>% filter(G > gpl)
} else{
  p_static$Year = factor(p_static$Year, levels = rev(unique(p_static$Year)))
}
top2 = p_static %>% arrange(Player) %>% distinct(Player, .keep_all = T)
p_static = p_static %>% separate(Player,into = c("disPlayer","bbref"),sep = " \\(") %>% mutate(display_stat = paste0(round(Stat,3)," "))

plot = p_static %>% 
    ggplot(aes(x = Season, y = Stat)) + 
    theme_bw() + 
    scale_color_manual(values = c(top2$Hex[1],"grey50"), name = NULL) +
    scale_x_continuous(breaks = 1:max(p_static$Season)) +
    scale_y_continuous(name = paste0(stat_input,ifelse(pg_factor," (Per Game) "," (Total) ")),
                       limits = c(min(p_static$Stat)-(abs(min(p_static$Stat)/2)),max(p_static$Stat)+(max(p_static$Stat)/44))) +
    theme(legend.position = "top") + geom_point(aes(color = disPlayer), size = 1) + geom_line(aes(x = Season, y = Stat, color = disPlayer), alpha = I(.2)) + geom_text(aes(x = Season, y = Stat, label = Year, color = disPlayer), size = 2, vjust = 0, nudge_y = (max(p_static$Stat)-min(p_static$Stat))/80, show.legend = F)
  
plot

# general - ("By Season")
  year_input = "2024-2025"
  year_input_2 = "2023-2024"
    years = gpl_df$Year[min(which(gpl_df$Year %in% c(year_input,year_input_2))):max(which(gpl_df$Year %in% c(year_input,year_input_2)))]

py = player_df %>% filter(Year %in% years)
top2 = py %>% arrange(Player) %>% distinct(Player, .keep_all = T)
py_sum = py %>% group_by(Player) %>% 
  summarise(.groups = "drop"
      ,G = sum(G),FG. = sum(FG)/sum(FGA),X2P. = sum(X2P)/sum(X2PA),X3P. = sum(X3P)/sum(X3PA),FT. = sum(FT)/sum(FTA),TRBpG = sum(TRB)/sum(G),
      ASTpG = sum(AST)/sum(G),PTSpG = sum(PTS)/sum(G),STLpG = sum(STL)/sum(G),BLKpG = sum(BLK)/sum(G),TOVpG = sum(TOV)/sum(G),X2PAddpG = sum(X2PAdd)/sum(G),
      X3PAddpG = sum(X3PAdd)/sum(G),FTAddpG = sum(FTAdd)/sum(G),PTSAddpG = sum(PTSAdd)/sum(G))

py_toplot = py_sum %>% pivot_longer(cols = G:PTSAddpG,
                                    names_to = "variable", # Name for the new 'variable' column 
                                    values_to = "value" # Name for the new 'value' column 
                                    )
sample_plt = py_toplot %>% ggplot(aes(x = variable, y = value, fill = Player)) + 
  geom_bar(stat = 'identity', position = "dodge") +
  coord_flip()



