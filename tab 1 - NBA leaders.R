library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes)
source("totals_collect.R") # totals_collect.R must be run!
df = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
menu_map = function(input){
  map = read.csv("Complete Data/menu_options.csv")[,-1]
  return(map$col_name[which(map$display_name == input)])
}
psearch = function(input){
  new = df %>% filter(grepl(input,Player))
  return(new$Player %>% unique())
}

#Start here!
year_input = "2023-2024"
stat_input = "3-Pointers Added"
    stat_col = menu_map(stat_input)
#per_game = "Per Game"
    per_game = "Total"
    pg_factor = ifelse(per_game == "Per Game",T,F)

all_year = df %>% filter(Year == year_input)
leaders_static = all_year[,c("Player","Team", "Hex","G",stat_col)]
names(leaders_static)[ncol(leaders_static)] = "Stat"
if (pg_factor){
  min_games = .75*(max(leaders_static$G))
  leaders_static = leaders_static %>% mutate(Stat = Stat/G) %>% filter(G >= min_games) %>%  # if looking at per game stats, divide Stat by G and remove players who missed 25%+ of the season.
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(Player = paste0(rk,". ",Player), display_stat = round(Stat,3))
} else{
  leaders_static = leaders_static %>% 
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(Player = paste0(rk,". ",Player), display_stat = round(Stat,3))      
}

leaders_static$Player = factor(leaders_static$Player, levels = rev(leaders_static$Player))
plot = leaders_static %>% ggplot(aes(x = Player, y = Stat, fill = Hex, alpha = I(4/5))) +
  geom_bar(stat = "identity", color = "black", aes(fill = Hex)) + theme_bw() + coord_flip() +
  scale_fill_identity() + theme(legend.position = "none") +
  scale_y_continuous(name = stat_input, limits = c(0,max(leaders_static$Stat)+((max(leaders_static$Stat)/9.5)))) + scale_x_discrete(name = "") +
  theme(axis.text.y = element_text(hjust = 0)) + 
  geom_text(aes(label = display_stat), hjust = -0.25) + ggtitle(label = paste0(ifelse(pg_factor,"Per Game ","Total "), stat_input, " Leaders"),subtitle = paste0("for the ",year_input, " NBA season"))

leaders_display = leaders_static %>% select(Player, Team, G, Stat)
names(leaders_display)[ncol(leaders_display)] = paste0(ifelse(pg_factor,"Per Game ","Total "), stat_input)
  
# Compare to
year_input_2 = "2017-2018"
all_year_2 = df %>% filter(Year == year_input_2)
leaders_static_2 = all_year_2[,c("Player","Team", "Hex","G",stat_col)]
names(leaders_static_2)[ncol(leaders_static_2)] = "Stat"
if (pg_factor){
  min_games = .75*(max(leaders_static_2$G))
  leaders_static_2 = leaders_static_2 %>% mutate(Stat = Stat/G) %>% filter(G >= min_games) %>%  # if looking at per game stats, divide Stat by G and remove players who missed 25%+ of the season.
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(Player = paste0(rk,". ",Player), display_stat = round(Stat,3))
} else{
  leaders_static_2 = leaders_static_2 %>% 
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(Player = paste0(rk,". ",Player), display_stat = round(Stat,3))      
}

comp_df = data.frame(leaders_static, Year = year_input) %>% rbind.data.frame(
  data.frame(leaders_static_2, Year = year_input_2)
) %>% as_tibble() %>% mutate(rk = paste0("#",rk)) %>% separate(Player, into = c("r_u","Player"),sep = "[\\d+]\\. ")
comp_df$rk = factor(comp_df$rk, levels = rev(paste0("#",seq(1:10))))
comp_df$r_u = 1

plot_2 = comp_df %>% ggplot(aes(x = rk, y = Stat, fill = Hex)) +
  facet_grid(Year ~ .) + 
  geom_bar(stat = "identity", color = "black", aes(fill = Hex),alpha = I(3/5)) + theme_bw() + coord_flip() +
  scale_fill_identity() + theme(legend.position = "none") +
  scale_y_continuous(name = stat_input, limits = c(0,max(comp_df$Stat)+((max(comp_df$Stat)/9.5)))) + scale_x_discrete(name = "") +
  theme(axis.text.y = element_text(hjust = 0)) + 
  geom_text(aes(label = Player), hjust = 1, size = I(2.25)) + 
  geom_text(aes(label = display_stat), hjust = -0.25) +
  ggtitle(label = paste0(ifelse(pg_factor,"Per Game ","Total "), stat_input, " Leaders"),subtitle = paste0("comparing the ",year_input," and ",year_input_2," NBA seasons"))

# Summary Statistics
comb_df = rbind.data.frame(all_year, all_year_2)
summ = comb_df[,c("Player","Team", "Year","Hex","G",stat_col)]
names(summ)[ncol(summ)] = "Stat"
if (pg_factor){
  min_games = .75*(max(summ$G))
  summ = summ %>% mutate(Stat = Stat/G) %>% filter(G >= min_games) %>%  # if looking at per game stats, divide Stat by G and remove players who missed 25%+ of the season.
    arrange(desc(Stat))
} else{
  summ = summ %>% 
    arrange(desc(Stat))
}
summ = summ %>% left_join(comp_df %>% select(Player, Year, r_u), by = c("Player", "Year"))
plot_3 = 
  summ %>% ggplot(aes(x = Stat, fill = Year)) + geom_histogram(bins = 50, position = "dodge") +
  geom_boxplot(data = summ %>% filter(r_u == 1), 
              aes(x = Stat, y = 10, fill = Year), alpha = I(2/5)) + theme_bw()

print(leaders_display)
print(plot)
print(plot_2)