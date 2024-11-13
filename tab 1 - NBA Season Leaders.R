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
year_input = "2024-2025"
stat_input = "3-Pointers Added"
    stat_col = menu_map(stat_input)
per_game = "Per Game"
    #per_game = "Total"
    pg_factor = ifelse(per_game == "Per Game",T,F)

all_year = df %>% filter(Year == year_input)
leaders_static = all_year[,c("Player","Team", "Hex","G",stat_col)]
names(leaders_static)[ncol(leaders_static)] = "Stat"
if (pg_factor){
  min_games = .75*(max(leaders_static$G))
  leaders_static = leaders_static %>% mutate(Stat = Stat/G) %>% filter(G >= min_games) %>%  # if looking at per game stats, divide Stat by G and remove players who missed 25%+ of the season.
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(Player = paste0(rk,". ",Player), display_stat = paste0(" ",round(Stat,3)))
} else{
  leaders_static = leaders_static %>% 
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(Player = paste0(rk,". ",Player), display_stat = paste0(" ",round(Stat,3)))
}

leaders_static$Player = factor(leaders_static$Player, levels = rev(leaders_static$Player))
leaders_static = leaders_static %>% mutate(display_stat_single = paste0(display_stat, " "))

plot = leaders_static %>% ggplot(aes(x = Player, y = Stat, fill = Hex, alpha = I(3/5))) +
  geom_bar(stat = "identity", color = "black", aes(fill = Hex)) + theme_bw() + coord_flip() +
  scale_fill_identity() + theme(legend.position = "none") +
  scale_y_continuous(name = paste0(stat_input,ifelse(pg_factor," (Per Game) "," (Total) ")), limits = c(0,max(leaders_static$Stat)+((max(leaders_static$Stat)/9.5)))) + scale_x_discrete(name = "") +
  theme(axis.text.y = element_text(hjust = 0,size=6)) +
  geom_text(aes(label = display_stat_single), hjust = 1, size = 3) + 
  ggtitle(label = "", subtitle = paste0(year_input, " Season Leaders"))
leaders_display = leaders_static %>% select(Player, Team, G, Stat)
names(leaders_display)[ncol(leaders_display)] = paste0(ifelse(pg_factor,"Per Game ","Total "), stat_input)

# Compare to
year_input_2 = "2007-2008"
all_year_2 = df %>% filter(Year == year_input_2)
leaders_static_2 = all_year_2[,c("Player","Team", "Hex","G",stat_col)]
names(leaders_static_2)[ncol(leaders_static_2)] = "Stat"
if (pg_factor){
  min_games = .75*(max(leaders_static_2$G))
  leaders_static_2 = leaders_static_2 %>% mutate(Stat = Stat/G) %>% filter(G >= min_games) %>%  # if looking at per game stats, divide Stat by G and remove players who missed 25%+ of the season.
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(Player = paste0(rk,". ",Player), display_stat = paste0(" ",round(Stat,3)))
} else{
  leaders_static_2 = leaders_static_2 %>% 
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(Player = paste0(rk,". ",Player), display_stat = paste0(" ",round(Stat,3)))
}

# remove extra formatting column from leaders_static that we don't need!
leaders_static = leaders_static %>% select(-display_stat_single)
comp_df = data.frame(leaders_static, Year = year_input) %>% rbind.data.frame(
  data.frame(leaders_static_2, Year = year_input_2)
) %>% as_tibble() %>% mutate(rk = paste0("#",rk)) %>% separate(Player, into = c("r_u","Player"),sep = "[\\d+]\\. ")
comp_df$rk = factor(comp_df$rk, levels = rev(paste0("#",seq(1:10))))
comp_df$r_u = 1
comp_df = comp_df %>% mutate(disPlayer = paste0(Player," "))

plot_2 = comp_df %>% ggplot(aes(x = rk, y = Stat, fill = Hex)) +
  facet_grid(Year ~ .) + 
  geom_bar(stat = "identity", color = "black", aes(fill = Hex),alpha = I(3/5)) + theme_bw() + coord_flip() +
  scale_fill_identity() + theme(legend.position = "none") +
  scale_y_continuous(name = paste0(stat_input,ifelse(pg_factor," (Per Game) "," (Total) ")), limits = c(0,max(comp_df$Stat)+((max(comp_df$Stat)/9.5)))) + scale_x_discrete(name = "") +
  theme(axis.text.y = element_text(hjust = 0)) + 
  geom_text(aes(label = disPlayer), hjust = 1, size = I(2.25)) + 
  geom_text(aes(label = display_stat), hjust = 0, size = I(2.25)) +
  ggtitle(label = "",subtitle = paste0("Comparing the ",year_input," and ",year_input_2," NBA seasons"))

# Summary Statistics
comb_df = rbind.data.frame(all_year, all_year_2)
summ = comb_df[,c("Player","Team", "Year","Hex","G",stat_col)]
names(summ)[ncol(summ)] = "Stat"
if (pg_factor){
  min_games = .75*(min(max(all_year$G),max(all_year_2$G)))
  summ = summ %>% mutate(Stat = Stat/G) %>% filter(G >= min_games) %>%  # if looking at per game stats, divide Stat by G and remove players who missed 25%+ of the (in this case, shorter) season.
    arrange(desc(Stat))
} else{
  summ = summ %>% 
    arrange(desc(Stat))
}
summ = summ %>% left_join(comp_df %>% select(Player, Year, r_u), by = c("Player", "Year"))
summ_yi = summ %>% filter(Year == year_input); summ_yi2 = summ %>% filter(Year == year_input_2)
sim_yi = c(); sim_yi2 = c()
for (i in 1:10000){
  sim_yi = c(sim_yi, mean(sample(summ_yi$Stat,replace = T,size = 5)))
  sim_yi2 = c(sim_yi2, mean(sample(summ_yi2$Stat,replace = T,size = 5)))
}
sims = rbind.data.frame(data.frame(sim = sim_yi,Year = year_input), data.frame(sim = sim_yi2,Year = year_input_2))

plot_3_in = 
  sims %>% ggplot(aes(x = sim, color = Year)) + 
  geom_histogram(alpha = I(1/4), position = "identity", bins = 30, aes(y = ..density.., fill = Year)) +
  geom_density(alpha = I(4/5))
# Extract the data from the ggplot object 
plot_data <- ggplot_build(plot_3_in)$data[[1]]
plot_3 = plot_3_in + geom_boxplot(data = summ %>% filter(r_u == 1), width = (1/15)*(max(plot_data$ymax)-(min(plot_data$ymin))),
             aes(x = Stat, y = ((1/4)*min(plot_data$ymin) + (3/4)*max(plot_data$ymax)), color = Year), alpha = I(2/5), varwidth = T) + theme_bw() +
  geom_text(size = 3, color = "black", hjust = 0.5, aes(x = ((1/2)*min(comp_df$Stat) + (1/2)*max(comp_df$Stat)), y = ((1/6)*min(plot_data$ymin) + (5/6)*max(plot_data$ymax)), label = "Leaders")) +
  scale_y_continuous("Normalized Density") + ggtitle(label = "Distribution Comparison") +
  scale_x_continuous(name = paste0(stat_input,ifelse(pg_factor," (Per Game) "," (Total) ")))

print(leaders_display)
print(plot)
print(plot_2)
print(plot_3)
