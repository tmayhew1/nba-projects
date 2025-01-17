library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes); library(data.table)
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
lesearch = function(input_df,input_p,gp_input=""){
  gpl_control = input_df %>% arrange(desc(valueAdd/G)) %>% inner_join(gpl_df,by = join_by(Year)) %>% filter(G > ifelse(gp_input=="",gpl,gp_input)) %>% select(-gpl) 
  input_df = gpl_control %>% mutate(Rk = 1:nrow(gpl_control))
  input_df = input_df %>% mutate(vaPG = valueAdd/G)
  m = mean(input_df$vaPG);sd = sd(input_df$vaPG);input_df$z_s = (input_df$vaPG-m)/sd
  output = input_df[(max(((which(grepl(pattern = input_p,x = input_df$Player)))-5),1)):(min(((which(grepl(pattern = input_p,x = input_df$Player)))+5),nrow(input_df))),]
  output = output %>% transmute(Rk,Player,Team,G,Pos,MP = MP/G,PTS = PTS/G, TRB = TRB/G, AST = AST/G, X3P. = X3P/X3PA, X3PA = X3PA/G, FG. = FG/FGA, FGA = FGA/G, STK = (STL+BLK)/G, vaPG, z_s)
  for (i in 1:nrow(output)){
    laPTS = sum(input_df$PTS)/sum(input_df$MP);output$PTS[i] = paste0(round(as.double(output$PTS[i]),2),"(",ifelse(as.double(output$PTS[i])/output$MP[i]>laPTS,"+","-"),")")
    laTRB = sum(input_df$TRB)/sum(input_df$MP);output$TRB[i] = paste0(round(as.double(output$TRB[i]),2),"(",ifelse(as.double(output$TRB[i])/output$MP[i]>laTRB,"+","-"),")")
    laAST = sum(input_df$AST)/sum(input_df$MP);output$AST[i] = paste0(round(as.double(output$AST[i]),2),"(",ifelse(as.double(output$AST[i])/output$MP[i]>laAST,"+","-"),")")
    la3P = sum(input_df$X3P)/sum(input_df$X3PA);output$X3P.[i] = paste0(round(as.double(output$X3P.[i]),3),"(",ifelse(output$X3P.[i]>la3P,"+","-"),")")
    laFG = sum(input_df$FG)/sum(input_df$FGA);output$FG.[i] = paste0(round(as.double(output$FG.[i]),3),"(",ifelse(output$FG.[i]>laFG,"+","-"),")")
    laSTK = (sum(input_df$STL)+sum(input_df$BLK))/(sum(input_df$MP));output$STK[i] = paste0(round(as.double(output$STK[i]),2),"(",ifelse(as.double(output$STK[i])/output$MP[i]>laSTK,"+","-"),")")
  }
  output
}
tesearch = function(input_df,input_t){
  gpl_control = input_df %>% arrange(desc(valueAdd/G)) #%>% inner_join(gpl_df,by = join_by(Year)) %>% filter(G > gpl) %>% select(-gpl) 
  input_df = gpl_control %>% mutate(Rk = 1:nrow(gpl_control))
  output = input_df[which(input_df$Team==input_t),] %>% arrange(desc(MP*G)) %>% head(10)
  output %>% transmute(Rk,Player,Team,G,MP = MP/G,PTS = PTS/G, TRB = TRB/G, AST = AST/G, X3P. = X3P/X3PA, X3PA = X3PA/G, FG. = FG/FGA, FGA = FGA/G, STK = (STL+BLK)/G, vaPG = valueAdd/G) %>% arrange(desc(vaPG))
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
  min_games = gpl_df$gpl[which(gpl_df$Year == year_input)]
  leaders_static = leaders_static %>% mutate(Stat = Stat/G) %>% filter(G >= min_games) %>%  # if looking at per game stats, divide Stat by G and remove players who missed 25%+ of the season.
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(display_stat = paste0(" ",round(Stat,3)))
} else{
  leaders_static = leaders_static %>% 
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(display_stat = paste0(" ",round(Stat,3)))
}
leaders_static = leaders_static %>% separate(Player,into = c("disPlayer","bbref"),sep = "\\(",remove = F) %>% select(-bbref)
leaders_static = leaders_static %>% mutate(rk = paste0("#",rk), disPlayer = paste0(disPlayer, " "))
leaders_static$rk = factor(leaders_static$rk, levels = rev(paste0("#",seq(1:10))))

plot = leaders_static %>% ggplot(aes(x = rk, y = Stat, fill = Hex)) +
  geom_bar(stat = "identity", color = "black", aes(fill = Hex), alpha = I(3/5)) + theme_bw() + coord_flip() +
  scale_fill_identity() + theme(legend.position = "none") +
  scale_y_continuous(name = paste0(stat_input,ifelse(pg_factor," (Per Game) "," (Total) ")), limits = c(0,max(leaders_static$Stat)+((max(leaders_static$Stat)/9.5)))) + scale_x_discrete(name = "") +
  geom_text(aes(fontface = "bold",label = disPlayer), hjust = 1, size = I(2.25)) + 
  geom_text(aes(label = display_stat), hjust = 0, size = I(2.25)) +
  ggtitle(label = "", subtitle = ifelse(pg_factor,paste0(year_input, " Season Leaders (min. ",(floor(gpl_df[nrow(gpl_df),]$gpl))+1," games)"),paste0(year_input, " Season Leaders ")))

# Compare to
year_input_2 = "2004-2005"
all_year_2 = df %>% filter(Year == year_input_2)
leaders_static_2 = all_year_2[,c("Player","Team", "Hex","G",stat_col)]
names(leaders_static_2)[ncol(leaders_static_2)] = "Stat"
if (pg_factor){
  min_games = gpl_df$gpl[which(gpl_df$Year == year_input_2)]
  leaders_static_2 = leaders_static_2 %>% mutate(Stat = Stat/G) %>% filter(G >= min_games) %>%  # if looking at per game stats, divide Stat by G and remove players who missed 25%+ of the season.
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(display_stat = paste0(" ",round(Stat,3)))
} else{
  leaders_static_2 = leaders_static_2 %>% 
    arrange(desc(Stat)) %>% head(10) %>% data.frame(rk = 1:10) %>% mutate(display_stat = paste0(" ",round(Stat,3)))
}
leaders_static_2 = leaders_static_2 %>% separate(Player,into = c("disPlayer","bbref"),sep = "\\(", remove = F) %>% select(-bbref)
leaders_static_2 = leaders_static_2 %>% mutate(rk = paste0("#",rk), disPlayer = paste0(disPlayer, " "))
leaders_static_2$rk = factor(leaders_static$rk, levels = rev(paste0("#",seq(1:10))))

# remove extra formatting column from leaders_static that we don't need!
comp_df = data.frame(leaders_static, Year = year_input) %>% rbind.data.frame(
  data.frame(leaders_static_2, Year = year_input_2)
) %>% as_tibble()
comp_df$rk = factor(comp_df$rk, levels = rev(paste0("#",seq(1:10))));comp_df$r_u = 1;max_S = max(comp_df$Stat)
comp_df = comp_df %>% mutate(disPlayer = ifelse(Stat < .3*(max_S),"",disPlayer))

plot_2 = comp_df %>% ggplot(aes(x = rk, y = Stat, fill = Hex)) +
  facet_grid(Year ~ .) + 
  geom_bar(stat = "identity", color = "black", aes(fill = Hex),alpha = I(3/5)) + theme_bw() + coord_flip() +
  scale_fill_identity() + theme(legend.position = "none") +
  scale_y_continuous(name = paste0(stat_input,ifelse(pg_factor," (Per Game) "," (Total) ")), limits = c(0,max(comp_df$Stat)+((max(comp_df$Stat)/9.5)))) + scale_x_discrete(name = "") +
  theme(axis.text.y = element_text(hjust = 0)) + 
  geom_text(aes(fontface = "bold",label = disPlayer), hjust = 1, size = I(2.25)) + 
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

print(plot_3)
print(plot)
#print(plot_2)

all_year %>% lesearch("LeBron James")
all_year %>% tesearch("CLE")

# Compare 3-Pointers Added
#p1 = psearch("CJ McCollum")
#p2 = psearch("Stephen Curry")
#all_year %>% filter(Player %in% c(p1,p2)) %>% transmute(Player, Team, X3PAdd, X3PAdd/G, X3P., X3PA/G) %>% arrange(desc(`X3PAdd/G`))

