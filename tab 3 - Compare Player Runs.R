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
lsearch = function(player,year){
  new = df %>% filter(grepl(player,Player))
  key = str_split(new$Player[1],"\\(|\\)")[[1]][2]; letter = str_split(key,"")[[1]][1]
  return(paste0("https://www.basketball-reference.com/players/",letter,"/",key,"/gamelog/",year))
}

p1_input = "Anthony Edwards"
  date_input_1_start = as.Date("2023-11-20")
  date_input_1_end = Sys.Date()
  
p2_input = "Dwyane Wade"
  date_input_2_start = as.Date("2005-11-20")
  date_input_2_end = (as.Date("2006-11-20"))#Sys.Date()
  
# Start data collect
years_1 = unique(c(as.double(str_split(date_input_1_start,"-")[[1]][1]),as.double(str_split(date_input_1_end,"-")[[1]][1]),as.double(str_split(date_input_1_end,"-")[[1]][1])+1))
years_2 = unique(c(as.double(str_split(date_input_2_start,"-")[[1]][1]),as.double(str_split(date_input_2_end,"-")[[1]][1]),as.double(str_split(date_input_2_end,"-")[[1]][1])+1))
years_1 = c(min(years_1),max(years_1));years_2 = c(min(years_2),max(years_2))
p1_df = data.frame();p2_df = data.frame()
for (y in years_1[1]:years_1[2]){
  url = lsearch(p1_input,y)
  page = read_html(url)
  data.raw = html_table(page, fill=TRUE)
  if (length(data.raw)==0){
    print("This year's page is empty:")
    print(y)
  } else{
    reg_games_1 = data.raw[[8]] %>% 
      select(G, Date, Tm, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc, `+/-`)
    reg_games_1 = reg_games_1 %>% set_names(nm = c("G", "Date", "Tm", "MP", "FG", "FGA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc", "PlusMinus"))
    reg_games_1 = reg_games_1 %>% filter(!is.na(as.double(FG))) %>% separate(col = MP, into = c("MP", "SP"),sep = "\\:") %>% mutate(MP = as.double(MP)+(as.double(SP)/60)) %>% select(-SP) %>% data.frame(Player = p1_input)
    p1_df = p1_df %>% rbind.data.frame(reg_games_1) %>% mutate(Date = as.Date(Date)) %>% as_tibble()
  }
}
for (y in years_2[1]:years_2[2]){
  url = lsearch(p2_input,y)
  page = read_html(url)
  data.raw = html_table(page, fill=TRUE)
  if (length(data.raw)==0){
    print("This year's page is empty:")
    print(y)
  } else{
    reg_games_2 = data.raw[[8]] %>% 
      select(G, Date, Tm, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc, `+/-`)
    reg_games_2 = reg_games_2 %>% set_names(nm = c("G", "Date", "Tm", "MP", "FG", "FGA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc", "PlusMinus"))
    reg_games_2 = reg_games_2 %>% filter(!is.na(as.double(FG))) %>% separate(col = MP, into = c("MP", "SP"),sep = "\\:") %>% mutate(MP = as.double(MP)+(as.double(SP)/60)) %>% select(-SP) %>% data.frame(Player = p2_input)
    p2_df = p2_df %>% rbind.data.frame(reg_games_2) %>% mutate(Date = as.Date(Date)) %>% as_tibble()
  }
}

p1_df = p1_df %>% filter(Date >= date_input_1_start, Date <= date_input_1_end) 
p2_df = p2_df %>% filter(Date >= date_input_2_start, Date <= date_input_2_end)
p1_df = p1_df %>% mutate(G = 1:nrow(p1_df)); p2_df = p2_df %>% mutate(G = 1:nrow(p2_df))

roll_avg_input = "20"
#stat_input = "Plus/Minus"
stat_input = "Game Score"
stat_col = menu_map(stat_input)

cdf = p1_df %>% rbind.data.frame(p2_df) %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = c("Tm" = "Team"))

top2 = cdf %>% arrange(Player) %>% distinct(Player, .keep_all = T)
static = cdf[,c("Player","Tm", "G", "Date", stat_col)]
names(static)[ncol(static)] = "Stat"
static = static %>% mutate(Stat = as.double(Stat))

ra = ifelse(roll_avg_input == "-",1,as.double(roll_avg_input))
if (ra==1){
  static$Stat_ra = static$Stat
} else{
  static$Stat_ra = NA
  for (i in 1:nrow(static)){
    if (static$G[i]<ra){
      static$Stat_ra[i] = NA
    } else{
      static$Stat_ra[i] = mean(static$Stat[(i-ra+1):(i)])
    }
  }
}

static_line = static %>% filter(!is.na(Stat_ra))
static_line = static_line %>% mutate(dateDisp = ifelse((G %in% c(ra,max(static_line$G))|Stat_ra == max(static_line$Stat_ra)),format(Date, "%m/%d/%y"),""))

plot = static_line %>% ggplot(aes(x = G, y = Stat_ra, color = Player, linetype = Player)) + theme_bw() +
  geom_line() + scale_y_continuous(name = stat_input) +
  scale_color_manual(values = c(top2$Hex[1],"grey50")) +
  theme(legend.position = "top") + scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous("Games Played (Time Span)") + geom_label(data = static_line %>% filter(dateDisp!=''), aes(label = dateDisp),vjust = 0, size = 2, label.padding = unit(0.1, "lines"),show.legend=F)
plot

sim_p1 = c();sim_p2 = c()
for (j in 1:10000){sim_p1 = c(sim_p1,mean(sample(x = static$Stat[which(static$Player==p1_input)],size = 5,replace = F)))}
for (k in 1:10000){sim_p2 = c(sim_p2,mean(sample(x = static$Stat[which(static$Player==p2_input)],size = 5,replace = F)))}
sims = rbind.data.frame(data.frame(sim = sim_p1,Player = p1_input), data.frame(sim = sim_p2,Player = p2_input)) %>% as_tibble()
# add a sample size (games played) for context
sims = sims %>% mutate(Player = ifelse(Player==p1_input,paste0(Player," (n=",(length(static$Stat[which(static$Player==p1_input)])),")"),paste0(Player," (n=",(length(static$Stat[which(static$Player==p2_input)])),")")))

plot_3_in = 
  sims %>% ggplot(aes(x = sim, color = Player)) + 
  geom_histogram(alpha = I(1/4), position = "identity", bins = 30, aes(y = ..density.., fill = Player)) +
  geom_density(alpha = I(4/5))
# Extract the data from the ggplot object 
plot_data <- ggplot_build(plot_3_in)$data[[1]]
plot_3 = plot_3_in + theme_bw() +
  scale_y_continuous("Normalized Density") + ggtitle(label = "Distribution Comparison") +
  scale_x_continuous(name = stat_input) +
  scale_color_manual("", values = c(top2$Hex[1],"grey50")) +
  scale_fill_manual("", values = c(top2$Hex[1],"grey50")) + theme(legend.position = "top")
