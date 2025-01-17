library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes); library(plotly); library(gridExtra)
source("totals_collect.R") # totals_collect.R must be run!
df = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
lga = read.csv("Complete Data/avgsSummary.csv")[,-1] %>% separate(Year, into = c("pre", "Year"), sep = "\\-") %>% select(-pre) %>% select(Year, everything())
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

### Player/Year inputs:
if (T){
  p1_input = "Dorian Finney-Smith"
  date_input_1_start = as.Date(Sys.Date()-260) #260
  date_input_1_end = as.Date(Sys.Date())
    
  p2_input = "Jalen Brunson"
  date_input_2_start = as.Date(Sys.Date()-260) #260
  date_input_2_end = as.Date(Sys.Date())
  
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
      if (length(which(names(data.raw[[8]])=="+/-"))==0){
        reg_games_1 = data.raw[[8]] %>% select(G, Date, Tm, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc)
        reg_games_1 = reg_games_1 %>% mutate(PlusMinus = 0)
        
      } else{
        reg_games_1 = data.raw[[8]] %>% 
          select(G, Date, Tm, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc, `+/-`)
        reg_games_1 = reg_games_1 %>% set_names(nm = c("G", "Date", "Tm", "MP", "FG", "FGA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc", "PlusMinus"))
      }
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
      if (length(which(names(data.raw[[8]])=="+/-"))==0){
        reg_games_2 = data.raw[[8]] %>% select(G, Date, Tm, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc)
        reg_games_2 = reg_games_2 %>% mutate(PlusMinus = 0)
        
      } else{
        reg_games_2 = data.raw[[8]] %>% 
          select(G, Date, Tm, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc, `+/-`)
        reg_games_2 = reg_games_2 %>% set_names(nm = c("G", "Date", "Tm", "MP", "FG", "FGA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc", "PlusMinus"))
      }
      reg_games_2 = reg_games_2 %>% filter(!is.na(as.double(FG))) %>% separate(col = MP, into = c("MP", "SP"),sep = "\\:") %>% mutate(MP = as.double(MP)+(as.double(SP)/60)) %>% select(-SP) %>% data.frame(Player = p2_input)
      p2_df = p2_df %>% rbind.data.frame(reg_games_2) %>% mutate(Date = as.Date(Date)) %>% as_tibble()
    }
  }
  
  p1_df = p1_df %>% filter(Date >= date_input_1_start, Date <= date_input_1_end) 
  p2_df = p2_df %>% filter(Date >= date_input_2_start, Date <= date_input_2_end)
  p1_df = p1_df %>% mutate(G = 1:nrow(p1_df),across(!c(Date,Tm,Player),as.double)); p2_df = p2_df %>% mutate(G = 1:nrow(p2_df),across(!c(Date,Tm,Player,G,MP),as.double))
  # end data collect
}

# Start here - add in RA input reactive, stat input reactive, load the data frame!
roll_avg_input = "10"
stat_input = "3-Pointers Added"
  stat_col = menu_map(stat_input)

cdf = p1_df %>% rbind.data.frame(p2_df) %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = c("Tm" = "Team"))
cdf = cdf %>% mutate(across(c(X3P,X3PA,FT,FTA,FG,FGA),as.numeric)) %>% mutate(X2P = FG-X3P, X2PA = FGA-X3PA)
cdf = cdf %>% separate(Date, into = c("Year", "m", "d"), remove=F) %>% select(-m, -d) %>% inner_join(lga, by = "Year")
cdf = cdf %>% mutate(X3PAdd = ((X3P/ifelse(X3PA==0,1,X3PA))-(la3P.))*(X3PA),X2PAdd = ((X2P/ifelse(X2PA==0,1,X2PA))-(la2P.))*(X2PA),FTAdd = ((FT/ifelse(FTA==0,1,FTA))-(laFT.))*(FTA),
         valueAdd = ((PTS/MP)-(laPTSperM))*(MP) + #points added (volume)
           ((3*X3PAdd)+(2*X2PAdd)+FTAdd) + #points added (efficiency)
           (((AST/MP)-(laASTperM))*(MP))*(laPTSperMake)*(0.5) + #assists added
           (((STL/MP)-(laSTLperM))*(MP))*(laPTSperPoss) + #steals added
           (((BLK/MP)-(laBLKperM))*(MP))*(laPTSperPoss)*(laDRBrate) + #blocks added
           -1*(((TOV/MP)-(laTOVperM))*(MP))*(laPTSperPoss) + #turnovers added
           (((DRB/MP)-(laDRBperM))*(MP))*(laPTSperPoss)*(laORBrate) + #d rebounds added
           (((ORB/MP)-(laORBperM))*(MP))*(laPTSperPoss)*(laDRBrate), #o rebounds added
         fPTS = 2*(FG) + -1*(FGA) + 1*(FT) + -1*(FTA) + 1*(X3P) + 1*(TRB) + 2*(AST) + 4*(STL) + 4*(BLK) + -2*(TOV) + 1*(PTS)
         )

top_color = cdf %>% arrange(Player) %>% head(1)
ra = ifelse(roll_avg_input == "-",1,as.double(roll_avg_input))

# modify Stat columns if stat_input is a percent!
if (grepl("[P|p]ercentage",stat_input)){
  static = cdf[,c("Player","Tm", "G", "Date", str_split(stat_col,"\\.")[[1]][1] %>% paste0(""), str_split(stat_col,"\\.")[[1]][1] %>% paste0("A"))]
  names(static)[(ncol(static)-1):ncol(static)] = c("Make", "Att")
  static$Stat = static$Make/static$Att
  if (ra==1){
    static$Stat_ra = static$Stat
  } else{
    static$Stat_ra = NA
    for (i in 1:nrow(static)){
      if (static$G[i]<ra){
        
        static$Stat_ra[i] = NA
        
      } else{
        static$Stat_ra[i] = (sum(static$Make[(i-ra+1):(i)]))/((sum(static$Att[(i-ra+1):(i)])))
      }
    }
  }
} else{
  static = cdf[,c("Player","Tm", "G", "Date", stat_col)]
  names(static)[ncol(static)] = "Stat"
  static = static %>% mutate(Stat = as.double(Stat))
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
}

static_line = static %>% filter(!is.na(Stat_ra))
static_line = static_line %>% mutate(dateDisp = ifelse((G %in% c(ra,max(static_line$G))|Stat_ra == max(static_line$Stat_ra)),format(Date, "%m/%d/%y"),""))

  plot = static_line %>% ggplot(aes(x = G, y = Stat_ra, color = Player, linetype = Player)) + theme_bw() +
    geom_line() + scale_y_continuous(name = stat_input) +
    scale_color_manual(name = paste0(ra,"-game rolling avg."), values = c(top_color$Hex[1],"grey50")) +
    theme(legend.position = "top") + scale_linetype_manual(name = paste0(ra,"-game rolling avg."), values = c("solid", "dashed")) +
    scale_x_continuous("Games Played (Time Span)") + geom_label(data = static_line %>% filter(dateDisp!=''), aes(label = dateDisp),vjust = 0, size = 2, label.padding = unit(0.1, "lines"),show.legend=F)
    
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
  scale_y_continuous("Normalized Density") + ggtitle("Distribution Comparison", subtitle = "  Based on random 5-game samples") +
  scale_x_continuous(name = stat_input) +
  scale_color_manual("", values = c(top_color$Hex[1],"grey50")) +
  scale_fill_manual("", values = c(top_color$Hex[1],"grey50")) + 
  theme(legend.position = "top", plot.subtitle = element_text(size = 8, face = "italic"))

grid.arrange(plot_3, plot, ncol = 2)

cdf %>% arrange(desc(valueAdd)) %>% transmute(Player, Date, PTS, TRB, AST, BLK, STL, X3PAdd, X2PFTAdd = X2PAdd + FTAdd, valueAdd) %>% head(3)
cdf %>% group_by(Player) %>% summarise(.groups = "drop", G = n(), MP = mean(MP), PTS = mean(PTS), TRB = mean(TRB), AST = mean(AST), X3P. = sum(X3P)/sum(X3PA), X3PA = mean(X3PA), FG. = sum(FG)/sum(FGA), STK = mean(STL+BLK))
cdf %>% filter(grepl(p1_input,Player)) %>% arrange(Date) %>% tail(as.integer(roll_avg_input)) %>% group_by(Player) %>% summarise(.groups = "drop", G = n(), MP = mean(MP), PTS = mean(PTS), TRB = mean(TRB), AST = mean(AST), X3P. = sum(X3P)/sum(X3PA), X3PA = mean(X3PA), FG. = sum(FG)/sum(FGA), STK = mean(STL+BLK)) %>% rbind.data.frame(cdf %>% filter(grepl(p2_input,Player)) %>% arrange(Date) %>% tail(as.integer(roll_avg_input)) %>% group_by(Player) %>% summarise(.groups = "drop", G = n(), MP = mean(MP), PTS = mean(PTS), TRB = mean(TRB), AST = mean(AST), X3P. = sum(X3P)/sum(X3PA), X3PA = mean(X3PA), FG. = sum(FG)/sum(FGA), STK = mean(STL+BLK)))
