library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes)
today_file = paste0("Complete Data/Totals_s_",Sys.Date(),".csv",collapse = "")
exist_yn = ifelse(file.exists(today_file),T,F)
if (!exist_yn){
  for (i in 1950:(as.numeric(format(Sys.Date(), "%Y"))+1)){
    if (file.exists(paste0("Totals/",i,".csv")) & i!= as.numeric(format(Sys.Date(), "%Y")) & i!= as.numeric(format(Sys.Date(), "%Y"))+1){
      print("Totals for this year exist already:")
      print(i)
    } else{
      url = paste0("https://www.basketball-reference.com/leagues/NBA_",i,"_totals.html")
      page = read_html(url)
      data.raw = html_table(page, fill=TRUE)
      if (length(data.raw)==0){
        print("This year's page is empty:")
        print(i)
      } else{
        df = data.raw[[1]] %>% as_tibble()
        links = page %>% html_nodes("td a") %>% html_attr("href"); players = c(); teams = c()
        for (link in links){
          match = grepl("players\\/[a-z]\\/", link)
          if (match){
            player_key = str_split(str_split(link,"players\\/[a-z]\\/")[[1]][2],"\\.html")[[1]][1]
            players = players %>% c(player_key)
          } else{
            team_key = str_split(str_split(link,"teams\\/")[[1]][2],"\\/")[[1]][1]
            teams = teams %>% c(team_key)
          }
        }
        player_keys = unique(players);team_keys = unique(teams)
        df$Team = factor(df$Team, levels = c("5TM","4TM","3TM","2TM",team_keys[order(team_keys)]))
        df_distinct = df %>% distinct(Player, Age,.keep_all = T) %>% filter(is.na(Team)==F)
        df = df_distinct %>% data.frame(key = player_keys[1:nrow(df_distinct)]) %>% select(-Rk) %>% data.frame(Year = i) %>% select(key, Year, everything())
        df %>% write.csv(paste0("Totals/",i,".csv"))
        print(i)
      }
    }
  }
  
  totals_files = list.files(path = "Totals/", pattern = 'csv')
  all_data = data.frame(); year_df = data.frame()
  for (file in totals_files){
    year_df = read.csv(paste0("Totals/",file))[,-1] %>% as_tibble()
    new_cols = c("GS","MP","X3P","X3PA","X3P.","ORB","DRB","TRB","STL","BLK","TOV")
    for (co in new_cols){
      if ((co %in% names(year_df)) == F){
        year_df = year_df %>% data.frame(co = NA)
        names(year_df)[ncol(year_df)] = co
      }
    }
    year_df = year_df %>% select("key","Year","Player","Age","Team","Pos","G","FG","FGA",
                                 "FG.","X2P","X2PA","X2P.","eFG.","FT","FTA","FT.","TRB",  
                                 "AST","PF","PTS","Awards","GS","MP","X3P","X3PA","X3P.",  
                                 "ORB","DRB","STL","BLK","TOV")
    
    all_data = rbind.data.frame(all_data, year_df)
    print(file)
  }
  
  all_data_standard = data.frame()
  for (y in unique(all_data$Year)[order(unique(all_data$Year))]){
    year_data = all_data %>% filter(Year == y)
    league_X2P. = (sum(year_data$X2P))/(sum(year_data$X2PA));league_X3P. = (sum(year_data$X3P))/(sum(year_data$X3PA)); league_FT. = (sum(year_data$FT))/(sum(year_data$FTA))
    year_data = year_data %>% mutate(X2PAdd = (X2P.-league_X2P.)*X2PA, X3PAdd = (X3P.-league_X3P.)*X3PA, FTAdd = (FT.-league_FT.)*FTA)
    for (j in 1:nrow(year_data)){
      for (k in (ncol(year_data)-2):(ncol(year_data))){
        if (is.na(year_data[j,k])){
          year_data[j,k] = 0
        }
      }
    }
    year_data = year_data %>% mutate(PTSAdd = (2*X2PAdd)+(3*X3PAdd)+FTAdd)
    all_data_standard = all_data_standard %>% rbind.data.frame(year_data)
  }
  
  all_data_standard = all_data_standard %>% mutate(Year = paste0(Year-1,"-",Year), Player = paste0(Player, " (", key, ")")) %>% select(-key)
  all_data_standard %>% write.csv(paste0("Complete Data/Totals_s_",Sys.Date(),".csv",collapse = ""))
  
}

#Start here!
year_input = "2024-2025"

stat_input = "3-Pointers Added"
stat_col = menu_map(stat_input)

# Per Game?
per_game = "Per Game"
#per_game = "Total"

# Player Input
player_input = ""#psearch("Anthony Edwards")

pg_factor = ifelse(per_game == "Per Game",T,F)
all_year = df %>% filter(Year == year_input)
leaders_static = all_year[,c("Player","Team", "Hex","G",stat_col)]
names(leaders_static)[ncol(leaders_static)] = "Stat"
if (player_input == ""){
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
    geom_text(aes(label = display_stat), hjust = -0.25) + ggtitle(label = paste0(ifelse(pg_factor,"Per Game ","Total "), stat_input),subtitle = paste0("for the ",year_input, " NBA season"))
  
  leaders_display = leaders_static %>% select(Player, Team, G, Stat)
  names(leaders_display)[ncol(leaders_display)] = paste0(ifelse(pg_factor,"Per Game ","Total "), stat_input)
  
} else{
  print("Player selected.")
  
  if (pg_factor){
    min_games = .75*(max(leaders_static$G))
    leaders_ = leaders_static %>% mutate(Stat = Stat/G) %>% filter(G >= min_games) %>%  # if looking at per game stats, divide Stat by G and remove players who missed 25%+ of the season.
      arrange(desc(Stat))
    leaders = leaders_ %>% data.frame(rk = 1:nrow(leaders_))
    
    player_rk = which(leaders$Player == player_input)
    if (player_rk < 7){
      leaders = leaders %>% head(10) %>% 
        mutate(Player = paste0(rk,". ",Player), display_stat = round(Stat,3))
    } else if (player_rk > (nrow(leaders)-5)){
      leaders = leaders %>% tail(10) %>% 
        mutate(Player = paste0(rk,". ",Player), display_stat = round(Stat,3))
    } else{
      leaders = leaders[(player_rk-5):(player_rk+4),]
      leaders = leaders %>% mutate(Player = paste0(rk,". ",Player), 
                                   display_stat = round(Stat,3))
    }
  } else{
    leaders_ = leaders_static %>% arrange(desc(Stat))
    leaders = leaders_ %>% data.frame(rk = 1:nrow(leaders_))
    
    player_rk = which(leaders$Player == player_input)
    if (player_rk < 7){
      leaders = leaders %>% head(10) %>% 
        mutate(Player = paste0(rk,". ",Player), display_stat = round(Stat,3))
    } else if (player_rk > (nrow(leaders)-5)){
      leaders = leaders %>% tail(10) %>% 
        mutate(Player = paste0(rk,". ",Player), display_stat = round(Stat,3))
    } else{
      leaders = leaders[(player_rk-5):(player_rk+4),]
      leaders = leaders %>% mutate(Player = paste0(rk,". ",Player), 
                                   display_stat = round(Stat,3))
    } 
  }
  
  # Plot it!
  leaders$Player = factor(leaders$Player, levels = rev(leaders$Player))
  leaders$rk_c = ifelse(leaders$rk == player_rk,.825,.475)
  top_pos = leaders$Stat[1] > 0
  limits_adj = c(0,max(leaders$Stat)+((max(leaders$Stat)/9.5)))
  if (top_pos){
    limits_set = limits_adj
  } else{
    limits_set = NULL
  }
  
  plot = leaders %>% ggplot(aes(x = Player, y = Stat, fill = Hex, alpha = rk_c)) +
    geom_bar(stat = "identity", color = "black", aes(fill = Hex)) + theme_bw() + coord_flip() +
    scale_fill_identity() + scale_alpha_identity() + 
    theme(legend.position = "none") + 
    scale_y_continuous(name = stat_input, limits = limits_set) + 
    scale_x_discrete(name = "") + theme(axis.text.y = element_text(hjust = 0)) + 
    geom_text(aes(label = display_stat), hjust = -0.25) + ggtitle(label = paste0(ifelse(pg_factor,"Per Game ","Total "), stat_input),subtitle = paste0("for the ",year_input, " NBA season"))
  
  leaders_display = leaders %>% select(Player, Team, G, Stat)
  names(leaders_display)[ncol(leaders_display)] = paste0(ifelse(pg_factor,"Per Game ","Total "), stat_input)
  
}

print(leaders_display)
print(plot)


