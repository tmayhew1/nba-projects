library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes)
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
      df$Tm = factor(df$Tm, levels = c("TOT",team_keys[order(team_keys)]))
      df = df %>% distinct(Player, Age,.keep_all = T) %>% filter(is.na(Tm)==F)
      df = df %>% data.frame(key = player_keys) %>% select(-Rk) %>% data.frame(Year = i) %>% select(key, Year, everything())
      df %>% write.csv(paste0("Totals/",i,".csv"))
      print(i)
    }
  }
}

totals_files = list.files(path = "Totals/", pattern = 'csv')
all_data = data.frame(); year_df = data.frame()
for (file in totals_files){
  year_df = read.csv(paste0("Totals/",file))[,-1] %>% as_tibble()
  all_data = rbind.data.frame(all_data, year_df)
}

all_data_norm = data.frame(); all_data_standard = data.frame()
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
  norm_data = scale(year_data %>% select(-key,-Year,-Player,-Pos,-Age,-Tm,-X2PAdd,-X3PAdd,-FTAdd))
  year_data_norm = year_data %>% select(key,Year,Player,Pos,Age,Tm) %>% data.frame(norm_data) %>% as_tibble()
  all_data_norm = all_data_norm %>% rbind.data.frame(year_data_norm)
}

all_data_standard %>% write.csv(paste0("Complete Data/Totals_s_",Sys.Date(),".csv"))
all_data_norm %>% write.csv(paste0("Complete Data/Totals_n_",Sys.Date(),".csv"))