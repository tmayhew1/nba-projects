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