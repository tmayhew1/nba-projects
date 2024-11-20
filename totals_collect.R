library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes)
today_file = paste0("Complete Data/Totals_s_",Sys.Date(),".csv",collapse = "")
exist_yn = ifelse(file.exists(today_file),T,F)
if (!exist_yn){
    for (i in 1950:(as.numeric(format(Sys.Date(), "%Y"))+1)){
    if (file.exists(paste0("Totals/",i,".csv")) & i!= as.numeric(format(Sys.Date(), "%Y")) & i!= as.numeric(format(Sys.Date(), "%Y"))+1){
      #print("Totals for this year exist already:")
      #print(i)
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
        #print(i)
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
    #print(file)
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

  df = all_data_standard %>% as_tibble()
  summary = df %>% group_by(Year) %>% 
    summarise(.groups = "drop"
              ,PTS = sum(PTS)
              ,TRB = sum(TRB)
              ,DRB = sum(DRB)
              ,ORB = sum(ORB)
              ,AST = sum(AST)
              ,STL = sum(STL)
              ,BLK = sum(BLK)
              ,TOV = sum(TOV)
              ,MP = sum(MP)
              ,X3PA = sum(X3PA)
              ,X3P = sum(X3P)
              ,X2PA = sum(X2PA)
              ,X2P = sum(X2P)
              ,FT = sum(FT)
              ,FTA = sum(FTA)
    ) %>% transmute(Year
                    ,laPTSperM = PTS/MP
                    ,laTRBperM = TRB/MP
                    ,laDRBperM = DRB/MP
                    ,laORBperM = ORB/MP
                    ,laASTperM = AST/MP
                    ,laSTLperM = STL/MP
                    ,laBLKperM = BLK/MP
                    ,laTOVperM = TOV/MP
                    ,laPTSperMake = ifelse(is.na(X3PA),2,((3*X3P)+(2*X2P))/(X3P + X2P)) 
                    ,laPTSperPoss = (PTS)/((X2PA + X3PA) + TOV + (FTA/2.1))
                    ,laORBrate = (ORB/TRB)
                    ,laDRBrate = (DRB/TRB)
    )
  
  # build a model to predict points per possession based on PTS per minute and TRB per minute
  summary = summary %>% mutate(model_set = ifelse(is.na(laPTSperPoss),"test","train"))
  model = lm(data = summary %>% filter(model_set == "train"), formula = laPTSperPoss ~ laPTSperM + laTRBperM)
  new_pr = predict(model, newdata = summary %>% filter(model_set == "test") %>% select(laPTSperM, laTRBperM))
  summary$laPTSperPoss[1:30] = new_pr
  
  va_df = df %>% left_join(summary, by = "Year") %>% mutate(
    valueAdd_1 = ((PTS/MP)-(laPTSperM))*(MP) + #points added (volume)
      PTSAdd + #points added (efficiency)
      (((AST/MP)-(laASTperM))*(MP))*(laPTSperMake)*(0.5) + #assists added
      (((STL/MP)-(laSTLperM))*(MP))*(laPTSperPoss) + #steals added
      (((BLK/MP)-(laBLKperM))*(MP))*(laPTSperPoss)*(laDRBrate) + #blocks added
      -1*(((TOV/MP)-(laTOVperM))*(MP))*(laPTSperPoss) + #turnovers added
      (((DRB/MP)-(laDRBperM))*(MP))*(laPTSperPoss)*(laORBrate) + #d rebounds added
      (((ORB/MP)-(laORBperM))*(MP))*(laPTSperPoss)*(laDRBrate), #o rebounds added
    
    valueAdd_2 = ((PTS/MP)-(laPTSperM))*(MP) + #points added (volume)               #fixes 74-77
      PTSAdd + #points added (efficiency)
      (((AST/MP)-(laASTperM))*(MP))*(laPTSperMake)*(0.5) + #assists added
      (((STL/MP)-(laSTLperM))*(MP))*(laPTSperPoss) + #steals added
      (((BLK/MP)-(laBLKperM))*(MP))*(laPTSperPoss)*(laDRBrate) + #blocks added
      #-1*(((TOV/MP)-(laTOVperM))*(MP))*(laPTSperPoss) + #turnovers added
      (((DRB/MP)-(laDRBperM))*(MP))*(laPTSperPoss)*(laORBrate) + #d rebounds added
      (((ORB/MP)-(laORBperM))*(MP))*(laPTSperPoss)*(laDRBrate), #o rebounds added
    
    valueAdd_3 = ((PTS/MP)-(laPTSperM))*(MP) + #points added (volume)               #fixes 52-73
      PTSAdd + #points added (efficiency)
      (((AST/MP)-(laASTperM))*(MP))*(laPTSperMake)*(0.5) + #assists added
      #-1*(((TOV/MP)-(laTOVperM))*(MP))*(laPTSperPoss) + #turnovers added
      (((TRB/MP)-(laTRBperM))*(MP))*(laPTSperPoss)*(0.28)*(0.72) + #(est) d rebounds added
      (((TRB/MP)-(laTRBperM))*(MP))*(laPTSperPoss)*(0.72)*(0.28) #(est) o rebounds added
    
  ) %>% mutate(valueAdd = coalesce(valueAdd_1,valueAdd_2,valueAdd_3))
  
  all_data_standard %>% left_join(va_df %>% select(Player, Year, valueAdd), by = c("Year", "Player")) %>% write.csv(paste0("Complete Data/Totals_s_",Sys.Date(),".csv",collapse = ""))
}

exist_yn = ifelse(file.exists(today_file),T,F)
print(paste0(Sys.Date()," - File created: ",exist_yn))