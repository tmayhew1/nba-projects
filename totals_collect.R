library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes)
today_file = paste0("Complete Data/Totals_s_",Sys.Date(),".csv",collapse = "")
exist_yn = ifelse(file.exists(today_file),T,F)
median_perM = function(df, stat_cols, year_col = "Year", minutes_col = "MP") {df %>% filter(!is.na(.data[[minutes_col]])) %>% mutate(across(all_of(stat_cols), ~ .x / .data[[minutes_col]], .names = "la{.col}perM")) %>% group_by(.data[[year_col]]) %>% summarise(across(ends_with("perM"), ~ median(.x, na.rm = TRUE)), .groups = "drop")}

if (!exist_yn){
  for (i in 1950:(as.numeric(format(Sys.Date(), "%Y"))+1)){
    if (file.exists(paste0("Totals/",i,".csv")) & i!= (as.numeric(format(Sys.Date(), "%Y"))) & i!= (as.numeric(format(Sys.Date(), "%Y"))+1)){
      #print("Totals for this year exist already:")
      #print(i)
      #print("Totals for this year do not:")
      #print(i+1)
    } else{
      url = paste0("https://www.basketball-reference.com/leagues/NBA_",i,"_totals.html")
      url_status = GET(url) %>% status_code()
      if (url_status == 200){
        page = read_html(url)
        data.raw = html_table(page, fill=TRUE)
        if (length(data.raw)==0){
          # print("This year's page is empty:")
          # print(i)
        } else{
          df_rs = data.raw[[1]] %>% data.frame(Playoffs = 0) %>% as_tibble()
          if (length(data.raw) < 2){
            df = df_rs
          } else{
            df_pl = data.raw[[2]] %>% data.frame(Playoffs = 1) %>% as_tibble()
            if (ncol(df_rs)==ncol(df_pl)){
              # print(paste0(i," has the same columns in reg. season and playoffs."))
            } else{
              # print(paste0(i," has different columns in reg. season and playoffs."))
              df_rs = df_rs[,-c(which(!(names(df_rs) %in% names(df_pl))))]
            } #check to see if reg. season and playoffs can be rbinded (same columns)
            df = df_rs %>% rbind.data.frame(df_pl)
          }
          
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
          
          df_link_basis = df %>% distinct(Player, Age, .keep_all = T) %>% filter(is.na(Team)==F) %>% data.frame(key = player_keys[1:nrow(df %>% distinct(Player, Age, .keep_all = T) %>% filter(is.na(Team)==F))]) %>% select(Player,Age,key)
          df_distinct = df %>% distinct(Player, Age, Playoffs, .keep_all = T) %>% filter(is.na(Team)==F) %>% inner_join(df_link_basis,by = join_by(Player,Age))
          df = df_distinct %>% #data.frame(key = player_keys[1:nrow(df_distinct)]) %>% 
            select(-Rk) %>% data.frame(Year = i) %>% select(key, Year, everything())
          
          df %>% write.csv(paste0("Totals/",i,".csv"))
          # print("Data collected for:")
          # print(i)
        }
        
      } else{
        print("Bad URL Status Code for:")
        print(i)
        break
      }
    }
  }
  totals_files = list.files(path = "Totals/", pattern = 'csv')
  all_data = data.frame(); year_df = data.frame()
  for (file in totals_files){
    year_df = read.csv(paste0("Totals/",file))[,-1] %>% as_tibble()
    new_cols = c("GS","MP","X3P","X3PA","X3P.","ORB","DRB","TRB","STL","BLK","TOV","X2P","X2PA","X2P.","eFG.")
    for (co in new_cols){
      if (!(co %in% names(year_df))){
        year_df = year_df %>% data.frame(co = NA)
        names(year_df)[ncol(year_df)] = co
      }
    }
    year_df = year_df %>% select("key","Year","Player","Age","Team","Pos","Playoffs","G","FG","FGA",
                                 "FG.","X2P","X2PA","X2P.","eFG.","FT","FTA","FT.","TRB",  
                                 "AST","PF","PTS","Awards","GS","MP","X3P","X3PA","X3P.",  
                                 "ORB","DRB","STL","BLK","TOV")
    
    all_data = rbind.data.frame(all_data, year_df)
    #print(file)
  }
  all_data = all_data %>% mutate(X2P = ifelse(is.na(X2P),FG,X2P),
                                 X2PA = ifelse(is.na(X2PA),FGA,X2PA),
                                 X2P. = round(X2P/X2PA,3))
  
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
  
  summary = df %>%
    group_by(Year) %>%
    summarise(
      .groups = "drop",
      PTS  = sum(PTS),
      TRB  = sum(TRB),
      DRB  = sum(DRB),
      ORB  = sum(ORB),
      TOV  = sum(TOV),
      X3PA = sum(X3PA),
      X3P  = sum(X3P),
      X2PA = sum(X2PA),
      X2P  = sum(X2P),
      FT   = sum(FT),
      FTA  = sum(FTA)
    ) %>%
    transmute(
      Year,
      laPTSperMake  = ifelse(is.na(X3PA), 2, ((3 * X3P) + (2 * X2P)) / (X3P + X2P)),
      laPTSperPoss  = PTS / ((X2PA + X3PA) + TOV + (FTA / 2.1)),
      laORBrate     = ORB / TRB,
      laDRBrate     = DRB / TRB,
      la3P.         = X3P / X3PA,
      la2P.         = X2P / X2PA,
      laFT.         = FT / FTA
    ) %>%
    left_join(
      median_perM(df, c("PTS", "TRB", "DRB", "ORB", "AST", "STL", "BLK", "TOV")),
      by = join_by(Year)
    )
  
  summary %>% write.csv("Complete Data/avgsSummary.csv")
  
  # build a model to predict points per possession based on PTS per minute and TRB per minute
  summary = summary %>% mutate(model_set = ifelse(is.na(laPTSperPoss),"test","train"))
  model = lm(data = summary %>% filter(model_set == "train"), formula = laPTSperPoss ~ laPTSperM + laTRBperM)
  new_pr = predict(model, newdata = summary %>% filter(model_set == "test") %>% select(laPTSperM, laTRBperM))
  summary$laPTSperPoss[1:30] = new_pr
  
  va_df = df %>% left_join(summary, by = "Year") %>% mutate(
    valueAdd_1 = ((PTS/MP)-(laPTSperM))*(MP) + #points added (volume)
      #  ((PTS)/((X2PA + X3PA) + TOV + (FTA/2.1)) - laPTSperPoss)*((X2PA + X3PA) + TOV + (FTA/2.1)) +
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
      (((TRB/MP)-(laTRBperM))*(MP))*(laPTSperPoss)*(0.72)*(0.28), #(est) o rebounds added
    
    vaPTSv = ((PTS/MP)-(laPTSperM))*(MP) #points added (volume)
    ,vaAST = (((AST/MP)-(laASTperM))*(MP))*(laPTSperMake)*(0.5) #assists added
    ,vaSTL = (((STL/MP)-(laSTLperM))*(MP))*(laPTSperPoss) #steals added
    ,vaBLK = (((BLK/MP)-(laBLKperM))*(MP))*(laPTSperPoss)*(laDRBrate) #blocks added
    ,vaTOV = -1*(((TOV/MP)-(laTOVperM))*(MP))*(laPTSperPoss) #turnovers added
    ,vaDRB = (((DRB/MP)-(laDRBperM))*(MP))*(laPTSperPoss)*(laORBrate) #d rebounds added
    ,vaORB = (((ORB/MP)-(laORBperM))*(MP))*(laPTSperPoss)*(laDRBrate)
    
  ) %>% mutate(valueAdd = coalesce(valueAdd_1,valueAdd_2,valueAdd_3))
  
  all_data_standard %>% left_join(va_df %>% select(Player, Year, Playoffs, valueAdd,vaPTSv,vaAST,vaSTL,vaBLK,vaTOV,vaDRB,vaORB), by = c("Year", "Player","Playoffs")) %>%
    filter(Playoffs == 0) %>% select(-Playoffs) %>% write.csv(paste0("Complete Data/Totals_s_",Sys.Date(),".csv",collapse = ""))
  all_data_standard %>% left_join(va_df %>% select(Player, Year, Playoffs, valueAdd,vaPTSv,vaAST,vaSTL,vaBLK,vaTOV,vaDRB,vaORB), by = c("Year", "Player","Playoffs")) %>%
    filter(Playoffs == 1) %>% select(-Playoffs) %>% write.csv(paste0("Complete Data/Totals_p_",Sys.Date(),".csv",collapse = ""))
  
}

exist_yn = ifelse(file.exists(today_file),T,F)
print(paste0(Sys.Date()," - File created: ",exist_yn))