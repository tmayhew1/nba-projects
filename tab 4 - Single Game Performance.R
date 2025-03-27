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
  p1_input = "Nikola Jokic"
  date_input_1_start = as.Date(Sys.Date()-260) #260
  date_input_1_end = as.Date(Sys.Date())
  
  p2_input = "Dyson Daniels"
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
stat_input = "Value Added"
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

sp_date_input = "2024-11-23";sp_player_input = "Player 1"
sp_output = cdf %>% filter(Date == sp_date_input,Player == ifelse(sp_player_input == "Player 1",p1_input,p2_input)) %>% 
              transmute(
                `Scoring (Volume)` = ((PTS/MP)-laPTSperM)*(MP)
                ,`Efficiency (3P)` = (3*X3PAdd)
                ,`Efficiency (2P)` = (2*X2PAdd)
                ,`Efficiency (FT)` = FTAdd
                ,`Assists` = (((AST/MP)-(laASTperM))*(MP))*(laPTSperMake)*(0.5)
                ,`Steals` = (((STL/MP)-(laSTLperM))*(MP))*(laPTSperPoss)
                ,`Blocks` = (((BLK/MP)-(laBLKperM))*(MP))*(laPTSperPoss)*(laDRBrate)
                ,`Turnovers` = -1*(((TOV/MP)-(laTOVperM))*(MP))*(laPTSperPoss)
                ,`Rebounds (D)` = (((DRB/MP)-(laDRBperM))*(MP))*(laPTSperPoss)*(laORBrate)
                ,`Rebounds (O)` = (((ORB/MP)-(laORBperM))*(MP))*(laPTSperPoss)*(laDRBrate)
                ) %>% gather(key = "key",value = "value") %>% 
  mutate(abs = abs(value),col_n = ifelse(value > 0,"green4","red4")) %>% arrange(desc(abs))
sp_output$key = factor(sp_output$key,levels = rev(sp_output$key))
sp_output %>% ggplot(aes(x = key, y = abs, fill = col_n)) + geom_bar(color = "black",stat="identity",width=I(1/2),alpha = I(.8)) +
  theme_bw() + geom_hline(yintercept = 0) + coord_flip() +
  scale_y_continuous("Value Added") + scale_x_discrete("") + 
  scale_fill_manual(values = unique(c(sp_output$col_n))) + theme(legend.position = "none")

                                                                               