library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes); library(plotly); library(gridExtra); library(DT); library(scales); library(shinyWidgets); library(shiny)
source("totals_collect.R") # totals_collect.R must be run!
today_file = paste0("Complete Data/Totals_s_",Sys.Date(),".csv",collapse = "")
df_ = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
gpl_df = df_ %>% group_by(Year) %>% summarize(.groups = "drop",gpl = 0.75*max(G))
df_ = df_ %>% inner_join(gpl_df,by = join_by(Year))
df_1 = df_ %>% filter(G > (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df_2 = df_ %>% filter(G <= (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df = df_1 %>% rbind.data.frame(df_2)
df$Player = iconv(df$Player, to = "UTF-8");maxYr = max(df$Year)

lga = read.csv("Complete Data/avgsSummary.csv")[,-1] %>% separate(Year, into = c("pre", "Year"), sep = "\\-") %>% select(-pre) %>% select(Year, everything()) %>% as_tibble()
team_map = function(input){
  map = read.csv("Complete Data/team_abbreviations.csv")[,-1]
  return(map$abb[which(map$name == input)])
}
team_gl = function(abb,year="2025"){
  url = paste0("https://www.basketball-reference.com/teams/",abb,"/",year,"/gamelog/")
  page = read_html(url)
  data.raw = html_table(page)
  
  df = data.raw[[1]]
  df = df[2:nrow(df),c(3:5)];names(df) = c("Date","at","Opp");df = df %>% filter(!(Date%in%c("Date","")))
  df = df %>% mutate(at = ifelse(at!="@","vs.","@"))
  df = df %>% separate(col = Date,into = c("year","month","day"),sep = "-",remove = F) %>% mutate(
    link = ifelse(at == "@",
                        paste0("https://www.basketball-reference.com/boxscores/",year,month,day,"0",Opp,".html"),
                        paste0("https://www.basketball-reference.com/boxscores/",year,month,day,"0",abb,".html")
        )
  ) %>% transmute(DateatOpp = paste0(Date," (",at," ",Opp,")"),link)
  return(df %>% arrange(desc(DateatOpp)))
}
gl = function(abb,ilink,date_choice){
  page = read_html(ilink)
  data.raw = html_table(page)
  opp_abb = strsplit(date_choice,split = ")| ")[[1]][length(strsplit(date_choice,split = ")| ")[[1]])]
  
  df1 = data.raw[[1]];names(df1) = df1[1,];df1 = df1 %>% filter(grepl("\\:",MP)) %>% mutate(Team = ifelse(grepl(abb,ilink),opp_abb,abb))
  df2 = data.raw[[(.5*length(data.raw))+1]];names(df2) = df2[1,];df2 = df2 %>% filter(grepl("\\:",MP)) %>% mutate(Team = ifelse(grepl(abb,ilink),abb,opp_abb))
  
  return_df = df1 %>% rbind.data.frame(df2)
  return_df = return_df %>% separate(col = MP, into = c("MP", "SP"),sep = "\\:") %>% mutate(MP = as.double(MP)+(as.double(SP)/60)) %>% select(-SP,-`FG%`,-`3P%`,-`FT%`) %>% select(Player = Starters,Team,everything())
  return(return_df)
}

tmLkp_choices = data.frame(abb = unique(df$Team[which(df$Year==maxYr)])) %>% 
  inner_join(read.csv("Complete Data/team_abbreviations.csv")[,-1],by = join_by(abb)) %>% arrange(name) %>% select(name)

team_input = "Los Angeles Lakers"
  team_abb = team_map(team_input)
game_log = team_gl(team_abb,year = "2025")
date_choice = game_log$DateatOpp[which(grepl("2025-04-11",game_log$DateatOpp))]
ilink = game_log$link[which(game_log$DateatOpp==date_choice)]

# open these up to start parsing box score stuff:
#https://www.basketball-reference.com/teams/LAL/2025/gamelog/
#https://www.basketball-reference.com/boxscores/202410220LAL.html

gl_df = gl(abb = team_abb,ilink = ilink,date_choice = date_choice)
gl_df = gl_df %>% mutate(across(-c("Player","Team"),as.double),X2P = FG-`3P`,X2PA = FGA-`3PA`)
if (T){
  calc = gl_df %>% cbind.data.frame(lga %>% arrange(Year) %>% tail(1))
  calc = calc %>% mutate(
    X3PAdd = ((`3P`/ifelse(`3PA`==0,1,`3PA`))-(la3P.))*(`3PA`),
    X2PAdd = ((X2P/ifelse(X2PA==0,1,X2PA))-(la2P.))*(X2PA),
    FTAdd = ((FT/ifelse(FTA==0,1,FTA))-(laFT.))*(FTA),
    valueAdd = ((PTS/MP)-(laPTSperM))*(MP) + #points added (volume)
      ((3*X3PAdd)+(2*X2PAdd)+FTAdd) + #points added (efficiency)
      (((AST/MP)-(laASTperM))*(MP))*(laPTSperMake)*(0.5) + #assists added
      (((STL/MP)-(laSTLperM))*(MP))*(laPTSperPoss) + #steals added
      (((BLK/MP)-(laBLKperM))*(MP))*(laPTSperPoss)*(laDRBrate) + #blocks added
      -1*(((TOV/MP)-(laTOVperM))*(MP))*(laPTSperPoss) + #turnovers added
      (((DRB/MP)-(laDRBperM))*(MP))*(laPTSperPoss)*(laORBrate) + #d rebounds added
      (((ORB/MP)-(laORBperM))*(MP))*(laPTSperPoss)*(laDRBrate), #o rebounds added
    fPTS = 2*(FG) + -1*(FGA) + 1*(FT) + -1*(FTA) + 1*(`3P`) + 1*(TRB) + 2*(AST) + 4*(STL) + 4*(BLK) + -2*(TOV) + 1*(PTS)
  )
}
gl_df = gl_df %>% inner_join(calc %>% select(Player,X3PAdd,X2PAdd,FTAdd,valueAdd,fPTS),by = join_by(Player)) %>% arrange(desc(valueAdd)) %>% select(Player, Team, MP, valueAdd,everything())

datatable(gl_df %>% transmute(Player, Team, MP = round(MP,2), PTS, TRB, AST, BLK, STL, TOV, FG = paste0(FG,"/",FGA), `3P` = paste0(`3P`,"/",`3PA`), VA = round(valueAdd,2), `+/-`))
# df$Hex[which(df$Team==team_abb)[1]] (make the row color of the team with team_abb this color)

sp_player_input = "Cam Whitmore"


