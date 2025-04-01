library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes); library(plotly); library(gridExtra); library(DT); library(scales); library(shinyWidgets); library(shiny)
source("totals_collect.R") # totals_collect.R must be run!
today_file = paste0("Complete Data/Totals_s_",Sys.Date(),".csv",collapse = "")
df_ = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
gpl_df = df_ %>% group_by(Year) %>% summarize(.groups = "drop",gpl = 0.75*max(G))
df_ = df_ %>% inner_join(gpl_df,by = join_by(Year))
df_1 = df_ %>% filter(G > (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df_2 = df_ %>% filter(G <= (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df = df_1 %>% rbind.data.frame(df_2)
df$Player = iconv(df$Player, to = "UTF-8");maxYr = max(df$Year)

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
                        paste0("https://www.basketball-reference.com/boxscores/",year,month,day,Opp,".html"),
                        paste0("https://www.basketball-reference.com/boxscores/",year,month,day,abb,".html")
        )
  ) %>% transmute(DateatOpp = paste0(Date," (",at," ",Opp,")"),link)
  return(df %>% arrange(desc(DateatOpp)))
}

tmLkp_choices = data.frame(abb = unique(df$Team[which(df$Year==maxYr)])) %>% 
  inner_join(read.csv("Complete Data/team_abbreviations.csv")[,-1]) %>% arrange(name) %>% select(name)

team_input = "Los Angeles Lakers"
  team_abb = team_map(team_input)
game_log = team_gl(team_abb)
date_choice = game_log$DateatOpp[1]
ilink = game_log$link[which(game_log$DateatOpp==date_choice)]

