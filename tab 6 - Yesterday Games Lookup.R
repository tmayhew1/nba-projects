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
team_sg = function(abb,ilink,date_choice){
  page = read_html(ilink)
  data.raw = html_table(page)
  opp_abb = strsplit(date_choice,split = ")| ")[[1]][length(strsplit(date_choice,split = ")| ")[[1]])]
  
  df1 = data.raw[[1]];names(df1) = df1[1,];df1 = df1 %>% filter(grepl("\\:",MP)) %>% mutate(Team = ifelse(grepl(abb,ilink),opp_abb,abb))
  df2 = data.raw[[(.5*length(data.raw))+1]];names(df2) = df2[1,];df2 = df2 %>% filter(grepl("\\:",MP)) %>% mutate(Team = ifelse(grepl(abb,ilink),abb,opp_abb))
  
  return_df = df1 %>% rbind.data.frame(df2)
  return_df = return_df %>% separate(col = MP, into = c("MP", "SP"),sep = "\\:") %>% mutate(MP = as.double(MP)+(as.double(SP)/60)) %>% select(-SP,-`FG%`,-`3P%`,-`FT%`) %>% select(Player = Starters,Team,everything())
  return(return_df)
}

date_inputs = c("2025-04-13")#c(Sys.Date()-c(1:10))

m = str_split(date_inputs[1],"-")[[1]][2]
d = str_split(date_inputs[1],"-")[[1]][3]
y = str_split(date_inputs[1],"-")[[1]][1]
url = paste0("https://www.basketball-reference.com/boxscores/index.fcgi?month=",m,"&day=",d,"&year=",y)
page = read_html(url) 
all_links = page %>% html_nodes("a") %>% html_attr("href")
links = unique(all_links[which(grepl("boxscores\\/\\d{0,10}[A-Za-z]{3}.html",all_links))])
data.raw = page %>% html_table()
matchups = data.frame(link = links,matchup = "")
for (i in 1:length(links)){matchups$matchup[i] = paste0(data.raw[[3*i-2]]$X1[1]," vs. ",data.raw[[3*i-2]]$X1[2])}

# team_input = "Los Angeles Lakers"
# team_abb = team_map(team_input)
# game_log = team_gl(team_abb,year = "2025")
# date_choice = game_log$DateatOpp[which(grepl("2025-02-08",game_log$DateatOpp))]
# ilink = game_log$link[which(game_log$DateatOpp==date_choice)]

