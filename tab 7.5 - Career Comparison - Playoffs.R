library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes); library(plotly); library(gridExtra); library(DT); library(scales); library(shinyWidgets); library(shiny)
source("totals_collect.R") # totals_collect.R must be run!
today_file = paste0("Complete Data/Totals_s_",Sys.Date(),".csv",collapse = "")
df_ = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
gpl_df = df_ %>% group_by(Year) %>% summarize(.groups = "drop",gpl = 0.75*max(G))
df_ = df_ %>% inner_join(gpl_df,by = join_by(Year))
df_1 = df_ %>% filter(G > (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df_2 = df_ %>% filter(G <= (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df = df_1 %>% rbind.data.frame(df_2)
df$Player = iconv(df$Player, to = "UTF-8");maxYr = max(df$Year)

lga = read.csv("Complete Data/avgsSummary.csv")[,-1] %>% separate(Year, into = c("pre", "Year"), sep = "\\-") %>% select(-pre) %>% select(Year, everything()) %>% as_tibble()
menu_map = function(input){
  map = read.csv("Complete Data/menu_options.csv")[,-1]
  return(map$col_name[which(map$display_name == input)])
}
psearch = function(input){
  new = df %>% filter(grepl(input,Player))
  return(new$Player %>% unique())
}
lsearch = function(player,year){
  key = str_split(player,"\\(|\\)")[[1]][2]; letter = str_split(key,"")[[1]][1]
  return(paste0("https://www.basketball-reference.com/players/",letter,"/",key,"/gamelog/",year))
}
glsearch = function(player,years){
  player_df = data.frame()
  if (player != '-'){
    for (y in years[1]:years[2]){
      url = lsearch(player,y)
      page = read_html(url)
      data.raw = html_table(page, fill=TRUE)
      if (length(data.raw)==0){
        print("This year's page is empty:")
        print(y)
      } else{
        if (length(which(names(data.raw[[8]])=="+/-"))==0){
          reg_games_1 = data.raw[[8]] %>% select(G = Gtm, Date, Tm = Team, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc)
          reg_games_1 = reg_games_1 %>% mutate(PlusMinus = 0)
          
        } else{
          reg_games_1 = data.raw[[8]] %>% 
            select(G = Gtm, Date, Tm = Team, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc, `+/-`)
          reg_games_1 = reg_games_1 %>% set_names(nm = c("G", "Date", "Tm", "MP", "FG", "FGA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc", "PlusMinus"))
        }
        reg_games_1 = reg_games_1 %>% filter(!is.na(as.double(FG))) %>% separate(col = MP, into = c("MP", "SP"),sep = "\\:") %>% mutate(MP = as.double(MP)+(as.double(SP)/60)) %>% select(-SP) %>% data.frame(Player = player)
        player_df = player_df %>% rbind.data.frame(reg_games_1) %>% mutate(Date = as.Date(Date)) %>% as_tibble()
      }
    }
  }
  return(player_df)
}
lighten_color = function(color, factor = .25){
  col_rgb <- col2rgb(color) / 255 
  col_light <- (1 - factor) * col_rgb + factor * 1 
  rgb(col_light[1], col_light[2], col_light[3], maxColorValue = 1)
}
team_map = function(input){
  map = read.csv("Complete Data/team_abbreviations.csv")[,-1]
  return(map$abb[which(map$name == input)])
}
team_map2 = function(input){
  map = read.csv("Complete Data/team_abbreviations.csv")[,-1]
  return(map$abb[which(map$city == input)])
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
zero = function(input){
  return(0)
}
team_dl = function(abb,ilink,opp_abb,period="Game"){
  page = read_html(ilink)
  data.raw = html_table(page)
  if (length(data.raw)==16){
    prs = c("Game","1st Quarter","2nd Quarter","1st Half","3rd Quarter","4th Quarter","2nd Half","Adv. Totals")
  } else{
    prs = c("Game","1st Quarter","2nd Quarter","1st Half","3rd Quarter","4th Quarter","2nd Half",c(paste0("OT",1:((length(html_table(read_html(ilink)))-16)/2))),"Adv. Totals")
  }
  otf_mapping = data.frame(prs = prs,abb = opp_abb) %>% rbind.data.frame(data.frame(prs = prs,abb = abb)) %>% mutate(entry = 1:length(data.raw))
  df1 = data.frame();df2 = data.frame()
  clear = ifelse(period == "OT"&all(!(grepl("OT",otf_mapping$prs))),T,F)
  if (clear){
    for (i in which(grepl("Game",otf_mapping$prs)&otf_mapping$abb==opp_abb)){
      df1_ = data.raw[[i]]
      names(df1_) = df1_[1,];df1_ = df1_ %>% filter(grepl("\\:",MP)) %>% mutate(Team = opp_abb)
      df1 = rbind.data.frame(df1,df1_)
    }
    for (j in which(grepl("Game",otf_mapping$prs)&otf_mapping$abb==abb)){
      df2_ = data.raw[[j]]
      names(df2_) = df2_[1,];df2_ = df2_ %>% filter(grepl("\\:",MP)) %>% mutate(Team = abb)
      df2 = rbind.data.frame(df2,df2_)
    }
    return_df = df1 %>% rbind.data.frame(df2) %>% mutate(across(-c("Starters","Team"),zero)) 
    return_df = return_df %>% select(-`FG%`,-`3P%`,-`FT%`) %>% select(Player = Starters,Team,everything())
  } else{
    for (i in which(grepl(period,otf_mapping$prs)&otf_mapping$abb==opp_abb)){
      df1_ = data.raw[[i]]
      names(df1_) = df1_[1,];df1_ = df1_ %>% filter(grepl("\\:",MP)) %>% mutate(Team = opp_abb)
      df1 = rbind.data.frame(df1,df1_)
    }
    for (j in which(grepl(period,otf_mapping$prs)&otf_mapping$abb==abb)){
      df2_ = data.raw[[j]]
      names(df2_) = df2_[1,];df2_ = df2_ %>% filter(grepl("\\:",MP)) %>% mutate(Team = abb)
      df2 = rbind.data.frame(df2,df2_)
    }
    return_df = df1 %>% rbind.data.frame(df2)
    return_df = return_df %>% separate(col = MP, into = c("MP", "SP"),sep = "\\:") %>% mutate(MP = as.double(MP)+(as.double(SP)/60)) %>% select(-SP,-`FG%`,-`3P%`,-`FT%`) %>% select(Player = Starters,Team,everything())
  }
  return(return_df)
}

pi1 = psearch("T.J. McConnell")
pi2 = psearch("Jose Alvarado")

player_df = df %>% filter(Player %in% c(pi1,pi2))
stat_input = "Value Added"
stat_col = menu_map(stat_input)
#per_game = "Per Game"
per_game = "Total"
pg_factor = ifelse(per_game == "Per Game",T,F)

p_static = player_df[,c("Player","Team", "Year", "Hex","G",stat_col)]
names(p_static)[ncol(p_static)] = "Stat"
p1_ = p_static %>% filter(Player == pi1) %>% as_tibble()
p2_ = p_static %>% filter(Player == pi2) %>% as_tibble()
p_static = p1_ %>% rbind.data.frame(p2_) %>% as_tibble(); p_static = p_static %>% arrange(Year)
if (pg_factor){
  p_static = p_static %>% mutate(Stat = Stat/G)
  p_static = p_static %>% inner_join(gpl_df, by = "Year")
  p_static$Year = factor(p_static$Year, levels = rev(unique(p_static$Year)))
  p_static = p_static %>% filter(G > gpl)
} else{
  p_static = p_static %>% inner_join(gpl_df, by = "Year")
  p_static$Year = factor(p_static$Year, levels = rev(unique(p_static$Year)))
}
top2 = p_static %>% arrange(Player) %>% distinct(Player, .keep_all = T)
p_static = p_static %>% separate(Player,into = c("disPlayer","bbref"),sep = " \\(",remove = F) %>% mutate(Rk = "",Rk_n = "")

for (i in 1:nrow(p_static)){
  if (pg_factor){
    temp = df %>% filter(Year == p_static$Year[i], G > p_static$gpl[i])
    temp[,stat_col] = temp[,stat_col]/temp[,"G"]
    temp2 = temp[rev(order(unname(as.vector(temp[,stat_col]))[[1]])),1:ncol(df)]
    
  } else{
    temp = df %>% filter(Year == p_static$Year[i])
    temp2 = temp[rev(order(unname(as.vector(temp[,stat_col]))[[1]])),1:ncol(df)]
    if (any(is.na(temp2[,stat_col]))){
      temp2 = temp2[-which(is.na(temp2[,stat_col])),]
    }
  }
  
  if (is_empty(str_split(which(temp2$Player == p_static$Player[i]),""))){
    ld = as.integer(which(temp2$Player == p_static$Player[i]))
  } else{
    ld = as.integer(str_split(which(temp2$Player == p_static$Player[i]),"")[[1]][length(str_split(which(temp2$Player == p_static$Player[i]),"")[[1]])])
  }
  su = ifelse(ld==0,"th",ifelse(ld>3,"th",ifelse(ld>2,"rd",ifelse(ld>1,"nd","st"))))
  p_static$Rk[i] = paste0(which(temp2$Player == p_static$Player[i]),su)
  p_static$Rk_n[i] = which(temp2$Player == p_static$Player[i])
}
p_static = p_static %>% mutate(Rk_n = as.integer(Rk_n)) %>% arrange(Rk_n,desc(Stat))
p_static$alpha = 1-percent_rank(p_static$Rk_n)
p_static = p_static %>% inner_join(df %>% transmute(Player,Year, PTS/G, TRB/G, AST/G, `STK/G` = (STL+BLK)/G, `FG%`=100*FG., `3P%`=100*X3P., `FT%`=100*FT.) %>% 
                                     mutate(across(where(is.double),~round(.x,1))),by = join_by(Player, Year)) %>% mutate(`Value (Rank)` = paste0(sprintf("%.2f", Stat)," (",Rk,")")) %>% 
  select(-c(Player, bbref, gpl, Rk, Rk_n, Stat))

p_static$Hex = sapply(1:nrow(p_static), function(i) lighten_color(p_static$Hex[i], 1-p_static$alpha[i]))
p_static$Text = sapply(1:nrow(p_static), function(i) lighten_color("#000000", p_static$alpha[i]))
p_static$Text = ifelse(p_static$alpha > 2/3,p_static$Text,"#000000")
p_static = p_static %>% select(-alpha)
colnames(p_static) = c("Player",colnames(p_static)[-1])
p_static = p_static %>% unite(col = "Year (G)", c(Year,G),remove = T, sep = " (") %>% mutate(`Year (G)` = paste0(`Year (G)`,")"))

p_static = p_static %>% mutate(across(c(`PTS/G`, `TRB/G`, `AST/G`, `STK/G`), ~ sprintf("%.1f", .))) %>%
  mutate(across(c(`FG%`,`3P%`,`FT%`), ~ sprintf("%.1f", .))) %>% 
  unite(col = "PTS | TRB | AST | STK", c(`PTS/G`,`TRB/G`,`AST/G`,`STK/G`),remove = T, sep = " | ") %>% 
  unite(col = "FG% | 3P% | FT%", c(`FG%`,`3P%`,`FT%`),remove = T, sep = " | ")
p_static = p_static %>% select(`Value (Rank)`, `Year (G)`, Team,`PTS | TRB | AST | STK`,`FG% | 3P% | FT%`, everything())

p1_static = p_static %>% filter(Player==str_split(pi1," \\(")[[1]][1])
p2_static = p_static %>% filter(Player==str_split(pi2," \\(")[[1]][1])
maxRow = max(nrow(p1_static),nrow(p2_static))
p1_static = p1_static %>% rbind.data.frame(setNames(data.frame(matrix(nrow = maxRow-nrow(p1_static),ncol = ncol(p1_static))),nm = names(p1_static)))
p2_static = p2_static %>% rbind.data.frame(setNames(data.frame(matrix(nrow = maxRow-nrow(p2_static),ncol = ncol(p2_static))),nm = names(p2_static)))
p1_static = p1_static[, rev(seq_along(p1_static))]
p_static = p1_static %>% data.frame("") %>% data.frame(p2_static)

names(p_static)[4:14] = c(" FG | 3P | FT"," PTS | TRB | AST | STK"," Team"," Year (G)",str_split(pi1," \\(")[[1]][1],
                          " ", #stat_input?
                          str_split(pi2," \\(")[[1]][1],rev(c("FG | 3P | FT ","PTS | TRB | AST | STK ","Team ","Year (G) ")))

p_static %>%
  datatable(options = 
              list(pageLength = 50,
                   columnDefs = list(list(visible = FALSE, targets = c(1:3, 15:17))))) %>%
  formatStyle(
    names(p_static)[8],
    backgroundColor = styleEqual(p_static[[8]], p_static$Hex),
    color = styleEqual(p_static[[8]], p_static$Text)
  ) %>% 
  formatStyle(
    names(p_static)[10],
    backgroundColor = styleEqual(p_static[[10]], p_static$Hex.1),
    color = styleEqual(p_static[[10]], p_static$Text.1)
  ) 