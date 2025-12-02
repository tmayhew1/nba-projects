library(tidyverse);library(lubridate);library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes); library(plotly); library(gridExtra); library(DT); library(scales); library(shinyWidgets); library(shiny);library(ggrepel);library(patchwork)
source("totals_collect.R") # totals_collect.R must be run!
today_file = paste0("Complete Data/Totals_s_",Sys.Date(),".csv",collapse = "")
df_ = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
gpl_df = df_ %>% group_by(Year) %>% summarize(.groups = "drop",gpl = ifelse(max(G) < 29,0.5*max(G),0.75*max(G)))
df_ = df_ %>% inner_join(gpl_df,by = join_by(Year))
df_1 = df_ %>% filter(G > (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df_2 = df_ %>% filter(G <= (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df = df_1 %>% rbind.data.frame(df_2)
df$Player = iconv(df$Player, to = "UTF-8");maxYr = max(df$Year)

lga = read.csv("Complete Data/avgsSummary.csv")[,-1] %>% as_tibble()
#lga = read.csv("Complete Data/avgsSummary.csv")[,-1] %>% separate(Year, into = c("pre", "Year"), sep = "\\-") %>% select(-pre) %>% select(Year, everything()) %>% as_tibble()

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
        #print("This year's page is empty:")
        #print(y)
      } else{
        if (length(data.raw)<9){
          data.raw[[9]] = data.frame(matrix(nrow = 0,ncol = ncol(data.raw[[8]]))) %>% set_names(nm = names(data.raw[[8]]))
        }
        
        if (length(which(names(data.raw[[8]])=="+/-"))==0){
          if (ncol(data.raw[[8]])==ncol(data.raw[[9]])){
            reg_games_1 = data.raw[[8]] %>% rbind.data.frame(data.raw[[9]]) %>% select(G = Gtm, Date, Tm = Team, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc)
            reg_games_1 = reg_games_1 %>% mutate(PlusMinus = 0)
          } else{
            reg_games_1 = data.raw[[8]] %>% select(G = Gtm, Date, Tm = Team, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc)
            reg_games_1 = reg_games_1 %>% mutate(PlusMinus = 0)
          }
        } else{
          if (ncol(data.raw[[8]])==ncol(data.raw[[9]])){
            reg_games_1 = data.raw[[8]] %>% rbind.data.frame(data.raw[[9]]) %>% select(G = Gtm, Date, Tm = Team, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc, `+/-`)
            reg_games_1 = reg_games_1 %>% set_names(nm = c("G", "Date", "Tm", "MP", "FG", "FGA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc", "PlusMinus"))
          } else{
            reg_games_1 = data.raw[[8]] %>% select(G = Gtm, Date, Tm = Team, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc, `+/-`)
            reg_games_1 = reg_games_1 %>% set_names(nm = c("G", "Date", "Tm", "MP", "FG", "FGA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc", "PlusMinus"))
          }
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
daily_l = function(ilink_dl){
  page = read_html(ilink_dl)
  data.raw = html_table(page)
  ret_df = data.raw[[1]][,-c(which(names(data.raw[[1]])==""|grepl("\\%",names(data.raw[[1]]))))]
  return(ret_df %>% filter(Player != "Player"))
}
add_suffix <- function(x){
  x = as.integer(x)
  suffix <- ifelse(x %% 100 %in% 11:13, "th",
                   ifelse(x %% 10 == 1, "st",
                          ifelse(x %% 10 == 2, "nd",
                                 ifelse(x %% 10 == 3, "rd", "th"))))
  paste0(x, suffix)
}
zero = function(input){
  return(0)
}
dts = function(date){
  ifelse(
    month(date) >= 9,
    paste0(year(date), "-", year(date) + 1),
    paste0(year(date) - 1, "-", year(date))
  )
}

player_input_1 = psearch("Josh Giddey")[1]
player_input_2 = psearch("Tyrese Haliburton")[1]
year_input_1 = "2025-2026"
year_input_2 = "2024-2025"

tp_df = df %>% filter(Year %in% c(year_input_1, year_input_2))
top_color = tp_df$Hex[which(tp_df$Player == player_input_1 & tp_df$Year == year_input_1)]
tp_df$Player = factor(tp_df$Player,levels = unique(c(player_input_1,player_input_2)))
tp_df$Year = factor(tp_df$Year,levels = unique(c(year_input_1,year_input_2)))
tp_df$Year_dp = paste0("'", str_extract(tp_df$Year, "\\d{2}$"));
tp_df$Player_dp = str_replace(tp_df$Player, "\\s*\\(.*\\)$", "")

# Plot 1 of 4 - Scoring Volume vs. Scoring Efficiency
tp_df_1 = tp_df %>% mutate(disPlayer = paste0(Year_dp," ",Player_dp,": ",sprintf("%.2f", (PTS/G))," PPG on ",eFG.," eFG")) %>% arrange(Player)
p1 = tp_df_1 %>% 
  ggplot(aes(x = vaPTSv/G, y = PTSAdd/G, color = Year)) + 
  geom_smooth(se = F,method = "lm",formula = 'y ~ x',color = "grey70",alpha = I(.75)) +
  geom_point(aes(color = Year),alpha = I(0.5)) +
  scale_y_continuous(
    name = "Efficiency Points Added",
    #breaks = pretty(sp_output$value, n = 2),  # or use a custom vector like c(-10, 0, 10)
    labels = function(x) ifelse(x == 0, "League Average", x),
    expand = expansion(mult = c(0.1, 0.1))  # 10% padding above the max
  ) + geom_hline(yintercept = 0) + 
  scale_x_continuous(
    name = "Scoring Volume Added",
    #breaks = pretty(sp_output$value, n = 2),  # or use a custom vector like c(-10, 0, 10)
    labels = function(x) ifelse(x == 0, "League Average", x),
    expand = expansion(mult = c(0.2, 0.2))  # 10% padding above the max
  ) + geom_vline(xintercept = 0) +
  theme_bw() + geom_segment(
    data = tp_df_1 %>%
      filter((Player == player_input_1 & Year == year_input_1) |
               (Player == player_input_2 & Year == year_input_2)) %>%
      mutate(y_adj = ifelse(PTSAdd/G == max(PTSAdd/G),
                            (PTSAdd/G) + 1/3,
                            (PTSAdd/G) - 1/3)),
    aes(x = vaPTSv/G, xend = vaPTSv/G,
        y = PTSAdd/G, yend = y_adj, color = Year),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) + geom_label(
    data = tp_df_1 %>%
      filter((Player == player_input_1 & Year == year_input_1) |
               (Player == player_input_2 & Year == year_input_2)) %>%
      mutate(y_adj = ifelse(PTSAdd/G == max(PTSAdd/G),
                            (PTSAdd/G) + 1/3,
                            (PTSAdd/G) - 1/3)),
    aes(x = vaPTSv/G, y = y_adj, label = disPlayer, color = Year),
    size = 2,
    show.legend = FALSE
  ) + scale_color_manual(values = setNames(c(top_color, "grey35"),c(year_input_1, year_input_2)))

# Plot 2 of 4 - Value Added on Assists vs. Turnovers
tp_df_2 = tp_df %>% mutate(disPlayer = paste0(Year_dp," ",Player_dp,": ",sprintf("%.2f", (AST/G))," APG on ",sprintf("%.1f", (TOV/G))," TOV")) %>% arrange(Player)
p2 = tp_df_2 %>% 
  ggplot(aes(x = vaTOV/G, y = vaAST/G, color = Year)) + 
  geom_smooth(se = F,method = "lm",formula = 'y ~ x',color = "grey70",alpha = I(.75)) +
  geom_point(aes(color = Year),alpha = I(0.5)) +
  scale_y_continuous(
    name = "Assists Added",
    #breaks = pretty(sp_output$value, n = 2),  # or use a custom vector like c(-10, 0, 10)
    labels = function(x) ifelse(x == 0, "League Average", x),
    expand = expansion(mult = c(0.1, 0.1))  # 10% padding above the max
  ) + geom_hline(yintercept = 0) + 
  scale_x_continuous(
    name = "Turnovers Saved",
    #breaks = pretty(sp_output$value, n = 2),  # or use a custom vector like c(-10, 0, 10)
    labels = function(x) ifelse(x == 0, "League Average", x),
    expand = expansion(mult = c(0.2, 0.2))  # 10% padding above the max
  ) + geom_vline(xintercept = 0) +
  theme_bw() + geom_segment(
    data = tp_df_2 %>%
      filter((Player == player_input_1 & Year == year_input_1) |
               (Player == player_input_2 & Year == year_input_2)) %>%
      mutate(y_adj = ifelse(vaAST/G == max(vaAST/G),
                            (vaAST/G) + .4,
                            (vaAST/G) - .4)),
    aes(x = vaTOV/G, xend = vaTOV/G,
        y = vaAST/G, yend = y_adj, color = Year),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) + geom_label(
    data = tp_df_2 %>%
      filter((Player == player_input_1 & Year == year_input_1) |
               (Player == player_input_2 & Year == year_input_2)) %>%
      mutate(y_adj = ifelse(vaAST/G == max(vaAST/G),
                            (vaAST/G) + .5,
                            (vaAST/G) - .5)),
    aes(x = vaTOV/G, y = y_adj, label = disPlayer, color = Year),
    size = 2,
    show.legend = FALSE
  ) + scale_color_manual(values = setNames(c(top_color, "grey35"),c(year_input_1, year_input_2)))

# Plot 3 of 4 - Value Added on Stocks vs. Rebounds
tp_df_3 = tp_df %>% mutate(disPlayer = paste0(Year_dp," ",Player_dp,": ",sprintf("%.2f",((STL+BLK)/G))," STK/G & ",sprintf("%.2f",(TRB/G))," TRB")) %>% arrange(Player)
p3 = tp_df_3 %>% 
  ggplot(aes(x = (vaORB+vaDRB)/G, y = (vaSTL+vaBLK)/G, color = Year)) + 
  geom_smooth(se = F,method = "lm",formula = 'y ~ x',color = "grey70",alpha = I(.75)) +
  geom_point(aes(color = Year),alpha = I(0.5)) +
  scale_y_continuous(
    name = "Steals + Blocks Added",
    #breaks = pretty(sp_output$value, n = 2),  # or use a custom vector like c(-10, 0, 10)
    labels = function(x) ifelse(x == 0, "League Average", x),
    expand = expansion(mult = c(0.1, 0.1))  # 10% padding above the max
  ) + geom_hline(yintercept = 0) + 
  scale_x_continuous(
    name = "Total Rebounding Value",
    #breaks = pretty(sp_output$value, n = 2),  # or use a custom vector like c(-10, 0, 10)
    labels = function(x) ifelse(x == 0, "League Average", x),
    expand = expansion(mult = c(0.2, 0.2))  # 10% padding above the max
  ) + geom_vline(xintercept = 0) +
  theme_bw() + geom_segment(
    data = tp_df_3 %>%
      filter((Player == player_input_1 & Year == year_input_1) |
               (Player == player_input_2 & Year == year_input_2)) %>%
      mutate(y_adj = ifelse((vaSTL+vaBLK)/G == max((vaSTL+vaBLK)/G),
                            ((vaSTL+vaBLK)/G) + 0.15,
                            ((vaSTL+vaBLK)/G) - 0.15)),
    aes(x = (vaORB+vaDRB)/G, xend = (vaORB+vaDRB)/G,
        y = (vaSTL+vaBLK)/G, yend = y_adj, color = Year),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) + geom_label(
    data = tp_df_3 %>%
      filter((Player == player_input_1 & Year == year_input_1) |
               (Player == player_input_2 & Year == year_input_2)) %>%
      mutate(y_adj = ifelse((vaSTL+vaBLK)/G == max((vaSTL+vaBLK)/G),
                            ((vaSTL+vaBLK)/G) + 0.15,
                            ((vaSTL+vaBLK)/G) - 0.15)),
    aes(x = (vaORB+vaDRB)/G, y = y_adj, label = disPlayer, color = Year),
    size = 2,
    show.legend = FALSE
  ) + scale_color_manual(values = setNames(c(top_color, "grey35"),c(year_input_1, year_input_2)))

# Plot 4 of 4 - Value Added on 3s vs. 1s + 2s
tp_df_4 = tp_df %>% mutate(disPlayer = paste0(Year_dp," ",Player_dp,": ",sprintf("%.2f", (PTS/G))," PPG on ",eFG.," eFG")) %>% arrange(Player)
p4 = tp_df_4 %>% filter(FGA > 25) %>% 
  ggplot(aes(x = (PTSAdd-(3*(X3PAdd)))/G, y = 3*(X3PAdd)/G, color = Year)) + 
  geom_smooth(se = F,method = "lm",formula = 'y ~ x',color = "grey70",alpha = I(.75)) +
  geom_point(aes(color = Year),alpha = I(0.5)) +
  scale_y_continuous(
    name = "3-Pointers Added",
    #breaks = pretty(sp_output$value, n = 2),  # or use a custom vector like c(-10, 0, 10)
    labels = function(x) ifelse(x == 0, "League Average", x),
    expand = expansion(mult = c(0.1, 0.1))  # 10% padding above the max
  ) + geom_hline(yintercept = 0) + 
  scale_x_continuous(
    name = "2-Pointers + Free Throws Added",
    #breaks = pretty(sp_output$value, n = 2),  # or use a custom vector like c(-10, 0, 10)
    labels = function(x) ifelse(x == 0, "League Average", x),
    expand = expansion(mult = c(0.2, 0.2))  # 20% padding above the max
  ) + geom_vline(xintercept = 0) +
  theme_bw() + geom_segment(
    data = tp_df_4 %>%
      filter((Player == player_input_1 & Year == year_input_1) |
               (Player == player_input_2 & Year == year_input_2)) %>%
      mutate(y_adj = ifelse(3*(X3PAdd)/G == max(3*(X3PAdd)/G),
                            3*(X3PAdd)/G + .2,
                            3*(X3PAdd)/G - .2)),
    aes(x = (PTSAdd-(3*(X3PAdd)))/G, xend = (PTSAdd-(3*(X3PAdd)))/G,
        y = 3*(X3PAdd)/G, yend = y_adj, color = Year),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) + geom_label(
    data = tp_df_4 %>%
      filter((Player == player_input_1 & Year == year_input_1) |
               (Player == player_input_2 & Year == year_input_2)) %>%
      mutate(y_adj = ifelse(3*(X3PAdd)/G == max(3*(X3PAdd)/G),
                            3*(X3PAdd)/G + .2,
                            3*(X3PAdd)/G - .2)),
    aes(x = (PTSAdd-(3*(X3PAdd)))/G, y = y_adj, label = disPlayer, color = Year),
    size = 2,
    show.legend = FALSE
  ) + scale_color_manual(values = setNames(c(top_color, "grey35"),c(year_input_1, year_input_2)))

theme_set(theme_minimal(base_size = 10))
(p1 + p2 + p3 + p4) + plot_layout(guides = "collect") &
  theme(legend.position = "top")
