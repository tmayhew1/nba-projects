library(tidyverse);library(lubridate);library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes); library(plotly); library(gridExtra); library(DT); library(scales); library(shinyWidgets); library(shiny)
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

# Define UI for application
ui = 
  fluidPage(titlePanel("Trey's NBA Stats Stuff"),
            tabsetPanel(type = "tabs",
                        tabPanel("Player Comparison",
                                 fluidPage(
                                   titlePanel(h1("Active NBA Player Comparison", style = "font-size: 18px;")),
                                   mainPanel(
                                     width = 12,
                                     column(
                                       width = 6,
                                       fluidRow(
                                         column(6, selectizeInput("p1_input","Player 1:",choices = df$Player[which(df$Year==maxYr)],selected = df$Player[which(df$Year==maxYr)][1]), style = "font-size: 12px;"),
                                         column(6, selectizeInput("p2_input","Player 2:",choices = c("-",df$Player[which(df$Year==maxYr)]),selected = "-"), style = "font-size: 12px;"),
                                       ),
                                       fluidRow(
                                         column(4, selectInput("stat_input", "Statistic of Interest:", choices = rev(read.csv("Complete Data/menu_options.csv")[, ncol(read.csv("Complete Data/menu_options.csv"))]), selected = "Value Added"), style = "font-size: 12px;")
                                         ,column(3, numericInput("roll_avg_input", "Rolling Average:", value = 10, min = 1, step = 1), style = "font-size: 12px;")
                                         ,column(5, selectInput("date_input", "Since:", choices = c("Past year (365 days)", paste0("Start of this NBA season (Oct. ", str_split(max(df$Year), pattern = "-")[[1]][1], ")"), "Past month (30 days)"), selected = "Past year (365 days)"), style = "font-size: 12px;")
                                       )
                                     ),
                                     column(
                                       width = 6,
                                       DTOutput("table2")
                                     )
                                   ),
                                   mainPanel(
                                     tags$br(), # Add a couple of line breaks
                                     tags$br(),
                                     width = 12,
                                     column(width = 5, 
                                            fluidRow(plotOutput("plot1")),
                                            fluidRow(plotOutput("plot2"))
                                     ),
                                     column(width = 7, DTOutput("table1"))
                                   ) 
                                 )
                        ),
                        tabPanel("Leaderboard",
                                 fluidPage(
                                   titlePanel(h1("NBA Season Leaderboard", style = "font-size: 18px;")),
                                   mainPanel(
                                     width = 12,
                                     column(
                                       width = 12,
                                       fluidRow(
                                         column(3, selectInput("year_input","Season:",choices = rev(unique(sort(df$Year))),selected = maxYr))
                                         ,column(2, selectInput("stat_input_2", "Statistic of Interest:", choices = rev(read.csv("Complete Data/menu_options_2.csv")[, ncol(read.csv("Complete Data/menu_options_2.csv"))]), selected = "Value Added"))
                                         ,column(3, selectizeInput("player_input", "Player (optional):",choices = NULL,selected = ""))
                                         ,column(2, selectInput("reg_playoff","",choices = c("Regular Season","Playoffs"),selected = "Regular Season"))
                                         #,column(2, switchInput("pg_factor", "Per game?", value = TRUE),textOutput("toggle_status"))
                                         ,br()
                                         ,column(2, switchInput("pg_factor", "Per/game?", value = T, size="small"),textOutput("toggle_status"))
                                       ),
                                       fluidRow(
                                         column(5, plotOutput("plot3"))
                                         ,column(width = 7, DTOutput("table3"))
                                       )
                                     )
                                   )
                                 )
                        ),
                        tabPanel("Game Lookup",
                                 fluidPage(
                                   titlePanel(h1("Single Game Search (by Team): Regular Season", style = "font-size: 18px;")),
                                   mainPanel(
                                     width = 12
                                     ,column(
                                       width = 12,
                                       fluidRow(
                                         column(2, selectInput("year_input_2","Season:",choices = rev(unique(sort(df$Year))),selected = maxYr))
                                         ,column(2, selectInput("team_input", "Team:", choices = sort((read.csv("Complete Data/team_abbreviations.csv") %>% filter(modern==1))[,3]), selected = "Los Angeles Lakers"))
                                         ,column(3, selectizeInput("date_input_2", "Date:",choices = NULL,selected = ""))
                                         ,column(3, selectizeInput("player_input_2", "Player (optional):",choices = NULL,selected = ""))
                                         ,br(),column(2, actionButton("run","Load/Reload", class = "btn-lg")),
                                       )
                                     )
                                     ,fluidRow(
                                       column(width = 8, DTOutput("table5"))
                                       ,column(4, plotOutput("plot5"))
                                     )
                                   )
                                 )
                        ),
                        tabPanel("Date Lookup",
                                 fluidPage(
                                   titlePanel(h1("Single Game Search (by Date): Regular Season and Playoffs", style = "font-size: 18px;")),
                                   mainPanel(
                                     width = 12
                                     ,column(
                                       width = 12,
                                       fluidRow(
                                         column(2, dateInput("date_input_3","Date:",value = as.Date(Sys.time() %>% as.POSIXct(tz = "America/New_York")-days(1))))
                                         ,column(3, selectInput("matchup_input", "Game:", choices = NULL, selected = ""))
                                         ,column(3, selectInput("period_input", "Period: ", choices = c("Game","1st Quarter","2nd Quarter","1st Half","3rd Quarter","4th Quarter","2nd Half","OT"), selected = "Game"))
                                         ,br(),column(2, actionButton("run_2","Load/Reload", class = "btn-lg")),
                                       )
                                     )
                                     ,fluidRow(
                                       column(width = 8, DTOutput("table6"))
                                       ,column(4, plotOutput("plot6"))
                                     )
                                   )
                                 )
                        )
                        ,
                        tabPanel("Career Comparison",
                                 fluidPage(
                                   titlePanel(h1("NBA Player Career Comparison", style = "font-size: 18px;")),
                                   mainPanel(
                                     width = 12,
                                     column(
                                       width = 6,
                                       fluidRow(
                                         column(6, selectizeInput("p1_i","Player 1:",choices = unique(df$Player[which(df$G>60)]),selected = "LeBron James (jamesle01)"), style = "font-size: 12px;"),
                                         column(6, selectizeInput("p2_i","Player 2:",choices = unique(df$Player[which(df$G>60)]),selected = "Michael Jordan (jordami01)"), style = "font-size: 12px;"),
                                       ),
                                       fluidRow(
                                         column(4, selectInput("stat_input_3", "", choices = rev(read.csv("Complete Data/menu_options_4.csv")[, ncol(read.csv("Complete Data/menu_options_4.csv"))]), selected = "Value Added"), style = "font-size: 12px;")
                                         ,column(3, selectInput("reg_playoff_2","",choices = c("Regular Season","Playoffs"),selected = "Regular Season"), style = "font-size: 12px;")
                                         ,br()
                                         ,column(1, switchInput("pg_factor_2", "Per/game?", value = T, size="small"),textOutput("toggle_status"))
                                       )
                                     )
                                     # ,
                                     # column(
                                     #   width = 6,
                                     #   DTOutput("table7")
                                     # )
                                   ),
                                   tags$br(),
                                   tags$br(),
                                   fluidRow(
                                     DTOutput("table8",width = "100%")
                                   )
                                   
                                 )
                        )
            )
  )

# Define server logic
server <- function(input, output, session) {
  
  # Observe the "year" input on the Leaderboard tab and update the options in "player" based on that!
  observeEvent(input$year_input, {
    updateSelectInput(session, "player_input", choices = {
      c("",df$Player[which(df$Year==input$year_input)])
    })
  })
  
  observeEvent(list(input$team_input,input$year_input_2), {
    updateSelectInput(session, "date_input_2", choices = {
      c(team_gl(team_map(input$team_input),str_split(input$year_input_2,"-")[[1]][2])[,1])$DateatOpp
    })
  })
  
  observeEvent(list(input$team_input,input$year_input_2), {
    updateSelectInput(session, "player_input_2", choices = {
      c("",html_table(read_html(paste0("https://www.basketball-reference.com/teams/",team_map(input$team_input),"/",str_split(input$year_input_2,"-")[[1]][2],".html")))[[2]]$Player)
    })
  })
  
  observeEvent(input$date_input_3, {
    updateSelectInput(session, "matchup_input", choices = {
      m = str_split(input$date_input_3,"-")[[1]][2]
      d = str_split(input$date_input_3,"-")[[1]][3]
      y = str_split(input$date_input_3,"-")[[1]][1]
      url = paste0("https://www.basketball-reference.com/boxscores/index.fcgi?month=",m,"&day=",d,"&year=",y)
      page = read_html(url)
      all_links = page %>% html_nodes("a") %>% html_attr("href")
      links = unique(all_links[which(grepl("boxscores\\/\\d{0,10}[A-Za-z]{3}.html",all_links))])
      data.raw = page %>% html_table()
      if (is_empty(links)){
        c("No game(s) data for this day!")
      } else{
        matchups = data.frame(link = links,matchup = "")
        for (i in 1:length(links)){matchups$matchup[i] = paste0(data.raw[[3*i-2]]$X1[1]," vs. ",data.raw[[3*i-2]]$X1[2])}
        c("-",matchups$matchup)
      }
    })
  })
  
  # Start data collect
  p1_df = reactive({glsearch(player = input$p1_input,years = c(format(Sys.Date(), "%Y") %>% as.integer()-1,format(Sys.Date(), "%Y") %>% as.integer()+1))})
  p2_df = reactive({glsearch(player = input$p2_input,years = c(format(Sys.Date(), "%Y") %>% as.integer()-1,format(Sys.Date(), "%Y") %>% as.integer()+1))})
  roll_avg_input = reactive({input$roll_avg_input})
  stat_input = reactive({input$stat_input})
  date_input = reactive({input$date_input})
  ###########################################
  year_input = reactive({input$year_input})
  stat_input_2 = reactive({input$stat_input_2})
  pg_factor = reactive({input$pg_factor})
  player_input = reactive({input$player_input})
  reg_playoff = reactive({input$reg_playoff})
  ###########################################
  year_input_2 = reactive({input$year_input_2})
  team_input = reactive({input$team_input})
  date_input_2 = reactive({input$date_input_2})
  player_input_2 = reactive({input$player_input_2})
  ###########################################
  date_input_3 = reactive({input$date_input_3})
  matchup_input = reactive({input$matchup_input})
  period_input = reactive({input$period_input})
  ###########################################
  p1_i = reactive({input$p1_i})
  p2_i = reactive({input$p2_i})
  stat_input_3 = reactive({input$stat_input_3})
  reg_playoff_2 = reactive({input$reg_playoff_2})
  pg_factor_2 = reactive({input$pg_factor_2})
  
  # Table 2: Summary Statistics
  output$table2 = renderDT({
    p1_df = p1_df();p2_df = p2_df();date_input = date_input()
    min_date = ifelse(date_input == "Past year (365 days)",Sys.Date()-365,ifelse(date_input=="Past month (30 days)",Sys.Date()-30,ifelse(Sys.Date() %>% format("%m") %>% as.integer() < 10,paste0((Sys.Date() %>% format("%Y") %>% as.integer() - 1),"-10-01"),paste0((Sys.Date() %>% format("%Y") %>% as.integer()),"-10-01")))) %>% as.Date()
    
    if (nrow(p2_df)==0){
      # if p2_df is empty, then treat data like we're only plotting one player (because we are!)
      p1_df = p1_df %>% filter(Date >= min_date)
      p1_df = p1_df %>% mutate(G = 1:nrow(p1_df),across(!c(Date,Tm,Player),as.double))
      cdf = p1_df %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = c("Tm" = "Team"))
    } else{
      # else: combine two player data frames!
      p1_df = p1_df %>% filter(Date >= min_date);p2_df = p2_df %>% filter(Date >= min_date)
      p1_df = p1_df %>% mutate(G = 1:nrow(p1_df),across(!c(Date,Tm,Player),as.double)); p2_df = p2_df %>% mutate(G = 1:nrow(p2_df),across(!c(Date,Tm,Player,G,MP),as.double))
      cdf = p1_df %>% rbind.data.frame(p2_df) %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = c("Tm" = "Team"))
    }
    
    cdf = cdf %>% mutate(across(c(X3P,X3PA,FT,FTA,FG,FGA),as.numeric)) %>% mutate(X2P = FG-X3P, X2PA = FGA-X3PA)
    cdf = cdf %>% mutate(Year = dts(Date)) %>% inner_join(lga, by = "Year")
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
    top_color = cdf %>% filter(Player == p1_df$Player[1]) %>% tail(1)
    cdf$Player = factor(cdf$Player,levels = unique(cdf$Player))
    
    if (nrow(p2_df)==0){
      cdf %>% group_by(Player) %>% summarise(.groups = "drop", G = formatC(n(),format = "f",digits=0),PTS = formatC(mean(PTS),format = "f",digits=1),TRB = round(mean(TRB),digits=1), AST = round(mean(AST),digits=1), STK = formatC(mean(STL+BLK),format = "f",digits=1), `FG%` = formatC(100*(sum(FG)/sum(FGA)),format = "f",digits=1), `3P%` = formatC(100*(sum(X3P)/sum(X3PA)),format = "f",digits=1), FGA = formatC(mean(FGA),format = "f",digits=1)) %>% 
        datatable(
          options = 
            list(dom = 't', # Only show the table, without additional interface elements
                 paging = FALSE, # Disable pagination
                 searching = FALSE # Disable the search box 
            )
        )
      
    } else{
      cdf %>% group_by(Player) %>% summarise(.groups = "drop", G = formatC(n(),format = "f",digits=0),PTS = formatC(mean(PTS),format = "f",digits=1),TRB = round(mean(TRB),digits=1), AST = round(mean(AST),digits=1), STK = formatC(mean(STL+BLK),format = "f",digits=1), `FG%` = formatC(100*(sum(FG)/sum(FGA)),format = "f",digits=1), `3P%` = formatC(100*(sum(X3P)/sum(X3PA)),format = "f",digits=1), FGA = formatC(mean(FGA),format = "f",digits=1), vapg_sort = mean(valueAdd)) %>% arrange(desc(vapg_sort)) %>% select(-vapg_sort) %>% 
        datatable(
          options = 
            list(dom = 't', # Only show the table, without additional interface elements
                 paging = FALSE, # Disable pagination
                 searching = FALSE # Disable the search box 
            )
        ) %>%
        formatStyle(
          'Player', 
          target = 'row', 
          backgroundColor = styleEqual(
            c(p1_df$Player[1], p2_df$Player[1]), 
            c(lighten_color(top_color$Hex[1],.25), "lightgrey")),
          color = styleEqual(
            c(p1_df$Player[1], p2_df$Player[1]), 
            c("white", "black")
          )
        )
    }
  })
  
  # Plot 1: Rolling Average Statistics
  output$plot1 <- renderPlot({
    p1_df = p1_df();p2_df = p2_df();roll_avg_input = roll_avg_input();stat_input = stat_input();date_input = date_input()
    #translate some inputs
    stat_col = menu_map(stat_input)
    ra = as.integer(roll_avg_input)
    min_date = ifelse(date_input == "Past year (365 days)",Sys.Date()-365,ifelse(date_input=="Past month (30 days)",Sys.Date()-30,ifelse(Sys.Date() %>% format("%m") %>% as.integer() <= 10,paste0((Sys.Date() %>% format("%Y") %>% as.integer() - 1),"-10-01"),paste0((Sys.Date() %>% format("%Y") %>% as.integer()),"-10-01")))) %>% as.Date()
    
    if (nrow(p2_df)==0){
      # if p2_df is empty, then treat data like we're only plotting one player (because we are!)
      p1_df = p1_df %>% filter(Date >= min_date)
      p1_df = p1_df %>% mutate(G = 1:nrow(p1_df),across(!c(Date,Tm,Player),as.double))
      cdf = p1_df %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = c("Tm" = "Team"))
    } else{
      # else: combine two player data frames!
      p1_df = p1_df %>% filter(Date >= min_date);p2_df = p2_df %>% filter(Date >= min_date)
      p1_df = p1_df %>% mutate(G = 1:nrow(p1_df),across(!c(Date,Tm,Player),as.double)); p2_df = p2_df %>% mutate(G = 1:nrow(p2_df),across(!c(Date,Tm,Player,G,MP),as.double))
      cdf = p1_df %>% rbind.data.frame(p2_df) %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = c("Tm" = "Team"))
    }
    cdf = cdf %>% mutate(across(c(X3P,X3PA,FT,FTA,FG,FGA),as.numeric)) %>% mutate(X2P = FG-X3P, X2PA = FGA-X3PA)
    cdf = cdf %>% mutate(Year = dts(Date)) %>% inner_join(lga, by = "Year")
    #cdf = cdf %>% separate(Date, into = c("Year", "m", "d"), remove=F) %>% select(-m, -d) %>% inner_join(lga, by = "Year")
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
                         ,tenPTS = ifelse(PTS>9,1,0),tenTRB = ifelse(TRB>9,1,0),tenAST = ifelse(AST>9,1,0),tenSTL = ifelse(STL>9,1,0),tenBLK = ifelse(BLK>9,1,0)) %>% 
      mutate(sum10s = tenPTS+tenTRB+tenAST+tenSTL+tenBLK) %>% 
      mutate(fPTS2 = (.5*PTS) + (TRB) + (AST) + (2*(STL)) + (2*(BLK)) + (-1*(TOV)) + (.5*X3P) +
               ifelse(sum10s > 1,1,0) + # double-double bonus
               ifelse(sum10s > 2,2,0) + # triple-double bonus
               ifelse(PTS > 39,2,0) + # 40+ points bonus
               ifelse(PTS > 49,2,0) # 50+ points bonus
      )
    top_color = cdf %>% filter(Player == p1_df$Player[1]) %>% tail(1)
    # modify Stat columns if stat_input is a percent!
    if (grepl("[P|p]ercentage",stat_input)){
      static = cdf[,c("Player","Tm", "G", "Date", str_split(stat_col,"\\.")[[1]][1] %>% paste0(""), str_split(stat_col,"\\.")[[1]][1] %>% paste0("A"))]
      names(static)[(ncol(static)-1):ncol(static)] = c("Make", "Att")
      static$Stat = static$Make/static$Att
      if (ra==1){
        static$Stat_ra = static$Stat
      } else{
        static$Stat_ra = NA
        for (i in 1:nrow(static)){
          if (static$G[i]<ra){
            static$Stat_ra[i] = NA
          } else{
            static$Stat_ra[i] = (sum(static$Make[(i-ra+1):(i)]))/((sum(static$Att[(i-ra+1):(i)])))
          }
        }
      }
    } else{
      static = cdf[,c("Player","Tm", "G", "Date", stat_col)]
      names(static)[ncol(static)] = "Stat"
      static = static %>% mutate(Stat = as.double(Stat))
      if (ra==1){
        static$Stat_ra = static$Stat
      } else{
        static$Stat_ra = NA
        for (i in 1:nrow(static)){
          if (static$G[i]<ra){
            static$Stat_ra[i] = NA
          } else{
            static$Stat_ra[i] = mean(static$Stat[(i-ra+1):(i)])
          }
        }
      }
    }
    static_line = static %>% filter(!is.na(Stat_ra))
    static_line = static_line %>% mutate(dateDisp = ifelse((G %in% c(ra,max(static_line$G))|Stat_ra == max(static_line$Stat_ra)),format(Date, "%m/%d/%y"),""))
    static_line$Player = factor(x = static_line$Player, levels = c(p1_df$Player[1],p2_df$Player[2]))
    if (ra > nrow(p1_df)){
      plot = data.frame(Error = "At least one player selected has not played enough games in the time period. Reduce rolling average or update timeframe.") %>% ggplot() + geom_label(aes(x = 1, y = 1, label = Error)) + theme_void()
    } else{
      if (ra==1){
        plot = static_line %>% ggplot(aes(x = G, y = Stat_ra, color = Player, linetype = Player)) + theme_bw() +
          geom_line() + scale_y_continuous(name = stat_input) +
          scale_color_manual(name = paste0(ra,"-game rolling avg."), values = c(top_color$Hex[1],"grey50")) +
          theme(legend.position = "top") + scale_linetype_manual(name = paste0(ra,"-game rolling avg."), values = c("solid", "dashed")) +
          scale_x_continuous("Games Played (Time Span)") + geom_label(data = static_line %>% filter(dateDisp!=''), aes(label = dateDisp),vjust = 0, size = 2, label.padding = unit(0.25, "lines"),show.legend=F) +
          geom_point()
      } else{
        plot = static_line %>% ggplot(aes(x = G, y = Stat_ra, color = Player, linetype = Player)) + theme_bw() +
          geom_line() + scale_y_continuous(name = stat_input) +
          scale_color_manual(name = paste0(ra,"-game rolling avg."), values = c(top_color$Hex[1],"grey50")) +
          theme(legend.position = "top") + scale_linetype_manual(name = paste0(ra,"-game rolling avg."), values = c("solid", "dashed")) +
          scale_x_continuous("Games Played (Time Span)") + geom_label(data = static_line %>% filter(dateDisp!=''), aes(label = dateDisp), vjust = 0, size = 2, label.padding = unit(0.1, "lines"),show.legend=F)
      }
    }
    
    plot
    
  })
  
  # Plot 2: Distribution Comparison
  output$plot2 = renderPlot({
    p1_df = p1_df();p2_df = p2_df();stat_input = stat_input();date_input = date_input()
    # ;roll_avg_input = roll_avg_input()
    # translate some inputs
    stat_col = menu_map(stat_input)
    # ra = as.integer(roll_avg_input)
    min_date = ifelse(date_input == "Past year (365 days)",Sys.Date()-365,ifelse(date_input=="Past month (30 days)",Sys.Date()-30,ifelse(Sys.Date() %>% format("%m") %>% as.integer() <= 10,paste0((Sys.Date() %>% format("%Y") %>% as.integer() - 1),"-10-01"),paste0((Sys.Date() %>% format("%Y") %>% as.integer()),"-10-01")))) %>% as.Date()
    
    if (nrow(p2_df)==0){
      # if p2_df is empty, then treat data like we're only plotting one player (because we are!)
      p1_df = p1_df %>% filter(Date >= min_date)
      p1_df = p1_df %>% mutate(G = 1:nrow(p1_df),across(!c(Date,Tm,Player),as.double))
      cdf = p1_df %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = c("Tm" = "Team"))
    } else{
      # else: combine two player data frames!
      p1_df = p1_df %>% filter(Date >= min_date);p2_df = p2_df %>% filter(Date >= min_date)
      p1_df = p1_df %>% mutate(G = 1:nrow(p1_df),across(!c(Date,Tm,Player),as.double)); p2_df = p2_df %>% mutate(G = 1:nrow(p2_df),across(!c(Date,Tm,Player,G,MP),as.double))
      cdf = p1_df %>% rbind.data.frame(p2_df) %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = c("Tm" = "Team"))
    }
    cdf = cdf %>% mutate(across(c(X3P,X3PA,FT,FTA,FG,FGA),as.numeric)) %>% mutate(X2P = FG-X3P, X2PA = FGA-X3PA)
    #cdf = cdf %>% separate(Date, into = c("Year", "m", "d"), remove=F) %>% select(-m, -d) %>% inner_join(lga, by = "Year")
    cdf = cdf %>% mutate(Year = dts(Date)) %>% inner_join(lga, by = "Year")
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
                         ,tenPTS = ifelse(PTS>9,1,0),tenTRB = ifelse(TRB>9,1,0),tenAST = ifelse(AST>9,1,0),tenSTL = ifelse(STL>9,1,0),tenBLK = ifelse(BLK>9,1,0)) %>% 
      mutate(sum10s = tenPTS+tenTRB+tenAST+tenSTL+tenBLK) %>% 
      mutate(fPTS2 = (.5*PTS) + (TRB) + (AST) + (2*(STL)) + (2*(BLK)) + (-1*(TOV)) + (.5*X3P) +
               ifelse(sum10s > 1,1,0) + # double-double bonus
               ifelse(sum10s > 2,2,0) + # triple-double bonus
               ifelse(PTS > 39,2,0) + # 40+ points bonus
               ifelse(PTS > 49,2,0) # 50+ points bonus
      )
    top_color = cdf %>% filter(Player == p1_df$Player[1]) %>% tail(1)
    
    # modify Stat columns if stat_input is a percent!
    if (grepl("[P|p]ercentage",stat_input)){
      static = cdf[,c("Player","Tm", "G", "Date", str_split(stat_col,"\\.")[[1]][1] %>% paste0(""), str_split(stat_col,"\\.")[[1]][1] %>% paste0("A"))]
      names(static)[(ncol(static)-1):ncol(static)] = c("Make", "Att")
      static$Stat = static$Make/static$Att
    } else{
      static = cdf[,c("Player","Tm", "G", "Date", stat_col)]
      names(static)[ncol(static)] = "Stat"
      static = static %>% mutate(Stat = as.double(Stat))
    }
    sim_p1 = c();sim_p2 = c();s1=c(static$Stat[which(static$Player==p1_df$Player[1])]);s2=c(static$Stat[which(static$Player==p2_df$Player[1])])
    
    for (j in 1:10000){sim_p1 = c(sim_p1,mean(s1[sample(length(s1),size = min(length(s1),5),replace = T)]))}
    if (nrow(p2_df)==0){
      # if p2_df is empty, then treat data like we're only plotting one player (because we are!)
      sim_p2 = c()
      sims = data.frame(sim = sim_p1,Player = p1_df$Player[1])
      # add a sample size (games played) for context
      sims = sims %>% mutate(Player = paste0(Player," (n=",(length(static$Stat[which(static$Player==p1_df$Player[1])])),")"))
    } else{
      for (k in 1:10000){sim_p2 = c(sim_p2,mean(s2[sample(length(s2),size = min(length(s2),5),replace = F)]))}
      sims = rbind.data.frame(data.frame(sim = sim_p1,Player = p1_df$Player[1]), data.frame(sim = sim_p2,Player = p2_df$Player[1])) %>% as_tibble()
      # add a sample size (games played) for context
      sims = sims %>% mutate(Player = ifelse(Player==p1_df$Player[1],paste0(Player," (n=",(length(static$Stat[which(static$Player==p1_df$Player[1])])),")"),paste0(Player," (n=",(length(static$Stat[which(static$Player==p2_df$Player[1])])),")")))
    }
    
    sims$Player = factor(sims$Player, levels = unique(sims$Player))
    plot_3_in =
      sims %>% ggplot(aes(x = sim, color = Player)) +
      geom_histogram(alpha = I(1/4), position = "identity", bins = 30, aes(y = ..density.., fill = Player)) +
      geom_density(alpha = I(4/5))
    # Extract the data from the ggplot object
    plot_data <- ggplot_build(plot_3_in)$data[[1]]
    plot_3 = plot_3_in + theme_bw() +
      scale_y_continuous("Normalized Density") + ggtitle("Distribution Comparison", subtitle = "  Based on random 5-game samples") +
      scale_x_continuous(name = stat_input) +
      scale_color_manual("", values = c(top_color$Hex[1],"grey50")) +
      scale_fill_manual("", values = c(top_color$Hex[1],"grey50")) +
      theme(legend.position = "top", plot.subtitle = element_text(size = 8, face = "italic"))
    
    plot_3
    
  })
  
  # Table 1: Game Log
  output$table1 = renderDT({
    p1_df = p1_df();p2_df = p2_df();date_input = date_input()
    min_date = ifelse(date_input == "Past year (365 days)",Sys.Date()-365,ifelse(date_input=="Past month (30 days)",Sys.Date()-30,ifelse(Sys.Date() %>% format("%m") %>% as.integer() <= 10,paste0((Sys.Date() %>% format("%Y") %>% as.integer() - 1),"-10-01"),paste0((Sys.Date() %>% format("%Y") %>% as.integer()),"-10-01")))) %>% as.Date()
    
    if (nrow(p2_df)==0){
      fi1 = str_split(p1_df$Player[1],"")[[1]][1];ln1 = rev(str_split(p1_df$Player[1]," ")[[1]][!(str_split(p1_df$Player[1]," ")[[1]] %in% c("Jr.","II","III"))])[2]
      p1_df = p1_df %>% mutate(Player = paste0(fi1,". ",ln1))
    } else{
      fi1 = str_split(p1_df$Player[1],"")[[1]][1];ln1 = rev(str_split(p1_df$Player[1]," ")[[1]][!(str_split(p1_df$Player[1]," ")[[1]] %in% c("Jr.","II","III"))])[2];fi2 = str_split(p2_df$Player[1],"")[[1]][1];ln2 = rev(str_split(p2_df$Player[1]," ")[[1]][!(str_split(p2_df$Player[1]," ")[[1]] %in% c("Jr.","II","III"))])[2]
      if ((fi1 == fi2 & ln1 == ln2)==F){
        p1_df = p1_df %>% mutate(Player = paste0(fi1,". ",ln1))
        p2_df = p2_df %>% mutate(Player = paste0(fi2,". ",ln2))
      }
    }
    if (nrow(p2_df)==0){
      # if p2_df is empty, then treat data like we're only plotting one player (because we are!)
      
      p1_df = p1_df %>% filter(Date >= min_date)
      p1_df = p1_df %>% mutate(G = 1:nrow(p1_df),across(!c(Date,Tm,Player),as.double))
      cdf = p1_df %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = c("Tm" = "Team"))
    } else{
      # else: combine two player data frames!
      
      p1_df = p1_df %>% filter(Date >= min_date);p2_df = p2_df %>% filter(Date >= min_date)
      p1_df = p1_df %>% mutate(G = 1:nrow(p1_df),across(!c(Date,Tm,Player),as.double)); p2_df = p2_df %>% mutate(G = 1:nrow(p2_df),across(!c(Date,Tm,Player,G,MP),as.double))
      cdf = p1_df %>% rbind.data.frame(p2_df) %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = c("Tm" = "Team"))
    }
    
    cdf = cdf %>% mutate(across(c(X3P,X3PA,FT,FTA,FG,FGA),as.numeric)) %>% mutate(X2P = FG-X3P, X2PA = FGA-X3PA)
    #cdf = cdf %>% separate(Date, into = c("Year", "m", "d"), remove=F) %>% select(-m, -d) %>% inner_join(lga, by = "Year")
    cdf = cdf %>% mutate(Year = dts(Date)) %>% inner_join(lga, by = "Year")
    cdf = cdf %>% mutate(X3PAdd = ((X3P/ifelse(X3PA==0,1,X3PA))-(la3P.))*(X3PA),X2PAdd = ((X2P/ifelse(X2PA==0,1,X2PA))-(la2P.))*(X2PA),FTAdd = ((FT/ifelse(FTA==0,1,FTA))-(laFT.))*(FTA),
                         valueAdd = ((PTS/MP)-(laPTSperM))*(MP) + #points added (volume)
                           ((3*X3PAdd)+(2*X2PAdd)+FTAdd) + #points added (efficiency)
                           (((AST/MP)-(laASTperM))*(MP))*(laPTSperMake)*(0.5) + #assists added
                           (((STL/MP)-(laSTLperM))*(MP))*(laPTSperPoss) + #steals added
                           (((BLK/MP)-(laBLKperM))*(MP))*(laPTSperPoss)*(laDRBrate) + #blocks added
                           (-1*(((TOV/MP)-(laTOVperM))*(MP))*(laPTSperPoss)) + #turnovers added
                           (((DRB/MP)-(laDRBperM))*(MP))*(laPTSperPoss)*(laORBrate) + #d rebounds added
                           (((ORB/MP)-(laORBperM))*(MP))*(laPTSperPoss)*(laDRBrate), #o rebounds added
                         fPTS = 2*(FG) + -1*(FGA) + 1*(FT) + -1*(FTA) + 1*(X3P) + 1*(TRB) + 2*(AST) + 4*(STL) + 4*(BLK) + -2*(TOV) + 1*(PTS)
    )
    top_color = cdf %>% filter(Player == p1_df$Player[1]) %>% tail(1)
    
    if (nrow(p2_df)==0){
      cdf %>% arrange(desc(valueAdd)) %>% transmute(Player, Date = format.Date(Date, "%y-%m-%d"), PTS, TRB, AST, BLK, STL, `3P` = paste0(X3P,"/",X3PA), `2P` = paste0(X2P,"/",X2PA), FT = paste0(FT,"/",FTA), VA = round(valueAdd,2)) %>% 
        datatable(options = list(pageLength = 25))
      
    } else{
      cdf %>% arrange(desc(valueAdd)) %>% transmute(Player, Date = format.Date(Date, "%y-%m-%d"), PTS, TRB, AST, BLK, STL, `3P` = paste0(X3P,"/",X3PA), `2P` = paste0(X2P,"/",X2PA), FT = paste0(FT,"/",FTA), VA = round(valueAdd,2)) %>% 
        datatable(options = list(pageLength = 25)) %>% 
        formatStyle(
          'Player', 
          target = 'row', 
          backgroundColor = styleEqual(
            c(p1_df$Player[1], p2_df$Player[1]), 
            c(lighten_color(top_color$Hex[1]), "lightgrey")),
          color = styleEqual(
            c(p1_df$Player[1], p2_df$Player[1]), 
            c("white", "black")
          )
        )
    }
  })
  
  # Plot 3: Leaders Bar Graph
  output$plot3 = renderPlot({
    year_input = year_input();stat_input_2 = stat_input_2();pg_factor = pg_factor();player_input = player_input();reg_playoff = reg_playoff()
    if (reg_playoff == "Playoffs"){
      today_file = paste0("Complete Data/Totals_p_",Sys.Date(),".csv",collapse = "")
      df_ = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
      gpl_df = df_ %>% group_by(Year) %>% summarize(.groups = "drop",gpl = ifelse(max(G) < 29,0.5*max(G),0.75*max(G)))
      df_ = df_ %>% inner_join(gpl_df,by = join_by(Year))
      df_1 = df_ %>% filter(G > (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df_2 = df_ %>% filter(G <= (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df = df_1 %>% rbind.data.frame(df_2)
      df$Player = iconv(df$Player, to = "UTF-8");maxYr = max(df$Year)
    }
    all_year = df %>% filter(Year == year_input)
    stat_col = menu_map(stat_input_2)
    leaders_static = all_year[,c("Player","Team", "Hex","G",stat_col)]
    names(leaders_static)[ncol(leaders_static)] = "Stat"
    if (grepl("^F.*|^X.*",stat_col)){
      leaders_static = leaders_static %>% filter(!(Stat %in% c(0,1)))
    }
    if (pg_factor&(!grepl("Percentage",stat_input_2))){
      min_games = ifelse(player_input != "",min(leaders_static$G[which(leaders_static$Player==player_input)],gpl_df$gpl[which(gpl_df$Year == year_input)]),gpl_df$gpl[which(gpl_df$Year == year_input)])
      leaders_static = leaders_static %>% mutate(Stat = Stat/G) %>% filter(G >= min_games)  # if looking at per game stats, divide Stat by G and remove players who missed 25%+ of the season.
      leaders_static = leaders_static %>% arrange(desc(Stat)) %>% 
        data.frame(rk = 1:nrow(leaders_static)) %>% mutate(display_stat = paste0(" ",round(Stat,3)))
    } else{
      leaders_static = leaders_static %>% 
        arrange(desc(Stat)) %>% data.frame(rk = 1:nrow(leaders_static)) %>% mutate(display_stat = paste0(" ",round(Stat,3)))
    }
    if (player_input == ""){
      output = leaders_static[1:10,]
    } else{
      output = leaders_static[(max(((which(player_input==leaders_static$Player))-5),1)):(min(((which(player_input==leaders_static$Player))+5),nrow(leaders_static))),]  
    }
    output = output %>% separate(Player,into = c("disPlayer","bbref"),sep = "\\(",remove = F) %>% select(-bbref)
    output = output %>% mutate(rk = paste0("#",rk), disPlayer = paste0(disPlayer, " "))
    output$rk = factor(output$rk, levels = rev(output$rk))
    
    if (any(output$Stat<0)){
      plot = output %>% ggplot(aes(x = rk, y = Stat, fill = Hex)) +
        geom_bar(stat = "identity", color = "black", aes(fill = Hex), alpha = I(3/5)) +
        theme_bw() + coord_flip() + scale_fill_identity() + theme(legend.position = "none") +
        scale_y_continuous(name = paste0(stat_input_2,ifelse(pg_factor," (Per Game) "," (Total) ")),
                           limits = c(min(output$Stat)-((abs(min(output$Stat)))/4),max(max(output$Stat)+((abs(max(output$Stat))/9.5)),0))) + 
        scale_x_discrete(name = "") +
        geom_text(aes(fontface = "bold",label = disPlayer), hjust = 1, size = I(2.25)) +
        geom_text(aes(label = display_stat), hjust = 0, size = I(2.25)) +
        ggtitle(label = "", subtitle = ifelse(pg_factor,paste0(year_input," ",reg_playoff," Leaders (min. ",(floor(min_games))," game(s))"),paste0(year_input," ",reg_playoff," Leaders ")))
    } else{
      plot = output %>% ggplot(aes(x = rk, y = Stat, fill = Hex)) +
        geom_bar(stat = "identity", color = "black", aes(fill = Hex), alpha = I(3/5)) +
        theme_bw() + coord_flip() + scale_fill_identity() + theme(legend.position = "none") +
        scale_y_continuous(name = paste0(stat_input_2,ifelse(pg_factor," (Per Game) "," (Total) ")),
                           limits = c(0,max(output$Stat)+((max(output$Stat)/9.5)))) + 
        scale_x_discrete(name = "") +
        geom_text(aes(fontface = "bold",label = disPlayer), hjust = 1, size = I(2.25)) +
        geom_text(aes(label = display_stat), hjust = 0, size = I(2.25)) +
        ggtitle(label = "", subtitle = ifelse(pg_factor,paste0(year_input," ",reg_playoff," Leaders (min. ",(floor(min_games))," game(s))"),paste0(year_input," ",reg_playoff," Leaders ")))
    }
    
    plot
    
  })
  
  # Table 3: Leaders Summary Statistics
  output$table3 = renderDT({
    year_input = year_input();stat_input_2 = stat_input_2();pg_factor = pg_factor();player_input = player_input();reg_playoff = reg_playoff()
    if (reg_playoff == "Playoffs"){
      today_file = paste0("Complete Data/Totals_p_",Sys.Date(),".csv",collapse = "")
      df_ = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
      gpl_df = df_ %>% group_by(Year) %>% summarize(.groups = "drop",gpl = ifelse(max(G) < 29,0.5*max(G),0.75*max(G)))
      df_ = df_ %>% inner_join(gpl_df,by = join_by(Year))
      df_1 = df_ %>% filter(G > (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df_2 = df_ %>% filter(G <= (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df = df_1 %>% rbind.data.frame(df_2)
      df$Player = iconv(df$Player, to = "UTF-8");maxYr = max(df$Year)
    }
    v_cols = read.csv("Complete Data/menu_options_3.csv")[,-1] %>%
      filter(category == "volume") %>% select(col_name) %>% as.vector()
    all_year = df %>% filter(Year == year_input)
    stat_col = menu_map(stat_input_2)
    leaders_static = all_year[,c("Player","Team", "Hex","G",stat_col)]
    names(leaders_static)[ncol(leaders_static)] = "Stat"
    if (grepl("^F.*|^X.*",stat_col)){
      leaders_static = leaders_static %>% filter(!(Stat %in% c(0,1)))
    }
    if (pg_factor&(!grepl("Percentage",stat_input_2))){
      min_games = ifelse(player_input != "",min(leaders_static$G[which(leaders_static$Player==player_input)],gpl_df$gpl[which(gpl_df$Year == year_input)]),gpl_df$gpl[which(gpl_df$Year == year_input)])
      leaders_static = leaders_static %>% mutate(Stat = Stat/G) %>% filter(G >= min_games)  # if looking at per game stats, divide Stat by G and remove players who missed 25%+ of the season.
      leaders_static = leaders_static %>% arrange(desc(Stat)) %>% 
        data.frame(rk = 1:nrow(leaders_static)) %>% mutate(display_stat = paste0(" ",round(Stat,3)))
    } else{
      leaders_static = leaders_static %>% 
        arrange(desc(Stat)) %>% data.frame(rk = 1:nrow(leaders_static)) %>% mutate(display_stat = paste0(" ",round(Stat,3)))
    }
    if (player_input == ""){
      output = leaders_static[1:10,]
    } else{
      output = leaders_static[(max(((which(player_input==leaders_static$Player))-5),1)):(min(((which(player_input==leaders_static$Player))+5),nrow(leaders_static))),]  
    }
    output = output %>% separate(Player,into = c("disPlayer","bbref"),sep = "\\(",remove = F) %>% select(-bbref)
    output = output %>% mutate(rk = paste0("#",rk), disPlayer = paste0(disPlayer, " "))
    output$rk = factor(output$rk, levels = rev(output$rk))
    player_col = all_year$Hex[which(all_year$Player == input$player_input)]
    
    if (stat_col %in% v_cols$col_name){
      output %>% select(Player) %>% left_join(all_year, by = join_by(Player)) %>% 
        transmute(Player, G, MP = round(MP/G,2), PTS = round(PTS/G,2), TRB = round(TRB/G,2), AST = round(AST/G,2), 
                  STL = round(STL/G,2), BLK = round(BLK/G,2), valueAdd = round(valueAdd/G,2)) %>% 
        datatable(rownames = F, 
                  options = list(
                    pageLength = 11, # Set the default number of rows
                    dom = 't', # Only show the table, without additional interface elements
                    paging = FALSE, # Disable pagination
                    searching = FALSE # Disable the search box
                  )
        ) %>%
        formatStyle(
          'Player',
          target = 'row',
          backgroundColor = styleEqual(input$player_input, "gold")
        ) %>%
        formatStyle(
          columns = c('G','MP'),
          color = 'grey'
        )
    } else{
      output %>% select(Player) %>% left_join(all_year, by = join_by(Player)) %>% 
        transmute(Player, G, PTS = round(PTS/G,2), `3P.` = X3P., `3PA/G` = round(X3PA/G,2), 
                  `2P.` = X2P., `2PA/G` = round(X2PA/G,2), FT., `FTA/G` = round(FTA/G,2), `Eff/G` = round(PTSAdd/G,3)) %>% 
        datatable(rownames = F,
                  options = list(
                    pageLength = 11, # Set the default number of rows
                    dom = 't', # Only show the table, without additional interface elements
                    paging = FALSE, # Disable pagination
                    searching = FALSE # Disable the search box
                  )
        ) %>% 
        formatStyle(
          'Player',
          target = 'row',
          backgroundColor = styleEqual(input$player_input, "gold")
        ) %>%
        formatStyle(
          columns = c('G'),
          color = 'grey'
        )
    }
    
  })
  
  # Table 5: Single Game Statistics
  output$table5 = renderDT({
    req(input$run)
    isolate({
      year_input_2 = year_input_2()
      team_input = team_input()
      date_input_2 = date_input_2()
      player_input_2 = player_input_2()
      
      game_log = team_gl(team_map(team_input),year = str_split(year_input_2,"-")[[1]][2])
      date_choice = game_log$DateatOpp[which(game_log$DateatOpp==date_input_2)]
      ilink = game_log$link[which(game_log$DateatOpp==date_input_2)]
      
      gl_df = team_sg(abb = team_map(team_input),ilink = ilink,date_choice = date_input_2)
      gl_df = gl_df %>% mutate(across(-c("Player","Team"),as.double),X2P = FG-`3P`,X2PA = FGA-`3PA`)
      if (T){
        calc = gl_df %>% mutate(Year = dts(date_input_2)) %>% 
          inner_join(lga, by = join_by(Year))
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
      gl_df = gl_df %>% inner_join(calc %>% select(Player,X3PAdd,X2PAdd,FTAdd,valueAdd,fPTS),
                                   by = join_by(Player)) %>% 
        arrange(desc(valueAdd)) %>% select(Player, Team, MP, valueAdd,everything())
      if ("+/-" %in% names(gl_df)){
        datatable(gl_df %>% transmute(Player, Team, MP = round(MP,2), PTS, TRB, AST, BLK, STL, TOV, FG = paste0(FG,"/",FGA), `3P` = paste0(`3P`,"/",`3PA`), VA = round(valueAdd,2), `+/-`),
                  options = list(
                    pageLength = 50,
                    dom = 't', # Only show the table, without additional interface elements
                    paging = FALSE, # Disable pagination
                    searching = FALSE # Disable the search box
                    
                  )) %>% 
          formatStyle(
            'Team', 
            target = 'row', 
            backgroundColor = styleEqual(
              c(team_map(team_input), str_split(date_input_2,"\\)| ")[[1]][3]),
              c(lighten_color(df$Hex[which(df$Team==team_map(team_input))[1]],.25), "lightgrey")),
            color = styleEqual(
              c(team_map(team_input), str_split(date_input_2,"\\)| ")[[1]][3]),
              c("white", "black")
            )
          ) %>% 
          formatStyle(
            'Player',
            target = 'row',
            backgroundColor = styleEqual(input$player_input_2, "gold"),
            color = styleEqual(input$player_input_2, "black")
          )
      } else{
        datatable(gl_df %>% transmute(Player, Team, MP = round(MP,2), PTS, TRB, AST, BLK, STL, TOV, FG = paste0(FG,"/",FGA), `3P` = paste0(`3P`,"/",`3PA`), VA = round(valueAdd,2)),
                  options = list(
                    pageLength = 50,
                    dom = 't', # Only show the table, without additional interface elements
                    paging = FALSE, # Disable pagination
                    searching = FALSE # Disable the search box
                    
                  )) %>% 
          formatStyle(
            'Team', 
            target = 'row', 
            backgroundColor = styleEqual(
              c(team_map(team_input), str_split(date_input_2,"\\)| ")[[1]][3]),
              c(lighten_color(df$Hex[which(df$Team==team_map(team_input))[1]],.25), "lightgrey")),
            color = styleEqual(
              c(team_map(team_input), str_split(date_input_2,"\\)| ")[[1]][3]),
              c("white", "black")
            )
          ) %>% 
          formatStyle(
            'Player',
            target = 'row',
            backgroundColor = styleEqual(input$player_input_2, "gold"),
            color = styleEqual(input$player_input_2, "black")
          )
      }
    })
  })
  
  # Plot 5: Game Scatter or Player Performance Breakdown
  output$plot5 = renderPlot({
    req(input$run)
    isolate({
      year_input_2 = year_input_2()
      team_input = team_input()
      date_input_2 = date_input_2()
      player_input_2 = player_input_2()
      
      game_log = team_gl(team_map(team_input),year = str_split(year_input_2,"-")[[1]][2])
      date_choice = game_log$DateatOpp[which(game_log$DateatOpp==date_input_2)]
      ilink = game_log$link[which(game_log$DateatOpp==date_input_2)]
      
      gl_df = team_sg(abb = team_map(team_input),ilink = ilink,date_choice = date_input_2)
      gl_df = gl_df %>% mutate(across(-c("Player","Team"),as.double),X2P = FG-`3P`,X2PA = FGA-`3PA`)
      if (T){
        calc = gl_df %>% mutate(Year = dts(date_input_2)) %>% 
          inner_join(lga, by = join_by(Year))
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
      
      if (player_input_2 == ""|(!(player_input_2 %in% calc$Player))){
        gl_df %>% mutate(col = ifelse(Team==team_map(team_input),df$Hex[which(df$Team==team_map(team_input))[1]],"grey40")) %>% 
          ggplot(aes(x = `+/-`,y = valueAdd,label = Player, fill = col)) + 
          geom_hline(yintercept = 0,alpha = I(1/3)) + 
          geom_vline(xintercept = 0,alpha = I(1/3)) + 
          geom_label(alpha = I(0.5), size = 3.25) +
          theme_bw() + scale_fill_identity() + 
          scale_y_continuous("Value Added") +
          scale_x_continuous(limits = c(min(gl_df$`+/-`)-((abs(min(gl_df$`+/-`)))/5),max(max(gl_df$`+/-`)+((abs(max(gl_df$`+/-`))/5)),0)))
        
      } else{
        #df %>% separate(Player,into = c("Player","key"),sep = " \\(") %>% select(-key) %>% inner_join(calc %>% mutate(Year = dts(date_input_3)) %>% select(Year,Player))
        season_avgs = df %>% filter(grepl(player_input_2,Player),Year == dts(date_input_2)) %>% 
          transmute(
            `Scoring (Volume)` = vaPTSv/G
            ,`Efficiency (3P)` = (3*X3PAdd)/G
            ,`Efficiency (2P)` = (2*X2PAdd)/G
            ,`Efficiency (FT)` = FTAdd/G
            ,`Assists` = vaAST/G
            ,`Steals` = vaSTL/G
            ,`Blocks` = vaBLK/G
            ,`Turnovers` = vaTOV/G
            ,`Rebounds (D)` = vaDRB/G
            ,`Rebounds (O)` = vaORB/G
          ) %>% gather(key = "key",value = "value") %>% 
          mutate(abs = (value))
        
        sp_output = calc %>% filter(Player == player_input_2) %>% 
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
          mutate(abs = ifelse(as.double(value)>0,value,0),col_n = ifelse(value > 0,lighten_color(df$Hex[which(df$Team==team_map(team_input))[1]],.25),"lightgrey")) %>% 
          data.frame(label = c(paste0(" PTS: ",calc$PTS[which(calc$Player==player_input_2)])
                               ,paste0(" 3P: ",calc$`3P`[which(calc$Player==player_input_2)],"/",calc$`3PA`[which(calc$Player==player_input_2)])
                               ,paste0(" 2P: ",calc$X2P[which(calc$Player==player_input_2)],"/",calc$X2PA[which(calc$Player==player_input_2)])
                               ,paste0(" FT: ",calc$FT[which(calc$Player==player_input_2)],"/",calc$FTA[which(calc$Player==player_input_2)])
                               ,paste0(" AST: ",calc$AST[which(calc$Player==player_input_2)])
                               ,paste0(" STL: ",calc$STL[which(calc$Player==player_input_2)])
                               ,paste0(" BLK: ",calc$BLK[which(calc$Player==player_input_2)])
                               ,paste0(" TOV: ",calc$TOV[which(calc$Player==player_input_2)])
                               ,paste0(" DRB: ",calc$DRB[which(calc$Player==player_input_2)])
                               ,paste0(" ORB: ",calc$ORB[which(calc$Player==player_input_2)])
                               )) %>% 
          arrange(desc(value))
        sp_output$key = factor(sp_output$key,levels = rev(sp_output$key))
        sp_output %>% ggplot(aes(x = key, y = value, fill = col_n)) + geom_bar(color = "black",stat="identity",width=I(1/2),alpha = I(.8)) +
          theme_bw() + geom_hline(yintercept = 0) + coord_flip() +
          #scale_y_continuous("Value Added") + 
          scale_y_continuous(
            name = "Value Added",
            breaks = pretty(sp_output$value),  # or use a custom vector like c(-10, 0, 10)
            labels = function(x) ifelse(x == 0, "League Average", x),
            expand = expansion(mult = c(0.1, 0.2))  # 10% padding above the max
          ) + scale_x_discrete("") + scale_fill_manual(values = unique(c(sp_output$col_n))) + 
          theme(legend.position = "none") + ggtitle(player_input_2,subtitle = date_choice) + 
          geom_text(aes(y = abs,label = label),hjust = 0,fontface="bold") +
          geom_bar(data = season_avgs,aes(x = key, y = value),stat = "identity",width = I(.075),fill = "black",alpha = I(.3)) + 
          geom_point(data = season_avgs,aes(x = key, y = value),size = 3,alpha = I(.25),inherit.aes = FALSE)
        
      }
      
    })
  })
  
  # Table 6: Date Lookup Single Game Statistics
  output$table6 = renderDT({
    req(input$run_2)
    isolate({
      date_input_3 = date_input_3()
      matchup_input = matchup_input()
      period_input = period_input()
      if (matchup_input == "No game data for this day!"){
        data.frame(Error = paste0("Date does not have any game data as of ",format(Sys.time() %>% as.POSIXct(tz = "America/New_York"), "%a %b %d %Y %X")," ET"))
      } else{
        m = str_split(date_input_3,"-")[[1]][2];d = str_split(date_input_3,"-")[[1]][3];y = str_split(date_input_3,"-")[[1]][1]
        url = paste0("https://www.basketball-reference.com/boxscores/index.fcgi?month=",m,"&day=",d,"&year=",y)
        page = read_html(url)
        all_links = page %>% html_nodes("a") %>% html_attr("href")
        links = unique(all_links[which(grepl("boxscores\\/\\d{0,10}[A-Za-z]{3}.html",all_links))])
        data.raw = page %>% html_table()
        matchups = data.frame(link = links,matchup = "")
        for (i in 1:length(links)){matchups$matchup[i] = paste0(data.raw[[3*i-2]]$X1[1]," vs. ",data.raw[[3*i-2]]$X1[2])}
        
        if (matchup_input == '-'){
          if (period_input != "Game"){
            collect_perf = NULL
            for (ind in 1:nrow(matchups)){
              ilink = paste0("https://www.basketball-reference.com", matchups$link[ind])
              sgm_input = matchups$matchup[ind]
              gl_df = team_dl(
                abb = team_map2(str_split(sgm_input, " vs. ")[[1]][2]),
                ilink = ilink,
                opp_abb = team_map2(str_split(sgm_input, " vs. ")[[1]][1]),
                period = period_input
              ) %>%
                mutate(
                  across(-c("Player", "Team"), as.double),
                  X2P = FG - `3P`,
                  X2PA = FGA - `3PA`
                ) %>%
                group_by(Player, Team) %>%
                summarise(.groups = "drop", across(everything(), sum))
              
              if (is.null(collect_perf)) {
                collect_perf = gl_df[0, ]  # Preallocate with structure
              }
              
              calc = gl_df %>% mutate(Year = dts(date_input_3)) %>% inner_join(lga, by = "Year") %>%
                mutate(
                  X3PAdd = ((`3P` / ifelse(`3PA` == 0, 1, `3PA`)) - la3P.) * `3PA`,
                  X2PAdd = ((X2P / ifelse(X2PA == 0, 1, X2PA)) - la2P.) * X2PA,
                  FTAdd = ((FT / ifelse(FTA == 0, 1, FTA)) - laFT.) * FTA,
                  valueAdd = ((PTS / MP) - laPTSperM) * MP +
                    ((3 * X3PAdd) + (2 * X2PAdd) + FTAdd) +
                    (((AST / MP) - laASTperM) * MP) * laPTSperMake * 0.5 +
                    (((STL / MP) - laSTLperM) * MP) * laPTSperPoss +
                    (((BLK / MP) - laBLKperM) * MP) * laPTSperPoss * laDRBrate +
                    -1 * (((TOV / MP) - laTOVperM) * MP) * laPTSperPoss +
                    (((DRB / MP) - laDRBperM) * MP) * laPTSperPoss * laORBrate +
                    (((ORB / MP) - laORBperM) * MP) * laPTSperPoss * laDRBrate,
                  fPTS = 2 * FG + -1 * FGA + FT + -1 * FTA + `3P` + TRB + 2 * AST + 4 * STL + 4 * BLK + -2 * TOV + PTS
                )
              
              gl_df = gl_df %>%
                inner_join(calc %>% select(Player, X3PAdd, X2PAdd, FTAdd, valueAdd, fPTS), by = join_by(Player)) %>%
                select(Player, Team, MP, valueAdd, everything())
              
              collect_perf = rbind(collect_perf, gl_df)
            }
            gl_df = collect_perf %>% arrange(desc(valueAdd))
          } else{
            ilink = paste0("https://www.basketball-reference.com/friv/dailyleaders.fcgi?month=",m,"&day=",d,"&year=",y,"&type=all")
            dail_df = daily_l(ilink) %>% select(-Rk)
            names(dail_df)[which(names(dail_df)=="Tm")] = 'Team'
            dail_df = dail_df %>% separate(col = MP, into = c("MP", "SP"),sep = "\\:") %>% mutate(MP = as.double(MP)+(as.double(SP)/60)) %>% select(-SP)
            if (T){
              calc = dail_df %>% mutate(Year = dts(date_input_3)) %>% inner_join(lga, by = "Year")
              calc = calc %>% mutate(across(-c("Player","Team","Opp"),as.double),X2P = FG-`3P`,X2PA = FGA-`3PA`)
              
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
            gl_df = dail_df %>% inner_join(calc %>% select(Player,X3PAdd,X2PAdd,FTAdd,valueAdd,fPTS),
                                           by = join_by(Player)) %>%
              arrange(desc(valueAdd)) %>% select(Player, Team, Opp, MP, valueAdd,everything())
            
          }
        } else{
          ilink = paste0("https://www.basketball-reference.com",matchups$link[which(matchups$matchup==matchup_input)])
          gl_df = team_dl(abb = team_map2(str_split(matchup_input," vs. ")[[1]][2]),ilink = ilink,opp_abb = team_map2(str_split(matchup_input," vs. ")[[1]][1]),period = period_input)
          gl_df = gl_df %>% mutate(across(-c("Player","Team"),as.double),X2P = FG-`3P`,X2PA = FGA-`3PA`) %>% group_by(Player,Team) %>% summarise(.groups = "drop",across(everything(),sum))
          
          if (T){
            #calc = gl_df %>% cbind.data.frame(lga %>% arrange(Year) %>% tail(1))
            calc = gl_df %>% mutate(Year = dts(date_input_3)) %>% inner_join(lga, by = "Year")
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
          gl_df = gl_df %>% inner_join(calc %>% select(Player,X3PAdd,X2PAdd,FTAdd,valueAdd,fPTS),
                                       by = join_by(Player)) %>%
            arrange(desc(valueAdd)) %>% select(Player, Team, MP, valueAdd,everything())
        }
        gl_df = gl_df %>% transmute(Player, Team, MP = round(MP,2), PTS, TRB, AST, BLK, STL, TOV, FG = paste0(FG,"/",FGA), `3P` = paste0(`3P`,"/",`3PA`), VA = round(valueAdd,2), `+/-`) %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1],by = join_by(Team))
        if (matchup_input == "-"){
          datatable(gl_df,
                    options = list(
                      pageLength = 10,
                      columnDefs = list(list(visible = FALSE, targets = 14))
                    )) %>%
            formatStyle(
              columns = 1:(ncol(gl_df)-1),
              valueColumns = "Hex",
              backgroundColor = styleEqual(gl_df$Hex, gl_df$Hex),
              color = "white"
            )
        } else{
          gl_df$Hex[which(gl_df$Team == team_map2(str_split(matchup_input," vs. ")[[1]][1]))] = "#808080"
          datatable(gl_df,
                    options = list(
                      pageLength = 50,
                      dom = 't', # Only show the table, without additional interface elements
                      paging = FALSE, # Disable pagination
                      searching = FALSE, # Disable the search box
                      columnDefs = list(list(visible = FALSE, targets = 14))
                    )) %>%
            formatStyle(
              columns = 1:(ncol(gl_df)-1),
              valueColumns = "Hex",
              backgroundColor = styleEqual(gl_df$Hex, gl_df$Hex),
              color = "white"
            )
        }
      }
    })
  })
  
  # Plot 6: Date Lookup Single Game Statistics Scatter
  output$plot6 = renderPlot({
    req(input$run_2)
    isolate({
      date_input_3 = date_input_3()
      matchup_input = matchup_input()
      period_input = period_input()
      if (matchup_input %in% c("No game(s) data for this day!","-")){
        ## do nothing if no games ##
        if (matchup_input == "-" & period_input == "Game"){
          m = str_split(date_input_3,"-")[[1]][2];d = str_split(date_input_3,"-")[[1]][3];y = str_split(date_input_3,"-")[[1]][1]
          ilink = paste0("https://www.basketball-reference.com/friv/dailyleaders.fcgi?month=",m,"&day=",d,"&year=",y,"&type=all")
          dail_df = daily_l(ilink) %>% select(-Rk)
          names(dail_df)[which(names(dail_df)=="Tm")] = 'Team'
          dail_df = dail_df %>% separate(col = MP, into = c("MP", "SP"),sep = "\\:") %>% 
            mutate(MP = as.double(MP)+(as.double(SP)/60)) %>% select(-SP) %>% 
            mutate(across(-c("Player","Team","Opp"),as.double),X2P = FG-`3P`,X2PA = FGA-`3PA`)
          if (T){
            #calc = dail_df %>% cbind.data.frame(lga %>% arrange(Year) %>% tail(1))
            calc = dail_df %>% mutate(Year = dts(date_input_3)) %>% inner_join(lga, by = "Year")
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
          gl_df = dail_df %>% inner_join(calc %>% select(Player,X3PAdd,X2PAdd,FTAdd,valueAdd,fPTS),
                                         by = join_by(Player)) %>%
            arrange(desc(valueAdd)) %>% select(Player, Team, Opp, MP, valueAdd,everything()) %>% head(10)
          gl_df = gl_df %>% mutate(Player = paste0(Player," vs. ",Opp)) %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1],by = "Team")
          gl_df %>% mutate(col = Hex) %>% 
            ggplot(aes(x = `+/-`,y = valueAdd,label = Player, fill = col)) + 
            #geom_hline(yintercept = 0,alpha = I(1/3)) + 
            geom_vline(xintercept = 0,alpha = I(1/3)) + 
            geom_label(alpha = I(0.5), size = 3.25) +
            theme_bw() + scale_fill_identity() + 
            scale_y_continuous("Value Added") +
            scale_x_continuous(limits = c(min(gl_df$`+/-`)-((abs(min(gl_df$`+/-`)))/4),max(max(gl_df$`+/-`)+((abs(max(gl_df$`+/-`))/4)),0))) +
            ggtitle(paste0("Top 10 Performances on ",date_input_3),"  value added vs. plus/minus")
          
        }
        ## also do nothing if anything but "game" is selected for period
      } else{
        m = str_split(date_input_3,"-")[[1]][2];d = str_split(date_input_3,"-")[[1]][3];y = str_split(date_input_3,"-")[[1]][1]
        url = paste0("https://www.basketball-reference.com/boxscores/index.fcgi?month=",m,"&day=",d,"&year=",y)
        page = read_html(url)
        all_links = page %>% html_nodes("a") %>% html_attr("href")
        links = unique(all_links[which(grepl("boxscores\\/\\d{0,10}[A-Za-z]{3}.html",all_links))])
        data.raw = page %>% html_table()
        matchups = data.frame(link = links,matchup = "")
        for (i in 1:length(links)){matchups$matchup[i] = paste0(data.raw[[3*i-2]]$X1[1]," vs. ",data.raw[[3*i-2]]$X1[2])}
        
        ilink = paste0("https://www.basketball-reference.com",matchups$link[which(matchups$matchup==matchup_input)])
        gl_df = team_dl(abb = team_map2(str_split(matchup_input," vs. ")[[1]][2]),ilink = ilink,opp_abb = team_map2(str_split(matchup_input," vs. ")[[1]][1]),period = period_input)
        gl_df = gl_df %>% mutate(across(-c("Player","Team"),as.double),X2P = FG-`3P`,X2PA = FGA-`3PA`) %>% group_by(Player,Team) %>% summarise(.groups = "drop",across(everything(),sum))
        
        if (T){
          #calc = gl_df %>% cbind.data.frame(lga %>% arrange(Year) %>% tail(1))
          calc = gl_df %>% mutate(Year = dts(date_input_3)) %>% inner_join(lga, by = "Year")
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
        
        gl_df %>% mutate(col = ifelse(Team==team_map2(str_split(matchup_input," vs. ")[[1]][2]),df$Hex[which(df$Team==team_map2(str_split(matchup_input," vs. ")[[1]][2]))[1]],"grey40")) %>% 
          ggplot(aes(x = `+/-`,y = valueAdd,label = Player, fill = col)) + 
          geom_hline(yintercept = 0,alpha = I(1/3)) + 
          geom_vline(xintercept = 0,alpha = I(1/3)) + 
          geom_label(alpha = I(0.5), size = 3.25) +
          theme_bw() + scale_fill_identity() + 
          scale_y_continuous("Value Added") +
          scale_x_continuous(limits = c(min(gl_df$`+/-`)-((abs(min(gl_df$`+/-`)))/5),max(max(gl_df$`+/-`)+((abs(max(gl_df$`+/-`))/5)),0)))
        
      }
    })
  })
  
  # # Table 7: Career Summary Statistics
  # output$table7 = renderDT({
  #   p1_i = p1_i();p2_i = p2_i()
  #   stat_input_3 = stat_input_3()
  #   reg_playoff_2 = reg_playoff_2()
  #   pg_factor_2 = pg_factor_2()
  #   if (reg_playoff_2 == "Playoffs"){
  #     today_file = paste0("Complete Data/Totals_p_",Sys.Date(),".csv",collapse = "")
  #     df_ = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
  #     gpl_df = df_ %>% group_by(Year) %>% summarize(.groups = "drop",gpl = ifelse(max(G) < 29,0.5*max(G),0.75*max(G)))
  #     df_ = df_ %>% inner_join(gpl_df,by = join_by(Year))
  #     df_1 = df_ %>% filter(G > (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df_2 = df_ %>% filter(G <= (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df = df_1 %>% rbind.data.frame(df_2)
  #     df$Player = iconv(df$Player, to = "UTF-8");maxYr = max(df$Year)
  #   }
  #   if (pg_factor_2){
  #     summary = df %>% filter(Player %in% c(p1_i,p2_i)) %>% group_by(Player) %>% summarize(
  #       PTS = sum(PTS)/sum(G)
  #       ,TRB = sum(TRB)/sum(G)
  #       ,AST = sum(AST)/sum(G)
  #       ,STL = sum(STL)/sum(G)
  #       ,BLK = sum(BLK)/sum(G)
  #       ,`FG%` = 100*(sum(FG)/sum(FGA))
  #       ,`3P%` = 100*(sum(X3P)/sum(X3PA))
  #       # ,FGA = sum(FGA)/sum(G)
  #     )
  #   } else{
  #     summary = df %>% filter(Player %in% c(p1_i,p2_i)) %>% group_by(Player) %>% summarize(
  #       PTS = sum(PTS)
  #       ,TRB = sum(TRB)
  #       ,AST = sum(AST)
  #       ,STL = sum(STL)
  #       ,BLK = sum(BLK)
  #       ,`FG%` = 100*(sum(FG)/sum(FGA))
  #       ,`3P%` = 100*(sum(X3P)/sum(X3PA))
  #     )
  #   }
  #   
  #   sum_df = df %>% filter(Player %in% c(p1_i,p2_i)) %>% group_by(Player) %>% summarize(
  #     G = sum(G), VA = sum(valueAdd)
  #   ) %>% arrange(desc(VA)) %>% select(-VA) %>% inner_join(summary,by = join_by(Player)) %>% 
  #     mutate(across(where(is.double),~sprintf("%.1f",.x))) 
  #   sum_df %>% 
  #     datatable(options = list(dom = 't', paging = FALSE, searching = FALSE)) %>%
  #     formatStyle("PTS",
  #                 backgroundColor = styleEqual(max(sum_df$PTS), "gold")) %>%
  #     formatStyle("TRB",
  #                 backgroundColor = styleEqual(max(sum_df$TRB), "gold")) %>%
  #     formatStyle("AST",
  #                 backgroundColor = styleEqual(max(sum_df$AST), "gold")) %>%
  #     formatStyle("STL",
  #                 backgroundColor = styleEqual(max(sum_df$STL), "gold")) %>%
  #     formatStyle("BLK",
  #                 backgroundColor = styleEqual(max(sum_df$BLK), "gold")) %>%
  #     formatStyle("FG%",
  #                 backgroundColor = styleEqual(max(sum_df$`FG%`), "gold")) %>%
  #     formatStyle("3P%",
  #                 backgroundColor = styleEqual(max(sum_df$`3P%`), "gold"))
  #   
  #   
  #   
  # })
  # 
  # Table 8: Year-by-Year Statistics
  output$table8 = renderDT({
    p1_i = p1_i();p2_i = p2_i();stat_input = stat_input_3();reg_playoff_2 = reg_playoff_2();pg_factor = pg_factor_2()
    
    if (reg_playoff_2 == "Playoffs"){
      today_file = paste0("Complete Data/Totals_p_",Sys.Date(),".csv",collapse = "")
      df_ = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team")
      gpl_df = df_ %>% group_by(Year) %>% summarize(.groups = "drop",gpl = ifelse(max(G) < 29,0.5*max(G),0.75*max(G)))
      df_ = df_ %>% inner_join(gpl_df,by = join_by(Year))
      df_1 = df_ %>% filter(G > (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df_2 = df_ %>% filter(G <= (1/3)*(gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd/G));df = df_1 %>% rbind.data.frame(df_2)
      df$Player = iconv(df$Player, to = "UTF-8");maxYr = max(df$Year)
    }
    player_df = df %>% filter(Player %in% c(p1_i,p2_i))
    stat_col = menu_map(stat_input)
    
    p_static = player_df[,c("Player","Team", "Year", "Hex","G",stat_col)]
    names(p_static)[ncol(p_static)] = "Stat"
    p1_ = p_static %>% filter(Player == p1_i) %>% as_tibble()
    p2_ = p_static %>% filter(Player == p2_i) %>% as_tibble()
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
      p_static$Rk_n[i] = which(temp2$Player == p_static$Player[i])
      p_static$Rk[i] = add_suffix(p_static$Rk_n[i])
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
    
    p1_static = p_static %>% filter(Player==str_split(p1_i," \\(")[[1]][1])
    p2_static = p_static %>% filter(Player==str_split(p2_i," \\(")[[1]][1])
    maxRow = max(nrow(p1_static),nrow(p2_static))
    p1_static = p1_static %>% rbind.data.frame(setNames(data.frame(matrix(nrow = maxRow-nrow(p1_static),ncol = ncol(p1_static))),nm = names(p1_static)))
    p2_static = p2_static %>% rbind.data.frame(setNames(data.frame(matrix(nrow = maxRow-nrow(p2_static),ncol = ncol(p2_static))),nm = names(p2_static)))
    p1_static = p1_static[, rev(seq_along(p1_static))]
    p_static = p1_static %>% data.frame("") %>% data.frame(p2_static)
    
    names(p_static)[4:14] = c(" FG | 3P | FT"," PTS | TRB | AST | STK"," Team"," Year (G)",str_split(p1_i," \\(")[[1]][1],
                              " ", #stat_input?
                              str_split(p2_i," \\(")[[1]][1],rev(c("FG | 3P | FT ","PTS | TRB | AST | STK ","Team ","Year (G) ")))
    
    p_static %>%
      datatable(options = 
                  list(dom = 't', paging = FALSE, searching = FALSE,
                       pageLength = 50,
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
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
