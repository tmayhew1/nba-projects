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
                                         column(6, selectInput("p1_input","Player 1:",choices = df$Player[which(df$Year==maxYr)],selected = df$Player[which(df$Year==maxYr)][1]), style = "font-size: 12px;"),
                                         column(6, selectInput("p2_input","Player 2:",choices = c("-",df$Player[which(df$Year==maxYr)]),selected = "-"), style = "font-size: 12px;"),
                                       ),
                                       fluidRow(
                                         column(4, selectInput("stat_input", "Statistic of Interest:", choices = rev(read.csv("Complete Data/menu_options.csv")[, ncol(read.csv("Complete Data/menu_options.csv"))]), selected = "Value Added"), style = "font-size: 12px;")
                                         ,column(3, numericInput("roll_avg_input", "Rolling Average:", value = 10, min = 1, step = 1), style = "font-size: 12px;")
                                         ,column(5, selectInput("date_input", "Since:", choices = c("Past year (365 days)", paste0("Start of this NBA season (Oct. ", str_split(max(df$Year), pattern = "-")[[1]][1], ")"), "Past month (30 days)"), selected = paste0("Start of this NBA season (Oct. ", str_split(max(df$Year), pattern = "-")[[1]][1], ")")), style = "font-size: 12px;")
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
                                         ,column(3, selectInput("stat_input_2", "Statistic of Interest:", choices = rev(read.csv("Complete Data/menu_options_2.csv")[, ncol(read.csv("Complete Data/menu_options_2.csv"))]), selected = "Value Added"))
                                         ,column(3, selectizeInput("player_input", "Player (optional):",choices = NULL,selected = ""))
                                         ,column(3,
                                                 switchInput("pg_factor", "Per game?", value = TRUE),
                                                 textOutput("toggle_status")
                                         )
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
                                   titlePanel(h1("Single Game Search", style = "font-size: 18px;")),
                                   mainPanel(
                                     width = 12
                                     ,column(
                                       width = 12,
                                       fluidRow(
                                         column(2, selectInput("year_input_2","Season:",choices = rev(unique(sort(df$Year))),selected = maxYr))
                                         ,column(2, selectInput("team_input", "Team:", choices = sort((read.csv("Complete Data/team_abbreviations.csv") %>% filter(modern==1))[,3]), selected = "Los Angeles Lakers"))
                                         ,column(3, selectizeInput("date_input_2", "Date:",choices = NULL,selected = ""))
                                         ,column(3, selectizeInput("player_input_2", "Player (optional):",choices = NULL,selected = ""))
                                         ,br(),column(2, actionButton("run","Load/Reload Graphs", class = "btn-lg")),
                                         )
                                       )
                                       ,fluidRow(
                                         column(width = 8, DTOutput("table5"))
                                         ,column(4, plotOutput("plot5"))
                                       )
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
  ###########################################
  year_input_2 = reactive({input$year_input_2})
  team_input = reactive({input$team_input})
  date_input_2 = reactive({input$date_input_2})
  player_input_2 = reactive({input$player_input_2})
  
  # Table 2: Summary Statistics
  output$table2 = renderDT({
    p1_df = p1_df();p2_df = p2_df();date_input = date_input()
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
    top_color = cdf %>% filter(Player == p1_df$Player[1]) %>% head(1)
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
                         ,tenPTS = ifelse(PTS>9,1,0),tenTRB = ifelse(TRB>9,1,0),tenAST = ifelse(AST>9,1,0),tenSTL = ifelse(STL>9,1,0),tenBLK = ifelse(BLK>9,1,0)) %>% 
      mutate(sum10s = tenPTS+tenTRB+tenAST+tenSTL+tenBLK) %>% 
      mutate(fPTS2 = (.5*PTS) + (TRB) + (AST) + (2*(STL)) + (2*(BLK)) + (-1*(TOV)) + (.5*X3P) +
               ifelse(sum10s > 1,1,0) + # double-double bonus
               ifelse(sum10s > 2,2,0) + # triple-double bonus
               ifelse(PTS > 39,2,0) + # 40+ points bonus
               ifelse(PTS > 49,2,0) # 50+ points bonus
      )
    top_color = cdf %>% filter(Player == p1_df$Player[1]) %>% head(1)
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
                         ,tenPTS = ifelse(PTS>9,1,0),tenTRB = ifelse(TRB>9,1,0),tenAST = ifelse(AST>9,1,0),tenSTL = ifelse(STL>9,1,0),tenBLK = ifelse(BLK>9,1,0)) %>% 
      mutate(sum10s = tenPTS+tenTRB+tenAST+tenSTL+tenBLK) %>% 
      mutate(fPTS2 = (.5*PTS) + (TRB) + (AST) + (2*(STL)) + (2*(BLK)) + (-1*(TOV)) + (.5*X3P) +
               ifelse(sum10s > 1,1,0) + # double-double bonus
               ifelse(sum10s > 2,2,0) + # triple-double bonus
               ifelse(PTS > 39,2,0) + # 40+ points bonus
               ifelse(PTS > 49,2,0) # 50+ points bonus
      )
    top_color = cdf %>% filter(Player == p1_df$Player[1]) %>% head(1)
    
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
    
    for (j in 1:10000){sim_p1 = c(sim_p1,mean(s1[sample(length(s1),size = 5,replace = F)]))}
    if (nrow(p2_df)==0){
      # if p2_df is empty, then treat data like we're only plotting one player (because we are!)
      sim_p2 = c()
      sims = data.frame(sim = sim_p1,Player = p1_df$Player[1])
      # add a sample size (games played) for context
      sims = sims %>% mutate(Player = paste0(Player," (n=",(length(static$Stat[which(static$Player==p1_df$Player[1])])),")"))
    } else{
      for (k in 1:10000){sim_p2 = c(sim_p2,mean(s2[sample(length(s2),size = 5,replace = F)]))}
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
    cdf = cdf %>% separate(Date, into = c("Year", "m", "d"), remove=F) %>% select(-m, -d) %>% inner_join(lga, by = "Year")
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
    top_color = cdf %>% filter(Player == p1_df$Player[1]) %>% head(1)
    
    if (nrow(p2_df)==0){
      cdf %>% arrange(desc(valueAdd)) %>% transmute(Player, Date = format.Date(Date, "%y-%m-%d"), PTS, TRB, AST, BLK, STL, `3P` = paste0(X3P,"/",X3PA), `2P` = paste0(X2P,"/",X2PA), FT = paste0(FT,"/",FTA), VA = sprintf("%.2f",valueAdd)) %>% 
        datatable(options = list(pageLength = 25))
      
    } else{
      cdf %>% arrange(desc(valueAdd)) %>% transmute(Player, Date = format.Date(Date, "%y-%m-%d"), PTS, TRB, AST, BLK, STL, `3P` = paste0(X3P,"/",X3PA), `2P` = paste0(X2P,"/",X2PA), FT = paste0(FT,"/",FTA), VA = sprintf("%.2f",valueAdd)) %>% 
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
    year_input = year_input();stat_input_2 = stat_input_2();pg_factor = pg_factor();player_input = player_input()
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
        ggtitle(label = "", subtitle = ifelse(pg_factor,paste0(year_input, " Season Leaders (min. ",(floor(min_games))," game(s))"),paste0(year_input, " Season Leaders ")))
    } else{
      plot = output %>% ggplot(aes(x = rk, y = Stat, fill = Hex)) +
        geom_bar(stat = "identity", color = "black", aes(fill = Hex), alpha = I(3/5)) +
        theme_bw() + coord_flip() + scale_fill_identity() + theme(legend.position = "none") +
        scale_y_continuous(name = paste0(stat_input_2,ifelse(pg_factor," (Per Game) "," (Total) ")),
                           limits = c(0,max(output$Stat)+((max(output$Stat)/9.5)))) + 
        scale_x_discrete(name = "") +
        geom_text(aes(fontface = "bold",label = disPlayer), hjust = 1, size = I(2.25)) +
        geom_text(aes(label = display_stat), hjust = 0, size = I(2.25)) +
        ggtitle(label = "", subtitle = ifelse(pg_factor,paste0(year_input, " Season Leaders (min. ",(floor(min_games))," game(s))"),paste0(year_input, " Season Leaders ")))
    }
    
    plot
    
  })
  
  # Table 3: Leaders Summary Statistics
  output$table3 = renderDT({
    year_input = year_input();stat_input_2 = stat_input_2();pg_factor = pg_factor();player_input = player_input()
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
                    pageLength = 11 # Set the default number of rows
                  )
        ) %>%
        formatStyle(
          'Player',
          target = 'row',
          backgroundColor = styleEqual(input$player_input, "lightgreen")
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
                    pageLength = 11 # Set the default number of rows
                    )
                  ) %>% 
        formatStyle(
          'Player',
          target = 'row',
          backgroundColor = styleEqual(input$player_input, "lightgreen")
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
      gl_df = gl_df %>% inner_join(calc %>% select(Player,X3PAdd,X2PAdd,FTAdd,valueAdd,fPTS),
                                   by = join_by(Player)) %>% 
        arrange(desc(valueAdd)) %>% select(Player, Team, MP, valueAdd,everything())
      
      datatable(gl_df %>% transmute(Player, Team, MP = round(MP,2), PTS, TRB, AST, BLK, STL, TOV, FG = paste0(FG,"/",FGA), `3P` = paste0(`3P`,"/",`3PA`), VA = round(valueAdd,2), `+/-`)) %>% 
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
        )
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
          mutate(abs = (value),col_n = ifelse(value > 0,lighten_color(df$Hex[which(df$Team==team_map(team_input))[1]],.25),"lightgrey")) %>% arrange(desc(abs))
        sp_output$key = factor(sp_output$key,levels = rev(sp_output$key))
        sp_output %>% ggplot(aes(x = key, y = abs, fill = col_n)) + geom_bar(color = "black",stat="identity",width=I(1/2),alpha = I(.8)) +
          theme_bw() + geom_hline(yintercept = 0) + coord_flip() +
          scale_y_continuous("Value Added") + scale_x_discrete("") + 
          scale_fill_manual(values = unique(c(sp_output$col_n))) + 
          theme(legend.position = "none") + ggtitle(player_input_2,subtitle = date_choice)
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
