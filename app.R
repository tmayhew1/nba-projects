library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes); library(plotly); library(gridExtra); library(DT); library(scales)
source("totals_collect.R") # totals_collect.R must be run!
library(shiny)
df = read.csv(today_file)[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team") %>% arrange(desc(valueAdd/G))
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
          reg_games_1 = data.raw[[8]] %>% select(G, Date, Tm, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc)
          reg_games_1 = reg_games_1 %>% mutate(PlusMinus = 0)
          
        } else{
          reg_games_1 = data.raw[[8]] %>% 
            select(G, Date, Tm, MP, FG, FGA, `3P`, `3PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS, GmSc, `+/-`)
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

# Define UI for application
ui = tabsetPanel(type = "tabs",
                tabPanel("Player Comparison",
                  fluidPage(
                    titlePanel(h1("Active NBA Player Comparison", style = "font-size: 18px;")),
                    mainPanel(
                      width = 12,
                      column(
                        width = 6,
                        fluidRow(
                          column(6, selectInput("p1_input","Player 1:",choices = df$Player[which(df$Year=="2024-2025")],selected = df$Player[which(df$Year=="2024-2025")][1]), style = "font-size: 12px;"),
                          column(6, selectInput("p2_input","Player 2:",choices = c("-",df$Player[which(df$Year=="2024-2025")]),selected = "-"), style = "font-size: 12px;"),
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
                      # ,tableOutput("table2")
                      # ,tableOutput("table3")
                    ) 
                  )
                ),
                # tabPanel("Leaderboard",
                #    fluidPage(
                #      titlePanel(h1("NBA Season Leaderboard", style = "font-size: 18px;")),
                #      mainPanel(
                #       # Select Inputs
                #        column(6, selectInput("year_input","Season:",choices = rev(unique(df$Year)),selected = "2024-2025"), style = "font-size: 12px;"),
                #         # SOI
                #         # Per Game vs. Total
                #         # (optional) Player
                #      )
                #    )
                # )
                
)

# Define server logic
server <- function(input, output) {
  # Start data collect
  p1_df = reactive({glsearch(player = input$p1_input,years = c(format(Sys.Date(), "%Y") %>% as.integer()-1,format(Sys.Date(), "%Y") %>% as.integer()+1))})
  p2_df = reactive({glsearch(player = input$p2_input,years = c(format(Sys.Date(), "%Y") %>% as.integer()-1,format(Sys.Date(), "%Y") %>% as.integer()+1))})
  roll_avg_input = reactive({input$roll_avg_input})
  stat_input = reactive({input$stat_input})
  date_input = reactive({input$date_input})
  
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
      cdf %>% group_by(Player) %>% summarise(.groups = "drop", G = sprintf("%.0f",n()), PTS = sprintf("%.1f",mean(PTS)), TRB = sprintf("%.1f",mean(TRB)), AST = sprintf("%.1f",mean(AST)), STK = sprintf("%.1f",mean(STL+BLK)), `FG%` = sprintf("%.1f",100*(sum(FG)/sum(FGA))), `3P%` = sprintf("%.1f",100*(sum(X3P)/sum(X3PA))), FGA = sprintf("%.1f",mean(FGA))) %>% 
        datatable(
          options = 
            list(dom = 't', # Only show the table, without additional interface elements
                 paging = FALSE, # Disable pagination
                 searching = FALSE # Disable the search box 
            )
        )
      
    } else{
      cdf %>% group_by(Player) %>% summarise(.groups = "drop", G = sprintf("%.0f",n()), PTS = sprintf("%.1f",mean(PTS)), TRB = sprintf("%.1f",mean(TRB)), AST = sprintf("%.1f",mean(AST)), STK = sprintf("%.1f",mean(STL+BLK)), `FG%` = sprintf("%.1f",100*(sum(FG)/sum(FGA))), `3P%` = sprintf("%.1f",100*(sum(X3P)/sum(X3PA))), FGA = sprintf("%.1f",mean(FGA)), vapg_sort = mean(valueAdd)) %>% arrange(desc(vapg_sort)) %>% select(-vapg_sort) %>% 
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
    )
    top_color = cdf %>% filter(Player == p1_df$Player[1]) %>% head(1)
    
    # modify Stat columns if stat_input is a percent!
    if (grepl("[P|p]ercentage",stat_input)){
      static = cdf[,c("Player","Tm", "G", "Date", str_split(stat_col,"\\.")[[1]][1] %>% paste0(""), str_split(stat_col,"\\.")[[1]][1] %>% paste0("A"))]
      names(static)[(ncol(static)-1):ncol(static)] = c("Make", "Att")
      static$Stat = static$Make/static$Att
      # if (ra==1){
      #   static$Stat_ra = static$Stat
      # } else{
      #   static$Stat_ra = NA
      #   for (i in 1:nrow(static)){
      #     if (static$G[i]<ra){
      #       static$Stat_ra[i] = NA
      #     } else{
      #       static$Stat_ra[i] = (sum(static$Make[(i-ra+1):(i)]))/((sum(static$Att[(i-ra+1):(i)])))
      #     }
      #   }
      # }
    } else{
      static = cdf[,c("Player","Tm", "G", "Date", stat_col)]
      names(static)[ncol(static)] = "Stat"
      static = static %>% mutate(Stat = as.double(Stat))
      # if (ra==1){
      #   static$Stat_ra = static$Stat
      # } else{
      #   static$Stat_ra = NA
      #   for (i in 1:nrow(static)){
      #     if (static$G[i]<ra){
      #       static$Stat_ra[i] = NA
      #     } else{
      #       static$Stat_ra[i] = mean(static$Stat[(i-ra+1):(i)])
      #     }
      #   }
      # }
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
  # 
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
  #
}

# Run the application 
shinyApp(ui = ui, server = server)
