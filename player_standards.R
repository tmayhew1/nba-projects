library(tidyverse);library(lubridate);library(httr);library(XML);library(rvest);library(ggplot2);library(ggthemes);library(plotly);library(gridExtra);library(DT);library(scales);library(shinyWidgets);library(shiny)
source("totals_collect.R") # totals_collect.R must be run!
today_file = paste0("Complete Data/Totals_s_", Sys.Date(), ".csv", collapse = "")
df_ = read.csv(today_file)[, -1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[, -1], by = "Team")
gpl_df = df_ %>% group_by(Year) %>% summarize(.groups = "drop",
                                              gpl = ifelse(max(G) < 29, 0.5 * max(G), 0.75 * max(G)))
df_ = df_ %>% inner_join(gpl_df, by = join_by(Year))
df_1 = df_ %>% filter(G > (1 / 3) * (gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd /
                                                                               G))
df_2 = df_ %>% filter(G <= (1 / 3) * (gpl)) %>% select(-gpl) %>% arrange(desc(valueAdd /
                                                                                G))
df = df_1 %>% rbind.data.frame(df_2)
df$Player = iconv(df$Player, to = "UTF-8")
maxYr = max(df$Year)
lga = read.csv("Complete Data/avgsSummary.csv")[, -1] %>% as_tibble()
#lga = read.csv("Complete Data/avgsSummary.csv")[,-1] %>% separate(Year, into = c("pre", "Year"), sep = "\\-") %>% select(-pre) %>% select(Year, everything()) %>% as_tibble()

menu_map = function(input) {
  map = read.csv("Complete Data/menu_options.csv")[, -1]
  return(map$col_name[which(map$display_name == input)])
}
psearch = function(input) {
  new = df %>% filter(grepl(input, Player))
  return(new$Player %>% unique())
}
lsearch = function(player, year) {
  key = str_split(player, "\\(|\\)")[[1]][2]
  letter = str_split(key, "")[[1]][1]
  return(
    paste0(
      "https://www.basketball-reference.com/players/",
      letter,
      "/",
      key,
      "/gamelog/",
      year
    )
  )
}
glsearch = function(player, years) {
  player_df = data.frame()
  if (player != '-') {
    for (y in years[1]:years[2]) {
      url = lsearch(player, y)
      page = read_html(url)
      data.raw = html_table(page, fill = TRUE)
      if (length(data.raw) == 0) {
        #print("This year's page is empty:")
        #print(y)
      } else{
        if (length(data.raw) < 9) {
          data.raw[[9]] = data.frame(matrix(nrow = 0, ncol = ncol(data.raw[[8]]))) %>% set_names(nm = names(data.raw[[8]]))
        }
        
        if (length(which(names(data.raw[[8]]) == "+/-")) == 0) {
          if (ncol(data.raw[[8]]) == ncol(data.raw[[9]])) {
            reg_games_1 = data.raw[[8]] %>% rbind.data.frame(data.raw[[9]]) %>% select(
              G = Gtm,
              Date,
              Tm = Team,
              MP,
              FG,
              FGA,
              `3P`,
              `3PA`,
              FT,
              FTA,
              ORB,
              DRB,
              TRB,
              AST,
              STL,
              BLK,
              TOV,
              PF,
              PTS,
              GmSc
            )
            reg_games_1 = reg_games_1 %>% mutate(PlusMinus = 0)
          } else{
            reg_games_1 = data.raw[[8]] %>% select(
              G = Gtm,
              Date,
              Tm = Team,
              MP,
              FG,
              FGA,
              `3P`,
              `3PA`,
              FT,
              FTA,
              ORB,
              DRB,
              TRB,
              AST,
              STL,
              BLK,
              TOV,
              PF,
              PTS,
              GmSc
            )
            reg_games_1 = reg_games_1 %>% mutate(PlusMinus = 0)
          }
        } else{
          if (ncol(data.raw[[8]]) == ncol(data.raw[[9]])) {
            reg_games_1 = data.raw[[8]] %>% rbind.data.frame(data.raw[[9]]) %>% select(
              G = Gtm,
              Date,
              Tm = Team,
              MP,
              FG,
              FGA,
              `3P`,
              `3PA`,
              FT,
              FTA,
              ORB,
              DRB,
              TRB,
              AST,
              STL,
              BLK,
              TOV,
              PF,
              PTS,
              GmSc,
              `+/-`
            )
            reg_games_1 = reg_games_1 %>% set_names(
              nm = c(
                "G",
                "Date",
                "Tm",
                "MP",
                "FG",
                "FGA",
                "X3P",
                "X3PA",
                "FT",
                "FTA",
                "ORB",
                "DRB",
                "TRB",
                "AST",
                "STL",
                "BLK",
                "TOV",
                "PF",
                "PTS",
                "GmSc",
                "PlusMinus"
              )
            )
          } else{
            reg_games_1 = data.raw[[8]] %>% select(
              G = Gtm,
              Date,
              Tm = Team,
              MP,
              FG,
              FGA,
              `3P`,
              `3PA`,
              FT,
              FTA,
              ORB,
              DRB,
              TRB,
              AST,
              STL,
              BLK,
              TOV,
              PF,
              PTS,
              GmSc,
              `+/-`
            )
            reg_games_1 = reg_games_1 %>% set_names(
              nm = c(
                "G",
                "Date",
                "Tm",
                "MP",
                "FG",
                "FGA",
                "X3P",
                "X3PA",
                "FT",
                "FTA",
                "ORB",
                "DRB",
                "TRB",
                "AST",
                "STL",
                "BLK",
                "TOV",
                "PF",
                "PTS",
                "GmSc",
                "PlusMinus"
              )
            )
          }
        }
        reg_games_1 = reg_games_1 %>% filter(!is.na(as.double(FG))) %>% separate(col = MP,
                                                                                 into = c("MP", "SP"),
                                                                                 sep = "\\:") %>% mutate(MP = as.double(MP) + (as.double(SP) / 60)) %>% select(-SP) %>% data.frame(Player = player)
        player_df = player_df %>% rbind.data.frame(reg_games_1) %>% mutate(Date = as.Date(Date)) %>% as_tibble()
      }
    }
  }
  return(player_df)
}
lighten_color = function(color, factor = .25) {
  col_rgb <- col2rgb(color) / 255
  col_light <- (1 - factor) * col_rgb + factor * 1
  rgb(col_light[1], col_light[2], col_light[3], maxColorValue = 1)
}
team_map = function(input) {
  map = read.csv("Complete Data/team_abbreviations.csv")[, -1]
  return(map$abb[which(map$name == input)])
}
team_map2 = function(input) {
  map = read.csv("Complete Data/team_abbreviations.csv")[, -1]
  return(map$abb[which(map$city == input)])
}
team_gl = function(abb, year = "2025") {
  url = paste0("https://www.basketball-reference.com/teams/",
               abb,
               "/",
               year,
               "/gamelog/")
  page = read_html(url)
  data.raw = html_table(page)
  
  df = data.raw[[1]]
  df = df[2:nrow(df), c(3:5)]
  names(df) = c("Date", "at", "Opp")
  df = df %>% filter(!(Date %in% c("Date", "")))
  df = df %>% mutate(at = ifelse(at != "@", "vs.", "@"))
  df = df %>% separate(
    col = Date,
    into = c("year", "month", "day"),
    sep = "-",
    remove = F
  ) %>% mutate(link = ifelse(
    at == "@",
    paste0(
      "https://www.basketball-reference.com/boxscores/",
      year,
      month,
      day,
      "0",
      Opp,
      ".html"
    ),
    paste0(
      "https://www.basketball-reference.com/boxscores/",
      year,
      month,
      day,
      "0",
      abb,
      ".html"
    )
  )) %>% transmute(DateatOpp = paste0(Date, " (", at, " ", Opp, ")"), link)
  return(df %>% arrange(desc(DateatOpp)))
}
team_sg = function(abb, ilink, date_choice) {
  page = read_html(ilink)
  data.raw = html_table(page)
  opp_abb = strsplit(date_choice, split = ")| ")[[1]][length(strsplit(date_choice, split = ")| ")[[1]])]
  
  df1 = data.raw[[1]]
  names(df1) = df1[1, ]
  df1 = df1 %>% filter(grepl("\\:", MP)) %>% mutate(Team = ifelse(grepl(abb, ilink), opp_abb, abb))
  df2 = data.raw[[(.5 * length(data.raw)) + 1]]
  names(df2) = df2[1, ]
  df2 = df2 %>% filter(grepl("\\:", MP)) %>% mutate(Team = ifelse(grepl(abb, ilink), abb, opp_abb))
  
  return_df = df1 %>% rbind.data.frame(df2)
  return_df = return_df %>% separate(col = MP,
                                     into = c("MP", "SP"),
                                     sep = "\\:") %>% mutate(MP = as.double(MP) + (as.double(SP) / 60)) %>% select(-SP, -`FG%`, -`3P%`, -`FT%`) %>% select(Player = Starters, Team, everything())
  return(return_df)
}
team_dl = function(abb, ilink, opp_abb, period = "Game") {
  page = read_html(ilink)
  data.raw = html_table(page)
  if (length(data.raw) == 16) {
    prs = c(
      "Game",
      "1st Quarter",
      "2nd Quarter",
      "1st Half",
      "3rd Quarter",
      "4th Quarter",
      "2nd Half",
      "Adv. Totals"
    )
  } else{
    prs = c(
      "Game",
      "1st Quarter",
      "2nd Quarter",
      "1st Half",
      "3rd Quarter",
      "4th Quarter",
      "2nd Half",
      c(paste0("OT", 1:((
        length(html_table(read_html(ilink))) - 16
      ) / 2))),
      "Adv. Totals"
    )
  }
  otf_mapping = data.frame(prs = prs, abb = opp_abb) %>% rbind.data.frame(data.frame(prs = prs, abb = abb)) %>% mutate(entry = 1:length(data.raw))
  df1 = data.frame()
  df2 = data.frame()
  clear = ifelse(period == "OT" &
                   all(!(grepl(
                     "OT", otf_mapping$prs
                   ))), T, F)
  if (clear) {
    for (i in which(grepl("Game", otf_mapping$prs) &
                    otf_mapping$abb == opp_abb)) {
      df1_ = data.raw[[i]]
      names(df1_) = df1_[1, ]
      df1_ = df1_ %>% filter(grepl("\\:", MP)) %>% mutate(Team = opp_abb)
      df1 = rbind.data.frame(df1, df1_)
    }
    for (j in which(grepl("Game", otf_mapping$prs) &
                    otf_mapping$abb == abb)) {
      df2_ = data.raw[[j]]
      names(df2_) = df2_[1, ]
      df2_ = df2_ %>% filter(grepl("\\:", MP)) %>% mutate(Team = abb)
      df2 = rbind.data.frame(df2, df2_)
    }
    return_df = df1 %>% rbind.data.frame(df2) %>% mutate(across(-c("Starters", "Team"), zero))
    return_df = return_df %>% select(-`FG%`, -`3P%`, -`FT%`) %>% select(Player = Starters, Team, everything())
  } else{
    for (i in which(grepl(period, otf_mapping$prs) &
                    otf_mapping$abb == opp_abb)) {
      df1_ = data.raw[[i]]
      names(df1_) = df1_[1, ]
      df1_ = df1_ %>% filter(grepl("\\:", MP)) %>% mutate(Team = opp_abb)
      df1 = rbind.data.frame(df1, df1_)
    }
    for (j in which(grepl(period, otf_mapping$prs) &
                    otf_mapping$abb == abb)) {
      df2_ = data.raw[[j]]
      names(df2_) = df2_[1, ]
      df2_ = df2_ %>% filter(grepl("\\:", MP)) %>% mutate(Team = abb)
      df2 = rbind.data.frame(df2, df2_)
    }
    return_df = df1 %>% rbind.data.frame(df2)
    return_df = return_df %>% separate(col = MP,
                                       into = c("MP", "SP"),
                                       sep = "\\:") %>% mutate(MP = as.double(MP) + (as.double(SP) / 60)) %>% select(-SP, -`FG%`, -`3P%`, -`FT%`) %>% select(Player = Starters, Team, everything())
  }
  return(return_df)
}
daily_l = function(ilink_dl) {
  page = read_html(ilink_dl)
  data.raw = html_table(page)
  ret_df = data.raw[[1]][, -c(which(names(data.raw[[1]]) == "" |
                                      grepl("\\%", names(data.raw[[1]]))))]
  return(ret_df %>% filter(Player != "Player"))
}
add_suffix <- function(x) {
  x = as.integer(x)
  suffix <- ifelse(x %% 100 %in% 11:13, "th", ifelse(x %% 10 == 1, "st", ifelse(
    x %% 10 == 2, "nd", ifelse(x %% 10 == 3, "rd", "th")
  )))
  paste0(x, suffix)
}
zero = function(input) {
  return(0)
}
dts = function(date) {
  ifelse(month(date) >= 9,
         paste0(year(date), "-", year(date) + 1),
         paste0(year(date) - 1, "-", year(date)))
}

closest_to_standard <- function(x, standards = NULL) {
  # Remove NA and sort input
  x <- sort(na.omit(x))
  
  # Define default standards if not provided
  if (is.null(standards)) {
    standards <- c(seq(-15, 20, by = 5), min(x), max(x))
    standards <- sort(unique(standards))  # Ensure uniqueness
  }
  
  # For each standard, find the closest value in x
  closest <- vapply(standards, function(s) {
    x[which.min(abs(x - s))]
  }, numeric(1))
  
  names(closest) <- standards
  return(closest)
}

df = df %>% filter(G > 65,Age>23,Age<30) %>% mutate(valueAddpG = valueAdd/G)
data.frame(standards = c(-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,30,35,40),
                  valueAddpG = closest_to_standard(df$valueAddpG,standards = c(-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,30,35,40))) %>% 
  inner_join(df %>% transmute(valueAddpG,Year,Player,Age,Team,G,PTS/G,TRB/G,AST/G,`STK/G`=(STL+BLK)/G)) %>% arrange(desc(valueAddpG)) %>% datatable()