library(tidyverse); library(httr); library(XML); library(rvest); library(ggplot2); library(ggthemes); library(data.table)
source("totals_collect.R") # totals_collect.R must be run!
df = read.csv("Complete Data/allstar_analysis.csv")[,-1] %>% as_tibble() %>% inner_join(read.csv("Complete Data/team_hex_colors.csv")[,-1], by = "Team") %>% filter(G > 9)
  as = df %>% filter(allstar==1)
#gpl_df = df %>% group_by(Year) %>% summarise(.groups = "drop", max_G = max(G)) %>% mutate(gpl = ifelse(max_G > 50,.75*max_G,.6*max_G)) %>% select(-max_G)
min(as$G);max(df$G) # Minimum games played by an all star is 32 (out of possible 45) 
                    #-- assume players must play 30 or more.

df = df %>% mutate(allstar = factor(ifelse(G < 30,"DNQ",allstar)))

# Top DNQs
df %>% filter(allstar == "DNQ") %>% arrange(desc(valueAdd)) %>% head(10)
df %>% filter(allstar == "DNQ") %>% arrange(desc(valueAdd/G)) %>% head(10)

# All Players
df %>% arrange(desc(valueAdd/G)) %>% transmute(Player,Team,G,Pos,MP = round(MP/G,2),PTS = round(PTS/G,2), TRB = round(TRB/G,2), AST = round(AST/G,2), STK = round((STL+BLK)/G,2), X3P. = round((X3P/X3PA),3), X3PA = round(X3PA/G,2), FG. = round(FG/FGA,3), FGA = round(FGA/G,2), round(valueAdd/G,3), allstar) %>% 
  write.csv("Complete Data/allstar2025_results.csv")

