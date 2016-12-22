library(tidyr)
library(ggplot2)
 countYearsPerTeam = function(data){
   data %>% 
     group_by( team_abbr, year ) %>%
     summarize( n() ) %>% 
     group_by( team_abbr )   %>%
     summarize( count = n() )   
 }

 
 findMissingSeasons = function(data, team_years){
   #manually checked that all years are present for the expansion teams
   d = data %>% group_by( team_abbr, year ) %>% 
     filter ( team_abbr != "rav") %>%
     filter ( team_abbr != "cle") %>%
     filter ( team_abbr != "jax") %>%
     filter ( team_abbr != "car") %>%
     filter ( team_abbr != "htx") %>%
     filter ( team_abbr != "sea") %>%
     summarize( count = n() ) %>%
     unite(team_year ,team_abbr, year) %>% 
     select( team_year)

     
  ty = team_years %>% 
    
    filter ( team_abbr != "rav") %>%
    filter ( team_abbr != "cle") %>%
    filter ( team_abbr != "jax") %>%
    filter ( team_abbr != "car") %>%
    filter ( team_abbr != "htx") %>%
    filter ( team_abbr != "sea") %>%
    filter ( team_abbr != "tam") %>%
    unite( team_year, team_abbr, year) %>% select( team_year)
    
  setdiff ( ty$team_year, d$team_year )
 }
 

  getNumStartingQbs =  function(data){
   d = data %>%
     filter( Pos == "QB" | Pos == "qb") %>%
     filter ( GS > 0 ) %>%
     filter ( !is.na(GS)) %>%
     group_by( team_abbr, year ) %>%
     select( Player, team_abbr, year , GS) %>%
     summarize( num_starting_qbs = n() ) %>%
     ungroup() %>%
     arrange(num_starting_qbs ) 
   }


  dd = d %>%
    group_by( team_abbr ) %>%
    summarize( avg_starting_qbs =  mean(num_starting_qbs ))
  dd$team_abbr =  factor(dd$team_abbr, levels = dd$team_abbr[order(dd$avg_starting_qbs)])
  ggplot( 
    dd
    ,
    aes( x = team_abbr, y = avg_starting_qbs )
  ) + geom_bar(stat='identity') +
    coord_flip() +
    theme(
      axis.text.y=element_text(size=14,face="bold"),
      axis.title.x=element_text(size=14,face="bold"),
      axis.title.y = element_blank()
    )  +
   xlab("") + 
    ylab("Starting QBs per season ( avg 1975 - present ) " )