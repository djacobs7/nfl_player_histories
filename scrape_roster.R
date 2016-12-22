#For scraping rosters

#"//widgets.sports-reference.com/wg.fcgi?css=1&site=pfr&url=%2Fteams%2Fcle%2F2015_roster.htm&div=div_games_played_team

library(rvest)
library(dplyr)

#To Scrape from the the player roster, year after year
#http://www.pro-football-reference.com/teams/phi/2014_roster.htm

OUTPUT_ROOT = "~/git/nfl_player_vis/data/rosters/"


years = 1975:2016
team_abbrs = c( 'nwe', 'mia', 'buf', 'nyj', 'pit', 'rav', 'cin', 'cle', 'htx', 'oti', 'clt', 'jax', 'rai', 'kan', 'den', 'sdg', 'dal', 'nyg', 'was', 'phi', 'det', 'gnb', 'min', 'chi', 'atl', 'tam', 'car', 'nor', 'sea', 'crd', 'ram', 'sfo')
team_years = expand.grid( years, team_abbrs )
names( team_years ) = c('year', 'team_abbr')
team_years = team_years %>% mutate( url = paste0( 
  "http://widgets.sports-reference.com/wg.fcgi?css=1&site=pfr&url=%2Fteams%2F", team_abbr ,"%2F" , year , "_roster.htm&div=div_games_played_team"
))


read_page_and_write_csv <- function( vec ){
  team_abbr = vec$team_abbr
  year = vec$year
  url = vec$url
  roster_page = read_html( url )
  roster_table = roster_page %>% 
    html_table() %>%
    write.csv(file = paste0( OUTPUT_ROOT, team_abbr, "_", year, ".csv")) 
}

read_all_files<-function() {
  num_years = count(team_years)$n
  for ( idx in 1:(num_years)){
    tryCatch( {
      read_page_and_write_csv( team_years[ idx, ])
    }, error = function(e){
      print("THERE WAS AN ERROR")
      print(e)
      print(idx)
    })
  }
}

read_all_files()






l = data.frame()
num_years = count(team_years)$n

for ( idx in 1:num_years){
  row = team_years[ idx, ]
  team_abbr = row$team_abbr
  year = row$year
  
  filename = paste0( OUTPUT_ROOT, team_abbr, "_", year, ".csv")
  
  
  
  tryCatch( {
    df = read.csv( filename ) %>%
      mutate( team_abbr = team_abbr, year = year ) 
    
    
    if ( "Player." %in% names(df)) {
      df = df %>% rename( Player = Player.)
    }
    if ( "Cap.Hit" %in% names(df)) {
      
    } else {
      df = df %>% mutate( Cap.Hit = NA)
    }
    
    
    l = rbind(l , df )      
  }, error = function(e){
    print("")
    print(e)
    print(paste(team_abbr))
    print(year)
  })
  
}




l2 = l %>%
  separate("Drafted..tm.rnd.yr.", c ("Draft1", "Draft2"), sep =",", fill = "right") %>%
  separate( "Draft1", c("Draft.Team", "Draft.Round", "Draft.Pick", "Draft.year"), remove = FALSE, sep = " / ", fill = "right") %>%
  separate( "Draft2", c("Draft.Team2", "Draft.Round2", "Draft.Pick2", "Draft.year2"), remove = FALSE, sep = " / ", fill = "right") 
    
write.csv(l2, file = paste0( OUTPUT_ROOT,"all_rosters.csv")) 

l = read.csv(paste0( OUTPUT_ROOT,"all_rosters.csv") )
