library(rvest)
library(dplyr)

#To Scrape from the the player roster, year after year
#http://www.pro-football-reference.com/teams/phi/2014_roster.htm

#roster_page = read_html( "http://www.pro-football-reference.com/teams/phi/2014_roster.htm" )

OUTPUT_ROOT = "~/git/nfl_player_vis/data/starters/"


years = 1975:2016
team_abbrs = c( 'nwe', 'mia', 'buf', 'nyj', 'pit', 'rav', 'cin', 'cle', 'htx', 'oti', 'clt', 'jax', 'rai', 'kan', 'den', 'sdg', 'dal', 'nyg', 'was', 'phi', 'det', 'gnb', 'min', 'chi', 'atl', 'tam', 'car', 'nor', 'sea', 'crd', 'ram', 'sfo')
team_years = expand.grid( years, team_abbrs )
names( team_years ) = c('year', 'team_abbr')
team_years = team_years %>% mutate( url = paste0( "http://www.pro-football-reference.com/teams/",team_abbr, "/" ,year , "_roster.htm"))


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
  for ( idx in 1:1344){
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
for ( idx in 1:1344){
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
      
      l = rbind(l , df )      
    }, error = function(e){
      print("")
      print(e)
      print(paste(team_abbr))
      print(year)
    })

}




l = l %>%
  filter( Player != "Defensive Starters") %>%
  filter( Player != "Offensive Starters") %>%
  filter( Player != "Special Teams Starters")

write.csv(l, file = paste0( OUTPUT_ROOT,"all_rosters.csv")) 