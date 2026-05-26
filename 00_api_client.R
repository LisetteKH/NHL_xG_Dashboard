# connecting via API!

library(httr)
library(jsonlite)
library(glue)

NHL_API <- "https://api-web.nhle.com/v1"

# Core Request Function: takes the URL and the GET requet, checks the response and returns the parsed JSON as an R list 


# GET request
nhl_get <- function(url){
response <- httr::GET(
  url, 
  httr::user_agent("NHL GOALS")
)


#Checking the status code; 200 means success
 status <-httr::status_code(response) #looking for 200 
 
 if (status != 200){
   message("API returned status " , status, "for: ", url)
   return(NULL)
 }
 

 #extract the response bosy as text and then we will parse it from JSON into an R list!
 raw_text <- httr::content(response, as = "text", encoding = "UTF-8")
 parsed <- jsonlite::fromJSON(raw_text, simplifyVector = FALSE)
 
 return(parsed)
 

}
# Schedule for Today 
# uses the nhl_get() function to hit schedule/now endpoint
#reutrns the raw parsed list 

nhl_schedule_today <- function() {
  url <- glue("{NHL_API}/schedule/now")
  result <- nhl_get(url)
  return(result)
}



