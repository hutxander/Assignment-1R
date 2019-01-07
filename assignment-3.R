library(tidyverse)
library(xml2)
library(RCurl)
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"


#' Question 1: Get Population Ranking
#'
#' @return
#' @export
#'
#' @examples
get_population_ranking <- function(){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  url = str_c(base_url, "fields/335rank.html")
  #download url and execute all XPath queries which will each return a column for a data_frame
  raw_html <- read_html(getURL(url, .encoding = "UTF-8"))
  
  for(i in 1:length(xpath_expressions)){
    column = raw_html %>% xml_find_all(xpath_expressions[i]) %>%
      as_list() %>% unlist() #%>% str_c(collapse=" ")
    if(i == 1) {
      data_countries <- column #as.data.frame(column)
    } else {
      data_countries <- cbind(data_countries, column)#as.data.frame(column))
    }
  }
  colnames(data_countries) = names(xpath_expressions)
  data_countries <- as.data.frame(data_countries)
  
  #make the necessary adjustments to the data frame as given by the assignment

  data_countries <- rename(data_countries, population = "value", rank.population = "rank")
  adjusted_links <- data_countries %>% select(country_link) %>% as.matrix() %>% str_extract("g.*")  
  data_countries <- mutate(data_countries, country_link = adjusted_links)
}

scraped_data <- get_population_ranking()
#is.data.frame(scraped_data)

#' Question 2: Retrieve Land Area
#'
#' @param country_link A character vector of one or more country_link urls
#'
#' @return 
#' @export
#'
#' @examples
get_land_area <- function(country_link){
  #country_link <- head(country_link, 10)
  nrow(country_link)
  xpath <- str_c("//div[@id='","field-area","']/div[",2,"]/span[2]")
  #download the file from country_link and execute the xpath query
  land_area = country_link[FALSE]
  for(i in 1:length(country_link)){
    url = str_c(base_url, as.matrix(country_link)[i])
    raw_html <- read_html(getURL(url, .encoding = "UTF-8"))
    land_area[i] <- raw_html %>% xml_find_all(xpath) %>%
      as_list() %>% unlist()
  }
  
  return(land_area)
}

#note, this is done because of ambiguity in the assignment.
#in the code provided to us, a comment in Question 1 implies that scraped_data should be a data frame.
#however, in the assignment question 2, country link should be a character vector (and not a data frame column)
#thus, scraped data is made into a matrix to get country link equal to a character vector.
scraped_data <- as.matrix(scraped_data)
country_link <- scraped_data[,1]
#is.character(country_link)
#is.vector(country_link)

#country_link <- scraped_data %>% select(country_link)# %>% as.vector()
land_area <- get_land_area(country_link[2:6])
#is.character(land_area)
#is.vector(land_area)

#' Question 3: Get Population Density
#'
#' @return
#' @export
#'
#' @examples
get_population_density <- function(){
  
}


#' Question 4: Get All Provided Rankings
#'
#' @return
#' @export
#'
#' @examples
get_rankings <- function(){
  url <- "https://www.cia.gov/library/publications/the-world-factbook/docs/rankorderguide.html"
  xpath <- c("characteristic" = "//div[@class='field_label']/strong/a",
             "characteristic_link" = "//div[@class='field_label']/strong/a/@href")
  #...
}


#' Question 5 - Part 1: Get Ranking
#'
#' @param url The url of the ranking
#' @param characteristic What this ranking is about
#'
#' @return
#' @export
#'
#' @examples
get_ranking <- function(url = "fields/335rank.html", characteristic = "population"){
  xpath_expressions <- c("country_link" = "//td[@class='region']/a/@href",
                         "country" = "//td[@class='region']/a",
                         "value" = "//tr/td[3]",
                         "rank" = "//tr/td[1]")
  #...
}

#' Question 5 - Part 2: Get Country Characteristic
#'
#' @param country_link 
#' @param xpath_field_id 
#' @param item 
#'
#' @return
#' @export
#'
#' @examples
get_country_characteristic <- function(country_link, xpath_field_id = "field-area", item = 2){
  #update the xpath and use similar code other than that
}


#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return
#' @export
#'
#' @examples
combine_rankings <- function(rankings){
  
}



