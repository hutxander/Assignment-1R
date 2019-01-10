library(tidyverse)
library(xml2)
library(RCurl)
base_url <- "https://www.cia.gov/library/publications/the-world-factbook/"


# Question 1 --------------------------------------------------------------
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
      as_list() %>% unlist() 
    if(i == 1) {
      data_countries <- column 
    } else {
      data_countries <- cbind(data_countries, column)
    }
  }

  #make the necessary adjustments to the data frame as given by the assignment
  data_countries[,1] <- gsub("\\.\\./", "", data_countries[,1])
  data_countries[,3] <- gsub(",", "", data_countries[,3])
  data_countries <- as.tibble(data_countries)
  data_countries[[3]] <- as.numeric(data_countries[[3]])
  data_countries[[4]] <- as.numeric(data_countries[[4]])

  colnames(data_countries) = names(xpath_expressions)
  data_countries <- rename(data_countries, population = "value", rank.population = "rank")

  return(data_countries)
}

scraped_data <- get_population_ranking()
is.data.frame(scraped_data)


# Question 2 --------------------------------------------------------------
#' Question 2: Retrieve Land Area
#'
#' @param country_link A character vector of one or more country_link urls
#'
#' @return 
#' @export
#'
#' @examples
get_land_area <- function(country_link){
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
country_link <- as.matrix(scraped_data)[,1]
is.character(country_link)
is.vector(country_link)

land_area <- get_land_area(country_link) #[1:5])
is.character(land_area)
is.vector(land_area)
#land_area_total <- land_area
#land_area <- land_area_total


# Question 3 --------------------------------------------------------------
#' Question 3: Get Population Density
#'
#' @return
#' @export
#'
#' @examples
get_population_density <- function(){
  #get information we need
  scraped_data <- get_population_ranking()
  country_link <- as.matrix(scraped_data)[,1]
  land_area <- get_land_area(country_link)
  
  #adjust land area data format
  #note: data format for scraped_data is already done in Q1, 
  #I only noticed later on that this should be done here. 
  #however, I assume it is sufficient to leave it in Q1, 
  #as this is a similar demonstration of programming skills as when I would have included it here.
  
  land_area <- gsub(",", "", land_area)
  land_area <- gsub(" ", "", land_area)
  land_area <- gsub("sqkm", "", land_area)
  land_area <- gsub("million","000000", land_area)
  land_area <- as.numeric(land_area)
  country_data <- cbind(scraped_data, land_area)
  
  #adjust data format, add population_density
  country_data <- mutate(country_data, population_density = population / land_area)
}

#note, while running this function I get a warming message:
#in get_population_density(): NAs introduced by coercion.
#however, the output does not contain NAs - it looks fine - I am unsure what goes wrong here.
density_data <- get_population_density()


# Question 4 --------------------------------------------------------------
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
  
  raw_html <- read_html(getURL(url, .encoding = "UTF-8"))
  characteristic <- raw_html %>% xml_find_all(xpath[1]) %>%
    as_list() %>% unlist()
  characteristic <- tolower(characteristic %>% str_extract("[^:]*"))

  characteristic_link <- raw_html %>% xml_find_all(xpath[2]) %>%
    as_list() %>% unlist()
  characteristic_link <- characteristic_link %>% str_extract("f.*") 
  
  return(cbind(characteristic, characteristic_link))
}

rankings <- get_rankings()

# Question 5 --------------------------------------------------------------
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
  url_characteristic = str_c(base_url, url)
  #download url and execute all XPath queries which will each return a column for a data_frame
  raw_html <- read_html(getURL(url_characteristic, .encoding = "UTF-8"))
  
  for(i in 1:length(xpath_expressions)){
    column = raw_html %>% xml_find_all(xpath_expressions[i]) %>%
      as_list() %>% unlist()
    if(i == 1) {
      data_countries <- column 
    } else {
      data_countries <- cbind(data_countries, column)
    }
  }
  colnames(data_countries) = names(xpath_expressions)
  data_countries <- as.data.frame(data_countries)
  
  #make the necessary adjustments to the data frame as given by the assignment
  
  data_countries <- rename(data_countries, !!characteristic:= "value", !!str_c("rank.", characteristic):= "rank")
  adjusted_links <- data_countries %>% select(country_link) %>% as.matrix() %>% str_extract("g.*")  
  data_countries <- mutate(data_countries, country_link = adjusted_links)
  
}

scraped_ranking <- get_ranking(url = "fields/279rank.html", characteristic = "area")

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
  xpath <- str_c("//div[@id='",xpath_field_id,"']/div[",item,"]/span[2]")
  #download the file from country_link and execute the xpath query
  characteristic_value = country_link[FALSE]
  for(i in 1:length(country_link)){
    url = str_c(base_url, as.matrix(country_link)[i])
    raw_html <- read_html(getURL(url, .encoding = "UTF-8"))
    characteristic_value[i] <- raw_html %>% xml_find_all(xpath) %>%
      as_list() %>% unlist()
  }
  
  return(characteristic_value)
}

characteristic2 <- get_country_characteristic(country_link[2:6], xpath_field_id = "field-area", item = 2)

# Question 6 --------------------------------------------------------------
#' Question 6: Combine Rankings
#'
#' @param rankings Rankings from get_rankings (or a selection thereof)
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' Issues with this one
combine_rankings <- function(rankings){
  rankings <- rankings[1:3, 1:2]
  for(i in 1:nrow(rankings)){
    new_ranking <- get_ranking(url = rankings[i,2], characteristic = rankings[i,1])
    if(i==1){
      combined_rankings = new_ranking
    } else {  
      combined_rankings = full_join(combined_rankings, new_ranking, by = c("country_link"))
    }
  }
}
nrow(rankings[1:3, 1:2])
rm(combined_rankings)
combined_rankings <- combine_rankings(rankings = rankings[1:3, 1:2])


