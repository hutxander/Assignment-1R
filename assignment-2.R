library(tidyverse)


# Question 1 ------------------------------------------------------------------------------------------------------

#' Tidy dataframe
#'
#' Automatically tydies a dataframe by gathering all columns 
#'   that start with the given column prefix.
#'
#' @param data, Data frame, titles in first row, values below.
#' @param column_prefix, String, start of variable names to be gathered.
#' @return A tidy data frame, gathered based on prefix.
tidy_df <- function(data, column_prefix = "var"){
  # Select the correct variables based on column_prefix.
  # For this, we loop over all column titles and check if they match prefix.
  titles <- names(data)
  titles_start <- str_sub(titles, 1, nchar(column_prefix))
  # k is a counter: nr of vars to gather
  k = 0
  for(i in 1:length(titles)) {
    # If the required prefix is longer than the title, 
    #   we immediately know we will not have a fit.
    if(nchar(column_prefix) > nchar(titles[i])) {
      next
    }
    
    # If start of title equals required prefix, we add the variable to gather.
    if(titles_start[i] == column_prefix) {
      k = k + 1
      
      # First time, gather_vars must be initialized.
      if(k == 1) {
        gather_vars <- titles[i]
      } else {
        gather_vars <- c(gather_vars, titles[i])
      }
    }
    
    next
  }

  # If there is nothing to gather, stop here.
  stopifnot(k > 0)
  
  # Use gather on the previously selected variables
  data %>% 
    gather(
      key = "variable", 
      value = "value",
      gather_vars
    )
}

# Question 2 ------------------------------------------------------------------------------------------------------

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row
get_jane_austen_data <- function(){
  
  tryCatch({library(gutenbergr)}, error = function(e){install.packages("gutenbergr")})
  library(gutenbergr)
  
  austen_text <- gutenberg_works(author == "Austen, Jane") %>% 
    gutenberg_download(meta_fields = "title") %>% mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir=.GlobalEnv)
  invisible()
}

#' Extract possible names
#' 
#' This function extracts all words that start with a capital letter.
#' 
#' @param data A data frame, textual
#' @return A data frame with a row per extracted name and a column for the
#'   name identifier, original data frame identifier, and the extracted name.
extract_possible_names <- function(data){
  
  
}


# Question 3 ------------------------------------------------------------------------------------------------------

# filter_names



# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book
