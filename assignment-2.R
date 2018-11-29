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
get_jane_austen_data()

#' Extract possible names
#' 
#' This function extracts all words that start with a capital letter.
#' 
#' @param data A data frame, textual. Must contain a 'text' column,
#'   in this column the name search is done. Also must contain an 'id' column,
#'   which is used in return to refer back to original data.
#' @return A data frame with a row per extracted name and a column for the
#'   name identifier, original data frame identifier, and the extracted name.
extract_possible_names <- function(data){
  # Start with lists to gather names and id's - initialization.
  # Use lists for efficiency in loop.
  name_list <- vector("list", nrow(data))
  id_list <- vector("list", nrow(data))
  for(i in 1:nrow(data)){
    row <- data[i,] %>% select(text, id)
    
    # Extract al words that start with a capital in a single row. Simplify to get matrix.
    name_list[[i]] <- tolower(str_extract_all(row[,1],'[A-Z]([[:alpha:]]*)', simplify = TRUE))

    # Make a corresponding id list.
    id_list[[i]] <- rep(as.numeric(row[,2]),length(name_list[[i]])) 
  }
 
  name <- unlist(name_list)
  text_id <- unlist(id_list)
  names <- t(rbind(name, text_id))
  
  # Change to data frame
  names <- as.data.frame(names)

  # Provide a unique id for every name.
  unique_names <- as.data.frame(unique(names[,1]))
  unique_id <- as.data.frame(1:nrow(unique_names))
  unique <- cbind(unique_names, unique_id)
  colnames(unique) <- c("name", "id")
  
  # Add the third column to the return: unique id for each name.
  names <- left_join(names, unique, by = c("name"))
  
  # Set factors to chr and int.
  names[,1] <-  as.character(levels(names[,1]))[names[,1]]
  names[,2] <-  as.integer(levels(names[,2]))[names[,2]]
  return(names)
}

#rm(names1)
#names1 <- extract_possible_names(austen_text)

# Question 3 ------------------------------------------------------------------------------------------------------

#' Filter names
#' 
#' This function filters out words that are not capitalized at least 75% of the time.
#' 
#' @param data A data frame of word frequencies.
#' @return A data frame with words that are capitalized often enough.
filter_names <- function(names){
  # Load data frequency provided
  freq <- read_rds("austen_word_freqs.Rds")
  
  # Make frequencies found based on question 2 data.
  help_filter <- names %>% 
    group_by(name) %>% 
    summarize(
      capital_freq = n()
    ) 
  
  # Add just computed frequencies and provided frequencies to the data frame,
  #   then compute the proportion, if this is larger than 0.75: retain.
  #   Finally, delete the columns added to come back to the same format as in 2.
  filtered_names <- names %>% 
    left_join(help_filter, by = c("name")) %>% 
    left_join(freq, by = c("name" = "word")) %>% 
    mutate(capital_proportion = capital_freq / count) %>% 
    filter(capital_proportion >= 0.75) %>% 
    mutate(
      capital_proportion = NULL,
      capital_freq = NULL,
      count = NULL
    )
  
}

#filtered_names <- filter_names(names)

# Question 4 ------------------------------------------------------------------------------------------------------

#' Count names per book
#' 
#' This function counts the unique and total name occurences per book.
#'  
#' @param data A textual data frame - the original input as in austen_text.
#' @param names A textual data frame containing (filtered >75% capital) names.
#' @return A data frame with columns book title, 
#'   number of unique names, and total name occurences.
count_names_per_book <- function(data, names){
  
  # Use the data to separate the filtered_names frame into unique books (third column)
  # Within each book: sum filtered names and again add frequency of name and count these as well
  rm(book_specific)
  book_specific <- austen_text %>% 
    group_by(title) %>% 
    summarize(max_id = max(id)) %>% 
    arrange(max_id)

  data_needed <- austen_text %>% select(id, title)
  test_specifics <- left_join(test, austen_text %>% select(id, title),
                              by = c("text_id" = "id"))
  
  test_specific2 <- test_specifics %>% 
    group_by(title) %>% 
    summarize(
      unique_names = ncol(as.data.frame(unique(name))),
      name_occurrences = count(name)
    )

}

#count_names_per_book(austen_text, filtered_names)



names <- arrange(names, text_id) 
names <- names %>% rename(new_unique = "id")
test2 <- cbind(head(names,100000), head(names1,100000)) %>% 
  filter(text_id == 16345)

