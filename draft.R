#-----------------------
# 1. setting up
#-----------------------

# makes sure to install these with install.packages() if they aren't already installed
library(rvest)
library(stringr)
library(jsonlite)
b
#-----------------------
# 2. getting urls and parsing storifies
#-----------------------

# scrapes page to generate vector of storify urls
urls <- read_html("https://ngsschat.wikispaces.com/") %>% 
            html_nodes("a") %>% 
            html_attr("href") 

tmp_index <- grep("storify", urls) # creates index of urls
storify_urls_all <- urls[tmp_index] # uses index to select only storify urls
storify_urls <- str_split(storify_urls_all, "/") # prettifies urls

out_df <- data.frame(url = storify_urls_all)
for (i in 1:nrow(out_df)){
      out_df$api_url[i] <- paste0("https://api.storify.com/v1/stories/", storify_urls[[i]][4], "/", storify_urls[[i]][5]) # makes a url
      temp_dat <- fromJSON(out_df$api_url[i], simplifyDataFrame = T)  # makes a data frame 
      out_df$num_tweets[i] <- temp_dat$content$stats$elements$quote # number of tweets in story
      out_df$n_pages[i] <- ceiling(out_df$num_tweets[i] / 50) # number of pages, to be used later
      out_df$date[i] <- str_split(temp_dat$content$date$created, "T")[[1]][1] # adds date storify was created
      print(paste0("Processed Storify ", i, " from ", out_df$date[i], " with ", out_df$num_tweets[i], " tweets")) 
}

#-----------------------
# 3. processing text from storifies
#-----------------------

write.csv(out_df, "~/dropbox/research/ngsschat/out_df.csv")

# function for handling links without http by pasting new link

http_func <- function(my_var){ 
      out <- tryCatch({
            temp_link <- my_var # gets links
      },
      error = function(cond){
            message(cond)
            temp_link <- paste0("http:", my_var) # pastes amended link for broken links

      },
      finally = print(paste0("Processing JSON for links for Storify ", i, " page ", j))
      )
      return(out)
}

# function for handling broken links by adding NA instead of text to row

missing_func <- function(temp_link){ 
      out <- tryCatch({
            temp_vec <- read_html(temp_link)
            temp_vec <- html_nodes(temp_vec, ".tweet-text") # extracts tweet
            my_output_vec[k] <- html_text(temp_vec) # saves text to vector for page
      },
      error = function(cond){
            message("URL does not seem to exist")
            message(cond)
            my_output_vec[k] <- NA
      },
      finally = print(paste0("Parsing HTML for Storify ", i, " page ", j, ", tweet ", k))
      )
      return(out)
}

for (i in 1:nrow(out_df)){ # this iterates through the storifies
      
      temp_page_num <- 1:out_df$n_pages[i] # temp_page_num
      temp_urls_to_process <- paste0(out_df$api_url[i], "?page=", temp_page_num, "&per_page=50") # temp_urls_to_process

      out_text <- list() # for output for text
      out_time <- list() # for output for time
      out_username <- list() # for output for username
      
      for (j in 1:length(temp_urls_to_process)){ # this selects one page of one storify
            temp_dat <- fromJSON(temp_urls_to_process[1])
            temp_link <- http_func(temp_dat$content$elements$permalink)
            my_output_vec <- vector(length = length(temp_link)) # makes vector for text output for page
           
            for (k in 1:length(my_output_vec)){ # this processes one page of one storify
                  my_output_vec[k] <- missing_func(temp_link[k])
            }
            
            out_time[[j]] <- temp_dat$content$elements$posted_at # timestamp
            out_username[[j]] <- temp_dat$content$elements$source$username # username
            out_text[[j]] <- my_output_vec # text
      }
      
      out_data <- data.frame(time = unlist(out_time), username = unlist(out_username), text = unlist(out_text))
      write.csv(out_data, paste0("~/dropbox/research/ngsschat/storify-data", "/", i, ".csv"), row.names = F) # change to local dir
}
