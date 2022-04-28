#### Preamble ####
# Purpose: this script will scrape comments from reddit. it uses the .json
#    endpoint available to any reddit url that expresses the content as json.
#    comments are extracted as follows:
#      1. gather posts from given subreddits into a list of links
#      2. select a link at random, gather all comments, and append them to all_data
#      3. if the link is a user, add all of the posts the commented on to links
#      4. if the link is a post, add all the users that commented on it
#      5. goto 2
# Author: William Gerecke
# Email: wlgfour@gmail.com
# Date: 4/26/2022
# Prerequisites: R software
# Notes: the script will run until stopped, but will cache the current dataframe
#    as a chunk un outputs/raw every 10 posts visited. chunks are automatically labelled


library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(crayon)


# === types and their associated content ===
# t1 	Comment
# t2 	Account
# t3 	Link
# t4 	Message
# t5 	Subreddit
# t6 	Award
# t8 	PromoCampaign

# === data structure ===
# subreddit
# author
# id
# to_id
# permalink
# timestamp
# body
# ups


# generate output file
output_file <- paste('outputs/raw/', 'chunk-', format(Sys.time(), "%F-%H-%M-%S"), '.csv', sep='')

# takes user name ('author')
get_usr <- function(s) list('u', paste('http://reddit.com/user', s, 'comments', '.json', sep='/'))

# takes subreddit name ('subreddit')
get_sub <- function(s) paste('http://reddit.com/r', s, '.json', sep='/')

# takes post id (listing -> children -> id)
#get_pst <- function(s) paste('http://reddit.com', s, '.json', sep='/')

# not possible?
#get_com <- function(s) paste(c('reddit.com/u', s, '.json'), sep='/')


#l <- "http://reddit.com/r/gifs/.json"
#data <- jsonlite::fromJSON(l)
#children <- data$data$children$data
#links <- children |>
#  filter(locked == FALSE, pinned == FALSE) |>
#  select(permalink) |>
#  mutate(permalink=map(permalink, function(s) paste('reddit.com', s, '.json', sep='')))
#lks <- c(map(links$permalink, function(s) c('p', s)))


# initialize with a list of top posts on a subreddit
# get test link
#links <- map(c('saviyazzinlebox'), get_usr)

all_data <- data.frame()
links <- c(map(
  c('AskReddit', 'worldnews', 'gifs'),
  function(s) {
    links <- jsonlite::fromJSON(get_sub(s))$data$children$data |>
      filter(locked == FALSE, pinned == FALSE) |>
      select(permalink) |>
      mutate(permalink = map(permalink, function(s) paste('http://reddit.com', s, '.json', sep='')))
  }
))
links <- map(unlist(links), function(s) list('p', s))

process_comment <- function (children, depth) {
  # children <- post$data$children -> [(kind, data), ...]
  
  # skip if empty
  #print(class(children))
  if (is.null(children) || nrow(children) == 0) {
    return(data.frame())
  }
  colnames(children$data)
  
  # get comment data
  raw <- tryCatch({
    children$data |>
      separate(parent_id, c('parent_kind', 'parent_id'), '_') |>
      select(
        subreddit,
        author,
        id,
        parent_kind,
        to_id=parent_id,
        permalink,
        timestamp=created_utc,
        body,
        ups
      ) |>
      mutate(permalink = as.character(map(permalink, function(s) paste('http://reddit.com', s, sep=''))))
  }, error=function(err) {
    cat(red('!!') %+% ' error in recursive comment collection at depth ' %+% as.character(depth) %+% '\n')
    return(data.frame())
  })
  
  # get replies to comments
  replies <- children$data$replies[unlist(map(children$data$replies, function(t) length(names(t)) > 0))]
  reply_dfs <- map(replies, function(s) s$data$children)
  reply_raws <- map(reply_dfs, function(s) process_comment(s, depth+1))
  reply_tree <- bind_rows(reply_raws)
  rbind(raw, reply_tree)
}

read_url <- function(url, tries) {
  tryCatch({
    jsonlite::fromJSON(url)
  }, error=function(err) {
    if (tries < 5) {
      # wait & retry
      cat(yellow('[' %+% as.character(tries) %+% ']') %+% ' couldn\'t reach url ' %+% url %+% '\n')
      Sys.sleep(0.75)
      return(read_url(url, tries+1))
    } else {
      cat(red('[' %+% as.character(tries) %+% ']') %+% ' aborting' %+% '\n')
      return(NULL)
    }
  })
}

links_seen <- c()
users_seen <- c()
i <- 0
while (length(links) > 0) {
  i <- i + 1
  ind <- sample(1:length(links), 1)
  typ <- links[[ind]][[1]]
  url <- links[[ind]][[2]]
  links_seen <- c(links_seen, url)
  links <- links[-ind]
  cat(blue('[' %+% as.character(nrow(all_data)) %+% '/' %+% as.character(i) %+% '/' %+% as.character(length(links)) %+% ']')
      %+% ' type: ' %+% bold(typ) %+% '   url: ' %+% bold(url) %+% '\n')
  
  # get comments from post or user
  post <- read_url(url, 0)
  if (is.null(post)) next
  
  # extract data from posts and comments differently
  if (typ == 'p') {
    if (length(post$data$children) == 0) next
    if (length(post$data$children[[2]]) == 0) next
    children <- post$data$children[[2]] |>
      filter(kind == 't1')
  } else if (typ == 'u') {
    children <- post$data$children |>
      filter(kind == 't1')
  }
  #colnames(children$data)
  
  # skip if empty
  if (nrow(children) == 0) {
    next
  }
  
  # get comment data
  raw <- process_comment(children, 0)
  
  # drop parent kind
  cleaned <- raw |>
    select(!parent_kind)
  
  # comment data to all data and checkpoint
  all_data <- rbind(all_data, cleaned)
  
  # save the scraped data if i%x==0
  if (i %% 10 == 0) {
    cat('--> caching at ' %+% bold(output_file) %+% '\n')
    write.csv(all_data, output_file)
  }
  
  # add to list of links to visit
  # - if a user page: the post that the comments were on
  # - if a post page: user pages for the people who commented
  # user
  if (typ == 'u') {
    # filter to only have replies to comments
    post_links <- raw |>
#      filter(parent_kind == 't1') |>
      select(permalink) |>
      mutate(permalink=map(permalink, function(s) list('p', paste(s, '.json', sep=''))))# |>
      #filter(!(permalink %in% links_seen))
    t <- post_links$permalink
    links <- c(links, t)
    
  # post
  } else if (typ == 'p') {
    #unseen_users <- raw |>
    #  filter(!(author %in% users_seen))
    t <- map(raw$author, get_usr)
    users_seen <- c(users_seen, raw$author)
    links <- c(links, t)
    #print(raw$author)
  }
  links <- unique(links)
  all_data <- drop_na(all_data) |>
    distinct(id, to_id, .keep_all=T)
}


