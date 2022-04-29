#### Preamble ####
# Purpose: fit the model that will be used to describe polarity
# Author: William Gerecke
# Email: wlgfour@gmail.com
# Date: 4/25/2022
# Prerequisites: R software
# Notes: This script takes a significant amount of memory and will take a long
#    time to run. It saves models and gof statistics to outputs/models.
#    it will crash because r errors out when processing medium amounts of data I guess

# === data structure ===
# subreddit
# author
# id
# to_id
# permalink
# timestamp
# body



#install.packages('statnet')
library(dplyr)
library(statnet)
library(stringr)
library(hash)


#comments <- read.csv('outputs/simulated/processed.csv')
all_comments <- read.csv('outputs/processed/data.csv') |>
  select(!X)
#comments <- comments[sample(nrow(comments), 500), ]
colnames(all_comments)

subs <- (all_comments |>
  select(subreddit) |>
  unique())$subreddit

# datastructures to store models, nets, and gof stats
tryCatch({
  nets <- readRDS('outputs/models/nets.rds')
  models <- readRDS('outputs/models/models.rds')
  gofs <- readRDS('outputs/models/gofs.rds')
}, error = {
  nets <- hash()
  models <- hash()
  gofs <- hash()
})



# for manually looping because r keeps crashing
i <- 1
sub <- subs[i]
for (sub in subs) {
  if (sub %in% keys(gofs)) {
    print(paste('skipping', sub))
    next
  }
  comments <- all_comments |>
    filter(subreddit == sub)
  # get unique users
  #from_users <- comments[['from']] |>
  #  unique()
  #to_users <- comments[['to']] |>
  #  unique()
  #all_users <- unique(c(from_users, to_users)) |>
  #  str_sort()
  # build adjacency list with users as nodes
  #adj <- matrix(0, nrow=N, ncol=N)
  #rownames(adj) <- all_users
  #colnames(adj) <- all_users
  #for (i in 1:nrow(comments)) {
  #  from <- comments[i, 'from']
  #  to <- comments[i, 'to']
  #  sentiment <- comments[i, 'sentiment']
  #  adj[from, to] <- adj[from, to] + sentiment
  #}
  
  
  # get unique comment ids
  from_ids <- comments[['id']] |>
    unique()
  to_ids <- comments[['to_id']] |>
    unique()
  all_ids <- unique(c(from_ids, to_ids)) |>
    str_sort()
  
  # build adjacency matrix
  N <- length(all_ids)
  adj <- matrix(0, nrow=N, ncol=N)
  rownames(adj) <- all_ids
  colnames(adj) <- all_ids
  
  for (i in 1:nrow(comments)) {
    from <- comments[i, 'id']
    to <- comments[i, 'to_id']
    if (to != 'NA') {
      adj[to, from] <- 1
    }
  }
  
  # create edge attributes matrix
  # would create a matrix like the adjacency matrix, but with values
  
  # create node attributes
  node_values <- data.frame(matrix(nrow=length(all_ids), ncol=4))
  colnames(node_values) <- c('id', 'sentiment', 'subreddit', 'ups')
  node_values['id'] <- all_ids
  rownames(node_values) <- all_ids
  
  for (i in 1:nrow(comments)) {
    from <- comments[i, 'id']
    to <- comments[i, 'to_id']
    sub_ <- comments[i, 'subreddit']
    ups <- comments[i, 'ups']
    sentiment <- comments[i, 'sentiment']
    
    node_values[node_values$id == from, 'sentiment'] <- sentiment
    node_values[node_values$id == from, 'ups'] <- ups
    node_values[node_values$id == from, 'subreddit'] <- sub_
    node_values[node_values$id == to, 'subreddit'] <- sub_
  }
  node_values[is.na(node_values$sentiment), 'sentiment'] <- 0
  node_values[is.na(node_values$ups), 'ups'] <- 0
  
  sentiments <- node_values[['sentiment']]
  subreddits <- node_values[['subreddit']]
  ups <- node_values[['ups']]
  
  # build network
  net <- as.network(
    x = adj,
    directed = TRUE,
    loops = TRUE,
    matrix.type = 'adjacency'
  )
  
  
  # set node values
  net %v% 'sentiment' <- sentiments
  net %v% 'subreddit' <- subreddits
  net %v% 'ups' <- ups
  
  
  plot.network(net, # our network object
               vertex.col = 'sentiment', # color nodes by gender
               vertex.cex = log(abs(ups) + 1) * 0.5, # size nodes by their age
               displaylabels = F, # show the node names
               label.pos = 5 # display the names directly over nodes
  )
  
  
  # === fit the model ===
  
  print(paste('fitting model for ', sub))
  #model <- ergm(net~edges+nodecov('sentiment')+nodeicov('sentiment')+nodecov('ups'))
  model <- ergm(net~edges+nodecov('sentiment'))
  
  print(summary(model))
  
  print(paste('generating gof for ', sub))
  g <- gof(model)
  
  plot(g)
  
  
  # save model and associated data
  nets[[sub]] <- net
  models[[sub]] <- model
  gofs[[sub]] <- g
  
  
  
  # save the nets, models, and gofs. do it here instead of outside of the loop because r will error
  saveRDS(nets, 'outputs/models/nets.rds')
  saveRDS(models, 'outputs/models/models.rds')
  saveRDS(gofs, 'outputs/models/gofs.rds')
  
  # for manually looping because r sucks
  i <- i + 1
}













