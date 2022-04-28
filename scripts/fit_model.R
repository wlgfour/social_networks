#### Preamble ####
# Purpose: fit the model that will be used to describe polarity
# Author: William Gerecke
# Email: wlgfour@gmail.com
# Date: 4/25/2022
# Prerequisites: R software
# Notes:

# === data structure ===
# subreddit
# author
# id
# to_id
# permalink
# timestamp
# body



#install.packages('statnet')
library(statnet)
library(stringr)


#comments <- read.csv('outputs/simulated/processed.csv')
comments <- read.csv('outputs/processed/data.csv')
#comments <- comments[sample(nrow(comments), 500), ]
colnames(comments)

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
node_values <- data.frame(matrix(nrow=length(all_ids), ncol=3))
colnames(node_values) <- c('id', 'sentiment', 'subreddit')
node_values['id'] <- all_ids
rownames(node_values) <- all_ids
for (i in 1:nrow(comments)) {
  from <- comments[i, 'id']
  to <- comments[i, 'to_id']
  sub <- comments[i, 'subreddit']
  sentiment <- comments[i, 'sentiment']
  
  node_values[node_values$id == from, 'sentiment'] <- sentiment
  node_values[node_values$id == from, 'subreddit'] <- sub
  node_values[node_values$id == to, 'subreddit'] <- sub
}
node_values[is.na(node_values$sentiment), 'sentiment'] <- 0

sentiments <- node_values[['sentiment']]
subreddits <- node_values[['subreddit']]

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



#plot.network(net, # our network object
#             vertex.col = 'subreddit', # color nodes by gender
#             vertex.cex = sentiments * 5, # size nodes by their age
#             displaylabels = F, # show the node names
#             label.pos = 5 # display the names directly over nodes
#)


# === fit the model ===

model <- ergm(net~edges+nodecov('sentiment')+nodeicov('sentiment')+nodefactor('subreddit'))

summary(model)

g <- gof(model)

plot(g)














