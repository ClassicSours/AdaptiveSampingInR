## Adaptive Sampling in R
## Aaron Shaffer
## ashaffer7@mail.csuchico.edu
## May 05 2017

pkgs <-c('reshape2','dplyr','ggplot2','plotly','igraph','plotrix')
for(p in pkgs)
  if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p)}
for(p in pkgs)
  suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))
rm('p','pkgs')

melt.matrix <- function(world){
  suppressMessages(
    world %>% 
      data.frame %>% 
      melt() %>% 
      mutate(id = seq_len(nrow(world) * ncol(world)), 
             x = rep(c(1:ncol(world)),nrow(world)),
             y = sort(rep(c(1:nrow(world)),ncol(world)))) %>%
             data.frame() %>% 
      select(value,id,x,y)
  )
}

getid <- function(x,y,ncol){return(x + ((y-1) * ncol))}

plotly.adjmatrix <- function(world, name){
  gg <- melt.matrix(world) %>% 
    ggplot(aes(x = as.factor(x), 
               y = as.factor(y),
               "X" = x,
               "Y" = y,
               "ID" = id)) + 
    geom_raster(aes(fill = value, alpha = value/max(value))) + 
    scale_fill_gradient(low="grey90",high="red") + 
    geom_text(aes(label = ifelse(value,value,""))) +
    theme(panel.background = element_blank(), legend.position = "none") + 
    labs(x = "X",y = "Y") + ggtitle(name)
  
  ggplotly(gg, tooltip = c("X","Y","value","id"))
}

plotly.adjmatrix.id <- function(world, name){
  gg <- melt.matrix(world) %>% 
    ggplot(aes(x = as.factor(x), 
               y = as.factor(y),
               "value" = value,
               "X" = x,
               "Y" = y,
               "ID" = id)) + 
    geom_tile(aes(fill = as.factor(id)),color = "black") + 
    geom_text(aes(label = id)) +
    theme(panel.background = element_blank(), legend.position = "none") + 
    labs(x = "X",y = "Y") + ggtitle(paste("Index values of ",name))
  
  ggplotly(gg, tooltip = c("X","Y","value","id"))
}

n <- 5
N <- 100
m <- c(1,4,1,5,2)
y <- c(0,12,1,15,7)

adaptive.sample.confint.fun <- function(y,m,n,N) {
  mu.hat <- 1/n * sum(y/m) 
  s.y.2 <- var(y/m)
  v.mu <- (1 - n/N) * s.y.2/n
  c("mu.hat" = mu.hat, 
    "s.y.2" = s.y.2,
    "v.mu" = v.mu,
    "var" = 2 * sqrt(v.mu),
    "lower" = mu.hat - 2 * sqrt(v.mu),
    "upper" = mu.hat + 2 * sqrt(v.mu))
}

adaptive.sample.confint.fun(y,m,n,N)

example.world <- matrix(
  c( 1, 0, 3, 3, 0, 0,-1,
     0, 0, 3, 0, 3, 0,-1,
     3, 0, 0, 0, 0, 0, 0,
     0, 4, 0, 0, 0, 3, 3,
     0, 0, 0, 3, 3, 3, 0,
    -1,-1, 0, 0, 0, 0, 0, 
    -1,-1,-1,-1,-1,-1,-1),
    nrow = 7, ncol = 7,byrow=TRUE)

example.world

plotly.adjmatrix(example.world,"example.world, -1 means you cannot infer what value might be there")

figure10.2 <- matrix(rep(rep(0,10),10),
                     nrow = 10, ncol = 10)

figure10.2[,10] <- c(4,0,0,0,0,0,0,0,0,7)
figure10.2[,9]  <- c(3,0,2,1,0,0,0,0,4,1)
figure10.2[,8]  <- c(0,0,0,4,5,0,0,3,2,0)
figure10.2[,7]  <- c(5,4,0,0,0,0,0,0,0,0)
figure10.2[,6]  <- c(0,0,0,2,0,1,0,0,7,0)
figure10.2[,5]  <- c(0,5,0,4,0,0,0,0,0,0)
figure10.2[,4]  <- c(0,0,3,0,0,4,0,3,0,0)
figure10.2[,3]  <- c(0,1,0,0,0,3,0,0,4,0)
figure10.2[,2]  <- c(0,0,0,2,6,0,0,0,0,0)
figure10.2[,1]  <- c(0,4,0,4,0,0,2,3,0,0)

plotly.adjmatrix(figure10.2,"Figure 10.2")

adjlist <- function(world){
  nrow <- nrow(world)
  ncol <- ncol(world)
  
  G <- as.vector(world)
  
  adj.list <- rep(list(c()),nrow*ncol)  
  for(index in 1:length(G)){
    last.column <- index %% ncol == 0
    first.column <- (index - 1) %% ncol == 0
    first.row <- index <= ncol
    last.row <- index > (nrow * ncol - ncol)
    
    left <- ifelse(first.column,NA,index - 1)
    up.left <- ifelse(last.row | first.column,NA,index + ncol - 1)
    up <- ifelse(last.row,NA,index + ncol)
    up.right <- ifelse(last.row | last.column, NA,index + ncol + 1)
    right <- ifelse(last.column, NA, index + 1)
    down.right <- ifelse(first.row | last.column, NA, index - ncol + 1)
    down <- ifelse(first.row, NA, index - ncol)
    down.left <- ifelse(first.row | first.column , NA, index - ncol - 1)
    
    adjlist.i <- c()
    if(!is.na(left))
      adjlist.i <- c(adjlist.i,"W" = left)
    if(!is.na(up.left))
      adjlist.i <- c(adjlist.i,"NW" = up.left)
    if(!is.na(up))
      adjlist.i <- c(adjlist.i,"N" = up)
    if(!is.na(up.right))
      adjlist.i <- c(adjlist.i,"NE" = up.right)
    if(!is.na(right))
      adjlist.i <- c(adjlist.i,"E"= right)
    if(!is.na(down.left))
      adjlist.i <- c(adjlist.i,"SE" = down.left)
    if(!is.na(down))
      adjlist.i <- c(adjlist.i,"S" = down)
    if(!is.na(down.right))
    adjlist.i <- c(adjlist.i,"SW" = down.right)
    
    adj.list[[index]] <- adjlist.i
  }
  return(adj.list)
}

    adjlist.adaptive <- function(world){
    nrow <- nrow(world)
    ncol <- ncol(world)
    
    G <- as.vector(world)
    
    adj.list <- rep(list(c()),nrow*ncol)  
    for(index in 1:length(G)){
    if(G[index] != 0) {
    last.column <- index %% ncol == 0
    first.column <- (index - 1) %% ncol == 0
    first.row <- index <= ncol
    last.row <- index > (nrow * ncol - ncol)
    
    left <- ifelse(first.column,NA,index - 1)
    up.left <- ifelse(last.row | first.column,NA,index + ncol - 1)
    up <- ifelse(last.row,NA,index + ncol)
    up.right <- ifelse(last.row | last.column, NA,index + ncol + 1)
    right <- ifelse(last.column, NA, index + 1)
    down.right <- ifelse(first.row | last.column, NA, index - ncol + 1)
    down <- ifelse(first.row, NA, index - ncol)
    down.left <- ifelse(first.row | first.column , NA, index - ncol - 1)
    
    adj <- c()
    if(!is.na(left)) {
      if(G[left] != 0) {
        adj <- c(adj,"W" = left) 
      }
    }
    if(!is.na(up.left)){
      if(G[up.left] != 0) {
        adj <- c(adj,"NW" = up.left)  
      }
    }
    if(!is.na(up)) {
      if(G[up] != 0){
        adj <- c(adj,"N" = up)
      }
    }
    if(!is.na(up.right)) {
      if(G[up.right] != 0){
        adj <- c(adj,"NE" = up.right)
      }
    }
    if(!is.na(right)) {
      if(G[right] != 0){
        adj <- c(adj,"E"= right)
      }
    }
    if(!is.na(down.left)) {
      if(G[down.left] != 0){
        adj <- c(adj,"SE" = down.left)
      }
    }
    if(!is.na(down)) {
      if(G[down] != 0){
        adj <- c(adj,"S" = down)
      }
    }
    if(!is.na(down.right)) {
      if(G[down.right] != 0){
        adj <- c(adj,"SW" = down.right)
      }
    }
    
    adj.list[[index]] <- adj
    } 
    else {
      adj.list[[index]] <- c("NA" = NA)
    }
  }
  return(adj.list)
}

plotly.adjmatrix.id(figure10.2, "Figure 10.2")

figure10.2.adjlist <- adjlist(figure10.2)
figure10.2.adjlist

getEdges <- function(world,adaptive){
  if(adaptive == TRUE)
    world.adj <- adjlist.adaptive(figure10.2)
  else
    world.adj <- adjlist(figure10.2)
  edge.list <- c()
  for(i in 1:length(world.adj)){
    for(j in 1:length(world.adj[[i]])){
      if(i <= ifelse(!gtools::invalid(world.adj[[i]][j]),world.adj[[i]][j],0)){
        edge.list <- c(edge.list,i,ifelse(!gtools::invalid(world.adj[[i]][j]),world.adj[[i]][j],i))
      }
    }
  }
  edge.list
}

g1 <- graph(edges = rev(getEdges(figure10.2,FALSE)), 
directed = F, n = 100)
plot(g1, layout=layout_on_grid)

g2 <- graph(edges = rev(getEdges(figure10.2,TRUE)), 
directed = F, n = 100)
plot(g2, layout=layout_on_grid)

adjlist(figure10.2)
adjlist.adaptive(figure10.2)

Queue <- setRefClass(Class = "Queue",
  fields = list(
    name = "character",
    data = "list"
  ),
  methods = list(
    size = function() {
      'Returns the number of items in the queue.'
      return(length(data))
    },
    #
    push = function(item) {
      'Inserts element at back of the queue.'
      data[[size()+1]] <<- item
    },
    #
    pop = function() {
      'Removes and returns head of queue (or raises error if queue is empty).'
      if (size() == 0) stop("queue is empty!")
      value <- data[[1]]
      data[[1]] <<- NULL
      value
    },
    #
    initialize=function(...) {
      callSuper(...)
      #
      # Initialise fields here (place holder)...
      #
    .self
    },
    empty=function(...){
      return(size() == 0)
    }
  )
)

# Algorithm: Breadth First Search (BFS)
# BFS (V,E,s) {
#   for each u within V - {s}
#     u.distance <- inf
#     u.parent <- u
#   s.d <- 0
#   Q <- 0
#   Enqueue(Q,s)
#   while Q is not empty{
#     u <- Dequeue(Q)
#     for each v within G.adj[u]{
#       if v.d == inf {
#         v.d = u.d + 1
#         v.parent = u
#         Enqueue(Q,v)
#       }
#     }
#   }
# }
BFS <- function(V,G.adj,s){
  d <- rep(0,nrow(V)*ncol(V))
  p <- rep(0,nrow(V)*ncol(V))
  for(u in 1:length(V)){
    d[u] <- -1
    p[u] <- u
  }
  d[s] <- 0
  Q <- Queue$new()
  Q$push(s)
  id <- c()
  while(!Q$empty()){
    u <- Q$pop()
    id <- c(id,u)
    if(!is.na(G.adj[u])){
      for(v in G.adj[[u]]){
        if(d[v] == -1){
          d[v] <- d[u] + 1
          p[v] <- u
          Q$push(v)
        }
      } 
    }
  }
  return(list("adaptive.sample" = data.frame("y" = sum(V[d != -1]),"m" = length(d[d != -1])),
              "sampled.squares" = data.frame("id" = id, "yi" = V[id]), 
              "d" = matrix(d, nrow = nrow(V), ncol=ncol(V)), 
              "p" = matrix(p, nrow = nrow(V), ncol=ncol(V))))
}

BFS.results <- BFS(figure10.2,adjlist.adaptive(figure10.2),83)
world <- BFS.results$d

plotly.distance <- function(o.world,world,s){
gg <- melt.matrix(world) %>% 
  mutate(id = getid(x,y,ncol(world)), 
         actual.value = as.vector(o.world),
         distance = value, 
         x = x, 
    y = y) %>% 

    ggplot(aes(x = as.factor(x), 
  y = as.factor(y),
    "X" = x,
  "Y" = y,
  "id" = id,
  "distance" = distance,
  "actual.value" = actual.value)) + 
  geom_raster(aes(fill = value, alpha = value/max(value))) + 
  scale_fill_gradient(low="grey90",high="red") + 
  geom_text(aes(label = ifelse(value == -1,"inf",value))) +
  theme(panel.background = element_blank()) + labs(x = "X",y = "Y") +
  ggtitle(paste("Value of d, distance from s = ",s)) + theme(legend.position = "none")
  
  ggplotly(gg, tooltip = c("X","Y","distance","id","actual.value"))
}

plotly.distance(figure10.2, BFS.results$d,83)

plotly.parent <- function(o.world,world,name,cols) {
  gg <- melt.matrix(world) %>% 
          mutate(id = getid(x,y,ncol(world)), 
                 actual.value = as.vector(o.world), 
                 parent = value, x = x, y = y) %>% 
        ggplot(aes(x = as.factor(x), 
                   y = as.factor(y),
                   "X" = x,
                   "Y" = y,
                   "id" = id,
                   "parent" = parent,
                   "actual.value" = actual.value)) + 
          geom_raster(aes(fill = as.factor(value))) + 
          scale_fill_manual(values=cols) +
          geom_text(aes(label = ifelse(value == -1,"inf",value))) +
          theme(panel.background = element_blank()) + labs(x = "X",y = "Y") +
          ggtitle("Value of p, parent of each cell/vertex") + theme(legend.position = "none")
  
  ggplotly(gg, tooltip = c("X","Y","parent","id","actual.value"))
}

cols = rainbow(100, s=.6, v=.9)[sample(1:100,100)]
plotly.parent(figure10.2,BFS.results$p,"Figure 10.2",cols)

BFS.results <- BFS(figure10.2,adjlist.adaptive(figure10.2),97)
plotly.distance(figure10.2,BFS.results$d,97)

plotly.parent(figure10.2,BFS.results$p,"Figure 10.2",cols)
plotly.adjmatrix(figure10.2, "Figure 10.2")

adaptive.sample <- function(world, vertecies){
  world.adj <- adjlist.adaptive(world)
  samples <- data.frame("y" = numeric(0),"m" = numeric(0))
  for(i in vertecies){
    samples <- rbind(samples,BFS(world,world.adj,i)$adaptive.sample)
  }
  return(samples)
}

adaptive.sample.figure10.2 <- adaptive.sample(figure10.2, c(97,83,56,44,39))
adaptive.sample.figure10.2

adaptive.sample.confint.fun(adaptive.sample.figure10.2$y,
                            adaptive.sample.figure10.2$m,
                            nrow(adaptive.sample.figure10.2),
                            nrow(figure10.2)*ncol(figure10.2)
                            )[c("mu.hat","v.mu","lower","upper")]

bfs.97 <- BFS(figure10.2,figure10.2.adj,97)
bfs.97$adaptive.sample
bfs.97$sampled.squares

bfs.83 <- BFS(figure10.2,figure10.2.adj,83)
bfs.83$adaptive.sample
bfs.83$sampled.squares

bfs.56 <- BFS(figure10.2,figure10.2.adj,56)
bfs.56$adaptive.sample
bfs.56$sampled.squares

bfs.44 <- BFS(figure10.2,figure10.2.adj,44)
bfs.44$adaptive.sample
bfs.44$sampled.squares

bfs.29 <- BFS(figure10.2,figure10.2.adj,29)
bfs.29$adaptive.sample
bfs.29$sampled.squares

figure10.2.sampled.squares <- rbind(
  cbind(bfs.97$sampled.squares,"BFS" = 1),
  cbind(bfs.83$sampled.squares,"BFS" = 2),
  cbind(bfs.56$sampled.squares,"BFS" = 3),
  cbind(bfs.44$sampled.squares,"BFS" = 4),
  cbind(bfs.29$sampled.squares,"BFS" = 5)
)

plotly.sampled <- function(world,sampled.squares,name){
  cols = rainbow(100, s=.6, v=.9)[sample(1:100,100)]
  df <- melt.matrix(world) %>% 
          left_join(sampled.squares, by = "id")

  df$BFS[is.na(df$BFS)] <- 0
  
  gg <- df %>%
          ggplot(aes(x = as.factor(x), 
                     y = as.factor(y),
                     "X" = x,
                     "Y" = y,
                     "id" = id,
                     "value" = value,
                     "BFS" = BFS)) + 
          geom_raster(aes(fill = as.factor(BFS), alpha = ifelse(BFS == 0,0, 1))) + 
          # scale_fill_manual(values=cols) +
          geom_text(aes(label = ifelse(BFS == 0 & value == 0, 0, BFS))) +
          theme(panel.background = element_blank()) + labs(x = "X",y = "Y") +
          ggtitle(paste("BFS Groups of",name)) + theme(legend.position = "none")
  
  ggplotly(gg, tooltip = c("X","Y","value","BFS","id"))
}
plotly.sampled(figure10.2,sampled.squares,"Figure 10.2")

set.seed(58039847)
nrow <- 15
ncol <- 30
world.15.30 <- matrix(sample(c(rep(0,25),c(1:10)),
nrow*ncol,
replace = TRUE),
nrow,ncol, byrow = TRUE)

plotly.adjmatrix(world.15.30,"randomly generated 15x30 World w/ seed = 58039847")

world.15.30.adj <- adjlist.adaptive(world.15.30)
interesting.squares <- c(1,13,53,57,128,227,241,280,358)
adaptive.sample(world.15.30,interesting.squares)

get.sampled.squares <- function(world,to.sample){
  sampled.squares <- data.frame()
  world.adj <- adjlist.adaptive(world)
  for(i in c(1:length(to.sample))) {
    bfs <- BFS(world,world.adj,to.sample[i]) 
    a <- cbind(bfs$sampled.squares,"BFS" = i)
    sampled.squares <- rbind(sampled.squares,a)
  }
  return(sampled.squares)
}
sampled.squares <- get.sampled.squares(world.15.30,interesting.squares)
plotly.sampled(world.15.30,sampled.squares,"world")

set.seed(NULL) ## randomize the seed again
nrow <- sample(50:100,1)
ncol <- sample(50:100,1)
world <- matrix(sample(c(rep(0,25),c(1:10)),nrow*ncol,replace = TRUE),
nrow = nrow,ncol = ncol,byrow=TRUE)

gg <- world %>%
  melt.matrix() %>% ggplot(aes(x = as.factor(x), y = as.factor(y), label = getid(x,y,ncol), "X" = x, "Y" = y, "ID" = id)) +
  geom_raster(aes(fill = value, alpha = value/max(value))) +
  scale_fill_gradient(low="grey90",high="red") +
  theme(panel.background = element_blank()) + 
  labs(x = "X",y = "Y") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  legend.position = "none") + ggtitle(paste("random",nrow,"x",ncol,"world"))

ggplotly(gg, tooltip = c("X","Y","value","ID"))

squares.to.sample <- c(sample(which(as.vector(world)!=0),30),
sample(which(as.vector(world)==0),30))

adaptive.samples <- adaptive.sample(world, squares.to.sample)

sampled.squares <- get.sampled.squares(world,squares.to.sample)

cols = rainbow(100, s=.6, v=.9)[sample(1:100,100)]
cols[1] <- "white"

df <- melt.matrix(world) %>% 
left_join(sampled.squares, by = "id")

df$BFS[is.na(df$BFS)] <- 0

gg <- df %>%
  ggplot(aes(x = as.factor(x), 
             y = as.factor(y),
             "X" = x,
             "Y" = y,
             "id" = id,
             "value" = value,
             "BFS" = BFS)) + 
  geom_raster(aes(fill = as.factor(BFS))) + 
  scale_fill_manual(values=cols) +
  ggtitle(paste("BFS Groups of random",nrow,"x",ncol,"world")) + 
  labs(x = "X",y = "Y") +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggplotly(gg, tooltip = c("X","Y","value","BFS","id"))

adaptive.sample.confint.fun(
  adaptive.samples$y,
  adaptive.samples$m,
  nrow(adaptive.samples),
  nrow(world)*ncol(world)
)
    