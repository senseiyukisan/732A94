djikstra <- function(graph, init_node){
  #assert dataframe of the correct form
  stopifnot(init_node %in% graph[["v1"]])
  stopifnot(length(graph) == 3)
  stopifnot(is.numeric(unlist(graph[[1]])) && is.numeric(unlist(graph[[2]])) && is.numeric(unlist(graph[[3]])))
  stopifnot(length(graph[[1]]) == length(graph[[2]]) && length(graph[[1]]) == length(graph[[3]]))
  
  #initial setup of vectors and unique nodes visited
  nodes <- c(unique(graph[[1]]))
  nodes_to_visit <- nodes
  distance <- numeric(length = length(nodes))
  distance[] <- Inf
  visited <- c()
  
  #set up of adjacency matrix
  adj_matrix <- matrix(Inf, nrow = length(nodes), ncol = length(nodes))
  colnames(adj_matrix) <- nodes
  rownames(adj_matrix) <- nodes
  
  for(i in 1:length(graph[[1]])){
    adj_matrix[graph[[2]][i], graph[[1]][i]] <- graph[[3]][i]
  }
  diag(adj_matrix) <- 0
  
  #matrix loop/djikstra's algorithm
  #initialize distance and visited nodes
  distance[init_node] <- 0
  #algorithm
  while(length(nodes_to_visit != 0)){
    u <- which.min(distance)
    for (v in nodes_to_visit){
      alt <- distance[u] + adj_matrix[v,u]
      if (alt < distance[v]){
        distance[v] <- alt
      }
    }
    visited <- c(visited, u)
    nodes_to_visit <- nodes_to_visit[-u] #removes the node just visited from being considered
  }
  #return distance vector
  return(distance)
}