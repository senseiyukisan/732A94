#' @title Dijkstra
#' @param graph A data frame with three variables that contain same-sized vectors describing nodes, edges and wieghts of a graph
#' @param init_node The node from which the shortest distance to all other nodes is calculated
#' @description Implementation of Djikstra's algorithm to find the shortest path to all nodes from an initial node using an adjacency matrix
#' @return A vector of the distances from init_node to all other nodes
#' @references \url{https://en.wikipedia.org/wiki/Dijkstras_algorithm} and \url{https://www.youtube.com/watch?v=Ww4nU_NcAIQ}
#' @export
dijkstra <- function(graph, init_node){
  #assert
  stopifnot(init_node %in% graph[["v1"]])
  stopifnot(length(graph) == 3)
  stopifnot(is.numeric(unlist(graph[[1]])) && is.numeric(unlist(graph[[2]])) && is.numeric(unlist(graph[[3]])))
  stopifnot(length(graph[[1]]) == length(graph[[2]]) && length(graph[[1]]) == length(graph[[3]]))
  
  #initial vectors and unique nodes visited
  nodes <- c(unique(graph[[1]]))
  nodes_to_visit <- nodes
  distance <- numeric(length = length(nodes))
  distance[] <- Inf
  # visited <- c()
  
  #adjacency matrix
  adj_matrix <- matrix(Inf, nrow = length(nodes), ncol = length(nodes))
  colnames(adj_matrix) <- nodes
  rownames(adj_matrix) <- nodes
  
  for(i in 1:length(graph[[1]])){
    adj_matrix[graph[[2]][i], graph[[1]][i]] <- graph[[3]][i]
  }
  diag(adj_matrix) <- 0
  
  #matrix loop/dijkstra's algorithm
  distance[init_node] <- 0
 
  while(length(nodes_to_visit != 0)){
    u <- which.min(distance[nodes_to_visit])
    u_actual <- nodes_to_visit[u]
    for (v in nodes_to_visit){
      alt <- distance[u_actual] + adj_matrix[u_actual,v]
      if (alt < distance[v]){
        distance[v] <- alt
      }
    }
    visited <- c(visited, u_actual)
    nodes_to_visit <- nodes_to_visit[-u] 
  }
  #return distance vector
  # print(visited)
  return(distance)
}
wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6), v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5), w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)

