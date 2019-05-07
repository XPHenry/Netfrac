#========================================
#= create subgraph or 2 groups from a complex graph
#========================================
library(SDDE)
library(igraph)

#assuming that we already have an igragh object with group information
#' subgroup_graph
#'
#' @param graph
#' @param groups
#'
#' @return
#' @export
#'
#' @examples
subgroup_graph <- function(graph, groups){
  subvertice <- V(graph)[V(graph)$tax %in% groups]
  graph <- induced.subgraph(graph, subvertice)
  return (graph)
}

