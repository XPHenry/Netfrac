#========================================
#= create subgraph or 2 groups from a complex graph
#========================================
#assuming that we already have an igragh object with group information
#' Create a subgraph based on chosen species communities
#'
#' Creates another igraph object containing only the nodes from some chosen species
#' communities (two or more), and the edges connecting them.
#'
#' @param graph The igraph object
#' @param groups The communities that should be isolated, indicated as a list of two items
#'
#' @return
#' @export
#'
#' @examples
#' CAT_env = load_network("exemples/CAT_edges_env.csv","exemples/CAT_node_env.csv")
#' CAT_env1 = set_color(subgroup_graph(CAT_env, c("host", "ubiquitous")))
#' plot(CAT_env1)
subgroup_graph <- function(graph, groups){
  subvertice <- V(graph)[V(graph)$tax %in% groups]
  graph <- induced.subgraph(graph, subvertice)
  return (graph)
}

