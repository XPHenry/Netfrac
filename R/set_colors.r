#==========================================================================================================================================================
#= Function to set the vertex colors of a graph for plotting a graph
#=
#= Input: an igraph with colors as tax attribute (V(graph)$tax)
#= Output: a modified igraph object
#=
#= Requires SDDE and igraph packages
#=
#= Ex: graph<-set_color(graph)
#=     plot(graph)
#==========================================================================================================================================================


#modifie automne 2019
#' Set colors for the communities in the network
#'
#' Visualizes the network by adding different colors to different species communities, when plotting
#' either with R own color list or a provided color list.
#'
#' @param graph The igraph object used to define colors. The nodes accessed with V() should have
#' a $tax attribute which links them to a community, so a different color can be assigned to each community.
#' @param colors_list Optional; the colors can be defined in a list with the community names defined with names().
#' Otherwise, the function will return the numbers already associated to R colors.
#'
#' @return
#' @export
#'
#' @examples
#' net_a = load_network("exemples/network_a.txt","exemples/network_a_tax.txt")
#' net_a = set_color(net_a)
#' plot(net_a)
set_color<- function(graph, colors_list=""){
  if (colors_list != ""){
    for (i in 1:length(V(graph))){
      V(graph)[i]$color = colors_list[V(graph)[i]$tax]
    }
  }
  else{
    V(graph)$color = V(graph)$tax
    color <-levels(as.factor(V(graph)$tax))

    for (i in 1:length(color)){
      V(graph)$color = replace(V(graph)$color, which(V(graph)$color==color[i]), i)
    }
  }
  return(graph)
}

#how to make a foo color_list

#foo_TetA = c("orangered","pink","teal","green")
#names(foo_TetA) = c("Gammaproteobacteria","Other","Actinobacteria","Bacilli")


