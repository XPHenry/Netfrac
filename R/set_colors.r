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


#modifie automne 2018
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


