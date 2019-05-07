get_paths<-function(graph, nom_col1, nom_col2, opti,maxcores = 1){
  cl <- makeCluster(multicore(maxcores))
  registerDoParallel(cl=cl)
  
  ## Get vertex by colors
  graph = subgroup_graph(graph,c(nom_col1,nom_col2))
  col1 <- nom_col1
  col2 <- nom_col2
  
  mask.col1 <-which(V(graph)$tax == nom_col1)
  mask.col2 <-which(V(graph)$tax == nom_col2)
  v.col1 <-V(graph)[mask.col1] #all vertices of color 1
  v.col2 <-V(graph)[mask.col2] #all vertices of color 2
  
  # All vertices of color1 and 2 (Those vertices may or may not be in the graph, it depends on the network type)
  # (This does not affect the distance calculation for graph with no vertices of mix colors)
  if(col1 > col2){
    mix <-paste0(col2, col1, sep="")
  }else{
    mix <-paste0(col1, col2, sep="")
  }
  mask.mix <-which(V(graph)$tax == mix)
  v.mix <-V(graph)[mask.mix]
  paths = c()

  ## Shortest path search for all pair of same color vertex
  
  # Color 1
  if(length(v.col1) >=2){
    
    x.col1<-foreach(a=v.col1[-(length(v.col1))], i=icount(), .combine="+", .inorder=FALSE, .packages='igraph') %:%
      foreach(b=v.col1[(i+1):length(v.col1)], .combine="+", .inorder=FALSE, .packages='igraph') %dopar% { #for all color 1 vertices
        owarn <- getOption("warn")
        options(warn = -1) # Silence warnings for not reachable vertices
        # Get all shortest paths from the v.col1[i] vertex to v.col1[j] vertex
        # The $res attribute of paths contains all shortest paths (vertices sequence) calculated in a list.
        if (opti=="all"){
          paths = append(paths,get.all.shortest.paths(graph, a, b, mode= "all")$res)
        } else if(opti == "single"){
          paths = append(paths,get.shortest.paths(graph, a, b, mode= "all")$vpath)
        }
        options(warn = owarn)
      }
  }

  if(length(v.col2) >=2){
          
    x.col2<-foreach(a=v.col2[-(length(v.col2))], i=icount(), .combine="+", .inorder=FALSE, .packages='igraph') %:%
      foreach(b=v.col2[(i+1):length(v.col2)], .combine="+", .inorder=FALSE, .packages='igraph') %dopar% { #for all color 2 vertices
        owarn <- getOption("warn")
        options(warn = -1) # Silence warnings for not reachable vertices
        # Get all shortest paths from the v.col2[i] vertex to v.col2[j] vertex
        # The $res attribute of paths contains all shortest paths (vertices sequence) calculated in a list.
        #paths = get.all.shortest.paths(graph, a, b, mode= "all")$res
        if (opti=="all"){
          paths = append(paths,get.all.shortest.paths(graph, a, b, mode= "all")$res)
        } else if(opti == "single"){
          paths = append(paths,get.all.shortest.paths(graph, a, b, mode= "all")$res)
        }
        options(warn = owarn)
      }
        }
  stopCluster(cl)
  return(paths)
}