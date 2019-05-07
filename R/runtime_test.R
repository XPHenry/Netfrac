all_paths <- 0
shortest_paths_graph <- function(graph,v1,v2,v.mix,col1,col2,opti,mean_w,max_w){
  x.col1<-foreach(a=v1[-(length(v1))], i=icount(), .combine="+", .inorder=FALSE, .packages='igraph') %:%
    foreach(b=v1[(i+1):length(v1)], .combine="+", .inorder=FALSE, .packages='igraph') %dopar% { #for all color 1 vertices
      owarn <- getOption("warn")
      options(warn = -1) # Silence warnings for not reachable vertices
      # Get all shortest paths from the v.col1[i] vertex to v.col1[j] vertex
      # The $res attribute of paths contains all shortest paths (vertices sequence) calculated in a list.
      if (opti == "all"){
        paths = get.all.shortest.paths(graph, a, b, mode= "all")$res
      } else if(opti == "single"){
        paths = get.shortest.paths(graph, a, b, mode= "all")$vpath
      }
      options(warn = owarn)
      #all_paths <- all_paths + 1
    }
  return(paths)
}

shortest_paths_graph_2 <- function(graph,v1,v2,v.mix,col1,col2,opti,mean_w,max_w){
  x.col1<-foreach(a=v1[-(length(v1))], i=icount(), .combine="+", .inorder=FALSE, .packages='igraph') %:%
    foreach(b=v1[(i+1):length(v1)], .combine="+", .inorder=FALSE, .packages='igraph') %dopar% { #for all color 1 vertices
      owarn <- getOption("warn")
      options(warn = -1) # Silence warnings for not reachable vertices
      # Get all shortest paths from the v.col1[i] vertex to v.col1[j] vertex
      # The $res attribute of paths contains all shortest paths (vertices sequence) calculated in a list.
      if (opti == "all"){
        paths = get.all.shortest.paths(graph, a, b, mode= "all")$res
      } else if(opti == "single"){
        paths = get.shortest.paths(graph, a, b, mode= "all")$vpath
      }
      options(warn = owarn)

      #Parameters initialization

      #dpr
      dpr_mix_color_paths=0
      dpr_share_paths = 0
      dpr_monocolor_paths=0

      #dnpr
      dnpr_no_v=0
      dnpr_mono_v=0
      dnpr_mix_v=0
      dnpr_mix = 0
      #dl
      dl_mix_edges=0
      dl_mono_edges=0
      dl_prop = 0

      #dlpr
      dlpr_mono_edges=0
      dlpr_mix_edges=0
      dlpr_prop = 0

      #=========== Distance Calculation
      if(length(paths)!=0){ #If there is at least one shortest path
        for(k in 1:length(paths)){
          path = paths[[k]]
          if (length(path) < 2){
            next
          }
          # Associate the path vertices with their colors
          v_path_col1 = intersection(path, v1)
          v_path_col2 = intersection(path, v2)
          v_path_mix = intersection(path, v.mix)

          #=========== Spp distance

          # Check if there is at least one vertex of a different color as the start and end vertex

          # if( (length(v_path_col2) != 0) || (length(v_path_mix) != 0) ){
          #   #dpr_mix_color_paths = dpr_mix_color_paths +1
          #   dpr_mix_color_paths = dpr_mix_color_paths + (1/length(paths))
          # } else if (length(v_path_col1) == length(path)){
          #   #dpr_monocolor_paths = dpr_monocolor_paths +1
          #   dpr_monocolor_paths = dpr_monocolor_paths + (1/length(paths))
          # }
          if (length(v_path_col1) == length(path) && length(path)>1){
            #dpr_monocolor_paths = dpr_monocolor_paths +1
            dpr_monocolor_paths = dpr_monocolor_paths + (1.0/length(paths))
          } else if(length(v_path_mix) != 0) {
            dpr_share_paths = dpr_share_paths + (1.0/length(paths))
          } else if(length(v_path_col2) != 0){
            dpr_mix_color_paths = dpr_mix_color_paths + (1.0/length(paths))
          }

          dnpr_no_v= 0
          dnpr_mono_v=0
          dnpr_mix = 0
          dnpr_prop = 0

          #=========== Spinp distance
          # Remove start vertex and end vertex
          path_i = path[-length(path)]
          path_i = path_i[-1]

          # Associate the path vertices with their colors
          v_path_i_col1 = intersection(path_i, v1)
          v_path_i_col2 = intersection(path_i, v2)
          v_path_i_mix = intersection(path_i, v.mix)

          if(length(path_i) == 0){ # If there is no vertex in the path besides the start and end vertices
            #dnpr_no_v = dnpr_no_v +1
            dnpr_no_v = 1.0/length(paths)
          }else if(length(v_path_i_col2) != 0){
            #dnpr_mono_v = dnpr_mono_v + length(v_path_i_col1)
            dnpr_prop = 1.0*length(v_path_i_col1)/(length(v_path_i_col1)+length(v_path_i_col2))
          }else if(length(v_path_i_mix)!= 0){
            # Number of not col1 vertices in shortest path
            #dnpr_mix_v = dnpr_mix_v + length(v_path_i_col2) + length(v_path_i_mix)
            dnpr_mix = dnpr_mix + (1.0*length(v_path_i_col2) + length(v_path_i_mix))/length(paths)

          }
          #calculate the proportion of the nodes same color over all nodes
          dnpr_mix_v = dnpr_mix_v + (1.0*dnpr_prop/length(paths))+ dnpr_no_v

          #=========== Spep and Spelp distance



          if(length(path) >1){
            dl_mix_edges=0
            dl_mono_edges=0

            #dlpr
            dlpr_mono_edges=0
            dlpr_mix_edges=0
            for(l in 1:length(path)){
              if((l+1) <= length(path)){
                # check the end vertices (path[l] and path[l+1]) of each edge of the path
                if( (V(graph)[path[l]]$tax == col1) & (V(graph)[path[l+1]]$tax == col1) ){
                  #dl_mono_edges = dl_mono_edges+1
                  #dlpr_mono_edges = dlpr_mono_edges + E(graph)[ V(graph)[path[l]] %--% V(graph)[path[l+1]] ]$weight

                  dl_mono_edges = dl_mono_edges+ 1
                  if (E(graph)[ V(graph)[path[l]] %--% V(graph)[path[l+1]] ]$weight != max_w){
                    dlpr_mono_edges = dlpr_mono_edges + (E(graph)[ V(graph)[path[l]] %--% V(graph)[path[l+1]] ]$weight)
                  }else{
                    dlpr_mono_edges = dlpr_mono_edges + mean_w
                  }
                }else{
                  #dl_mix_edges = dl_mix_edges+1
                  #dlpr_mix_edges = dlpr_mix_edges + E(graph)[ V(graph)[path[l]] %--% V(graph)[path[l+1]] ]$weight

                  dl_mix_edges = dl_mix_edges+ 1
                  if (E(graph)[ V(graph)[path[l]] %--% V(graph)[path[l+1]] ]$weight != max_w){
                    dlpr_mix_edges = dlpr_mix_edges + (E(graph)[ V(graph)[path[l]] %--% V(graph)[path[l+1]] ]$weight)
                  }else{
                    dlpr_mix_edges = dlpr_mix_edges + mean_w
                  }
                }
              }
            }
            #calculate the proportion of monochrome edges
            dl_prop = dl_prop + (1.0*dl_mono_edges/(dl_mono_edges+dl_mix_edges))/length(paths)
            dlpr_prop = dlpr_prop + (1.0*dlpr_mono_edges/(dlpr_mono_edges+dlpr_mix_edges))/length(paths)
          }
        }
      }

      res = c(dpr_mix_color_paths, dpr_monocolor_paths, dpr_share_paths, dnpr_mix_v, dl_mix_edges, dl_prop, dlpr_prop, dlpr_mix_edges)

    }
  return(x.col1)
}

dist_par<-function(igraph, nom_col1, nom_col2,distance,opti="single",maxcores=1,share_w){
  tic("total")
  tic("graph init")
  cl <- makeCluster(multicore(maxcores))
  registerDoParallel(cl=cl)

  #modifie juillet 2018 par henry
  #find the nodes with mixed communities
  col1_mix <- paste(append(nom_col1,"-"),collapse = "")
  col1_mix2 <- paste(append("-",nom_col1),collapse = "")

  #find the ones that contain the communities in this iteration
  v.mix <- grep(col1_mix,V(igraph)$tax)
  v.mix <- append(v.mix,grep(col1_mix2,V(igraph)$tax))

  #same for second community
  col2_mix <- paste(append(nom_col2,"-"),collapse = "")
  col2_mix2 <- paste(append("-",nom_col2),collapse = "")
  v.mix2 <- grep(col1_mix,V(igraph)$tax)
  v.mix2 <- append(v.mix2,grep(col1_mix2,V(igraph)$tax))

  graph = subgroup_graph(igraph,c(nom_col1,nom_col2,V(igraph)$tax[v.mix],V(igraph)$tax[v.mix2]))
  m_weight = mean(E(graph)$weight)
  max_weight = sum(E(graph)$weight)
  if (components(graph)$no > 1){
    graph = reconnect(graph)
  }
  #print(V(graph))

  ## Get vertex by colors
  col1 <- nom_col1
  col2 <- nom_col2
  mask.col1 <-which(V(graph)$tax == col1)
  mask.col2 <-which(V(graph)$tax == col2)
  v.mix <- grep(col_mix,V(graph)$tax)
  v.col1 <-V(graph)[mask.col1] #all vertices of color 1 and mix
  v.col1 <- append(v.col1,V(graph)[v.mix])
  v.col2 <-V(graph)[mask.col2] #all vertices of color 2
  #print(v.col1)
  #print(v.col2)

  #cat(v.col1,v.col2)
  # All vertices of color1 and 2 (Those vertices may or may not be in the graph, it depends on the network type)
  # (This does not affect the distance calculation for graph with no vertices of mix colors)
  # if(col1 > col2){
  # 	mix <-paste0(col2, col1, sep="")
  # }else{
  # 	mix <-paste0(col1, col2, sep="")
  # }
  # mask.mix <-which(V(graph)$tax == mix)
  # v.mix <-V(graph)[mask.mix]

  ## Shortest path search for all pair of same color vertex
  toc()
  tic("shortest paths")

  # Color 1
  if(length(v.col1) >=2){
    x.col1 = shortest_paths_graph(graph,v.col1,v.col2,v.mix,col1,col2,opti,m_weight,max_weight)
  }

  v.col1 <-V(graph)[mask.col1]
  v.col2 <- append(v.col2,V(graph)[v.mix2])
  # Color 2
  if(length(v.col2) >=2){
    x.col2 = shortest_paths_graph(graph,v.col2,v.col1,v.mix2,col2,col1,opti,m_weight,max_weight)
  }
  toc()
  tic("shortest_path and dist calc")
  if(length(v.col1) >=2){
    x.col3 = shortest_paths_graph_2(graph,v.col1,v.col2,v.mix,col1,col2,opti,m_weight,max_weight)
  }

  v.col1 <-V(graph)[mask.col1]
  v.col2 <- append(v.col2,V(graph)[v.mix2])
  # Color 2
  if(length(v.col2) >=2){
    x.col4 = shortest_paths_graph_2(graph,v.col2,v.col1,v.mix2,col2,col1,opti,m_weight,max_weight)
  }
  toc()
  tic("finishing")
  stopCluster(cl)
  toc()
  toc()
  return(c(x.col1,x.col2,x.col3,x.col4))

}

test_netr2 = dist_par(netr3,"B","C","paths")
#test_eso = dist_par(eso_BC,"B","C","paths",maxcores = 4)




#=====================
vert = V(eso_BC)

tic("apply")
x.col1_list2 <- c(rep(NULL,684))
x.col1_list2 <- lapply(vert,function(x) shortest_paths(reconnect(eso_BC),vert[x],vert[x:length(vert)],"all")$vpath)
toc()

tic("loop")
x.col1_list <- c()
for(i in 1:length(vert)){
  short_list <- shortest_paths(graph,vert[i],vert[i:length(vert)],"all")$vpath
  x.col1_list <- append(x.col1_list,short_list)

}
toc()

tic("apply")
x.col1_list2 <- lapply(vert,function(x) shortest_paths(eso_r,vert[x],vert,"all")$vpath)
toc()
