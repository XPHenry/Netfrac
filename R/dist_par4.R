#==========================================================================================================================================================
#= Function to calculate dist_paths with doParallel
#=
#= Input: graph, an undirected igraph with color levels (V(graph)$tax)
#=        nom_col1, character string that indicates the color 1 attribute
#=		  nom_col2, character string that indicates the color 2 attribute
#=
#= Output: list of distances values in the following order : dpr,dl,dlpr,dnpr
#=
#= All shortest paths are calculated between vertices of same colors
#= Requires SDDE, igraph, foreach and doParallel packages
#= Multi-core backend has to be set
#=
#= Ex:
#= Works only for two colors graph
#==========================================================================================================================================================

#modifie en septembre 2018 - henry
number_transfer_gen <- function(d, n){
  a = 1
  b = 2-n
  c = n*n -n - d*n*n + d*n
  delta = b*b - 4*a*c
  k = (-b - sqrt(delta))/2
  return(k)
}
number_transfer_1 <- function(d=0, n){
  if (d == 0){
    k = n
  } else {
    k = (-1 + 2*n - sqrt(4*d*n*n - 4*d*n +1))/2
  }
  return(k/n)
}

shortest_paths_graph <- function(graph,v1,v2,v.mix,col1,distance,opti,mean_w,max_w){
  # x.col1<-foreach(a=v1[-(length(v1))], i=icount(), .combine="+", .inorder=FALSE, .packages='igraph') %:%
  #   foreach(b=v1[(i+1):length(v1)], .combine="+", .inorder=FALSE, .packages='igraph') %dopar% { #for all color 1 vertices
  #     owarn <- getOption("warn")
  #     options(warn = -1) # Silence warnings for not reachable vertices
  #     # Get all shortest paths from the v.col1[i] vertex to v.col1[j] vertex
  #     # The $res attribute of paths contains all shortest paths (vertices sequence) calculated in a list.
  #     if (opti == "all"){
  #       paths = get.all.shortest.paths(graph, a, b, mode= "all")$res
  #     } else if(opti == "single"){
  #       paths = get.shortest.paths(graph, a, b, mode= "all")$vpath
  #     }
  #     options(warn = owarn)
  #tic("shortestPath")
  # x.col1 <- c()
  # for(i in 1:length(v1)){
  #   short_list <- shortest_paths(graph,v1[i],v1[i:length(v1)],"all")$vpath
  #   x.col1 <- append(x.col1,short_list)
  #
  # }
  x.col1_list <- c()
  #====== Calculation of all the shortest paths in one community
  x.col1 <- lapply(v1,function(x) shortest_paths(graph,x,v1[which(x == v1):length(v1)],"all")$vpath)

  for(node in x.col1){
    node = node[-1]
    x.col1_list <- append(x.col1_list,node)
  }
  x.col1 <- x.col1_list

  #toc()
  #====== Parameters initialization
  #tic("intersect")
  #dpr
  dpr_mix_color_paths=0
  dpr_share_paths = 0
  dpr_monocolor_paths=0

  #dnpr
  dnpr_no_v=0
  dnpr_mono_v=0
  dnpr_mix_v=0
  dnpr_mix = 0
  dnpr_prop = 0
  #dl
  dl_mix_edges=0
  dl_mono_edges=0
  dl_prop = 0

  #dlpr
  dlpr_mono_edges=0
  dlpr_mix_edges=0
  dlpr_prop = 0

  #=========== Distance Calculation
  for(path in x.col1){
  # if(length(paths)!=0){ #If there is at least one shortest path
  #   for(k in 1:length(paths)){
  #     path = paths[[k]]
    if (length(path) < 2){
      next
    }
    v_path_mix = c()
    # Associate the path vertices with their colors
    # if (distance == "transfer"){
    #   for (node in path){
    #     v_path_col1 <- c()
    #     if (node %in% v1){
    #       v_path_col1 <- append(v_path_col1,node)
    #       }
    #     else{
    #       dpr_share_paths = dpr_share_paths + 1.0
    #       break
    #       }
    #     }
    #   }
    # else{
      v_path_col1 = intersect(path, v1)
      v_path_col2 = intersect(path, v2)
      v_path_mix = intersect(path, v.mix)
      # }
    #=========== Spp distance

    # Check if there is at least one vertex of a different color as the start and end vertex

    if (length(v_path_col1) == length(path) && length(path)>1){
      #dpr_monocolor_paths = dpr_monocolor_paths +1
      dpr_monocolor_paths = dpr_monocolor_paths + 1.0
    } else if(length(v_path_mix) != 0) {
      dpr_mix_color_paths = dpr_mix_color_paths + 1.0
    } else if(length(v_path_col2) != 0){
      dpr_share_paths = dpr_share_paths + 1.0
    }

    #=========== Spinp distance
    #===== Remove start vertex and end vertex
    path_i = path[-length(path)]
    path_i = path_i[-1]

    #===== Associate the path vertices with their colors

    v_path_i_col1 = intersect(path_i, v1)
    v_path_i_col2 = intersect(path_i, v2)

    v_path_i_mix = intersect(path_i, v.mix)

    if(length(path_i) == 0){ # If there is no vertex in the path besides the start and end vertices
      #dnpr_no_v = dnpr_no_v +1
      dnpr_no_v = 1.0
    }else if(length(v_path_i_col2) != 0){
      #dnpr_mono_v = dnpr_mono_v + length(v_path_i_col1)
      dnpr_prop = 1.0*length(v_path_i_col1)/(length(v_path_i_col1)+length(v_path_i_col2))
    }else if(length(v_path_i_mix)!= 0){
      # Number of not col1 vertices in shortest path
      #dnpr_mix_v = dnpr_mix_v + length(v_path_i_col2) + length(v_path_i_mix)
      dnpr_mix = dnpr_mix + (1.0*length(v_path_i_col2) + length(v_path_i_mix))

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
      # dl_prop = dl_prop + (1.0*dl_mono_edges/(dl_mono_edges+dl_mix_edges))/length(paths)
      # dlpr_prop = dlpr_prop + (1.0*dlpr_mono_edges/(dlpr_mono_edges+dlpr_mix_edges))/length(paths)
      dl_prop = dl_prop + (1.0*dl_mono_edges/(dl_mono_edges+dl_mix_edges))
      dlpr_prop = dlpr_prop + (1.0*dlpr_mono_edges/(dlpr_mono_edges+dlpr_mix_edges))
      }
    }
    #toc()
    if(distance == "Spp" || distance == "transfer"){
      return(c(dpr_share_paths, dpr_monocolor_paths, dpr_mix_color_paths))
    }
    res = c(dpr_share_paths, dpr_monocolor_paths, dpr_mix_color_paths, dnpr_mix_v, dl_mix_edges, dl_prop, dlpr_prop, dlpr_mix_edges)
    return(res)
}

dist_par<-function(igraph, nom_col1, nom_col2, mat, distance,opti="single",maxcores=1,share_w){
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

  #graph = subgroup_graph(igraph,c(nom_col1,nom_col2,V(igraph)$tax[v.mix],V(igraph)$tax[v.mix2]))
  graph = subgroup_graph(igraph,c(nom_col1,nom_col2))
  m_weight = mean(E(graph)$weight)
  max_weight = sum(E(graph)$weight)
  if (components(graph)$no > 1){
    if (mat == ""){
      graph = reconnect_btw(graph)
    }
    else{
      graph = reconnect(graph,matrice = mat)
    }

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

  path=distance
  # Color 1
  if(length(v.col1) >=2){
    x.col1 = shortest_paths_graph(graph,v.col1,v.col2,v.mix,col1,path,opti,m_weight,max_weight)
  }

  v.col1 <-V(graph)[mask.col1]
  v.col2 <- append(v.col2,V(graph)[v.mix2])
  # Color 2
  if(length(v.col2) >=2){
    x.col2 = shortest_paths_graph(graph,v.col2,v.col1,v.mix2,col2,path,opti,m_weight,max_weight)
  }

  # For when col1 or col2 <= 1
  if(!exists("x.col1")){
    x.col1=c(rep(0,8))
  }
  if(!exists("x.col2")){
    x.col2=c(rep(0,8))
  }

  #the denominator is the same for every measure, because it becomes a proportion
  #num_paths = (length(v.col1)*(length(v.col1)-1)+length(v.col2)plot(eso*(length(v.col2)-1))/2

  #this is because not every node is connected (there might be many islands in the graph), so we use the number of paths that Spp found
  num_paths = x.col1[1]+x.col1[2]+x.col1[3]+x.col2[1]+x.col2[2]+x.col2[3]

  # 	dpr = (x.col1[2]+x.col2[2])/(x.col1[2]+x.col2[2]+x.col1[1]+x.col2[1])
  #
  # 	dnpr = (x.col1[5]+x.col2[5])/(x.col1[3]+x.col2[3]+x.col1[4]+x.col2[4]+x.col1[5]+x.col2[5])
  # 	dl = (x.col1[7]+x.col2[7])/(x.col1[6]+x.col2[6]+x.col1[7]+x.col2[7])
  # 	dlpr = (x.col1[8]+x.col2[8])/(x.col1[8]+x.col2[8]+x.col1[9]+x.col2[9])
  #   return(c(x=x.col1,y=x.col2))
  if(share_w == 1){
    dpr = (1.0*x.col1[2]+x.col2[2]+x.col1[3]+x.col2[3])/num_paths
  }else if (share_w == 0){
    dpr = (1.0*x.col1[2]+x.col2[2])/num_paths
  }
  dnpr = (1.0*x.col1[4]+x.col2[4])/num_paths
  dl = (1.0*x.col1[6]+x.col2[6])/num_paths
  dlpr = 1.0*(x.col1[7]+x.col2[7])/num_paths

  #Calculation for the transfer distance

  transfer_dir = 1.0*x.col1[2]/(x.col1[1]+x.col1[2])
  transfer_rev = 1.0*x.col2[2]/(x.col2[1]+x.col2[2])
  stopCluster(cl)

  if(path == "Spp"){
    return(c(Spp=dpr))
  } else if(distance == "paths"){
    return(c(Spp=dpr,Spep=dl,Spelp=dlpr,Spinp=dnpr, direct = transfer_dir, reverse = transfer_rev))
  } else if(distance == "transfer"){
    return(c(direct = transfer_dir, reverse = transfer_rev, abs_transfer1 = number_transfer_1(transfer_dir,length(v.col1)), abs_transfer2 = number_transfer_1(transfer_rev,length(v.col2))))
  }
}
