#==========================================================================================================================================================
#= Function to calculate dist_paths with doParallel
#=
#= Input: graph, an undirected igraph with color levels (V(graph)$tax)
#=        nom_col1, character string that indicates the color 1 attribute
#=		  nom_col2, character string that indicates the color 2 attribute
#=
#= Output: list of distances values in the following order : dpr,dn,dl,dlpr,dnpr
#=
#= All shortest paths are calculated between vertices of same colors
#= Requires SDDE, igraph, foreach and doParallel packages
#= Multi-core backend has to be set
#=
#= Ex:
#= Works only for two colors graph
#==========================================================================================================================================================

dist_par<-function(graph, nom_col1, nom_col2, distance, opti){

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
				  paths = get.all.shortest.paths(graph, a, b, mode= "all")$res
				} else if(opti == "single"){
				  paths = get.shortest.paths(graph, a, b, mode= "all")$vpath
				}
				options(warn = owarn)

				#Parameters initialization

					#dpr
					dpr_mix_color_paths=0
					dpr_monocolor_paths=0

					#dn
					dn_num=0
					dn_denom=0

					#dnpr
					dnpr_no_v=0
					dnpr_mono_v=0
					dnpr_mix_v=0

					#dl
					dl_mix_edges=0
					dl_mono_edges=0

					#dlpr
					dlpr_mono_edges=0
					dlpr_mix_edges=0


				#=========== Distance Calculation
				if(length(paths)!=0){ #If there is at least one shortest path
					for(k in 1:length(paths)){
						path = paths[[k]]

						# Associate the path vertices with their colors
						v_path_col1 = intersection(path, v.col1)
						v_path_col2 = intersection(path, v.col2)
						v_path_mix = intersection(path, v.mix)

						#=========== dpr and dn distance

							# Check if there is at least one vertex of a different color as the start and end vertex
							if( (length(v_path_col2) != 0) || (length(v_path_mix) != 0) ){
								dpr_mix_color_paths = dpr_mix_color_paths +1

								dn_num = dn_num +1
								dn_denom = dn_denom +1

							} else if (length(v_path_col1)>0){
								dpr_monocolor_paths = dpr_monocolor_paths +1

								dn_denom = dn_denom +1
							}

						#=========== dnpr distance
							# Remove start vertex and end vertex
							path_i = path[-length(path)]
							path_i = path_i[-1]

							# Associate the path vertices with their colors
							v_path_i_col1 = intersection(path_i, v.col1)
							v_path_i_col2 = intersection(path_i, v.col2)
							v_path_i_mix = intersection(path_i, v.mix)

							if(length(path_i) == 0){ # If there is no vertex in the path besides the start and end vertices
								dnpr_no_v = dnpr_no_v +1
							}else{
								dnpr_mono_v = dnpr_mono_v + length(v_path_i_col1)

								# Number of not col1 vertices in shortest path
								dnpr_mix_v = dnpr_mix_v + length(v_path_i_col2) + length(v_path_i_mix)
							}

						#=========== dl and dlpr distance
							if(length(path) !=0){
								for(l in 1:length(path)){
									if( (l+1) <= length(path)){
										# check the end vertices (path[l] and path[l+1]) of each edge of the path
										if( (V(graph)[path[l]]$tax == col1) & (V(graph)[path[l+1]]$tax == col1) ){
											dl_mono_edges = dl_mono_edges+1
											dlpr_mono_edges = dlpr_mono_edges + E(graph)[ V(graph)[path[l]] %--% V(graph)[path[l+1]] ]$weight
										}else{
											dl_mix_edges = dl_mix_edges+1
											dlpr_mix_edges= dlpr_mix_edges+ E(graph)[ V(graph)[path[l]] %--% V(graph)[path[l+1]] ]$weight
										}
									}
								}
							}
					}
				}

				res = c(dpr_mix_color_paths, dpr_monocolor_paths, dn_num, dn_denom, dnpr_no_v, dnpr_mono_v, dnpr_mix_v, dl_mix_edges, dl_mono_edges, dlpr_mono_edges, dlpr_mix_edges)
				res
		}
	}

	# Color 2
	if(length(v.col2) >=2){

		x.col2<-foreach(a=v.col2[-(length(v.col2))], i=icount(), .combine="+", .inorder=FALSE, .packages='igraph') %:%
					foreach(b=v.col2[(i+1):length(v.col2)], .combine="+", .inorder=FALSE, .packages='igraph') %dopar% { #for all color 2 vertices
				owarn <- getOption("warn")
				options(warn = -1) # Silence warnings for not reachable vertices
				# Get all shortest paths from the v.col2[i] vertex to v.col2[j] vertex
				# The $res attribute of paths contains all shortest paths (vertices sequence) calculated in a list.
				#paths = get.all.shortest.paths(graph, a, b, mode= "all")$res
				if (opti=="all"){
				  paths = get.all.shortest.paths(graph, a, b, mode= "all")$res
				} else if(opti == "single"){
				  paths = get.shortest.paths(graph, a, b, mode= "all")$vpath
				}
				options(warn = owarn)

				#Parameters initialization

					#dpr
					dpr_mix_color_paths=0
					dpr_monocolor_paths=0

					#dn
					dn_num=0
					dn_denom=0

					#dnpr
					dnpr_no_v=0
					dnpr_mono_v=0
					dnpr_mix_v=0

					#dl
					dl_mix_edges=0
					dl_mono_edges=0

					#dlpr
					dlpr_mono_edges=0
					dlpr_mix_edges=0


				#=========== Distance Calculation
				if(length(paths)!=0){ #If there is at least one shortest path
					for(k in 1:length(paths)){
						path = paths[[k]]

						# Associate the path vertices with their colors
						v_path_col1 = intersection(path, v.col1)
						v_path_col2 = intersection(path, v.col2)
						v_path_mix = intersection(path, v.mix)

						#=========== dpr and dn distance

							# Check if there is at least one vertex of a different color as the start and end vertex
							if( (length(v_path_col1) != 0) || (length(v_path_mix) != 0) ){
								dpr_mix_color_paths = dpr_mix_color_paths +1

								dn_num = dn_num + 1
								dn_denom = dn_denom + 1
							} else if (length(v_path_col2)>0){
								dpr_monocolor_paths = dpr_monocolor_paths + 1

								dn_denom = dn_denom + 1
							}

						#=========== dnpr distance

							# Remove start vertex and end vertex
							path_i = path[-length(path)]
							path_i = path_i[-1]

							# Associate the path vertices with their colors
							v_path_i_col1 = intersection(path_i, v.col1)
							v_path_i_col2 = intersection(path_i, v.col2)
							v_path_i_mix = intersection(path_i, v.mix)

							if(length(path_i) == 0){ # If there is no vertex in the path besides the start and end vertices
								dnpr_no_v = dnpr_no_v + 1
							}else{
								dnpr_mono_v = dnpr_mono_v + length(v_path_i_col2)

								# Number of not col2 vertices in shortest path
								dnpr_mix_v = dnpr_mix_v + length(v_path_i_col1) + length(v_path_i_mix)
							}

						#=========== dl and dlpr distance
							if(length(path) !=0){
								for(l in 1:length(path)){
									if( (l+1) <= length(path)){
										# check the end vertices (path[l] and path[l+1]) of each edge of the path
										if( (V(graph)[path[l]]$tax == col2) & (V(graph)[path[l+1]]$tax == col2) ){
											dl_mono_edges = dl_mono_edges+1
											dlpr_mono_edges = dlpr_mono_edges + E(graph)[ V(graph)[path[l]] %--% V(graph)[path[l+1]] ]$weight
										}else{
											dl_mix_edges = dl_mix_edges+1
											dlpr_mix_edges= dlpr_mix_edges+ E(graph)[ V(graph)[path[l]] %--% V(graph)[path[l+1]] ]$weight
										}
									}
								}
							}
					}
				}

				res = c(dpr_mix_color_paths, dpr_monocolor_paths, dn_num, dn_denom, dnpr_no_v, dnpr_mono_v, dnpr_mix_v, dl_mix_edges, dl_mono_edges, dlpr_mono_edges, dlpr_mix_edges)
				res
		}
	}

	# For when col1 or col2 <= 1
	if(!exists("x.col1")){
		x.col1=c(rep(0,11))
	}
	if(!exists("x.col2")){
		x.col2=c(rep(0,11))
	}

	dpr = (x.col1[2]+x.col2[2])/(x.col1[2]+x.col2[2]+x.col1[1]+x.col2[1])

	dnpr = (x.col1[7]+x.col2[7])/(x.col1[5]+x.col2[5]+x.col1[6]+x.col2[6]+x.col1[7]+x.col2[7])
	dl = (x.col1[9]+x.col2[9])/(x.col1[8]+x.col2[8]+x.col1[9]+x.col2[9])
	dlpr = (x.col1[10]+x.col2[10])/(x.col1[10]+x.col2[10]+x.col1[11]+x.col2[11])

	if (distance == "paths"){
	  return(c(Spp=dpr,Spep=dl,Spelp=dlpr,Spinp=(1-dnpr)))
	} else if(distance == "transfer"){
	  return(c(direct = x.col1[2]/(x.col1[1]+x.col1[2]), reverse = x.col2[2]/(x.col2[1]+x.col2[2])))
	  }

}
