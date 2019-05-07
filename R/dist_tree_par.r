#==========================================================================================================================================================
#= Function to calculate dist_tree with doParallel 
#=        
#= Input: graph, a tree represented as an undirected igraph with color levels (V(graph)$tax) and edge weigth(E(graph)weight)
#=        nom_col1, character string that indicates the color 1 attribute
#=		  nom_col2, character string that indicates the color 2 attribute
#=        tips, a vertex sequence that indicates which vertices are tree tips
#= Output: list of distances values 
#=
#= All shortest paths are calculated between tree tips of same colors
#= Requires SDDE, igraph, foreach and doParallel packages
#= Multi-core backend has to be set
#=
#= Ex:
#= Works only for two colors graph
#==========================================================================================================================================================

dist_tree_par<-function(graph, nom_col1, nom_col2, tips){
		
	## Get vertex by colors
	color.levels <-as.factor(V(graph)$tax)
	for (i in 1:length(levels(color.levels))){
		if(levels(color.levels)[i] == nom_col1){
			col1 = levels(color.levels)[i]
		}
		if(levels(color.levels)[i] == nom_col2){
			col2 = levels(color.levels)[i]
		}
	}
	
	mask.col1 <-which(V(graph)$tax == col1)
	mask.col2 <-which(V(graph)$tax == col2)
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
	
	# Get tips of each color
	vtips_col1 = tips[which(tips %in% v.col1)]
	vtips_col2 = tips[which(tips %in% v.col2)]
	
	## Shortest path search for all pair of same color vertex

	# Color 1
	if(length(vtips_col1) >=2){
		
		x.col1<-foreach(a=vtips_col1[-(length(vtips_col1))], i=icount(), .combine="+", .inorder=FALSE, .packages='igraph') %:%
					foreach(b=vtips_col1[(i+1):length(vtips_col1)], .combine="+", .inorder=FALSE, .packages='igraph') %dopar% { #for all color 1 vertices
				owarn <- getOption("warn")
				options(warn = -1) # Silence warnings for not reachable vertices
				# Get the shortest paths from the v.col1[i] vertex to v.col1[j] vertex (There is only one in a tree)
				# The $vpath attribute contains the shortest path as a vertex sequence
				# The $epath attribute contains the shortest path as an edge sequence
				v.paths = shortest_paths(graph, a, b, mode= "all", output = "both")
				options(warn = owarn)

				#Parameters initialization
					
					#dpr_tre
					dpr_mix_color_paths=0
					dpr_monocolor_paths=0
					
					#dn_tree
					dn_nb_col1=0
					dn_nb_col_others=0

					#dl_tree
					dl_length_col1 =0
					dl_length_others=0

					
				#=========== Distance Calculation
				
					#=========== vpath and epath
					path_v = v.paths$vpath[[1]]
					path_e = v.paths$epath[[1]]
					
					#=========== dpr_tree
						
						# Associate each vertex in the path to its color
						v_path_col1 = intersection(path_v, v.col1)
						v_path_col2 = intersection(path_v, v.col2)
						v_path_mix = intersection(path_v, v.mix)
						
						# Check if the path only contains vertex of color 1
						if( (length(v_path_col2) != 0) || (length(v_path_mix) != 0) ){
							dpr_mix_color_paths = dpr_mix_color_paths + 1
						} else if (length(v_path_col1)>0){
							dpr_monocolor_paths = dpr_monocolor_paths + 1
						}
					
					#=========== dn_tree
					
						# Get only the vertices that correspond to internal nodes
						if(length(path_v)> 2){ # In a tree, there will always be at least three vertices in a path between two tips
							path_i = path_v[2:(length(path_v)-1)] # Remove the tips from the path
							if(length(path_i) != 0){
								for(k in 1: length(path_i)){ # For each vertex that corresponds to an internal node
									# Check if the vertex (internal node) is the same color as the tips
									if(path_i[k]$tax == col1){
										dn_nb_col1 = dn_nb_col1 + 1
									} else{
										dn_nb_col_others = dn_nb_col_others +1
									}
								}
							}
						}
					
					#=========== dl_tree
					
						# For each edge in the shortest path 
						for(k in 1: length(path_e)){
							# Get its end vertices
							v.t <-tail_of(graph, path_e[k])
							v.h <-head_of(graph, path_e[k])
						
							# Check the end vertices colors
							if(v.t$tax == v.h$tax){
								if(v.t$tax == col1){
									dl_length_col1 = dl_length_col1 + path_e[k]$weight
								}
								else if (v.t$tax == col2){
									dl_length_others = dl_length_others + path_e[k]$weight
								}else{ # mix colors
									dl_length_others = dl_length_others + path_e[k]$weight
								}
							}else{
								dl_length_others = dl_length_others + path_e[k]$weight
							}
						}
				
				res = c(dpr_mix_color_paths, dpr_monocolor_paths, dn_nb_col1, dn_nb_col_others, dl_length_col1, dl_length_others)
				res
		}
	}
	
	# Color 2
	if(length(vtips_col2) >=2){
		x.col2<-foreach(a=vtips_col2[-(length(vtips_col2))], n=icount(), .combine="+", .inorder=FALSE, .packages='igraph') %:%
					foreach(b=vtips_col2[(n+1):length(vtips_col2)], .combine="+", .inorder=FALSE, .packages='igraph') %dopar% { #for all color 1 vertices
				owarn <- getOption("warn")
				options(warn = -1) # Silence warnings for not reachable vertices
				# Get the shortest paths from the v.col2[i] vertex to v.col2[j] vertex (There is only one in a tree)
				# The $vpath attribute contains the shortest path as a vertex sequence
				# The $epath attribute contains the shortest path as an edge sequence
				v.paths = shortest_paths(graph, a, b, mode= "all", output = "both")
				options(warn = owarn)
				
				#Parameters initialization
					
					#dpr_tre
					dpr_mix_color_paths=0
					dpr_monocolor_paths=0
					
					#dn_tree
					dn_nb_col2=0
					dn_nb_col_others=0

					#dl_tree
					dl_length_col2 =0
					dl_length_others=0

					
				#=========== Distance Calculation
				
					#=========== vpath and epath
					path_v = v.paths$vpath[[1]]
					path_e = v.paths$epath[[1]]
					
					#=========== dpr_tree
						
						# Associate each vertex in the path to its color
						v_path_col1 = intersection(path_v, v.col1)
						v_path_col2 = intersection(path_v, v.col2)
						v_path_mix = intersection(path_v, v.mix)
						
						# Check if the path only contains vertex of color 2
						if( (length(v_path_col1) != 0) || (length(v_path_mix) != 0) ){
							dpr_mix_color_paths = dpr_mix_color_paths + 1
						} else if (length(v_path_col2)>0){
							dpr_monocolor_paths = dpr_monocolor_paths + 1
						}
					
					#=========== dn_tree
					
						# Get only the vertices that correspond to internal nodes
						if(length(path_v)> 2){ # In a tree, there will always be at least three vertices in a path between two tips
							path_i = path_v[2:(length(path_v)-1)] # Remove the tips from the path
							if(length(path_i) != 0){
								for(k in 1: length(path_i)){ # For each vertex that corresponds to an internal node
									# Check if the vertex (internal node) is the same color as the tips
									if(path_i[k]$tax == col2){
										dn_nb_col2 = dn_nb_col2 + 1
									} else{
										dn_nb_col_others = dn_nb_col_others +1
									}
								}
							}
						}
					
					#=========== dl_tree
					
						# For each edge in the shortest path 
						for(k in 1: length(path_e)){
							# Get its end vertices
							v.t <-tail_of(graph, path_e[k])
							v.h <-head_of(graph, path_e[k])
						
							# Check the end vertices colors
							if(v.t$tax == v.h$tax){
								if(v.t$tax == col2){
									dl_length_col2 = dl_length_col2 + path_e[k]$weight
								}
								else if (v.t$tax == col1){
									dl_length_others = dl_length_others + path_e[k]$weight
								}else{ # mix colors
									dl_length_others = dl_length_others + path_e[k]$weight
								}
							}else{
								dl_length_others = dl_length_others + path_e[k]$weight
							}
						}
				
				res = c(dpr_mix_color_paths, dpr_monocolor_paths, dn_nb_col2, dn_nb_col_others, dl_length_col2, dl_length_others)
				res
		}
	}
	# For when col1 or col2 <= 1
	if(!exists("x.col1")){
		x.col1=c(rep(0,6))
	}
	if(!exists("x.col2")){
		x.col2=c(rep(0,6))
	}
	
	dpr_tree = (x.col1[2]+x.col2[2])/(x.col1[1]+x.col2[1]+x.col1[2]+x.col2[2])
	dn_tree = (x.col1[3]+x.col2[3])/(x.col1[3]+x.col2[3]+x.col1[4]+x.col2[4])
	dl_tree = (x.col1[5]+x.col2[5])/(x.col1[5]+x.col2[5]+x.col1[6]+x.col2[6])
	
	return(list(dpr_tree=dpr_tree,dn_tree=dn_tree,dl_tree=dl_tree))

}