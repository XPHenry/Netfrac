#==========================================================================================================================================================
#= Function to get unconnected motifs of size 3 (two colors)
#= 
#= Input: graph, an undirected igraph with color levels (V(graph)$tax)
#=        nom_col1, character string that indicates the color 1 attribute
#=		  nom_col2, character string that indicates the color 2 attribute
#= Output: list containing nb of unique motifs (monocolor) and nb of shared motifs (multicolor)
#=
#= Needs igraph package and kcombination.cpp function file
#= Ex:
#= 		library(Rcpp)
#= 		sourceCpp("D:\\Utilisateurs\\BIOINFO\\Desktop\\Tissicca\\TissUni\\src\\kcombination.cpp")
#= 		unconnected_motifs3(network1, "A", "B")
#==========================================================================================================================================================

unconnected_motifs3<-function(graph, nom_col1, nom_col2){

	# Get the position of vertices that are either of color 1 or 2
	vcolors_pos<-which( (V(graph)$tax == nom_col1) | (V(graph)$tax == nom_col2))

	# Get all k3 combinations of those vertices 
	res<-k3combination(vcolors_pos)

	# Parameters initialization
	nb_unique=0
	nb_shared=0

	for(i in 1:length(res)){ # For each k3 combination
		
		# Get the nb of edges between the k3 combination vertices
		vp<-c(res[[i]][1],res[[i]][2],res[[i]][1],res[[i]][3],res[[i]][2],res[[i]][3])
		edges<-get.edge.ids(graph,vp) # Returns a vector with edge id or 0 if no edge is found
		nb_edges=length(which(edges!=0))
	
		if(nb_edges == 1){
			# If all vertices have the same colors
			if( (V(graph)[res[[i]][1]]$tax == V(graph)[res[[i]][2]]$tax) & (V(graph)[res[[i]][1]]$tax == V(graph)[res[[i]][3]]$tax) ){
				nb_unique=nb_unique+1
			}else{
				ed <- E(graph)[edges[which(edges!=0)]]
				# If the end vertices of the edge have the same color
				if(V(graph)[head_of(graph,ed)]$tax != V(graph)[tail_of(graph,ed)]$tax){
					nb_shared=nb_shared+1
				}else{
					nb_unique=nb_unique+1
				}
			}
		}#else if(nb_edges ==2){ #déjà calculé par graph.atlas
			# If all vertices have the same colors
			#if( (V(graph)[res[[i]][1]]$tax == V(graph)[res[[i]][2]]$tax) & (V(graph)[res[[i]][1]]$tax == V(graph)[res[[i]][3]]$tax) ){
			#	nb_unique=nb_unique+1
			#}else{
			#	nb_shared=nb_shared+1
			#}
		#}
	}
	return(list(nb_unique=nb_unique,nb_shared=nb_shared))
}

#TO DO: version en C plus rapide?
#Ceux pas du tout connecté ne sont pas comptés (0 edges)