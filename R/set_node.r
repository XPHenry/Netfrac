#==========================================================================================================================================================
#= Recursive function to set the colors of internal nodes of a tree represented as a graph
#= 
#= Input: edgelistdf, a dataframe of the edges list (can be obtained by x$edge, x is a phylo class object)
#=		  vvector, a vector of all vertices names
#=		  vlistna, a vector of vertices names with no colors ($tax attribute for those vertices is NA)
#=		  vlist, a vector of all vertices names with colors (e.g. tips of the tree)
#=		  vtax, a vector of all vertices colors (including NA)
#= Output: a vector of all vertices colors (used to set the $tax attribute of a network)
#=
#= Ex: set_node(x$edge, vvector, vlistna, vlist, tax)
#= Works for two colors
#==========================================================================================================================================================
#attention nom et numéro
set_node<-function(edgelistdf, vvector, vlistna, vlist, vtax, col1, col2){
	
	# Stop condition
	if(length(vlistna) == 0){ # If there is no more vertex without color
		return(vtax)
	}
	
	# Get all the edges that have at least one end vertex that has a color and put them into another dataframe
	pos <-c(which(edgelistdf[,1] %in% vlist), which(edgelistdf[,2] %in% vlist))
	subset_df <-edgelistdf[pos,]

	# Get the end vertices that are duplicated (They are vertices, corresponding to internal nodes, that are connected to two vertices with colors)
	elements <-c(subset_df[,1],subset_df[,2])
	duplicates <-which(duplicated(elements))

	for (i in 1:length(duplicates)){ # For each duplicated end vertices
		if(!(elements[duplicates[i]] %in% vlist)){ # Check if it does not already have a color
			p <-which(subset_df == elements[duplicates[i]]) # Get the two edges that contains it
			# Get the other vertex (neighbour) in the edge
			ngb1 = subset_df[p[1],][which(subset_df[p[1],] != elements[duplicates[i]])]
			ngb2 = subset_df[p[2],][which(subset_df[p[2],] != elements[duplicates[i]])]
			
			# Get the names of each vertex (neighbours and the vertex of interest)
			p_ngb1 <-which(vvector == ngb1)
			p_ngb2 <-which(vvector == ngb2)
			p_el <-which(vvector == elements[duplicates[i]])
			
			if(vtax[p_ngb1] == vtax[p_ngb2]){ # If the two neighbours have the same color
			
				vtax[p_el] = vtax[p_ngb1] # Set the same color for the vertex of interest
				vlistna <-vlistna[-(which(vlistna == elements[duplicates[i]]))] # Remove the vertex of interest from the vector of vertices without color
				vlist <-c(vlist, elements[duplicates[i]]) # Add the vertex of interest to the vector of vertices with colors
			
			}else{ # If the two neighbours have different colors
			
				# Set a mix color for the vertex of interest
				if( (vtax[p_ngb1] == paste0(col1,col2)) | (vtax[p_ngb2] == paste0(col1,col2)) ){ #TO DO: peut simplifier en regardant si col1>col2
					vtax[p_el] = paste0(col1,col2)
				}else if( (vtax[p_ngb1] == paste0(col2,col1)) | (vtax[p_ngb2] == paste0(col2,col1)) ){
					vtax[p_el] = paste0(col2,col1)
				} else{
					if(vtax[p_ngb1] > vtax[p_ngb2]){
						vtax[p_el] = paste0(vtax[p_ngb2],vtax[p_ngb1])
					}else{
						vtax[p_el] = paste0(vtax[p_ngb1],vtax[p_ngb2])
					}
				}
				
				vlistna <-vlistna[-(which(vlistna == elements[duplicates[i]]))] # Remove the vertex of interest from the vector of vertices without color
				vlist <-c(vlist, elements[duplicates[i]]) # Add the vertex of interest to the vector of vertices with colors
			}
		}
	}
	
	# Repeat the function for the updated vectors
	set_node(edgelistdf, vvector, vlistna, vlist, vtax, col1, col2)
}