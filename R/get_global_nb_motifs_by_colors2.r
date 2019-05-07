#### Function to get motifs numbers by colors of a graph

# requires SDDE and igraph packages and create_subgraph_by_colors function
# input: an igraph with colors levels (V(graph)$tax) and the motif size
# output: list of different attributes

get_global_nb_motifs_by_colors<-function(graph,size_motif, nom_col1, nom_col2){
	subgraphs<-create_subgraph_by_colors(graph, nom_col1, nom_col2)
	
	if (size_motif ==(2)){
		total_motifs=E(graph)
		nb_total_motifs=length(E(graph));
		degree_col1<-degree(subgraphs$gcol1,v=V(subgraphs$gcol1))
		degree_col2<-degree(subgraphs$gcol2,v=V(subgraphs$gcol2))
		
		if ((sum(degree_col1))==0){ #pour les cas où il n'y a pas de edges
			unique_motifs_col1=0
			nb_unique_motifs_col1=0
		} else{
			unique_motifs_col1=E(subgraphs$gcol1)
			nb_unique_motifs_col1=length(unique_motifs_col1)
		}
		
		if ((sum(degree_col2))==0){
			unique_motifs_col2=0
			nb_unique_motifs_col2=0
		} else{
			unique_motifs_col2=E(subgraphs$gcol2)
			nb_unique_motifs_col2=length(unique_motifs_col2)
		}
		
		nb_unique_motifs= nb_unique_motifs_col1 + nb_unique_motifs_col2
		nb_shared_motifs=length(total_motifs)-nb_unique_motifs
		
		#if (nb_shared_motifs==0){
		#	ratio=0
		#} else{
			ratio=nb_unique_motifs/nb_total_motifs
		#}
	}
	
	if (size_motif ==(3) || size_motif ==(4)){
		total_motifs=graph.motifs(graph, size_motif)
		nb_total_motifs=graph.motifs.no(graph, size_motif)
		unique_motifs_col1=graph.motifs(subgraphs$gcol1, size_motif)
		unique_motifs_col2=graph.motifs(subgraphs$gcol2, size_motif)
		nb_unique_motifs=graph.motifs.no(subgraphs$gcol1, size_motif)+graph.motifs.no(subgraphs$gcol2, size_motif)
		nb_shared_motifs=graph.motifs.no(graph, size_motif)-nb_unique_motifs
		
		if(size_motif ==3){
			unc<-unconnected_motifs3(graph, nom_col1, nom_col2)
			nb_unique_motifs= nb_unique_motifs + unc$nb_unique
			nb_shared_motifs= nb_shared_motifs + unc$nb_shared
			nb_total_motifs=nb_total_motifs + unc$nb_unique + unc$nb_shared
		}
		
		#if (nb_shared_motifs==0){
		#	ratio=0
		#} else{
			ratio=nb_unique_motifs/nb_total_motifs
		#}
	}
	#Possible case if there is no motif at all present
	if (nb_total_motifs==0) ratio=1;
	return(list(ratio=ratio, size_motif=size_motif,col1_tax=subgraphs$col1,col2_tax=subgraphs$col2,total_motifs=total_motifs,unique_motifs_col1=unique_motifs_col1,unique_motifs_col2=unique_motifs_col2,nb_shared_motifs=nb_shared_motifs,nb_unique_motifs=nb_unique_motifs, nb_total_motifs=nb_total_motifs))
}

# À modifier:
	#Ne prend que les motifs connectés
	#À rajouter: Option pour échantillonner au lieu de tout prendre
