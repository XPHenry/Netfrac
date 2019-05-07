# source("https://bioconductor.org/biocLite.R")
# biocLite("netresponse")
library(netresponse)
library(igraph)

#obtenir les aretes dans une table
edges = read.table("Glomero/Glomeromycota_ea.csv",header = TRUE,sep = ",")
#dans ce cas, la colonne selected (de cytoscape) permet de savoir lesquels nous gardons
edges = subset(edges,select = c("name","selected",edges_tot$Column.11))
#changer l'ecriture pour correspondre a un fichier .sif
edges$name <- gsub(" (interacts with) ","\tinteracts with\t", edges$name, fixed = TRUE)
#choisir les selected = true
#edges <- subset(edges, edges$selected == "true")

#ecrire dans un nouveau fichier les aretes qui correspondent a notre choix
glomero = read.table("Glomero/Glomeromycota.sif",sep = ",")
glomero <- glomero[which(edges$name %in% glomero$V1),1]
glomero <- as.data.frame(glomero)
write.table(glomero,"Glomero/Glomeromycota_final.sif",1,sep="\t",row.names = FALSE,col.names = FALSE, quote = FALSE)

glom = read.sif("Glomero/Glomeromycota.sif","igraph",header = FALSE)
glom = simplify(glom)
sub_gs =components(glom)
rm_nodes <- names(which(sub_gs$membership!=1))
glom = delete.vertices(glom, rm_nodes)

plot(glom)

node = read.table("Glomero/Glomeromycota_na.csv",header = TRUE,sep = ",")
node = subset(node,select = c("name","node.color"))
node = node[which(!node$name %in% rm_nodes),]



for(i in 1:length(node$name)){
  for(j in 1:length(V(glom))){
    if(node$name[i] == V(glom)$name[j]){
      V(glom)$tax[j] = node$node.color[i]
    }
  }
}

glom = set_color(glom)
plot(glom)

E(glom)$weight = rep(1,length(E(glom)))

