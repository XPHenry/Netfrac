# HGT1 = load_network("HGT1_edges.txt","HGT1_nodes.txt","proportional")
# HGT1 = simplify(HGT1)
# HGT1 = set_color(HGT1,foo)
# Netfrac(HGT1,"transfer")
#
# HGT2 = load_network("HGT2_edges.txt","HGT2_nodes.txt","proportional")
# HGT2 = simplify(HGT2)
# HGT2 = set_color(HGT2)
# Netfrac(HGT2,"transfer")

HGT1_fam = load_network("HGT1_edges.txt","HGT1_nodes.txt","proportional")
HGT1_fam = simplify(HGT1_fam)
HGT1_fam = set_color(HGT1_fam,foo_2)
plot(HGT1_fam)
Netfrac(HGT1_fam,"transfer")

HGT2_fam = load_network("HGT2_edges.txt","HGT2_nodes.txt","proportional")
HGT2_fam = simplify(HGT2_fam)
HGT2_fam = set_color(HGT2_fam,foo_3)
plot(HGT2_fam)
HGT2_transfer = Netfrac(HGT2_fam,"transfer")

#======================
#= make a new network from the sequences of TetA
#======================

TetA_table <- read.table("./HGT/mothur/TetA_align_good.square.dist")
TetA_mat <- as.matrix(TetA_table[2:51])
rownames(TetA_mat) = TetA_table[,1]
colnames(TetA_mat) = TetA_table[,1]

#convert to list
TetA_mat[upper.tri(TetA_mat)] <- 88
TetA_df <- melt(TetA_mat)
TetA_df_bad <- c()

for (i in (1:length(TetA_df[,1]))){
  if (TetA_df$Var1[i] == TetA_df$Var2[i]){
    TetA_df_bad <- append(TetA_df_bad,i)
  }
  else if (TetA_df$value[i] >= 0.08){
    TetA_df_bad <- append(TetA_df_bad,i)
  }
}
TetA_df_bad

#choose the similarity threshold
# for (i in (1:length(TetA_df[,1]))){
#   if (TetA_df$value[i] >= 0.20){
#     TetA_df_bad <- append(TetA_df_bad,i)
#   }
# }

TetA_df <- TetA_df[-TetA_df_bad,]
TetA_df

TetA_df_final <- TetA_df[,-3]
TetA_mat_final <- as.matrix(TetA_df_final)
TetA_net <- graph_from_edgelist(TetA_mat_final,FALSE)
plot(TetA_net, vertex.label=NA, vertex.size = 10)

TetA_nodes_df <- get.data.frame(TetA_net,"vertices")
write.table(TetA_nodes_df,"./HGT/TetA_nodes.txt", sep="\t",row.names = FALSE, col.names = FALSE,quote = FALSE)

#modification in python, add the taxonomy (community) after
TetA_nodes = read.table("./HGT/TetA_nodes_final.txt",sep = "\t")
V(TetA_net)$tax = as.character(TetA_nodes$V2)
TetA_net = set_color(TetA_net,foo_TetA)

plot(TetA_net, vertex.label=NA, vertex.size = 10)

#======================
#== same thing for CAT
#======================

CAT_table <- read.table("./HGT/mothur/CAT_align_good.square.dist")
CAT_mat <- as.matrix(CAT_table[2:39])
rownames(CAT_mat) = CAT_table[,1]
colnames(CAT_mat) = CAT_table[,1]

#convert to list
CAT_mat[upper.tri(CAT_mat)] <- 88
CAT_df <- melt(CAT_mat)

CAT_df_bad <- c()

for (i in (1:length(CAT_df[,1]))){
  if (CAT_df$Var1[i] == CAT_df$Var2[i]){
    CAT_df_bad <- append(CAT_df_bad,i)
  }
  else if (CAT_df$value[i] >= 0.10){
    CAT_df_bad <- append(CAT_df_bad,i)
  }
}

CAT_df <- CAT_df[-CAT_df_bad,]

CAT_df_final <- CAT_df[,-3]
CAT_mat_final <- as.matrix(CAT_df_final)
CAT_net <- graph_from_edgelist(CAT_mat_final,FALSE)

CAT_nodes_df <- get.data.frame(CAT_net,"vertices")
write.table(CAT_nodes_df,"./HGT/CAT_nodes.txt", sep="\t",row.names = FALSE, col.names = FALSE, quote = FALSE)

#modification in python, add the taxonomy (community) after
CAT_nodes = read.table("./HGT/CAT_nodes_final.txt",sep = "\t")
V(CAT_net)$tax = as.character(CAT_nodes$V2)
CAT_net = set_color(CAT_net,foo_CAT)

CAT_net2<- delete.vertices(CAT_net,c("194733791_Salmonella_pCVM19633_110","60115566_Salmonella_pSC138",
                                     "134047163_Salmonella_pSN254","152973773_Klebsiella_pKPN5","63219717_Pasteurella_pCCK381"))
plot(CAT_net2, vertex.label=NA, vertex.size = 10)

CAT_edges_df <- get.data.frame(CAT_net2,"edges")
write.table(CAT_edges_df,"./HGT/CAT_edges.txt", sep="\t",row.names = FALSE, col.names = FALSE, quote = FALSE)
