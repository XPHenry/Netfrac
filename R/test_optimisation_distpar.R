tax_assign <- function (x){
  c(rep("A",x),rep("B",100-x))
}

reseau  <- erdos.renyi.game(100, 0.05, "gnp", directed = FALSE, loops = FALSE)

V(reseau)$tax = tax_assign(20)
reseau = set_color(reseau)
V(reseau)$color
E(reseau)$weight = rep(1,length(E(reseau)))
plot(reseau, vertex.label=NA, vertex.size =10)

v1 = V(reseau)[which(V(reseau)$tax =="A")]
v2 = V(reseau)[which(V(reseau)$tax =="B")]
v.mix = c()
col1 = "A"
col2 = "B"
distance = "paths"


tic()
x.col1 <- lapply(v1,function(x) shortest_paths(reseau,x,v1[which(x == v1):length(v1)],"all")$vpath)
toc()

m_weight = mean(E(reseau)$weight)
max_weight = sum(E(reseau)$weight)

dist_par(reseau,col1,col2,"",distance,share_w = 0)
x.col1 = shortest_paths_graph(reseau,v1,v2,v.mix,col1,"paths","single",m_weight,max_weight)

tic()
Netfrac(reseau,"transfer")
toc()
