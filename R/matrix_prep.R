#===================
#= Fonction pour les opérations avec les matrices
#===================


#initialize the matrix with random numbers
v_name <- list()
v_name <- append(v_name,paste0("A",1:50))
v_name <- append(v_name,paste0("B",1:50))

data <- runif(10000)

mat1 <- matrix(data,100,100)
colnames(mat1) <- v_name
rownames(mat1) <- v_name

mat1[lower.tri(mat1)] = t(mat1)[lower.tri(mat1)]

#extract line with specific threshold

results <- vector("list",6000)
results <- apply(mat1,c(1,2),function(x) x > 0.98)
g1 <- graph.adjacency(results)

V(g1)$tax = c(rep("A",50),rep("B",50))
g1 <-set_color(g1)
E(g1)$weight = rep(1,length(E(g1)))
g1 <- simplify(g1)
g1 <- as.undirected(g1)

