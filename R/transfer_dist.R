#==== optimized transfer distance with components
transfer2 <- function(graph){
  tic("components")
  taxlvl <- levels(factor(V(graph)$tax))
  taxlevels <- combn(taxlvl,2)
  all_comp <- c()
  all_csize <- c()
  list_all <- list()
  denom_all <- count(V(graph)$tax)

  #=== Add component in vertice, initialize matrix
  for (i in taxlvl){
    #== create the components
    group = subgroup_graph(graph,i)
    comp = components(group)
    #== create a unique name for each component
    name_comp <- names(comp$membership)
    comp$membership = paste0(i,comp$membership)
    #== store the information in lists
    V(graph)[name_comp]$comp <- comp$membership
    all_comp <- append(all_comp,levels(factor(comp$membership)))
    list_all <- append(list_all,list(levels(factor(comp$membership))))
    #== save the size for further use
    names(comp$csize) <- levels(factor(comp$membership))
    all_csize <- append(all_csize,comp$csize)
    #== calculate the size of alpha, first subtract, then find min() and the size of comm
    comp$diff <- ave(comp$csize, FUN=function(x) c(0, diff(x)))
    min_comp <- which(comp$diff == min(comp$diff))
    alpha <- (comp$csize[min_comp-1] + comp$csize[min_comp])/2
  }

  names(list_all) <- taxlvl
  mat1 <- matrix(0, length(all_comp),length(all_comp))
  dimnames(mat1) <- list(all_comp,all_comp)
  edges <- as_edgelist(graph)
  toc()
  #=== each edges determines if components are connected
  tic("matrix")
  nodes <- V(graph)$name
  compon <- V(graph)$comp
  for (i in 1:length(edges[,1])){
    comp1 <- compon[which(nodes == edges[i,1])]
    comp2 <- compon[which(nodes == edges[i,2])]
    if (all_csize[comp1] < all_csize[comp2]){
      mat1[comp1,comp2] <- 1
    }else if (all_csize[comp2] < all_csize[comp1]){
      mat1[comp2,comp1] <- 1
    }else{
      mat1[comp1,comp2] <- 0.5
      mat1[comp2,comp1] <- 0.5
    }
    #mat1[V(graph)$comp[as.integer(edges[i,2])],V(graph)$comp[as.integer(edges[i,1])]] <- 1
  }

  result <- matrix("-",length(taxlvl),length(taxlvl),dimnames = list(taxlvl,taxlvl))
  result2 <- matrix(0,length(taxlvl),length(taxlvl),dimnames = list(taxlvl,taxlvl))
  toc()
  tic("calculation")
  for (i in 1:(length(taxlevels)/2)){
    col1 = noquote(taxlevels[1,i])
    print(col1)
    col2 = noquote(taxlevels[2,i])
    print(col2)
    list_col1 <- unlist(list_all[col1])
    list_col2 <- unlist(list_all[col2])
    mat1_col1 <- mat1[list_col1,list_col2,drop=FALSE]
    mat1_col2 <- mat1[list_col2,list_col1,drop=FALSE]

    denom_T = denom_all[which(denom_all$x==col1),2]
    a = 0
    num_T = 0

    for(j in 1:length(mat1_col1[,1])){

      if(sum(mat1_col1[j,]) > 0){
        size = all_csize[row.names(mat1_col1)[j]]

        if (size < alpha){
          num = size*(denom_T-size)
          num_T = num_T + num
          a = a + size
        }
      }
    }
    num_T = num_T - (a*(a-1))/2
    denom = denom_T*(denom_T-1)/2
    if (denom_T == 1){
      denom = 1
    }
    dist_T = 1 - num_T/denom
    result[taxlevels[1,i],taxlevels[2,i]] <- paste0(round(dist_T,3),"(",round(a/denom_T*100,3),"%",")")
    result2[taxlevels[1,i],taxlevels[2,i]] <- a/denom_T

    #=== same operation for the other side
    denom_T2 = denom_all[which(denom_all$x==col2),2]
    a = 0

    num_T2 = 0
    for(j in 1:length(mat1_col2[,1])){

      if(sum(mat1_col2[j,]) > 0){
        size = all_csize[row.names(mat1_col2)[j]]

        if (size < alpha){
          num = size*(denom_T2-size)
          num_T2 = num_T2 + num
          a = a + size
        }
      }
    }

    num_T2 = num_T2 - (a*(a-1))/2
    denom = denom_T2*(denom_T2-1)/2
    if (denom_T2 == 1){
      denom = 1
    }
    dist_T2 = 1 - (num_T2/denom)
    result[taxlevels[2,i],taxlevels[1,i]] <- paste0(round(dist_T2,3),"(",round(a/denom_T2*100,3),"%",")")
    #result2[taxlevels[2,i],taxlevels[1,i]] <- a/denom_T2
  }
  toc()
  return(noquote(result))
  #return(list(result,result2))
}

