#nodeID for 3 movies 
nodeID = c(894353,779750,763762)
#nodeID = 894353
for (node in nodeID){
  index = which(V(g_movie)$name == node)
  nei = neighbors(g_movie,index)
  weight=numeric(0)
  for (nei_node in nei){
    #print(nei_node)
    weight_node = E(g_movie,P=c(index,nei_node))$weight
    weight = c(weight,weight_node)
  }
  sorted_weight = sort(weight,decreasing =T,index.return =T)
  near_nei = nei[sorted_weight$ix[1:5]]
  near_nei_name = V(g_movie)[near_nei]
  cat(node,"\n")
  cat("nodeID: ",V(g_movie)[near_nei_name]$name,"\n")
  cat("community: ",g_movie_com$membership[near_nei_name],"\n")
  cat("genre: ",V(g_movie)$genre[near_nei_name],"\n")
  ## to run the code after, should first run proj2_7.R to get rating attribute
  ## rating calculate by nearest neighbors
  cat("rating:",V(g_movie)$rating[near_nei_name],"\n")
  nei_rating = as.numeric(V(g_movie)$rating[near_nei_name])
  cat(nei_rating,"\n")
  print(mean(nei_rating[which(nei_rating!=0)]))
}
