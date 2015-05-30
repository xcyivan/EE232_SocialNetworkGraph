library("igraph")
#modify this filePath to the absolute path of facebook_combined.txt on your own computer
filePath = "Desktop/facebook_combined.txt"
g = read.graph(file = filePath,directed=FALSE)
#calculate core node
core_index = numeric(0)
core_degree = numeric(0)
for(i in 1: length(degree(g))){
  if(length(neighbors(g,i))>200){
    core_index = c(core_index, i)
    core_degree = c(core_degree, length(neighbors(g,i)))
  }
}
g_node1 = induced.subgraph(g, c(1, neighbors(g,1)))
vertexvector = rep(3,vcount(g_node1))
vertexvector[1]=5
vertexcolor = rep("magenta",vcount(g_node1))
vertexcolor[1] ="black"
plot.igraph(g_node1,vertex.size=vertexvector,vertex.label =NA,vertex.color=vertexcolor)
n_node = vcount(g_node1)
n_edge = ecount(g_node1)
V(g)$name = V(g)
embeddedness_total=numeric(0)
dispersion_total=numeric(0)
for(i in 1:length(core_index))
#i=6
{
print(i)
#embeddedness
embeddedness=numeric(0)
#core_presonal_network
#core no.
core_neighbors =neighbors(g,core_index[i])
core_personal = induced.subgraph(g,c(core_index[i],core_neighbors))
  for(j in 1:length(core_neighbors))
  {
    embeddedness =c(embeddedness,length(intersect(neighbors(g,core_index[i]),neighbors(g,core_neighbors[j]))))
  }
#dispersion
dispersion = numeric(0)
for(k in 1: length(core_neighbors))
{
  #delete core node and said node
  mutual_friends = intersect(neighbors(g,core_index[i]),neighbors(g,core_neighbors[k]))
  dispersion_sub_graph = delete.vertices(core_personal,c(which(V(core_personal)$name==core_index[i]),which(V(core_personal)$name==core_neighbors[k])))
  shortestpath=numeric(0)
  for(m in 1:length(mutual_friends))
  {
    for(n in (m+1): length(mutual_friends))
    {
    shortestpath = c(shortestpath,shortest.paths(dispersion_sub_graph,which(V(dispersion_sub_graph)$name==mutual_friends[m]),which(V(dispersion_sub_graph)$name==mutual_friends[n])))
    }
  }
  dispersion = c(dispersion, sum(shortestpath))
}

embeddedness_total=c(embeddedness_total,embeddedness)
dispersion_total=c(dispersion_total,dispersion)
#dispersion[which(dispersion ==Inf)]=0
max_dispersion = which.max(dispersion)
dis_highlight_node_in_g = core_neighbors[max_dispersion]
max_embeddedness = which.max(embeddedness)
emb_highlight_node_in_g = core_neighbors[max_embeddedness]
max_dispersion_embeddedness = which.max(dispersion*(1/embeddedness))
dis_emb_highlight_node_in_g = core_neighbors[max_dispersion_embeddedness]

core_community = walktrap.community(core_personal)
color_vec = core_community$membership+1
size_vec = rep(3,length(color_vec))
color_vec[max_dispersion_embeddedness[1]] = 0
size_vec[max_dispersion_embeddedness[1]] = 5
color_vec[which(V(core_personal)$name==core_index[i])] = 0
size_vec[which(V(core_personal)$name==core_index[i])] = 7
E(core_personal)$color = "grey"
E(core_personal,P = c(max_dispersion_embeddedness[1],which(V(core_personal)$name==core_index[i])))$color = "red"
E(core_personal,P = c(max_dispersion_embeddedness[1],which(V(core_personal)$name==core_index[i])))$width = 10
plot(core_personal,vertex.color=color_vec,vertex.label=NA,vertex.size=size_vec)
}
hist(embeddedness_total,breaks=50,main = "Embeddedness Distribution",xlab = "Embeddedness", col="blue")
hist(dispersion_total[which(dispersion_total!=Inf)],breaks = 100,main = "Dispersion Distribution",xlab = "Dispersion")