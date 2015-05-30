#########
library("igraph")
filePath = "Desktop/sorted_directed_net.txt"
##########
#problem 1
g = read.graph(file = filePath ,format = "ncol",directed = T)
con = is.connected(g)
cl = clusters(g)
gccIndex = which.max(cl$csize)
nonGccNodes = (1:vcount(g))[cl$membership != gccIndex]
gcc = delete.vertices(g,nonGccNodes)
##########
#problem 2
in_degree = degree(gcc,mode="in")
out_degree = degree(gcc,mode="out")
hist(in_degree, breaks=100,main ="In-Degree Distribution of GCC", xlab="degree",ylab="density")
hist(out_degree, breaks=100,main ="Out-Degree Distribution of GCC", xlab="degree",ylab="density")
##########
#problem 3
#option #1
gcc_undirected1 = as.undirected(gcc,mode = "each")
com_lp = label.propagation.community(gcc_undirected1)
color_vec_lp = com_lp$membership+1
#plot(gcc_undirected1,vertex.label=NA,vertex.size=3,vertex.color=color_vec_lp,main = "Community Structure Using Lable Propagation")
#option #2
sqrtweight<-function(weight){
  new_weight = sqrt(weight[1]*weight[2])
  new_weight
}
gcc_undirected2 = as.undirected(gcc,mode = "collapse",edge.attr.comb = sqrtweight)
com_fg = fastgreedy.community(gcc_undirected2)
color_vec_fg = com_fg$membership+1
#plot(gcc_undirected2,vertex.label=NA,vertex.size=3,vertex.color=color_vec_fg,main = "Community Structure Using Fast Greedy")
com_lp = label.propagation.community(gcc_undirected2)
color_vec_lp = com_lp$membership+1
#plot(gcc_undirected2,vertex.label=NA,vertex.size=3,vertex.color=color_vec_lp,main = "Community Structure Using Lable Propagation")
##########
#problem 4
non_max_com = V(gcc_undirected2)[which(com_fg$membership != which.max(sizes(com_fg)))]
sub_com = delete.vertices(gcc_undirected2,non_max_com)
sub_com_fg = fastgreedy.community(sub_com)
sub_color_vec_fg = sub_com_fg$membership+1
#plot(sub_com,vertex.label=NA,vertex.size=3,vertex.color=sub_color_vec_fg,main = "Sub Community Structure Using Fast Greedy")
sub_com_lp = label.propagation.community(sub_com)
sub_color_vec_lp = sub_com_lp$membership+1
#plot(sub_com,vertex.label=NA,vertex.size=3,vertex.color=color_vec_lp,main = "Sub Community Structure Using Lable Propagation")
##########
#problem 5
com_index = which(sizes(com_fg)>100)
for(i in 1:length(com_index))
{
  cat("\nSub-community index",com_index[[i]],":\n")
  non_com = V(gcc_undirected2)[which(com_fg$membership != com_index[i])]
  sub_com_100up = delete.vertices(gcc_undirected2,non_com)
  sub_com_100up_fg = fastgreedy.community(sub_com_100up)
  cat("sub-community structure using fastgreedy:\n")
  print(sizes(sub_com_100up_fg))
  print(modularity(sub_com_100up_fg))
  sub_com_100up_lp = label.propagation.community(sub_com_100up)
  cat("sub-community structure using labelpropagation:\n")
  print(sizes(sub_com_100up_lp))
  print(modularity(sub_com_100up_lp))
}