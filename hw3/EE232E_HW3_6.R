library("igraph")
library("netrw")
setwd("C:/Users/smr/Desktop")
filePath = "sorted_directed_net.txt"
g = read.graph(file = filePath,format = "ncol")
con = is.connected(g)
cl = clusters(g)
gccIndex = which.max(cl$csize)
nonGccNodes = (1:vcount(g))[cl$membership != gccIndex]
gcc = delete.vertices(g,nonGccNodes)
sqrtweight<-function(weight){
  new_weight = sqrt(weight[1]*weight[2])
  new_weight
}
gcc_undirected2 = as.undirected(gcc,mode = "collapse",edge.attr.comb = sqrtweight)
#caculate commuity
#com_fg = fastgreedy.community(gcc_undirected2)
com_lp = label.propagation.community(gcc_undirected2)
#threshold
threshold = 0.1
#random walk
walkernum=1
multi_com = numeric(0)
for(i in 1:vcount(g))
{
  teleprob = rep(0,vcount(g))
  teleprob[i]=1
  rw = netrw(g,walker.num = walkernum, 
             start.node = i,damping = 0.85,
             output.visit.prob=T,
             teleport.prob=teleprob)
  prob = rw$ave.visit.prob
  sorted_prob = sort(prob,decreasing=T,index.return=T)
  M=rep(0,length(com_lp))
  #sum largest 30 vj
  for(j in 1:30)
  {
    mj=rep(0,length(com_lp))
    mj[com_lp$membership[which(V(gcc)==V(g)[sorted_prob$ix[j]])]]=1
    M=M+sorted_prob$x[j]*mj
  }
  #if M has 2 or more elements >threshold then printout 
  if(length(which(M>threshold))>=2)
  {
    node_M=c(i,M)
    #save communtiny info
    multi_com=rbind(multi_com,node_M)
    cat("node:",i,"has multi-community\n")
  }
}
