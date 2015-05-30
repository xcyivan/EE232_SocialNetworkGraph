library("igraph")
library("netrw")
#(4a)
RandomWalker2(ggg,nNodes,0.85)
prank=page.rank(ggg)
#plot(prank$vector,main="PageRank",xlab="Node",ylab="rankscore")
#print(prank)
#(4b)
RandomWalker3=function(g,nNodes,DF,teleProb)
{
  r=netrw(g, damping=DF, teleport.prob=teleProb, output.walk.path=TRUE)
  deg=numeric(0)
  vst=numeric(0)
  deg=degree(g)
  #degree for directed graph
  #deg=degree(g,mode="in")
  #deg=degree(g,mode="out")
  vst=r$ave.visit.prob
  sumprob= numeric(max(deg)-min(deg)+1)
  count= numeric(max(deg)-min(deg)+1)
  # print(sumprob)
  for (i in 1:1000){
    sumprob[deg[i]]<-sumprob[deg[i]]+r$ave.visit.prob[i]
    count[deg[i]]<-count[deg[i]]+1
  }
  for (k in min(deg):max(deg))
  {
    sumprob[k]=sumprob[k]/count[k]
  }
  #plot(sumprob,main="Relationship Between Prob and Degree",xlab="degree",ylab="prob",type="o")
  cor=cor(deg,vst)
  print(cor)
}
RandomWalker3(ggg,nNodes,0.85,prank$vector)
prank_per=personalized.pagerank(g,damping=0.85,prob=prank$vector)
plot(prank_per,main="Personalized_PageRank",xlab="Node",ylab="rankscore")
cor()
#(4c)
probVec=rep(1/nNodes,nNodes)
RandomWalker3(ggg,nNodes,0.85,probVec)