#-------------problem1--------------------#

library("igraph")
library("netrw")
#(1a)
nNodes=1000
p=0.01
g=random.graph.game(nNodes,p,directed=FALSE)
d=diameter(g)

#(1b)
RandomWalker =function(g,nNodes,DF)
{
  ave_dis = std_var_dis = numeric(0)
  for(i in 1:nNodes){
    print(i)
      #100 is the default walker.num
    r=netrw(g, T=i,damping=DF, output.walk.path=TRUE)
    shortest_path=numeric(0)
    #j is the same with walker.number
    for(j in 1:100){
      temp = get.shortest.paths(g,from=r$walk.path[1,j],to=r$walk.path[i,j])
      shortest_path=c(shortest_path,temp$vpath[[1]])
    }
    ave = mean(shortest_path)
    std_var = sd(shortest_path)
    ave_dis=c(ave_dis, ave)
    std_var_dis=c(std_var_dis, std_var)
  }
  #output answer
  #print(ave_dis)
  #print(std_var_dis)
  
#  #plot
#  plot(1:nNodes,ave_dis,type="line")
#  plot(1:nNodes,std_var_dis, type="line")
#  
#  #(1e)
#  par(mfrow=c(1,2))
#  hist(degree.distribution(g))
#  deg2=numeric(0)
#  for(k in 1:100){
#    deg2 = c(deg2, degree(g,r$walk.path[100,k]))
#  }
#  hist(deg2)
  
}

#RandomWalker(g,nNodes,1)

#(1d)
nNodes2=100
g2=random.graph.game(nNodes2,p,directed=FALSE)
d2=diameter(g2)
#RandomWalker(g2,nNodes2,1)
nNodes3=10000
g3=random.graph.game(nNodes3,p,directed=FALSE)
d3=diameter(g3)
#RandomWalker(g3,nNodes3,1)

#(e) done in RandomWalker function



#--------------problem2----------------#

#(2a)
gg=barabasi.game(nNodes, directed=FALSE)
dd=diameter(gg)

#(2b)
#RandomWalker(gg,nNodes,1)

#(2d)
gg2=barabasi.game(nNodes2, directed=FALSE)
dd2=diameter(gg2)
#RandomWalker(gg2,nNodes2,1)

gg3=barabasi.game(nNodes3, directed=FALSE)
dd3=diameter(gg3)
#RandomWalker(gg3,nNodes3,1)

#(2e) done in RandonWalker function



#------------------problem3-----------------#
#(3a)
RandomWalker2 =function(g,nNodes, DF)
{
  r=netrw(g, damping=DF, output.walk.path=TRUE)
  par(mfrow=c(1,1))
  #plot(r$ave.visit.prob)
  
  #plot relationship between degree distribution and visted nodes'degree distribution
  deg=numeric(0)
  vst=numeric(0)
  
  for(i in r$walk.path[,1]){
    deg=c(deg,degree(g,i))
    vst=c(vst,r$ave.visit.prob[i])
  }
  plot(deg,vst)
}
  #the answer is yes, linearly related for undirected, no for directed
#RandomWalker2(g,nNodes,1)

#(3b)
ggg=random.graph.game(nNodes,p,directed=TRUE)
#RandomWalker2(ggg,nNodes,1)

#(3c)
 #still linearly related, but not as strong as in (3a)
RandomWalker2(g,nNodes,0.85)


#------------------problem4--------------#
#(4a)
RandomWalker2(ggg,nNodes,0.85)
prank=page.rank(ggg)
#(4b)
RandomWalker3=function(g,nNodes,DF,teleProb)
{
  r=netrw(g, damping=DF, teleport.prob=teleProb, output.walk.path=TRUE)
  deg=numeric(0)
  vst=numeric(0)
  for(i in r$walk.path[,1]){
    deg=c(deg,degree(g,i))
    vst=c(vst,r$ave.visit.prob[i])
  }
  par(mfrow=c(1,1))
  plot(deg,vst)
}
RandomWalker3(ggg,nNodes,0.85,prank$vector)

#(4c)
probVec=rep(1/nNodes,nNodes)
RandomWalker3(ggg,nNodes,0.85,probVec)
