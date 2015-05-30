library("igraph")
library("netrw")
#(1a)
nNodes=1000
p=0.01
g=random.graph.game(nNodes,p,directed=FALSE)
d=diameter(g)

#(1b)
RandomWalker =function(g,steps=1000,DF)
{
  ave_dis = std_var_dis = numeric(0)
  for(i in 1:steps){
    print(i)
    #100 is the default walker.num
    r=netrw(g, T=i,damping=DF, output.walk.path=TRUE)
    shortest_path=numeric(0)
    #j is the same with walker.number
    for(j in 1:100){
      temp = shortest.paths(g,r$walk.path[1,j],to=r$walk.path[i,j])
      shortest_path=c(shortest_path,temp)
    }
    ave = mean(shortest_path)
    std_var = sd(shortest_path)
    ave_dis=c(ave_dis, ave)
    std_var_dis=c(std_var_dis, std_var)
  }
  #output answer
  print(ave_dis)
  print(std_var_dis)
  
  #  #plot
    layout(matrix(c(1,2),1,2,byrow=T))
    plot(ave_dis,main='Average Distance Distribution',xlab="t")
    plot(std_var_dis,main='Std of Distance Distribution',xlab="t")
  #  
  #  #(1e)
    par(mfrow=c(1,2))
    hist(degree(g),main ="Degree Distribution of the Graph",xlab="degree",col="blue")
    deg2=numeric(0)
    deg2=r$walk.path[500,]
    hist(degree(g,deg2),main ="Degree Distribution at end of walk",xlab="degree",col="blue")
}

RandomWalker(g,500,DF=1)

#(1d)
#nNodes2=100
#g2=random.graph.game(nNodes2,p,directed=FALSE)
#d2=diameter(g2)
#RandomWalker(g2,100,DF=1)
#nNodes3=10000
#g3=random.graph.game(nNodes3,p,directed=FALSE)
#d3=diameter(g3)
#RandomWalker(g3,1000,1)

#(e) done in RandomWalker function