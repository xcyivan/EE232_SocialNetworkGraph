library("igraph")
library("netrw")

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
  #layout(matrix(c(1,2),1,2,byrow=T))
  par(mfrow=c(1,2))
  plot(ave_dis,main='Average Distance Distribution',xlab="t")
  plot(std_var_dis,main='Std of Distance Distribution',xlab="t")
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
#(2a)
nNodes=1000
gg=barabasi.game(nNodes, directed=FALSE)
dd=diameter(gg)

#(2b)
RandomWalker(gg,100,1)

#(2d)
nNodes2=100
gg2=barabasi.game(nNodes2, directed=FALSE)
dd2=diameter(gg2)
#RandomWalker(gg2,1000,1)
nNodes=10000
gg3=barabasi.game(nNodes3, directed=FALSE)
dd3=diameter(gg3)
#RandomWalker(gg3,1000,1)

#(2e) done in RandonWalker function
