library("igraph")
#(a)
NodesNum=1000
g = barabasi.game(NodesNum,directed=FALSE)
par(mfrow=c(1,2))
plot(degree.distribution(g), main='Degree Distribution',xlab = "degree",ylab="density")
plot(degree.distribution(g),log='xy', main='Degree Distribution in log',xlab = "degree",ylab="density")

Connectivity=Diameter=numeric(0)
for (i in 1:100){
  g = barabasi.game(NodesNum,directed=FALSE)
  Connectivity = c(Connectivity, is.connected(g))
  Diameter = c(Diameter, diameter(g))
}
Con_avg = mean(Connectivity)
Dia_avg = mean(Diameter)

#(b)
cl = clusters(g)
gccIndex = which.max(cl$csize)
nonGccNodes = (1:vcount(g))[cl$membership != gccIndex]
gcc = delete.vertices(g,nonGccNodes)
struct = fastgreedy.community(gcc)
mod = modularity(struct)

#(c)
NodesNum2 = 10000
g2 = barabasi.game(NodesNum2,directed=FALSE)
#par(mfrow=c(1,2))
#plot(degree.distribution(g2), main='Degree Distribution',xlab = "degree",ylab="density")
#plot(degree.distribution(g2),log='xy', main='Degree Distribution in log',xlab = "degree",ylab="density")

cl2 = clusters(g2)
gccIndex2 = which.max(cl2$csize)
nonGccNodes2 = (1:vcount(g2))[cl2$membership != gccIndex2]
gcc2 = delete.vertices(g2,nonGccNodes)
struct2 = fastgreedy.community(gcc2)
mod2 = modularity(struct2)

#(d)
DG=numeric(0)
for(i in 1:1000){
  rand = sample(1000,1)
  neib = neighbors(g,rand)
  if (length(neib)==1)
    picked = neib
  else
    picked = sample(neib, 1)
  DG = c(DG,degree(g,picked))
}
plot(density(DG),main='Degree distribution', xlab='degree', ylab='density')



