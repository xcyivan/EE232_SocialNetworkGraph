library("igraph")
#(a)
NodesNum = 1000
Diameter = numeric(0)
#create a graph with forward burning prob = 0.5 and backward burning ration = 0.5 and 1 ambassador
for (i in 1:100){
  g = forest.fire.game(NodesNum, 0.5, 0.5, 1, directed=TRUE)
  Diameter = c (Diameter, diameter(g))
}
par(mfrow=c(1,2))
plot(degree.distribution(g,mode="in"),type="h",main="In-Degree Distirbution",xlab="indegree",ylab="density")
plot(degree.distribution(g,mode="out"),type="h",main="Out-Degree Distirbution",xlab="outdegree",ylab="density")

#(b)
D_avg = mean(Diameter)

#(c)
struct = walktrap.community(g)
mod = modularity(struct)
plot(struct,g)