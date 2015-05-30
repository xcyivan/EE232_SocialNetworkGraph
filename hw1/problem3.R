library(igraph)

#(a)
g <- aging.prefatt.game(1000,1,-1,aging.bin = 1000,directed = F)
#plot(g)
plot(degree.distribution(g),type="h", main = "Degree Distribution",xlab="degree",ylab="density")

#(b)
community = fastgreedy.community(g)
mod = modularity(community)

