#=========================================================================
#################---------Problem 1-----------------######################
#=========================================================================
library("igraph")
#modify this filePath to the absolute path of facebook_combined.txt on your own computer
filePath = "/Users/Xcy/Code/R/project/facebook_combined.txt"

g = read.graph(file = filePath, format="ncol", directed=FALSE)
connectivity = is.connected(g)
d = diameter(g)
dg = degree.distribution(g)
plot(dg, type= "h", main="Degree Distribution", xlab="degree", ylab="density")

#fit a curve, refer StackOverFlow for the usage of stat_smooth() 
#http://stackoverflow.com/questions/14190883/fitting-a-curve-to-specific-data
#should install package "ggplot2" first
library(ggplot2)
h = hist(degree(g), breaks=seq(0, by=1 , length.out=max(degree(g))+2))
dat = data.frame(x=h$mids, y=h$density)
model = nls(y ~ I(1/x*a) + b*x, data = dat, start = list(a = 1, b = 1))
model2 = nls(y ~ I(exp(1)^(a + b * x)), data=dat, start = list(a=0,b=0))
ggplot(dat, aes(x, y)) + geom_point(size = 1)+
  stat_smooth(method = "nls", formula = as.formula(model), data=dat, start = list(a=0,b=0), size = 1, se = FALSE, colour = "red")+
  stat_smooth(method = "nls", formula = as.formula(model2), data=dat, start = list(a=0,b=0), size = 1, se = FALSE, colour = "blue")
summary(model2)
#find out model2 is better and a=-3.59 b =-0.02
dat2 = data.frame(x=h$mids, y=exp(1)^(-3.59-0.02*h$mids))
MSE=sum((dat2$y-dat$y)^2)/max(degree(g))
avg_degree = mean(degree(g))


#=========================================================================
#################-------------Problem 2-------------######################
#=========================================================================
g_node1 = induced.subgraph(g, c(1, neighbors(g,1)))
vertexvector = rep(3,vcount(g_node1))
vertexvector[1]=5
vertexcolor = rep("magenta",vcount(g_node1))
vertexcolor[1] ="black"
plot.igraph(g_node1,vertex.size=vertexvector,vertex.label =NA,vertex.color=vertexcolor)
n_node = vcount(g_node1)
n_edge = ecount(g_node1)


#=========================================================================
#################-------------Problem 3-------------######################
#=========================================================================
core_index = numeric(0)
core_degree = numeric(0)
for(i in 1: length(degree(g))){
  if(length(neighbors(g,i))>200){
    core_index = c(core_index, i)
    core_degree = c(core_degree, length(neighbors(g,i)))
  }
}
core_ave_degree = mean(core_degree)

fg = fastgreedy.community(g_node1)
color_vec = fg$membership+1
plot(g_node1,vertex.color=color_vec,vertex.label=NA,vertex.size=3)
eb = edge.betweenness.community(g_node1)
color_vec = eb$membership+1
plot(g_node1,vertex.color=color_vec,vertex.label=NA,vertex.size=3)
ic = infomap.community(g_node1)
color_vec = ic$membership+1
plot(g_node1,vertex.color=color_vec,vertex.label=NA,vertex.size=3)


#=========================================================================
#################-------------Problem 4-------------######################
#=========================================================================
g_node1_removed = induced.subgraph(g,neighbors(g,1))

fg2 = fastgreedy.community(g_node1_removed)
color_vec = fg2$membership+1
plot(g_node1_removed, vertex.color=color_vec, vertex.label=NA, vertex.size=3)
eb2 = edge.betweenness.community(g_node1_removed)
color_vec = eb2$membership+1
plot(g_node1_removed, vertex.color=color_vec,vertex.label=NA, vertex.size=3)
ic_r = infomap.community(g_node1_removed)
color_vec = ic_r$membership+1
plot(g_node1_removed, vertex.color=color_vec,vertex.label=NA, vertex.size=3)
