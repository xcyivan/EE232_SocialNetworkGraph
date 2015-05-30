#Problem 1
library("igraph")
#modify this filePath to the absolute path of facebook_combined.txt on your own computer
filePath = "/Users/Xcy/Code/R/project/facebook_combined.txt"

g = read.graph(file = filePath, format="ncol", directed=FALSE)
connectivity = is.connected(g)
d = diameter(g)
dg = degree.distribution(g)
plot(dg, type= "h")

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