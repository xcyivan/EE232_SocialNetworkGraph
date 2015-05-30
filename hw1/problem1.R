library ("igraph")
#(a)
p1 = 0.1
p2 = 0.05
p3 = 0.01
nodesNum = 1000
g1 = random.graph.game(nodesNum,p1,directed=FALSE)
g2 = random.graph.game(nodesNum,p2,directed=FALSE)
g3 = random.graph.game(nodesNum,p3,directed=FALSE)

dg1 = degree.distribution(g1)
dg2 = degree.distribution(g2)
dg3 = degree.distribution(g3)

#par(mfrow=c(1,2))
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(dg1,type="h", main = "Degree Distribution with p=0.1 ",xlab="degree",ylab="density")
plot(dg2,type="h", main = "Degree Distribution with p=0.05",xlab="degree",ylab="density")
plot(dg3,type="h", main = "Degree Distribution with p=0.01",xlab="degree",ylab="density")

#(b)
C1=C2=C3=D1=D2=D3=numeric(0)
for(i in 1:100){
  print(i)
  g1 = random.graph.game(nodesNum,p1,directed=FALSE)
  g2 = random.graph.game(nodesNum,p2,directed=FALSE)
  g3 = random.graph.game(nodesNum,p3,directed=FALSE)
  
  c1 = is.connected(g1)
  c2 = is.connected(g2)
  c3 = is.connected(g3)
  
  d1 = diameter(g1)
  d2 = diameter(g2)
  d3 = diameter(g3)
  
  C1 = c(C1,c1)
  C2 = c(C2,c2)
  C3 = c(C3,c3)
  D1 = c(D1,d1)
  D2 = c(D2,d2)
  D3 = c(D3,d3)
}
C1_avg=mean(C1)
C2_avg=mean(C2)
C3_avg=mean(C3)
D1_avg=mean(D1)
D2_avg=mean(D2)
D3_avg=mean(D3)

#(c)
MID=numeric(0)
for(j in 1:100)
{
  print(j)
  left = 0
  right = 1
  flag = F
  step = 0.0001
  mid = (left+right)/2
  while(!isTRUE(flag)){
    mid = (left+right)/2
    gm = random.graph.game(nodesNum,mid,directed=FALSE)
    gl = random.graph.game(nodesNum,(mid-step),directed=FALSE)
    gr = random.graph.game(nodesNum,(mid+step),directed=FALSE)
    if(!isTRUE(is.connected(gl)) && isTRUE(is.connected(gr))){
      flag=T
    }
    else if(isTRUE(is.connected(gm))){
      right = mid
    }
    else {
      left = mid
    }
  }
  MID = c(MID,mid)
}
MID_avg = mean(MID)