library(igraph)
filepath = "/Users/Xcy/Code/R/project/gplus/"
# in this directory, using linux shell to run this line to generate the nodeID:
# >>ls | grep .edges | awk 'BEGIN{FS="."}{print $1}'| sort >> ../gplus_unique_node
# now the unique nodeIDs are stored in file "gplus_unique_node"

all_node_id = read.table("/Users/Xcy/Code/R/project/gplus_unique_node", numerals = "no.loss")
#this huge loop is tooooooo slow, you can try to run this for days
#after test we found node 7 and node 12 have more than 2 circles, use them for analysis
#for(i in 1: length(all_node_id)){
for(i in 7:7){
  print ("i=")
  print (i)
  egoNodeId = all_node_id$V1[i]
  edgelistFile = paste(filepath , egoNodeId  , ".edges", sep="")
  circlesFile = paste(filepath , egoNodeId , ".circles" , sep="")
  circlesCountFile = paste(filepath , egoNodeId , ".circlescount" , sep="")
  #use a linux shell script "circlecount.sh" to read file and store the number of circles in nodeID.circlescount file
  n_circle = read.table(circlesCountFile, numerals = "no.loss")
  g = read.graph(edgelistFile, format = "ncol", directed = TRUE)
  g2 = add.vertices(g, 1, name=egoNodeId)
  if (n_circle$V1[1]<=2){next}
  print("circlecount=")
  print (n_circle$V1[1])
  for(j in 1:length(V(g))){
    if(j%%100==0){
      print("j=")
      print (j)
    }
    g2 = add.edges(g2, c(vcount(g2),j))
  }
  fp=file(circlesFile,open="r")
  content = readLines(fp)
  close(fp)
  circle=list()
  for(j in 1:length(content)){
    circle[j]=strsplit(content[j],"\t")
  }

  #cmt = walktrap.community(g2)
  cmt = infomap.community(g2)
  #do whatever processing next
  SELECT=list()
   for(m in 1:max(cmt$membership)){
     select = vector()
     for(n in 1:length(cmt$membership)){
       if(cmt$membership[n]==m){
         select = c(select,(cmt$name[n]))
       }
     }
     #print(select)
     #SELECT[[m]]=select
     percentage = vector()
    for(n in 1:n_circle$V1[1]){
      intersection <- intersect(select,circle[[n]])
      temp <- length(intersection)/length(select)
      percentage <- c(percentage, temp)
    }
    print(percentage)
  }
}