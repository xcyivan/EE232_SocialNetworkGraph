fileName = "Desktop/movieID_rating.txt"
file_rating = file(fileName,open = "r")
lines_rating = readLines(file_rating)
Rating = rep(0,vcount(g_movie))

counter=0
for (i in 1:length(lines_rating))
  {
    list_rating = strsplit(lines_rating[i],"\t\t")
    index = which(V(g_movie)$name == list_rating[[1]][1])
    if(length(index)!=0)
    {
      counter = counter+1
      Rating[index] = as.numeric(list_rating[[1]][2])
      if(counter %% 100 ==0)
      {
        cat(i," ")
        cat(lines_rating[i],index,list_rating[[1]][2],"\n")
      }
    }
  }
close(file_rating)
V(g_movie)$rating = Rating

for (node in nodeID)
{
  com_index = g_movie_com$membership[which(V(g_movie)$name == node)]
  node_com = which(g_movie_com$membership == com_index)
  node_com_rating =V(g_movie)[node_com]$rating[which(V(g_movie)[node_com]$rating != 0)]
  node_com_rating =as.numeric(node_com_rating)
  node_rating = mean(node_com_rating)
  print(node_rating)
}



