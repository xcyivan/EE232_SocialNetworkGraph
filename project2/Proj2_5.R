library("igraph")
filePath = "Desktop/graphMovie2.txt"
g_movie = read.graph(file = filePath ,format = "ncol",directed = F)
print("done")
is.directed(g_movie)
g_movie_com = fastgreedy.community(g_movie)

## add attribute genre to the graph
fileName = "Desktop/movie_genre.txt"
file_genre = file(fileName,open = "r")
lines = readLines(file_genre)
Genre = rep("null",vcount(g_movie))
count = 0
for (i in 1:length(lines)){
  if(count %in% V(g_movie)$name){
    list = strsplit(lines[i],"\t\t")
    index = which(V(g_movie)$name == count)
    Genre[index] = list[[1]][2]
    cat(lines[count+1],index,list[[1]][2],"\n")
  }
  count = count + 1
}
close(file_genre)
V(g_movie)$genre = Genre
## tag the communtiy with genre that appear more than 20% in the community
com_tag = numeric(0)
for (i in 1:length(g_movie_com)){
  com_genre = V(g_movie)[which(g_movie_com$membership ==i)]$genre
  max = 0
  max_index = "null"
  genre_type = unique(com_genre)
    for(genre in genre_type)
    {
      if(length(which(com_genre == genre))>max && length(which(com_genre == genre))>length(com_genre)*0.2 && genre!="null")
      {
        max = length(com_genre[which(com_genre == genre)])
        max_index = genre
      }
    }
  cat(i,"\t",max_index,"\n")
  com_tag = c(com_tag,max_index)
}
    
