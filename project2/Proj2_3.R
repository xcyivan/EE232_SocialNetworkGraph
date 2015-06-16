library("igraph")
filePath = "Desktop/graphAct2.txt"
g_actor = read.graph(file = filePath ,format = "ncol",directed = T)
print("done")
pageRank = page.rank(g_actor, directed = T, damping = 0.85)
sorted_pageRank = sort(pageRank$vector,decreasing=T,index.return=T)
#top 10 pagerank # the vertex ID start from 0, so line x+1 in the text file should be the actor
#run processAct_top_10.py to get a file with top 10 actors
print(sorted_pageRank$ix[1:10])