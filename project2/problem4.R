library('igraph')
filePath = '/Users/Xcy/Code/R/project2/project_2_data/graphMovie2.txt'
g = read.graph(file = filePath ,format = "ncol",directed = F)
com = fastgreedy.community(g)
