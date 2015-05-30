import re
f = open("./project_2_data/processedAct.txt","r")
#g = open("./project_2_data/graphAct.txt","w")
p = open("./project_2_data/movie_genre.txt","r")
q = open("./project_2_data/graphMovie.txt","w")
print 'the script begins running...'
#construct movie dictionary
movieDict={}
movieId=0
for line in p.readlines():
	tokens = line.split("\t\t")
	movieDict[tokens[0]]=[movieId]
	movieId+=1
print 'movie dictionary has been initialized successfully with length %d!'%(len(movieDict))

#construct actor dictionary
actDict={}
actId=0
for line in f.readlines():
	tokens = line.split("\t\t")
	name = tokens[0]
	# the starting element of the array is set as the actor's ID, which is also used as node ID
	tokens[0] = actId
	
	for i in range(1,len(tokens)):
		movie = tokens[i]
		year = re.search(r'\(\d\d\d\d\)|\(\?\?\?\?\)', movie)
		if year:
			end = movie.find(year.group())
			tokens[i] = movie[:end+6]

		if movieDict.has_key(tokens[i]):
			movieDict[tokens[i]].append(name)
		else:
			movieDict[tokens[i]]=[len(movieDict)]
			movieDict[tokens[i]].append(name)
	actDict[name] = tokens
	if actId%5000==0:
		print 'construct actDict: %d'%(actId)
	actId += 1
# print actDict
print 'actDict has been loaded successfully with length %d'%(len(actDict))
print 'movieDict have been loaded successfully with new length %d'%(len(movieDict))

# #---------------------------------------#
# ###########for problem 1-3#############
# #---------------------------------------#
# #add edge into edgeAct, which is a dict
# count = 0
# edgeAct={}
# for key in actDict:
# 	for i in range(1,len(actDict[key])):
# 		movie = actDict[key][i]
# 		for people in movieDict[movie]:
# 			# don't count the actor himself
# 			# 1st element of value of movieDict is set as movieId, skip this value
# 			if people==key or isinstance(people,int):
# 				continue
# 			if edgeAct.has_key((actDict[key][0],actDict[people][0])):
# 				edgeAct[(actDict[key][0],actDict[people][0])]+=1.0/(len(actDict[key])-1)
# 			else:
# 				edgeAct[(actDict[key][0],actDict[people][0])]=1.0/(len(actDict[key])-1)
# 	if count%1000==0:
# 		print 'construct edgeAct: %d'%(count)
# 	count+=1
# print 'edgeAct has been construct successfully with %d records!'%(len(edgeAct))

# #write to file graphAct
# count = 0
# for key in edgeAct:
# 	s = '%d\t%d\t%f\n'%(key[0],key[1],edgeAct[key])
# 	g.write(s)
# 	if count%10000==0:
# 		print 'write to file graphAct: %d'%(count)
# 	count+=1
# # print edgeAct
# print "Act graph edge processed successfully!"
# #---------------------------------------#
# ############end of this part#############
# #---------------------------------------#



#---------------------------------------#
###########for problem 4-6#############
#---------------------------------------#
newMovieDict={}
for key in movieDict:
	if(len(movieDict[key]))>5 and len(key)>0:
		newMovieDict[key]=movieDict[key]
		# s = '%s\t%s\n'%(key,str(newMovieDict[key]))
		# q.write(s)
print 'newMovieDict has loaded successfully with length %d'%(len(newMovieDict))

count=0
edgeMovie={}
edgeMovieDenom={}
for key in newMovieDict:
	for i in range(1,len(newMovieDict[key])):
		actor =  newMovieDict[key][i]
		for movie in actDict[actor]:
			if movie==key or isinstance(movie,int) or (not newMovieDict.has_key(movie)):
				continue
			totalAct = len(newMovieDict[key])+len(newMovieDict[movie])-2
			if edgeMovie.has_key((newMovieDict[key][0],newMovieDict[movie][0])):
				edgeMovie[(newMovieDict[key][0],newMovieDict[movie][0])]+=1
				edgeMovieDenom[(newMovieDict[key][0],newMovieDict[movie][0])]=totalAct
			elif edgeMovie.has_key((newMovieDict[movie][0], newMovieDict[key][0])):
				edgeMovie[(newMovieDict[movie][0], newMovieDict[key][0])]+=1
				edgeMovieDenom[(newMovieDict[movie][0], newMovieDict[key][0])]=totalAct
			else:
				edgeMovie[(newMovieDict[key][0],newMovieDict[movie][0])]=1
				edgeMovieDenom[(newMovieDict[key][0],newMovieDict[movie][0])]=totalAct
	if count%1000==0:
		print 'construct edgeMovie: %d'%(count)
	count+=1
print 'edgeAct has been construct successfully with %d records!'%(len(edgeMovie))

count=0
for key in edgeMovie:
	numerator = edgeMovie[key]/2
	denominator = edgeMovieDenom[key]-numerator
	weight = 1.0*numerator/denominator
	s = '%d\t%d\t%f\n'%(key[0],key[1],weight)
	q.write(s)
	if count%10000==0:
		print 'write to file graphMovie: %d'%(count)
	count+=1
print "Movie graph edge processed successfully!"


#---------------------------------------#
############end of this part#############
#---------------------------------------#


# # old method to construct graphAct, which is really really really really slow so is deprecated
# could be used to testify the correctness of graphAct
# print "finished loading data, processing the graph edge..."
# count=0
# for outterkey in actDict:
# 	for innerkey in actDict:
# 		if outterkey == innerkey:
# 			continue
# 		intersect = len(set(actDict[outterkey]) & set(actDict[innerkey]))
# 		if intersect > 0:
# 			weight = 1.0*intersect/(len(actDict[outterkey])-1)
# 			Edge = "%d\t%d\t%f\n"%(actDict[outterkey][0],actDict[innerkey][0],weight)
# 			g.write(Edge)
# 	count +=1
# 	if count%10 == 0 :
# 		print count

# # old method to construct graphMovie, which is really really really really slow so is deprecated
# could be used to testify the correctness of graphMovie
# count=0
# for outkey in newMovieDict:
# 	for inkey in newMovieDict:
# 		if outkey==inkey:
# 			continue
# 		intersect = len(set(newMovieDict[outkey])&set(newMovieDict[inkey]))
# 		combination = len(set(newMovieDict[outkey])|set(newMovieDict[inkey]))-2
# 		if intersect>0:
# 			weight = 1.0*intersect/combination
# 			edge = "%d\t%d\t%f\n"%(newMovieDict[outkey][0],newMovieDict[inkey][0],weight)
# 			q.write(edge)
# 	# if count%100==0:
# 	print 'write to file graphMovie: %d'%(count)
# 	count+=1
# print 'write to file graphMovie successfully!'

f.close()
#g.close()
p.close()
q.close()