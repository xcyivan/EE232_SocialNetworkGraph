f = open('./project_2_data/combineAct.txt','r')
g = open('./project_2_data/processedAct.txt','w')

split = '\t\t'
mycount = 0

for line in f.readlines():
	if(line.count(split)>=5):
		print "line %d has %d movies" % (mycount, line.count(split))
		g.write(str(line))
		mycount+=1

f.close()
g.close()