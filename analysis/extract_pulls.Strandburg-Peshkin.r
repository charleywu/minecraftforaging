#EXTRACT PULLS
#This file contains the functions required to extract successful ('pull') and failed ('anchor') pulling interactions. 

#FUNCTIONS

#get successful ('pull') and failed ('anchor') dyadic interactions between individuals a and b.
#Inputs:
#	xa: x coordinates for individual a (n.times x 1 vector or list)- must be continuous data (no NAs)
#	xb: x coordinates for individual b (n.times x 1 vector or list)- must be continuous data (no NAs)
#	ya: y coordinates for individual a (n.times x 1 vector or list)- must be continuous data (no NAs)
#	yb: y coordinates for individual b (n.times x 1 vector or list)- must be continuous data (no NAs)
#	a: index of the first individual 
#	b: index of the second individual
#	noise.thresh: noise threshold (defaults to 5 m)
#Outputs:
#	events: a dataframe containing dyadic interactions between a and b. 
#		Contains columns: t1, t2, t3, leader, follower, event.type, disparity, strength
get.dyad.interactions <- function(xa,xb,ya,yb,a,b, noise.thresh = 5, plot.results = F){
	
	#get the number of imtes
	n.times <- length(xa)
	
	#check to make sure xa, xb, ya, and yb all contain the same number of elements
	if(length(xb) != n.times | length(ya) != n.times | length(yb) != n.times){stop('ERROR: x and y data vectors are not all the same length')}
	
	#check to make sure data streams are continuous
	if(any(is.na(c(xa,xb,ya,yb)))){stop('ERROR: x and/or y data contain NAs for the specified individuals')}
	
	#get dyadic distances over time
	dyad.dist <- sqrt((xa-xb)^2 + (ya-yb)^2)
	
	#---find first minimum---
	#get first instance of a change in dyadic distance above noise.thresh, and determine whether it is going up or down
	i <- 1
	curr.min <- 1
	found = F
	while(found == F & i < length(dyad.dist)){
		dist.change <- dyad.dist[i] - dyad.dist[1]
		if(dyad.dist[i]<dyad.dist[curr.min]){
			curr.min <- i
		}
		if(abs(dist.change)>noise.thresh){
			found = T
			if(dist.change > 0){
				up.first <- T
			}
			else{
				up.first <- F
			}
		}
		else{
			i <- i + 1
		}
	}
	
	#if no first minimum found, return NULL
	if(!found){
		return(NULL)
	}

	#find starting point (if it went up first, this is curr.min, otherwise, this is the first minimum)
	if(up.first){
		first <- curr.min
	}
	else{
		curr.min <- 1
		found <- F
		while(i < n.times & !found){
			dist.diff <- dyad.dist[i] - dyad.dist[curr.min]
			if(dist.diff < 0){
				curr.min <- i
			}
			else{
				if(dist.diff > noise.thresh){
					found <- T
					first <- curr.min
				}
			}
			i<-i+1
		}
		if(!found){ #if no starting point found, return NULL
			return(NULL)
		}
	}
	
	#----pull out maxes and mins in dyadic distance----
	min.max.min <- data.frame(t1 = NA, t2 = NA, t3 = NA)
	min.max.min$t1[1] <- first
	ref.idx <- first
	ref.val <- dyad.dist[first]
	data.idx <- 1
	going.up <- T
	for(i in first:n.times){
		curr.idx <- i
		curr.val <- dyad.dist[i]
		ref.val <- dyad.dist[ref.idx] #NEW
		if(going.up){ #if going up
			if(curr.val > ref.val){ #if the current value is greater than the reference value
				ref.idx <- curr.idx #replace the reference value with the current value
				ref.val <- dyad.dist[ref.idx]
			}
			else{ #if not
				if(abs(curr.val - ref.val) > noise.thresh){ #if the difference between current and reference exceeds the noise threshold
          min.max.min$t2[data.idx] <- ref.idx #record the local max
					going.up <- F #switch to going down
          ref.idx <- i #NEW - ref index becomes the current index
				} 
			}
		}
		else{ #if going down
			if(curr.val < ref.val){ #if the current value is less than the reference value
				ref.idx <- curr.idx #replace the reference value with the current value
				ref.val <- dyad.dist[ref.idx]
			}
			else{ #if not
				if(abs(curr.val - ref.val) > noise.thresh){ #if the difference between current and reference exceeds a noise threshold
          min.max.min$t3[data.idx] <- ref.idx #record the local min as the end of the current sequence
					data.idx <- data.idx + 1 #increment the data index
					min.max.min <- rbind(min.max.min,c(ref.idx,NA,NA)) #record the local min as the start of the next sequence #USED TO BE REF.IDX
					going.up <- T #switch to going up
          ref.idx <- i #NEW - ref.idx becomes the current index
				}	
			}
		}
	}
	
	if(plot.results){
		plot(dyad.dist)
		for(i in 1:nrow(min.max.min)){
			abline(v=min.max.min$t1[i],col='red')
			abline(v=min.max.min$t2[i],col='green')
			abline(v=min.max.min$t3[i],col='red')
		}
	}
	
	#remove the last row if it is incomplete
	min.max.min <- min.max.min[which(!is.na(min.max.min$t3)),]
	
	#if no min.max.min sequences were found, return NULL
	if(nrow(min.max.min)==0){
		return(NULL)
	}
	
	#---determine whether they are successful pulls ('pull') or failed pulls ('anchor') and who is leading (attempting to pull)
	
	#get displacements for each individual during t1 to t2 (1) and t2 to t3 (2)
	min.max.min$disp.a.1 <- sqrt((xa[min.max.min$t2] - xa[min.max.min$t1])^2 + (ya[min.max.min$t2] - ya[min.max.min$t1])^2)
	min.max.min$disp.a.2 <- sqrt((xa[min.max.min$t3] - xa[min.max.min$t2])^2 + (ya[min.max.min$t3] - ya[min.max.min$t2])^2)
	min.max.min$disp.b.1 <- sqrt((xb[min.max.min$t2] - xb[min.max.min$t1])^2 + (yb[min.max.min$t2] - yb[min.max.min$t1])^2)
	min.max.min$disp.b.2 <- sqrt((xb[min.max.min$t3] - xb[min.max.min$t2])^2 + (yb[min.max.min$t3] - yb[min.max.min$t2])^2)
	
	#the leader is the one who moves more during the first time interval, the follower the one that moves less
	min.max.min$leader <- a
	min.max.min$follower <- b
	min.max.min$leader[which(min.max.min$disp.b.1 > min.max.min$disp.a.1)] <- b
	min.max.min$follower[which(min.max.min$disp.b.1 > min.max.min$disp.a.1)] <- a
	
	#it's a pull if the leader moves less during the second time interval
	min.max.min$type <- 'pull'
	min.max.min$type[which((min.max.min$leader == a) & (min.max.min$disp.a.2 > min.max.min$disp.b.2))] <- 'anchor'
	min.max.min$type[which((min.max.min$leader == b) & (min.max.min$disp.b.2 > min.max.min$disp.a.2))] <- 'anchor'
	
	#get the disparity of each event
	min.max.min$disparity <- (abs(min.max.min$disp.a.1 - min.max.min$disp.b.1) * abs(min.max.min$disp.a.2 - min.max.min$disp.b.2))/((min.max.min$disp.a.1 + min.max.min$disp.b.1)*(min.max.min$disp.a.2 + min.max.min$disp.b.2))
	
	#get the strength of each event
	min.max.min$strength <- (abs(dyad.dist[min.max.min$t2] - dyad.dist[min.max.min$t1])*abs(dyad.dist[min.max.min$t3] - dyad.dist[min.max.min$t2]))/((dyad.dist[min.max.min$t2] + dyad.dist[min.max.min$t1])*(dyad.dist[min.max.min$t2] + dyad.dist[min.max.min$t3]))
	
	#remove un-needed columns
	events <- min.max.min[,c(1,2,3,8,9,10,11,12)]
	
	return(events)
	
}

#get dyadic interactions for a pair of individuals, a and b, from continuous data (can contain NA's and can have breaks, e.g. day breaks)
#Inputs:
#	xa: x coordinates for individual a (n.times x 1 vector or list) - can contain NAs, and can cross sequences (give the indexes to the start of sequences in 'breaks')
#	xb: x coordinates for individual b (n.times x 1 vector or list) - can contain NAs, and can cross sequences (give the indexes to the start of sequences in 'breaks')
#	ya: y coordinates for individual a (n.times x 1 vector or list) - can contain NAs, and can cross sequences (give the indexes to the start of sequences in 'breaks')
#	yb: y coordinates for individual b (n.times x 1 vector or list) - can contain NAs, and can cross sequences (give the indexes to the start of sequences in 'breaks')
#	a: index of first individual
#	b: index of second individual
#	noise.thresh: noise threshold - minimum change in dyadic distance in order to escape a local max or min
#	breaks: list of break points in the data (e.g. indices to the beginning of days), (n.breaks x 1 vector or list)
#	min.seq.length: the minimum number of consecutive values needed in order to consider a sequence
get.dyad.interactions.all <- function(xa,xb,ya,yb,a,b,breaks,noise.thresh = 5, min.seq.length = 60){
	
	#check that xa, xb, ya, and yb are all the same length
	n.times <- length(xa)
	if(length(xb)!=n.times | length(ya)!=n.times | length(yb)!=n.times){
		stop('ERROR: input x and y data vectors are not all the same length')
	}
	
	#get events for each sequence, broken up by 'breaks' and any NAs
	events <- data.frame(NULL)
	for(i in 1:(length(breaks)-1)){
		begin <- breaks[i]
		finish <- breaks[i+1]-1
		xa.curr <- xa[begin:finish]
		xb.curr <- xb[begin:finish]
		ya.curr <- ya[begin:finish]
		yb.curr <- yb[begin:finish]
		
		#get indexes to NAs
		nas <- which(is.na(xa.curr) | is.na(xb.curr) | is.na(ya.curr) | is.na(yb.curr))
		nas <- c(0,nas,length(xa.curr)+1)
		
		na.diff <- diff(nas)
		
		starts <- nas[which(na.diff >= min.seq.length)]+1
		ends <- nas[which(na.diff >= min.seq.length)+1]-1

		
		if(length(starts)>0){
			for(j in 1:length(starts)){
			
				#get data to run analysis on
				xa.tmp <- xa.curr[starts[j]:ends[j]]
				xb.tmp <- xb.curr[starts[j]:ends[j]]
				ya.tmp <- ya.curr[starts[j]:ends[j]]
				yb.tmp <- yb.curr[starts[j]:ends[j]]
			
				#get dyadic interactions
				events.tmp <- get.dyad.interactions(xa.tmp,xb.tmp,ya.tmp,yb.tmp,a,b,noise.thresh=noise.thresh)
				
				if(!is.null(events.tmp)){
					#make times match up to the original sequence times
					events.tmp$t1 <- events.tmp$t1 + starts[j] + begin - 2
					events.tmp$t2 <- events.tmp$t2 + starts[j] + begin - 2
					events.tmp$t3 <- events.tmp$t3 + starts[j] + begin - 2
			
					#add current events to the data frame of events
					events.tmp$day <- i
					events.tmp$noise.thresh <- noise.thresh
					events <- rbind(events,events.tmp)
				}	
			}
		}

	}

	return(events)
	
}

#Get dyadic interactions for all pairs of individuals, based on matrices of x and y coordinates for all individuals
#Inputs:
#	xs: n.inds x n.times matrix of all x coordinates (for all individuals)
#	ys: n.inds x n.times matrix of all y coordinates (for all individuals)
#	breaks: indices to break points in the data (e.g. the indices to the starts of days)
#	noise.thresh: minimum change in dyadic distance needed to be considered an event
#	min.seq.length: minimum sequence length (non-NA) to analyze (sequences shorter than this will be dropped)
#Outputs:
#	events: a data frame containing the following columns:
#		
get.all.interactions<-function(xs,ys,breaks,noise.thresh=5,min.seq.length=60){
	N <- dim(xs)[1]
	t <- dim(xs)[2]
	
	if(dim(ys)[1] != N | dim(ys)[2] != t){
		stop('ERROR: x and y matrices must have the same dimensions')
	}
	
	if(N >= t){
		warning('WARNING: number of positions per individual is greater than number of individuals - do you need to transpose your x and y matrices?')
	}
	
	events <- data.frame(NULL)
	for(a in 1:(N-1)){
		for(b in (a+1):N){
			print(a)
			print(b)
			xa <- xs[a,]
			xb <- xs[b,]
			ya <- ys[a,]
			yb <- ys[b,]
			
			events.tmp <- get.dyad.interactions.all(xa,xb,ya,yb,a,b,breaks,noise.thresh,min.seq.length)
			
			if(!is.null(events.tmp)){
				events <- rbind(events,events.tmp)
			}
			
		}
	}
	return(events)
	
}

#EXAMPLE - DYADIC INTERACTIONS IN A TROOP OF BABOONS

#---Load data---

#You'll need to load in your data as two matrices called "xs" and "ys"
#Each matrix should be of dimensions N x T, where N is the number of individuals tracked, and T is the number of data points
#The matrix "xs" should hold the x coordinates for all individuals, and likewise for the matrix "ys"
#Here we will use x,y data from 26 baboons
#We also have a variable called "breaks" which gives the indexes at which new days begin (because we don't want to find pull events that overlap days)
# 
# #load the baboon data
# file.path <- '/Users/arianasp/Desktop/Dropbox/baboons_shared/ari/data/xy_level1.RData' 
# load(file.path)
# min.seq.length <- 60
# 
# #use the indexes of day starts as the breaks
# breaks <- day.start.idxs
# 
# #---Specify parameters---
# 
# #this parameter determines how much change in dyadic distance is needed in order to consider that the local maximum or minimum has been left
# #in general, this controls the amount of "jitter" allowed in the dyadic distance before it is counted as an event
# noise.thresh <- 5 
# 
# #this parameter controls how much one individual has to move more than another in order to differentiate it as the puller (or pullee)
# #it ranges from 0 to 1, where 0 lets in all events, and 1 requires that one individual do all the moving in each time segment of the event
# min.disparity <- 0.1
# 
# #this parameter controls how big the change in dyadic distance has to be (as compared to the total dyadic distance) in order to count an event
# #it ranges from 0 to 1, where 0 lets in all events, and 1 requires that the change in dyadic distance be equal to the sum of the dyadic distances for each time segment during the event
# min.strength <- 0.1
# 
# #---Get interactions for all dyads---
# events <- get.all.interactions(xs,ys,breaks,noise.thresh,min.seq.length)
# 
# #---Threshold events to remove events which do not exceed the thresholds---
# events <- events[which(events$disparity >= min.disparity & events$strength >= min.strength),]
# 
# #"events" now contains all of the interactions which have been discovered
# 
# 
# 
# 
# 













