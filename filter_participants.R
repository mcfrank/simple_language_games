### REMOVE REPEATING SUBJECTS ###
remove.repeats <- function(data) {
	data <- data[!is.na(data$Answer.answer1),]

	data$SubmitTimePosix <- rep(as.POSIXct(NA),length(data$SubmitTime))
	for (i in 1:length(data$SubmitTime)) {
		data$SubmitTimePosix[i] <- as.POSIXct(as.character(data$SubmitTime[i]),
					format="%a %b %d %H:%M:%S GMT %Y")
	}

	# get rid of the second time for participants who did the task more than once
	i = 1
	subs <- unique(data$WorkerId)

	while (i < length(subs)) {
		subs <- unique(data$WorkerId)

		ind <- data$WorkerId==subs[i]
		if (sum(ind) > 1) {
			times <- data$SubmitTimePosix[ind]
			times <- sort(times)
			for (j in 2:length(times)) {	
				data <- data[data$SubmitTimePosix != times[j],]
				subs <- subs[data$SubmitTimePosix != times[j]]			}
		}
		i = i + 1;
	}

	# get rid of null participants
	i = 1
	while (i < length(data$WorkerId)) {
		if (data$Answer.answer1[i] == "00000000000" && data$Answer.answer2[i] == "00000000000") {
			ind = rep(TRUE,length(data$WorkerId))
			ind[i] = FALSE
			data <- data[ind,]
		} else {
			i = i + 1
		}	
	}

	return(data)
}

### FILTER JUNK RESPONSES OBJECTS ###
filter.responses.obj <- function (data) { 
	data$Answer.obj1val <- as.numeric(gsub("$","",as.character(data$Answer.obj1val)))
	data$Answer.obj2val <- as.numeric(gsub("$","",as.character(data$Answer.obj2val)))
	data$Answer.obj3val <- as.numeric(gsub("$","",as.character(data$Answer.obj3val)))

	data$Answer.obj1val[is.na(data$Answer.obj1val)] <- 0
	data$Answer.obj2val[is.na(data$Answer.obj2val)] <- 0
	data$Answer.obj3val[is.na(data$Answer.obj3val)] <- 0

	f1 = !is.na(as.numeric(paste(data$Answer.obj1val))) & 
		!is.na(as.numeric(paste(data$Answer.obj2val))) & 
		!is.na(as.numeric(paste(data$Answer.obj3val))) 
	data = data[f1==TRUE,]
	f2 = data$Answer.answer1==data$Input.num_objs1 & 
		data$Answer.answer2==data$Input.num_objs2 
	f2[is.na(f2)] = FALSE
	data = data[f2==TRUE,]
	f3 = as.numeric(paste(data$Answer.obj1val)) + as.numeric(paste(data$Answer.obj2val)) + as.numeric(paste(data$Answer.obj3val)) >= 99 &
		as.numeric(paste(data$Answer.obj1val)) + as.numeric(paste(data$Answer.obj2val)) + as.numeric(paste(data$Answer.obj3val)) <= 101
	data = data[f3==TRUE,]

	return(data)
}

### FILTER JUNK RESPONSES ADJECTIVES ###
filter.responses.adj <- function (data) { 
	data$Answer.adj1val <- as.numeric(gsub("$","",as.character(data$Answer.adj1val)))
	data$Answer.adj2val <- as.numeric(gsub("$","",as.character(data$Answer.adj2val)))
	
	data$Answer.adj1val[is.na(data$Answer.adj1val)] <- 0
	data$Answer.adj2val[is.na(data$Answer.adj2val)] <- 0
	
	f1 = !is.na(as.numeric(paste(data$Answer.adj1val))) & 
		!is.na(as.numeric(paste(data$Answer.adj2val)))
	data = data[f1==TRUE,]
	f2 = data$Answer.answer1==data$Input.num_objs1 & 
		data$Answer.answer2==data$Input.num_objs2 
	f2[is.na(f2)] = FALSE
	data = data[f2==TRUE,]
	f3 = as.numeric(paste(data$Answer.adj1val)) + as.numeric(paste(data$Answer.adj2val)) >= 99 &
		as.numeric(paste(data$Answer.adj1val)) + as.numeric(paste(data$Answer.adj2val))  <= 101
	data = data[f3==TRUE,]

	return(data)
}

### FILTER JUNK RESPONSES ADJECTIVES ###
filter.responses.2adj <- function (data) { 
	data$Answer.adj1val <- as.numeric(gsub("$","",as.character(data$Answer.adj1val)))
	data$Answer.adj2val <- as.numeric(gsub("$","",as.character(data$Answer.adj2val)))
	data$Answer.adj1notval <- as.numeric(gsub("$","",as.character(data$Answer.adj1notval)))
	data$Answer.adj2notval <- as.numeric(gsub("$","",as.character(data$Answer.adj2notval)))

	data$Answer.adj1val[is.na(data$Answer.adj1val)] <- 0
	data$Answer.adj2val[is.na(data$Answer.adj2val)] <- 0
	data$Answer.adj1notval[is.na(data$Answer.adj1notval)] <- 0
	data$Answer.adj2notval[is.na(data$Answer.adj2notval)] <- 0


	f1 = !is.na(as.numeric(paste(data$Answer.adj1val))) & 
		!is.na(as.numeric(paste(data$Answer.adj2val))) &
		!is.na(as.numeric(paste(data$Answer.adj1notval))) &
		!is.na(as.numeric(paste(data$Answer.adj2notval)))
	data = data[f1==TRUE,]
	f2 = data$Answer.answer1==data$Input.num_objs1 & 
		data$Answer.answer2==data$Input.num_objs2 
	f2[is.na(f2)] = FALSE
	data = data[f2==TRUE,]
	summed_resps <- data$Answer.adj1val + data$Answer.adj2val + data$Answer.adj1notval + data$Answer.adj2notval
	f3 = summed_resps >= 99 & summed_resps <= 101
	data = data[f3==TRUE,]

	return(data)
}
