filter_chisq = function(dstrain, ypos = "pos", min_count = 5, chi_threshold = 10^(-5)){
	# dstrain: The training dataset for feature selection. 
	#          The first column should be the outcome of the classification problem
	# ypos: The label for a positive case in the first column
	# min_count: The minimal count for a feature that can be considered in feature selection. 
	# 			A feature that has a count <= min_count should be excluded in the feature selection process.
	# chi_threshold: The minimal value of chi squared statistics for a feature that should be returned

	# colpos: the position of useful features, ordered by the chi-squared statistics from large to small
	# colname: the column names of useful features following the same order as the those first component
	# chistat: the Chi-squared statistics of useful features following the same order as the those in the first component
	
	#setwd("/Users/xenia/Documents/R")
	#load('hw4ds1.rdata')
	#testfold = 1
	#dstrain = hw4ds1[-folds[[testfold]],]

	colpos = list()
	colname = list()
	chistat = list()

	#dstrain = as.matrix(dstrain[,2:ncol(dstrain)])
	#dstrain[dstrain>0]<-1

	chi_all = list()
	chi_all[1] = -1

  	ypos_case = as.numeric(dstrain[,1] == ypos)
	for(j in 2:ncol(dstrain)){ 
		x_data = as.numeric(dstrain[,j] != 0)
		if(sum(x_data) > min_count){
			tb1 = table(ypos_case, x_data)
			chi_all[j] = chisq.test(tb1, correct=FALSE)$statistic
		}
		else{
			chi_all[j] = -1
		}
	}

	a = list()
	b = list()
	c = list()

	for(j in 2:ncol(dstrain)){
		if(chi_all[j] > chi_threshold){
			a = append(a, j)
			b = append(b, colnames(dstrain)[j])
			c = append(c, chi_all[j])
		}
	}
	colpos = unlist(a)
	colname = unlist(b)
	chistat = unlist(c)

	if(length(colpos) > 0){
		colpos = colpos[order(chistat, decreasing=TRUE)]
		colname = colname[order(chistat, decreasing=TRUE)]
		chistat = chistat[order(chistat, decreasing=TRUE)]
	}
	else{
		result = list(colpos = NULL, colname = NULL, chistat = NULL)
		return(result)
	}
	result = list(colpos = colpos, colname = colname, chistat = chistat)
	return(result)
}
