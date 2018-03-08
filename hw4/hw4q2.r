filter_ig = function(dstrain, ypos = "pos", min_count = 5, ig_threshold = 10^(-5)){
	# dstrain: The training dataset for feature selection. 
	#          The first column should be the outcome of the classification problem
	# ypos: The label for a positive case in the first column
	# min_count: The minimal count for a feature that can be considered in feature selection. 
	# 			A feature that has a count <= min_count should be excluded in the feature selection process.
	# chi_threshold: The minimal value of chi squared statistics for a feature that should be returned

	# colpos: the position of useful features, ordered by the chi-squared statistics from large to small
	# colname: the column names of useful features following the same order as the those first component
	
	colpos = list()
	colname = list()
	igvalue = list()

	ig_all = list()
	ig_all[1] = -1

  	ypos_case = as.numeric(dstrain[,1] == ypos)
  	pos_proportion = sum(ypos_case)/nrow(dstrain)
  	#if(pos_proportion > 10^(-6)){
	#	info_d = (-pos_proportion)*log2(pos_proportion) - (1-pos_proportion)*log2(1-pos_proportion)
  	#}
  	#else{
  	#	info_d = 0
  	#}
  	info_d = (-pos_proportion)*log2(pos_proportion) - (1-pos_proportion)*log2(1-pos_proportion)

	for(j in 2:ncol(dstrain)){ 
		x_data = as.numeric(dstrain[,j] != 0)
		if(sum(x_data) > min_count){
			tb1 = table(ypos_case, x_data)
			ypos_prop = (tb1[3]+tb1[4]) / length(ypos_case)
			yneg_prop = 1 - ypos_prop

			ypos_xpos = tb1[3] / (tb1[3]+tb1[4])	
			ypos_xneg = 1 - ypos_xpos
			yneg_xpos = tb1[1] / (tb1[1]+tb1[2])
			yneg_xneg = 1 - yneg_xpos

			pp = (-ypos_xpos)*log2(ypos_xpos)
			pn = ypos_xneg*log2(ypos_xneg)
			np = (-yneg_xpos)*log2(yneg_xpos)
			nn = yneg_xneg*log2(yneg_xneg)
			if(!is.na(ypos_xpos) && ypos_xpos < 10^(-6)){
				#ypos_xpos = 0
				pp = 0
			}
			if(!is.na(ypos_xneg) && ypos_xneg < 10^(-6)){
				#ypos_xneg = 0
				pn = 0
			}
			if(!is.na(yneg_xpos) && yneg_xpos < 10^(-6)){
				#yneg_xpos = 0
				np = 0
			}
			if(!is.na(yneg_xneg) && yneg_xneg < 10^(-6)){
				#yneg_xneg = 0
				nn = 0
			}
			info_dword = ypos_prop * (pp - pn) + yneg_prop * (np - nn)
			ig_all[j] = info_d - info_dword
		}
		else{
			ig_all[j] = -1
		}
	}

	a = list()
	b = list()
	c = list()

	for(j in 2:ncol(dstrain)){
		if(!is.na(ig_all[j]) && ig_all[j] > ig_threshold){
			a = append(a, j)
			b = append(b, colnames(dstrain)[j])
			c = append(c, ig_all[j])
		}
	}
	colpos = unlist(a)
	colname = unlist(b)
	igvalue = unlist(c)

	if(length(colpos) > 0){
		colpos = colpos[order(igvalue, decreasing=TRUE)]
		colname = colname[order(igvalue, decreasing=TRUE)]
		igvalue = igvalue[order(igvalue, decreasing=TRUE)]
	}
	else{
		result = list(colpos = NULL, colname = NULL, igvalue = NULL)
		return(result)
	}
	result = list(colpos = colpos, colname = colname, igvalue = igvalue)
	return(result)

	#setwd("/Users/xenia/Documents/R")
	#load('hw4ds1.rdata')
	#testfold = 1
	#dstrain = hw4ds1[-folds[[testfold]],]
}
