# Estimate the testing performance of the Random Forest classifier using a typical train-tune-test method
rf_carton = function(dsall, folds, testfold=1, ypos = "pos", chi_threshold = 0.1, grid_length = 20, 
						grid_type = "loglinear", rfntree = 500, debuglevel = 1){
	# dsall: The input dataset. The first column should be the outcome of the classification problem
	# folds: The variable specifies how dsall should be partitioned for cross validation
	# testfold: the fold that should be reserved for testing
	# ypos: The label for a positive case in the first column
	# chi_threshold: The minimal value of chi squared statistics for a feature that should be returned
	# grid_length: The length of grids that should be used in parameter tuning
	# grid_type: The type of grids. The value should be either “equal” or “loglinear.”
	# rfntree: Number of trees in a forest
	# When debuglevel==0, don't output any debug info

	ycol = 1
	dstrain_all = dsall[-folds[[testfold]],]
	dstest = dsall[folds[[testfold]],]

	if(testfold==1) {
    	tunefold = 10
	} else {
    	tunefold = testfold-1
	}

	dstune = dsall[folds[[tunefold]],]
	#this is sub-train
	subtrain = dsall[-c(folds[[tunefold]], folds[[testfold]]),]

	subtrain_y = as.factor(subtrain[,ycol] == ypos)
	dstune_y = as.factor(dstune[,ycol] == ypos)

	subtrain_select <- filter_chisq(subtrain, ypos = ypos, chi_threshold = chi_threshold)
	filter_subtrain = subtrain[,subtrain_select$colpos]
  	filter_tune = dstune[,subtrain_select$colpos]

	# mtry: Determine number of features to be randomly selected as candidates before splitting a node
	m_min = 2
	m_max = ncol(filter_subtrain)
	if(grid_type == "equal"){
		grids = unique(round(seq(m_min, m_max, length=grid_length)))
	}else if(grid_type == "loglinear"){
		grids = unique(round(exp(seq(log(m_min), log(m_max), length=grid_length))))
	}

	f1_score = c()

	#sub_rf <- randomForest(subtrain_y, data = dstune, ntree = rfntree, proximity = TRUE)
	for (i in 1:length(grids)){
		model1 = randomForest(x = filter_subtrain, y = subtrain[,1], xtest = filter_tune, ytest = dstune[,1], ntree = rfntree, mtry = grids[i])
    	confusion1 = model1$test$confusion

    	if (ypos == "pos"){
     		precision1 = confusion1[2,2] / (confusion1[1,2] + confusion1[2,2])
      		recall1 = confusion1[2,2] / (confusion1[2,1] + confusion1[2,2])
      		f1_score1 = (2 * precision1 * recall1) / (precision1 + recall1)
    	}else if(ypos == "neg"){
      		precision1 = confusion1[1,1] / (confusion1[1,1] + confusion1[1,2])
      		recall1 = confusion1[1,1] / (confusion1[1,1] + confusion1[1,2])
      		f1_score1 = (2 * precision1 * recall1) / (precision1 + recall1)
    	}
    	f1_score[i] = f1_score1

		# F1-score = 2 * precision * recall / (precision + recall)
	}

	#max_index = which(f1_score == max(f1_score))
	#if(length(max_index) > 1){
	#	temp = c()
	#	for(i in length(max_index)){
	#		temp[i] = grids[max_index[i]]
	#	}
	#	best_mtry = grids[max_index[which.min(temp)]]
	#}else{
	#	best_mtry = grids[max_index]
	#}

	max_index = which.max(f1_score)
    best_mtry = grids[max_index]

	ftrain_select <- filter_chisq(dstrain_all, ypos, chi_threshold = chi_threshold)
	filter_train = dstrain_all[,ftrain_select$colpos]
  	filter_test = dstest[,ftrain_select$colpos]

	model2 <- randomForest(filter_train, dstrain_all[,1], filter_test, dstest[,1], ntree = rfntree, mtry = best_mtry)
	confusion2 = model2$test$confusion

	if (ypos == "pos"){
    	precision2 = confusion2[2,2] / (confusion2[1,2] + confusion2[2,2])
    	recall2 = confusion2[2,2] / (confusion2[2,1] + confusion2[2,2])
    	f1_score2 = (2 * precision2 * recall2) / (precision2 + recall2)
  	}else if(ypos == "neg"){
    	precision2 = confusion2[1,1] / (confusion2[1,1] + confusion2[1,2])
    	recall2 = confusion2[1,1] / (confusion2[1,1] + confusion2[1,2])
    	f1_score2 = (2 * precision2 * recall2) / (precision2 + recall2)
  	}
  
	test = list(precision = precision2, recall = recall2, f1 = f1_score2)
	return(list(mgrids = grids, f1_all = f1_score, best_m = best_mtry, test = test, fselect = ftrain_select))
}
