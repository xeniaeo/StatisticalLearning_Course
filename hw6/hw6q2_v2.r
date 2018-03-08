#setwd('/Users/xenia/Documents/R')
#data = load('cwsas_train_v2.rdata')
#model=hmm_train(train_sent$text2, train_sent$bmes_tag)
#sepchar = " "
#addsmooth = 1
#allsent = sample_sent

hmm_predict = function(model, allsent, sepchar = " ", addsmooth = 1){
	# model: data structure from hmm_train
	# allsent: the sentence we want to handle. it is a vector
	# sepchar: we seperate the word by this. default is " " (half space)
	# addsmooth: the value we want to add on additive smoothing. default is 1

	outsent = vector(mode="character", length(allsent))
	outtag = vector(mode="character", length(allsent))
	sbme = c("S","B","M","E")

	# prior log probability
	prior_prob = matrix(0,1,4)
	for(i in 1:4){
		prior_prob[1,i] = log(model$tprior_count[i] / sum(model$tprior_count))
	}

	# tseq additive smoothing & log probability
	tseq_prob = matrix(0,4,4)
	for(i in 1:4){
		for(j in 1:4){
			tseq_prob[i,j] = model$tseq_count[i,j] + 1
		}
	}
	sum = sum(tseq_prob)
	for(i in 1:4){
		for(j in 1:4){
			tseq_prob[i,j] = log(tseq_prob[i,j] / sum)
		}
	}

	ct = model1$ct_count
	ct = sweep(ct,1,rep(addsmooth,nrow(ct)),"+")
	ct = sweep(ct,1,sum(ct),"/")
	ct = log(ct)

	for(p in 1:length(allsent)){
		if(allsent[p]==""){
			outsent[p] = ""
			outtag[p] = ""
		}
		else{
			utf = unlist(utf8ToInt(allsent[p])) # 每個字轉成 utf 編碼
			for(j in 1:length(utf)){
				addsmooth_matrix[j,] = model$ct_count[utf[j],] + addsmooth
			}
			# input sentence log probability
			prob_log = matrix(0,length(utf),4)
			for(k in 1:length(utf)){
					prob_log[k,] = ct[utf[k],] #addsmooth_matrix
					# (sum(model$ct_count)+4*length(model$ct_count))
					# log(addsmooth_matrix[k,m]/(sum(addsmooth_matrix)))
			}
			# prob_log + prior_prob
			input_prior_sum = matrix(0,length(utf),4)
			for(i in 1:length(utf)){
				for(j in 1:4){
					input_prior_sum[i,j] = prob_log[i,j] + prior_prob[1,j]
				}
			}
			

			# 儲存每句句子當中，每個字最終算出來的 SBME 結果 # 最後一列的結果應該是不能用的
			sbme_temp = matrix(,length(utf),4)
			max_value = matrix(,length(utf),4)
			# tseq_prob + input_prior_sum
			for(i in 1:length(utf)){
				real_process=as.matrix(prior_prob[,])
				pre=c(ct[utf[j],]+t(real_process))
				input_prior_sum=sweep(tseq_prob,1,pre,"+")
				
				tseq_ip_sum = matrix(0,4,4)
				for(j in 1:4){
					tseq_ip_sum[j,] = tseq_prob[j,] + input_prior_sum[i,j]
				}
				# 找每行最大值的位置
				for(k in 1:4){
					max_value[i,k] = max(tseq_ip_sum[,k])
					sbme_temp[i,k] = sbme[which.max(tseq_ip_sum[,k])]
				}
			}

			# last word: max_value + prob_log
			last_word = matrix(0,1,4)
			for(i in 1:4){
				last_word[1,i] = max_value[length(utf)-1,i] + prob_log[length(utf),i]
			}
			last_tag = sbme[which.max(last_word)]
			# back track!
			best_tag = vector(mode="character", length(utf))
			best_tag[length(utf)] = last_tag
			for(i in length(utf):1){
				index = 0
				if(best_tag[i] == "S"){
					index = 1
				}
				else if(best_tag[i] == "B"){
					index = 2
				}
				else if(best_tag[i] == "M"){
					index = 3
				}
				else if(best_tag[i] == "E"){
					index = 4
				}
				best_tag[i-1] = sbme_temp[i-1,index]
			}
			#outsent[p] = allsent[p]

			outtag[p] = paste(best_tag, sep="", collapse="") 

			outsent[p] = ""
			for(j in 1:nchar(allsent[p])){
				temp = substr(allsent[p],j,j)
				letter = substr(outtag[p],j,j)
				outsent[p] = paste(outsent[p], temp, sep="")
				if(letter=="E" || letter=="S"){
					if(j!=nchar(allsent[p])){
						outsent[p] = paste(outsent[p], sepchar, sep="")
					}
				}
			}
		}
	}

	return(list(outsent = outsent, outtag = outtag))
}
