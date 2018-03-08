#setwd('/Users/xenia/Documents/R')
#data = load('cwsas_train_v2.rdata')
#sentvec=train_sent$text2[1:100]
#tagvec = train_sent$bmes_tag[1:100]


hmm_train = function(sentvec, tagvec){
	# sentvec: training sentence. stored as a vector
	# tagvec: sentvec's BMSE tag. its length should be equal to sentvec's
	# B:2, M:3, E:4, S:1

	# SBME 的出現頻率
	tprior_count = matrix(,4,1)
	rownames(tprior_count)<-c("S","B","M","E")
	# SBME 的轉換頻率
	tseq_count = matrix(0,4,4)
	colnames(tseq_count)<-c("S","B","M","E")
	rownames(tseq_count)<-c("S","B","M","E")
	# 􏱆􏲲􏲯􏲹􏲺􏱈􏴍􏴎􏳷􏱬􏴏􏴐􏴆􏱆􏲲􏲯􏲹􏲺􏱈􏴍􏴎􏳷􏱬􏴏􏴐􏴆字元與標記的聯合次數分配表
	ct_count = matrix(0,70000,4)
	colnames(ct_count)<-c("S","B","M","E")

	for(i in 1:length(sentvec)){
		utf = unlist(utf8ToInt(sentvec[i]))
		tag = strsplit(tagvec[i],"")
		tag = unlist(tag)
		for(j in 1:length(utf)){
			if(tag[j]=="S"){
				ct_count[utf[j],1] = ct_count[utf[j],1] + 1
				if(j!=length(utf)){
					if(tag[j+1]=="S"){
						tseq_count[1,1] = tseq_count[1,1] + 1
					}
					else if(tag[j+1]=="B"){
						tseq_count[1,2] = tseq_count[1,2] + 1
					}
					else if(tag[j+1]=="M"){
						tseq_count[1,3] = tseq_count[1,3] + 1
					}
					else if(tag[j+1]=="E"){
						tseq_count[1,4] = tseq_count[1,4] + 1
					}
				}
			}
			else if(tag[j]=="B"){
				ct_count[utf[j],2] = ct_count[utf[j],2] + 1
				if(j!=length(utf)){
					if(tag[j+1]=="S"){
						tseq_count[2,1] = tseq_count[2,1] + 1
					}
					else if(tag[j+1]=="B"){
						tseq_count[2,2] = tseq_count[2,2] + 1
					}
					else if(tag[j+1]=="M"){
						tseq_count[2,3] = tseq_count[2,3] + 1
					}
					else if(tag[j+1]=="E"){
						tseq_count[2,4] = tseq_count[2,4] + 1
					}
				}
			}
			else if(tag[j]=="M"){
				ct_count[utf[j],3] = ct_count[utf[j],3] + 1
				if(j!=length(utf)){
					if(tag[j+1]=="S"){
						tseq_count[3,1] = tseq_count[3,1] + 1
					}
					else if(tag[j+1]=="B"){
						tseq_count[3,2] = tseq_count[3,2] + 1
					}
					else if(tag[j+1]=="M"){
						tseq_count[3,3] = tseq_count[3,3] + 1
					}
					else if(tag[j+1]=="E"){
						tseq_count[3,4] = tseq_count[3,4] + 1
					}
				}
			}
			else if(tag[j]=="E"){
				ct_count[utf[j],4] = ct_count[utf[j],4] + 1
				if(j!=length(utf)){
					if(tag[j+1]=="S"){
						tseq_count[4,1] = tseq_count[4,1] + 1
					}
					else if(tag[j+1]=="B"){
						tseq_count[4,2] = tseq_count[4,2] + 1
					}
					else if(tag[j+1]=="M"){
						tseq_count[4,3] = tseq_count[4,3] + 1
					}
					else if(tag[j+1]=="E"){
						tseq_count[4,4] = tseq_count[4,4] + 1
					}
				}
			}
		}
	}

	tprior_count[1,1] = sum(ct_count[,1])
	tprior_count[2,1] = sum(ct_count[,2])
	tprior_count[3,1] = sum(ct_count[,3])
	tprior_count[4,1] = sum(ct_count[,4])

	return(list(ct_count = ct_count, tseq_count = tseq_count, tprior_count = tprior_count))
}
