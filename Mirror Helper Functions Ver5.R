# # # # Mirror Helper Functions # # # # 


# Variable Importance Helper Function
varImp <- function(datAreaImp, WBASE, WCOUNT, account_code, unweighted=FALSE){
  
  #account_code = accountCode # For testing purposes.
  VarImp = NA; attempt = 1
	while(all(is.na(VarImp)) && attempt <=1000){
    attempt=attempt+1
    tryCatch({fit <- ranger(X202 ~ .,
                            data= select(datAreaImp, -retail_weight_value),
                            num.trees = 1000,
                            importance = "permutation",
                            case.weights = datAreaImp$retail_weight_value)
    
    VarImp = importance(fit); VarImp[VarImp < 0] = 0  }, error = function(e) {})}
  
  if( sum(is.na(VarImp)) > 0 ){

    nonconvergent_accounts = account_code
    accounts_that_fail_to_converge <<- rbind(accounts_that_fail_to_converge, cbind(nonconvergent_accounts, WBASE, WCOUNT))

  }
  
  
# CONVERSION TO 0-100
if(!all(is.na(VarImp))) { 
	if (max(VarImp)>0){
	VarImp = 100 * VarImp / max(VarImp)
	}
	return(VarImp)
	}
}



# Batch imputation Helper Function
batchImpute <- function(datAreaImp){
kk= nrow(datAreaImp)%/%5000

		randomDraw <- rnorm(nrow(datAreaImp))
		kQuantiles <- quantile(randomDraw, 0:kk/kk)
		whichK <- cut(randomDraw, kQuantiles, include.lowest = TRUE)  # Divide randomDraw into kk equally-sized groups

		datAreaImp$Chunk = whichK

		datAreaImp <- split(datAreaImp, f = datAreaImp$Chunk); rm(randomDraw, kQuantiles, whichK)

		dat = data.frame() # Imputed results data frame

		# Random Forest Imputation below
		pb <- winProgressBar(title = "progress bar", min = 0,
							 max = kk, width = 300)

		for (i in c(1:kk)){
		  if(any(is.na(datAreaImp[[i]]))) temp = rfImpute(X202 ~ ., data= select(datAreaImp[[i]],-Chunk), ntree= 500, iter= 3) else temp = datAreaImp[[i]]
		  dat = rbind(dat,temp)
		  rm(temp)
		  Sys.sleep(0.1)
		  setWinProgressBar(pb, i, title=paste( round(i/kk*100, 0),
												"% done"))
		}
		close(pb)
	
	return(dat)
}



# OPS Function
OPS <- function(datInput, area, accountCode = 9000){
  # datInput is the input data frame, datInput = dat9000, datInput = datRet, accountCode = retailer
  # area is the function group 
  # unweighted = FALSE if results should be weighted, TRUE otherwise (FALSE by default)
  # accountCode is the retailer code (9000 by default)
  
  #str(datRet)
  
  datArea <- datInput %>%
    filter(FUNCTION_GROUP_CODE == area) %>% # Subset by functional group
    filter(!is.na(.$X202)) %>% # Remove all the rows that have not rated practice 202
    select(which(colMeans(is.na(.)) != 1)) %>%
    distinct()
  
  #################################################################
  ######Debugging tool for seeing what the UBASE is for different factor
  #length(unique(datArea$retailrespondent_id[!is.nan(datArea$X1)]))
  #length(unique(datArea$retailrespondent_id[!is.nan(datArea$X4)]))
  #length(unique(datArea$retailrespondent_id[!is.nan(datArea$X6)]))
  #length(unique(datArea$retailrespondent_id[!is.nan(datArea$X7)]))
  # #write.csv(datArea, file = "AUSGRMI18_AFTER202filter_account9000.csv")
  ###################################################################
  
  qver_count=data.frame(table(datArea$qver)); names(qver_count)=c("qver","tot")
  qver_prac=aggregate(. ~ qver, data=datArea[,-c(1,2,4,5,6)], function(x) {sum(!is.na(x))>0}, na.action = NULL)
  t = merge(qver_prac,qver_count)[,-1]
  
  tot=c(1,1,1,1,1,1)
  for(i in c(2:ncol(t)-1)){
    tot = c(tot, t[,i]%*%t$tot)
  }
  
  datArea = datArea %>%  select(which(colSums(!is.na(datArea))/tot > 0.3)) %>% distinct()
  
  dat=datArea[,-c(1,2,3,5,6)] %>% group_by(retailrespondent_id) %>% summarise_at(.vars = colnames(datArea)[-c(1:6)],.funs=sum, na.rm=TRUE); dat[dat==0]=NA
    person_count = as.numeric(colSums(!is.na(subset(dat,select=c(-retailrespondent_id,-X202)))))

  dat=datArea[,-c(1:5)] %>% group_by(identifier) %>% summarise_at(.vars = colnames(datArea)[-c(1:6)],.funs=sum, na.rm=TRUE); dat[dat==0]=NA
    WBASE = as.numeric(colSums(!is.na(subset(dat,select=c(-identifier,-X202)))))
    
  datAreaImp <- datArea %>%
    select(-c(FUNCTION_GROUP_CODE, manufacturerrollup_id, retailrespondent_id, identifier,qver))
  
  WCOUNT = as.numeric(colSums(!is.na(subset(datAreaImp,select=c(-retail_weight_value,-X202)))))
  
  set.seed(2018)  # Set seed to make sure it is consistent
  
  # TRY IMPUTING
  tryCatch({if (any(is.na(datAreaImp)) & nrow(datAreaImp) < 15000) datAreaImp = rfImpute(X202 ~ ., data= datAreaImp, ntree= 500, iter= 3)}, error = function(e) {})
  
  # IF FAILS (i.e. DATASET TOO LARGE) DO IT IN BATCHES
  if (any(is.na(datAreaImp))){datAreaImp = batchImpute(datAreaImp)}
  
  out = data.frame()
  
  # WEIGHTED OPS
  WVarImp = varImp(datAreaImp, WBASE = WBASE, WCOUNT = WCOUNT, account_code = accountCode) # We take WBASE, WCOUNT and accountCode into the funtion because in case of lack of convergence we want to identity the accounts.
  if(!all(is.na(WVarImp))){
    out = rbind(out, data.frame(FACTOR_NUMBER = as.numeric(substr(names(WVarImp),2,5)), VALUE = WVarImp, ACCOUNT_CODE = accountCode, FUNCTION_GROUP_CODE = area, TYPE="W", WBASE = WBASE, PERSON_COUNT = person_count, ANSWER_COUNT = WCOUNT, stringsAsFactors = F))
  }
  
  # UNWEIGHTED OPS
  if (length(unique(datAreaImp$retail_weight_value)) == 1) {
  UVarImp = WVarImp
  } else {
  datAreaImp$retail_weight_value[datAreaImp$retail_weight_value!=0] = 1; UVarImp = varImp(datAreaImp, WBASE= WBASE, WCOUNT = WCOUNT, account_code = accountCode)
  }
  if(!all(is.na(UVarImp))){
    out = rbind(out, data.frame(FACTOR_NUMBER = as.numeric(substr(names(UVarImp),2,5)), VALUE = UVarImp, ACCOUNT_CODE = accountCode, FUNCTION_GROUP_CODE = area, TYPE="U", WBASE = WBASE, PERSON_COUNT = person_count, ANSWER_COUNT = WCOUNT, stringsAsFactors = F))
  }
  
  return(out)
}
