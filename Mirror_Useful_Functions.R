# USefull functions

rank_set_code = function(theweights) {
  
  if(theweights==""){
    
    rankcode = ""
    
  } else {
    
    rankcode = as.numeric(gsub("[^0-9]", "", theweights))
  }
  return(rankcode)
}






# Function to calculate the percentage of missing data for each practice (FACTOR_NUMBER) for each account (ACCOUNT_CODE). 
OPS_cutoffs <- function(datInput, area, accountCode = 9000){
  # datInput is the input data frame
  # area is the function group
  # unweighted = FALSE if results should be weighted, TRUE otherwise (FALSE by default)
  # accountCode is the retailer code (9000 by default)
  
  datArea <- datInput %>%
    filter(FUNCTION_GROUP_CODE == area) %>% # Subset by functional group
    filter(!is.na(.$X202)) %>% 
    select(which(colMeans(is.na(.)) != 1)) %>%
    distinct()
  
  qver_count=data.frame(table(datArea$qver)); names(qver_count)=c("qver","tot")
  qver_prac=aggregate(. ~ qver, data=datArea[,-c(1,2,4,5,6)], function(x) {sum(!is.na(x))>0}, na.action = NULL)
  t = merge(qver_prac,qver_count)[,-1]
  
  tot=c(1,1,1,1,1,1)
  for(i in c(2:ncol(t)-1)){
    tot = c(tot, t[,i]%*%t$tot)
  }
  
  EXCLUDED = colSums(!is.na(datArea))/tot <= 0.3
  PERCENT = colSums(!is.na(datArea))/tot 
  
  CutOffs = data.frame(PERCENT, area, accountCode, EXCLUDED)
  CutOffs = CutOffs[-(1:6), ]
  Practice_names = rownames(CutOffs)
  CutOffs = cbind(Practice_names, CutOffs)
  rownames(CutOffs) = NULL
  
  return(CutOffs)
}