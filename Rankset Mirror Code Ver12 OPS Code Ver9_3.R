# MIRROR OPS BY RANKSET PRIORITIES


# LOADING LIBRARIES
source("Q:/MARKET DEVELOPMENT/OPS Runs 2018/OPS2018/Important R Scripts/Mirror Helper Functions Ver5.r")
source("Q:/MARKET DEVELOPMENT/OPS Runs 2018/OPS2018/Important R Scripts/Mirror_Useful_Functions.R")
library(readxl)
library(openxlsx)
library(dplyr)
library(reshape2)
library(randomForest)
library(ranger)

#######################################################################################################################################################
############################################   INPUT PANEL  ###########################################################################################
#################################    ALL OPTIONS AND INPUT IS FED HERE   ##############################################################################
#################################    EVERYTHING IS CONTROLLED FROM HERE    ############################################################################
#######################################################################################################################################################


Computer = "VM1 16GB"      # Options: "Desktop" or "VM1 16GB" or "VM2" or "VM3" or "VM4"
CodeVersion = "Ver9.3"    # The latest version of the OPS algorithm code is indicated here. 
SaveAnalyitcs = "Yes"     # Options: "Yes" or "No". This will save the analytics of this run to the Main Analytics file. 
OPS_request_by ="Maulin"  # Options: "Tatyana" or "Cindy" or "Maulin" or "Chaya" or "Fatih", Enter the name of the person requesting OPS data: Typical
Run = "Final Run"           # Options: "Final Run", "Partial Run", "Post-program Run"
Program_year = 2019       # Option: 2018, 2019
Mirror_run = "by Rankset" # Option: "by Rankset"
Mirror_by_rankset_version = "Ver12"  # The latest version of the Mirror Implementation of the OPS algorithm code is indicated here. 
Program_type = "Mirror"    # Option: "Mirror"


PROJECTNAME = ""       # E.g.: "MEXGRHO18 WHL_P5640 - AG3_Specs_OPS - 07102018" 
weights = ""             # Either Empty "" or like "RS5100"

######################################################################################################################################################
####################################   END OF INPUT PANEL ############################################################################################
#################
#################    The data files required are the excel tabs: "FDATA", "Ransksets", and "Factor Rollups"
#################
#################
#################   EVERYTHING BELOW IS THE OPS R CODE ALGORITHM IMPLEMENTATION ######################################################################
#################
#################   PLEASE PROCEED WITH GREAT CARE BEYOND THIS POINT #################################################################################
#################   
######################################################################################################################################################

### LOADING FILES
t0 = Sys.time()

rank_code = rank_set_code(weights)
Accounts = trimws(paste("Accounts", weights, sep = " "))
PATH = paste(PROJECTNAME, ".xlsx", sep = "")
FILENAME = trimws(paste(weights, PROJECTNAME, sep = " "))


PROG <- read_excel(PATH, sheet = "FDATA")
Accounts <- read_excel(PATH, sheet = Accounts)
Functions <- read_excel(PATH, sheet = "Functions")
Practice <- read_excel(PATH, sheet = "Practice")
Parent <- read_excel(PATH, sheet = "Parent")
Reporting_Sequence <- read_excel(PATH, sheet = "Factor Rollups") # We also need to read this sheet so can do a join at the end. 
rankset = read_excel(PATH, sheet = "Ranksets") # Pull the list of ranksets from this sheet. 

# STEP 2 1a) and 1b)
t1 = Sys.time() - t0

PROG = rename(PROG, businessuint = "BUSINESS UNITS") # Rename BUSINESS UNIT to businessunit. 

# Find the unique ranksets in the business code

#The below code 
BUS = paste0(PROG$businessuint, collapse = "," )
#PROG$businessuint
# class(BUS)
# dim(BUS)
# length(BUS)

#BUS
rankcodes = unlist(as.vector(strsplit(BUS, "[,]")))
#rankcodes
#class(rankcodes)
is.vector(rankcodes)

#which(rankcodes == "")
rankcodes = rankcodes[!rankcodes == ""]
#rankcodes
rankset_codes = sort(as.numeric(unique(rankcodes)))
rankset_codes
BU_ranksets = sort(subset(rankset, TYPE == "BU")$RANK_SET_CODE)
BU_ranksets

# Give error if even one FALSE, and do not proceed until receive okay from I.S. 
all(rankset_codes == BU_ranksets) == TRUE 
# This is just a check, in general should be TRUE and may not be in all cases. 

# IF this is not TRUE, THEN stop and do not proceed with OPS. Need to let Maulin and 
# dataprocessing know they do not match. Means there is an Error in the datasomewhere. 

#BU_ranksets %in% rankset_codes


########   CREATE THE Rankset as rollup for Parent


# For "all" program use BU ranksets from the "Rankset" tab. 
rankset_codes = BU_ranksets # very imprtant set. Do not skip. 
# The BU_ranksets from the Rankset tab are more updated, and are changed in the Rankset tab
# based on what the program cutoffs, specificities and % of missing data. They all incfluence
# the ranksets that are removed from the program and the updated list of ranksets is only in the 
# "Rankset" tab. 

# The Parent sheet should be empty in the Specs file. We will create the proper Parent data below. 


# Use rankset_codes for the ranksets. This was pulled from the FDATA.
# Use this because this is more robust. 


parent_dataset = NULL
for (i in rankset_codes ){ # Previously used unique(rankset$RANK_SET_CODE)
  
  #i = 2112
  #i = 9000 # Should 9000 be in here? I don't think so.
  
  I = as.character(i)
  
  # Select only the rows with accounts that have the RANK_SET_CODE 'i' in the variable 'BUSINESS UNIT'. 
  prog = PROG[ grep(I, PROG$businessuint) , ] %>% select(ACCOUNT_CODE)
  prog = subset(prog,!duplicated(prog$ACCOUNT_CODE))
  prog$ACCOUNT_CODE = as.numeric(prog$ACCOUNT_CODE)
  #prog # print to check everything looks good. 
  
  Parent = i + .5 # Add 0.5 to differentiate rankset code from the ACCOUNT_CODE codes. 
  prog = cbind(prog, Parent)
  parent_dataset = bind_rows(parent_dataset, prog)
  #print(i)
  #print(prog)
} 

all(unique(parent_dataset$Parent) == unique(BU_ranksets + .5)) # This should be true. If not check ranksets.
Parent = parent_dataset

####

PROG = PROG %>% merge(Accounts, by=c("ACCOUNT_CODE")) %>% merge(Practice, by=c("FACTOR_NUMBER","qver")) %>%  mutate(identifier=retailrespondent_id)


# SUBSET DATA TO KEEP ONLY REQUIRED COLUMNS

person <- subset( PROG,
                  select=c(
                    ACCOUNT_CODE,
                    manufacturerrollup_id,
                    FACTOR_NUMBER,
                    FUNCTION_GROUP_CODE, qver,
                    retailrespondent_id,
                    rating,
                    retail_weight_value,
                    identifier,
                    businessuint
                  )
)

# CHANGE DATA TYPES TO ENSURE THEY ARE IDEAL
person$ACCOUNT_CODE = as.numeric(person$ACCOUNT_CODE)
person$manufacturerrollup_id = as.numeric(person$manufacturerrollup_id)
person$FACTOR_NUMBER = as.numeric(person$FACTOR_NUMBER)
person$FUNCTION_GROUP_CODE = as.factor(person$FUNCTION_GROUP_CODE)
person$qver = as.factor(person$qver)
person$retailrespondent_id = as.factor(person$retailrespondent_id)
person$rating = as.numeric(person$rating)
person$retail_weight_value = as.numeric(person$retail_weight_value)
person$businessuint= as.character(person$businessuint)

# STEP 2 1c)
t2 = Sys.time() - t0

person <- filter(person, FACTOR_NUMBER != 80)
# ADD "X" TO BEGINNING OF EACH FACTOR NUMBER (e.g. 202 -> X202)
person$FACTOR_NUMBER = paste0("X", person$FACTOR_NUMBER)

# STEP 2 1e)
t3 = Sys.time() - t0

for (area in unique(person$FUNCTION_GROUP_CODE)){
  tryCatch({
    p = person[person$FUNCTION_GROUP_CODE!=area & person$FACTOR_NUMBER == "X202",]; p$FUNCTION_GROUP_CODE=area
    person = rbind(person,p); rm(p)
  }, error=function(e) {} )
}

# STEP 2 1d)
t4 = Sys.time() - t0


rollup_accounts = merge(person,Parent, by=c("ACCOUNT_CODE"))
rollup_accounts$ACCOUNT_CODE = rollup_accounts$Parent
rollup_accounts = rollup_accounts %>% select(-Parent)

# What we need to do now is for each rankset set (ENDING with 0.5) using the "businessunit" column
# filter keep only the rows that have the rankset_code. 
# I.e. Need to filter by the BU and only keep the rows with the correctly filtered BU's. 
# We do this because not all business units are mapped to the accounts. 
# "There are programs who will do business units by people and not by account 
# Which is why it is important to go by the business unit column." - Cindy from IS.

################# Simple Checks ######################
# unique(rollup_accounts$ACCOUNT_CODE)
# dim(rollup_accounts)
# class(rollup_accounts$ACCOUNT_CODE)
# length(rankset_codes)
# length(unique(rollup_accounts$ACCOUNT_CODE))
###################################################


# Need to do this before we row bind. 
proper_rollup_accounts = NULL
counter = 0 
for (i in unique(rollup_accounts$ACCOUNT_CODE)) {
  
  # # #i = 2002.5
  # Make a seperate indicator j to do the manupulations in the loop
  j = i 
  
  #print("t1")
  data_i = filter(rollup_accounts, ACCOUNT_CODE == j )
  # Place a check to make sure i has decimal 0.5. 
  
  j = j - 0.5 
  
  I = as.character(j)
  
  #print("t2")
  
  # Select only the rows with accounts that have the RANK_SET_CODE 'i' in the variable 'BUSINESS UNIT'.
  data_i  = data_i[ grep(I, data_i$businessuint) , ] 
  
  #print("t3")
  
  #dim(data_i)
  #unique(data_i$ACCOUNT_CODE)
  proper_rollup_accounts = bind_rows(proper_rollup_accounts, data_i)
  
  #print(i)
  #print(unique(data_i$ACCOUNT_CODE))
  
  #print("t4")
  # Delete them at the end to make sure no carry over effect.
  rm(i)
  rm(j)
  #print(counter)
  counter = counter +1
}

#dim(proper_rollup_accounts)

# Now row bind the correct rollups
person = rbind(person, proper_rollup_accounts) 

#write.csv(person, file = "person.csv")

# See which accounts and ranksets will have OPS data.
unique(person$ACCOUNT_CODE)


### STEP 3 BEGINS HERE ###
t5 = Sys.time() - t0

# TRANSFORM INPUT DATA FRAME INTO A PIVOT TABLE WITH ONE COLUMN FOR EACH FACTOR NUMBER
df <- dcast(person, ACCOUNT_CODE + manufacturerrollup_id + FUNCTION_GROUP_CODE + qver + retailrespondent_id + retail_weight_value + identifier ~ FACTOR_NUMBER, mean, value.var = "rating", fill = NaN)

# Remove rows where everything is missing except 202
cleanup = subset(df,select=-c(ACCOUNT_CODE, manufacturerrollup_id,FUNCTION_GROUP_CODE,qver, retailrespondent_id,retail_weight_value,identifier,X202)); cleanup = apply(cleanup,1,function(x)any(!is.na(x)))
df=df[cleanup,]

# ORDER DATA FRAME TO ENSURE RESULTS ARE THE SAME EACH TIME
df=df[with(df,order(retailrespondent_id,identifier,ACCOUNT_CODE,qver, manufacturerrollup_id, FUNCTION_GROUP_CODE)),]


# CREATING THE DATA FRAMES TO STORE MAIN OUTPUT RESULTS DATA
Output = data.frame() # empty results data frame
accounts_that_fail_to_converge = data.frame() # Empty data frame

# RETAILER 9000
t6 = Sys.time() - t0

dat9000 = df %>%
  select(which(colMeans(is.na(.)) != 1)) %>% # REMOVE PRACTICES WITH NO RESPONSES (shouldn't be any)
  select(-c(ACCOUNT_CODE)) %>% # REMOVE ACCOUNT_CODE COLUMN - NOT RELEVANT ANYMORE
  droplevels

# WITHIN RETAILER 9000, CALCULATE OPS FOR EACH FUNCTION SEPARATELY
t7 = Sys.time() - t0

for (area in levels(dat9000$FUNCTION_GROUP_CODE)){ # FUNCTIONAL AREA LOOP
  tryCatch({ # CHECK IF FUNCTIONAL AREA IS MISSING
    if (all(dat9000$retail_weight_value==0)){dat9000$retail_weight_value=1}
    temp = OPS(dat9000, area) # DERIVED FUNCTION
    Output = rbind(Output,temp) # ADD RESULTS TO FINAL DATA FRAME
  }, error = function(e) {})
}


# BY Rankset
t8 = Sys.time() - t0

###################################################
####### Debugging tool for ransets that are missing
# names(df)
# subset(df, ACCOUNT_CODE == 2050.5)
# str(df)
# df$ACCOUNT_CODE
###################################################

for (retailer in unique(Parent$Parent) ){ # Rankset LOOP
  #retailer = 5016.5
  datRet = df %>% filter(ACCOUNT_CODE == retailer) %>% # FILTER DATA TO ONLY LOOK AT THIS RETAILER
    select(which(colMeans(is.na(.)) != 1)) %>% # REMOVE PRACTICES WITH NO RESPONSES
    select(-c(ACCOUNT_CODE)) %>% # REMOVE ACCOUNT_CODE COLUMN - NOT RELEVANT ANYMORE
    droplevels
  
  # #################################################
  # # Debugging for c(2003.5, 2008.5, 2112.5, 5014.5)
  # # Turn debugging off by commenting out this section
  # 
  # #retailer = 5102.5
  # datRet = df %>% filter(ACCOUNT_CODE %in% c(2003.5, 2008.5, 2112.5, 5014.5)) %>% # FILTER DATA TO ONLY LOOK AT THIS RETAILER
  #   select(which(colMeans(is.na(.)) != 1)) %>% # REMOVE PRACTICES WITH NO RESPONSES
  #   select(-c(ACCOUNT_CODE)) %>% # REMOVE ACCOUNT_CODE COLUMN - NOT RELEVANT ANYMORE
  #   droplevels
  # 
  # #write.csv( datRet, file = "Rdata_forGLO_MissingUBASEranksets_10122018.csv" )
  # 
  # #length(unique(datRet$retailrespondent_id[!is.nan(datRet$X22)]))
  # #Amazingly this gives the correct UBASE, it gives 81 instead of 80.
  # #So everything upto here should be good.
  # 
  # length(unique(datRet$retailrespondent_id[!is.nan(datRet$X65)]))
  # ids_202 = as.numeric(unique(datRet$retailrespondent_id[!is.nan(datRet$X65)]))
  # 
  # datRet = datRet[ !is.na(datRet$X202) , ]
  # length(unique(datRet$retailrespondent_id[!is.nan(datRet$X65)]))
  # 
  # ids_202_65 = as.numeric(unique(datRet$retailrespondent_id[!is.nan(datRet$X65)]))
  # 
  # ids_202[!(ids_202 %in% ids_202_65)]
  # # Find respondant_id that is NaN
  # 
  # length(unique(datRet$retailrespondent_id[!is.nan(datRet$X26)]))
  # datRet = datRet[ !is.na(datRet$X202) , ]
  # length(unique(datRet$retailrespondent_id[!is.nan(datRet$X65)]))
  # 
  # # Which id's are missing
  # unique(datRet$retailrespondent_id[is.nan(datRet$X65)])
  # 
  # # End of Debugging
  # #############################################
  
  
  # Check if response missing completely - if so go to next retailer
  if (is.null(datRet$X202)) next
  
  if (all(datRet$retail_weight_value==0)){datRet$retail_weight_value=1}
  
  # BY FUNCTION
  for (area in levels(datRet$FUNCTION_GROUP_CODE)){  # FUNCTIONAL AREA LOOP
    #area = "ALL"
    tryCatch({ # CHECK IF FUNCTIONAL AREA IS MISSING
      temp = OPS(datRet, area, accountCode=retailer) # DERIVED FUNCTION
      Output = rbind(Output,temp) # ADD RESULTS TO FINAL DATA FRAME
    }, error = function(e) {})
  }
} # END by retailer loop




# STEP 4
t9 = Sys.time() - t0

## Aggregating to ALL



# THIS is an important step. So far everything is correct. The only issue is that if we have FUNCTION WEIGHTS for 
# more than one FUNCTION GROUP "ALL". E.g. have EX/SM, CM, IL in the Functions sheet of the data, then
# have to replace the WVALUE of ALL, (the weighted OPS values/results that we computed for ALL) with the weighted average of the 
# OPS values/results of the three FUNCTIONS EX/SM, CM, IL. 

if(dim(Functions)[1] > 1 ){ # Previous and Erica's criterion: length(unique(df$FUNCTION_GROUP_CODE))> 1 
  
  # This Criterion will make sure the below does not run when Function weights are blank and 
  # there are more than just the FUNCTION_GROUP_CODE "ALL" function, like CM, EX/SM, etc.  
  # This is an alternative criterion to the above which could also be used: unique(df$retail_weight_value)>1
  
  function_weights = Functions %>% filter(!is.na(function_weight_value) & function_weight_value != 0)
  
  Output = Output %>% filter(!(FUNCTION_GROUP_CODE == "ALL" & TYPE == "W"))
  
  datMarket = merge(Output, function_weights)
  
  # Weighted
  WAll = datMarket %>% filter(TYPE == "W") %>% group_by(FACTOR_NUMBER,ACCOUNT_CODE) %>% summarise(VALUE = weighted.mean(VALUE,function_weight_value),PERSON_COUNT=sum(PERSON_COUNT),ANSWER_COUNT=sum(ANSWER_COUNT),WBASE=sum(WBASE), TYPE="W", FUNCTION_GROUP_CODE="ALL")
  
  Output = rbind(Output,data.frame(WAll))
  
}


t10 = Sys.time() - t0


weighted= Output %>% filter(TYPE=="W")
unweighted= Output %>% filter(TYPE=="U")
names(weighted)=c("FACTOR_NUMBER","WVALUE","ACCOUNT_CODE","FUNCTION_GROUP_CODE","TYPE","WBASE","PERSON_COUNT","WCOUNT")
names(unweighted)=c("FACTOR_NUMBER","UVALUE","ACCOUNT_CODE","FUNCTION_GROUP_CODE","TYPE","UBASE","PERSON_COUNT","UCOUNT")

output_dataframe=merge(weighted,unweighted,all.x=TRUE,by=c("FACTOR_NUMBER","ACCOUNT_CODE","FUNCTION_GROUP_CODE"))

output_dataframe$PROG_VER_ID = PROG$PROG_VER_ID[1]
output_dataframe$MODULE_NAME = '';
output_dataframe$RANK_SET_CODE ='';
output_dataframe$RANKSET_NAME = '';
output_dataframe$REV_DATE = '';

t11 = Sys.time() - t0


output_dataframe = output_dataframe %>% group_by(ACCOUNT_CODE, FUNCTION_GROUP_CODE) %>% mutate(URNK=min_rank(round(-UVALUE)))
output_dataframe = output_dataframe %>% group_by(ACCOUNT_CODE, FUNCTION_GROUP_CODE) %>% mutate(WRNK=min_rank(round(-WVALUE)))
a9000 <- output_dataframe[output_dataframe$ACCOUNT_CODE==9000,]
a9000$UTOTAL = a9000$UVALUE
a9000$WTOTAL = a9000$WVALUE
a9000 <- subset(a9000, select=c("FACTOR_NUMBER", "FUNCTION_GROUP_CODE", "UTOTAL", "WTOTAL"))
AggR <- merge(output_dataframe,a9000, by=c("FACTOR_NUMBER","FUNCTION_GROUP_CODE"),all=TRUE)
AggR$UTOTAL[is.na(AggR$UTOTAL)]=0; AggR$WTOTAL[is.na(AggR$WTOTAL)]=0


t12 = Sys.time() - t0

######### Step 5: Formatting the final OPS data. 


#### Performing the below to have correct OPS values for the "exception case". 
AggR = AggR[,c("PROG_VER_ID", "ACCOUNT_CODE","MODULE_NAME","RANK_SET_CODE","RANKSET_NAME", "FACTOR_NUMBER", "FUNCTION_GROUP_CODE","WBASE","WVALUE","WCOUNT", "WTOTAL", "WRNK", "UBASE", "UVALUE", "UCOUNT", "UTOTAL", "URNK", "REV_DATE")]

# Here I am simply adding the rank_code to the ops data in the case we have more account weights like "RS2900"
AggR$RANK_SET_CODE = rank_code

# Replace all the UBASE values with their corresponding WBASE values. 
AggR$UBASE=AggR$WBASE

# Here if the UVALUE and hence also the URNK are blank, or NA, or empty it will be replaced by zero.
AggR$UVALUE[is.na(AggR$UVALUE)]=0
AggR$URNK[is.na(AggR$URNK)]=0
AggR$UCOUNT[is.na(AggR$UCOUNT)]=0



#### Now putting the OPS data into the Final Format for Mirror as requested. 

# For the Mirror we need to remove "MODULE_NAME","RANK_SET_CODE","RANKSET_NAME",
AggR = AggR[,c("PROG_VER_ID", "FACTOR_NUMBER", "ACCOUNT_CODE", "UBASE", "UVALUE", "UCOUNT", "UTOTAL", "URNK")]

## Merge the Rating Sequence and Priorities to the final OPS data. 

# Keep only the relavent columns.
RS = select(Reporting_Sequence, FACTOR_NUMBER, REPORTING_SEQUENCE, ENGLISH_WORDING)

# Select only one row, drop the duplicates that appear in Reporting_Sequence

RS_unique = NULL
for (i in unique(RS$FACTOR_NUMBER)) {
  
  # Here we subset the RS data set per each factor number then 
  # add the first row to the new data set RS_unique. 
  RS_unique = rbind(RS_unique, subset(RS, FACTOR_NUMBER == i)[1,] )
  
}
# This is where we left off last time. 
# Now need to join REPORTING_SEQUENCE and ENGLISH_WORDING to the Final Ops Mirror Output. 
MERGED = left_join( AggR, RS_unique)

# Remove the 0.5 from the ACCOUNT_CODE's that are ranksets. 
MERGED$ACCOUNT_CODE[MERGED$ACCOUNT_CODE %in% unique(Parent$Parent)] = MERGED$ACCOUNT_CODE[MERGED$ACCOUNT_CODE %in% unique(Parent$Parent)] - 0.5

# They have asked to change account code 9000 to 'ALL'
MERGED$ACCOUNT_CODE[MERGED$ACCOUNT_CODE == 9000] = "ALL"
#unique(MERGED$ACCOUNT_CODE)

# Rearrage the variables to requested format and rename "ACCOUNT_CODE" TO Q_VER_CODE and call the final data frame AggR.
MERGED = MERGED[,c("PROG_VER_ID", "FACTOR_NUMBER", "ACCOUNT_CODE","ENGLISH_WORDING", "REPORTING_SEQUENCE", "UBASE", "UVALUE", "UCOUNT", "UTOTAL", "URNK")]
AggR = rename(MERGED, Q_VER_CODE =  ACCOUNT_CODE, ENGLISH_SHORT = ENGLISH_WORDING)

#write.csv(AggR, "MERGEDwithENGLISHWORDING.csv")


t13 = Sys.time() - t0


#OUTPUTNAME1 = paste("f ", CodeVersion, " ", FILENAME, ".csv", sep = "")
OUTPUTNAME2 = paste("f ", CodeVersion, " ", FILENAME, ".xlsx", sep = "")

#write.csv(AggR, OUTPUTNAME, row.names=FALSE)
#write.xlsx(AggR, file = OUTPUTNAME2, sheetName = "OPS")

# First create the cutoff data then export it together with the output data. 
# This is done below.

######################## Computing the cutoffs

### EMPTY DATA FRAME
Output_Cutoffs = data.frame()

#### COMPUTE THE CUFFOFFS FOR ACCOUNT 9000
for (area in levels(dat9000$FUNCTION_GROUP_CODE)){ # FUNCTIONAL AREA LOOP
  tryCatch({ # CHECK IF FUNCTIONAL AREA IS MISSING
    if (all(dat9000$retail_weight_value==0)){dat9000$retail_weight_value=1}
    temp_Cutoffs = OPS_cutoffs(dat9000, area) # DERIVED FUNCTION
    Output_Cutoffs = rbind(Output_Cutoffs,temp_Cutoffs) # ADD RESULTS TO FINAL DATA FRAME
  }, error = function(e) {})
}


#### COMPUTE THE CUFFOFFS FOR the remaining ACCOUNTs
for (retailer in unique(Parent$Parent)){ # Rankset LOOP
  #retailer  = 6326
  datRet = df %>% filter(ACCOUNT_CODE == retailer) %>% # FILTER DATA TO ONLY LOOK AT THIS RETAILER
    select(which(colMeans(is.na(.)) != 1)) %>% # REMOVE PRACTICES WITH NO RESPONSES
    select(-c(ACCOUNT_CODE)) %>% # REMOVE ACCOUNT_CODE COLUMN - NOT RELEVANT ANYMORE
    droplevels
  
  # Check if response missing completely - if so go to next retailer
  if (is.null(datRet$X202)) next
  
  if (all(datRet$retail_weight_value==0)){datRet$retail_weight_value=1}
  
  # BY FUNCTION
  for (area in levels(datRet$FUNCTION_GROUP_CODE)){  # FUNCTIONAL AREA LOOP
    #area = "ALL1"
    #datInput = datRet
    #accountCode = retailer
    tryCatch({ # CHECK IF FUNCTIONAL AREA IS MISSING
      temp_Cutoffs = OPS_cutoffs(datRet, area, accountCode=retailer) # DERIVED FUNCTION
      Output_Cutoffs = rbind(Output_Cutoffs,temp_Cutoffs) # ADD RESULTS TO FINAL DATA FRAME
    }, error = function(e) {})
  }
} # END by retailer loop

# Recode accountCode 9000 to "ALL" for Mirror OPS. %>% shortcut is Ctrl+Shit+M. %>% 
Output_Cutoffs$accountCode[Output_Cutoffs$accountCode == 9000] = "ALL"

practice_numbers = as.numeric(gsub("X", "", Output_Cutoffs$Practice_names))
#percent_practice = paste0(round(Output_Cutoffs$PERCENT, 2)*100, "%")

Cutoff_data = data.frame("FACTOR_NUMBER" = practice_numbers, 
                         "PERCENT" = Output_Cutoffs$PERCENT, 
                         "FUNCTION_GROUP_CODE" = Output_Cutoffs$area,
                         "ACCOUNT_CODE" = Output_Cutoffs$accountCode, 
                         "RANK_SET_CODE" = rank_code,
                         "EXCLUDED" = Output_Cutoffs$EXCLUDED)



#################### Now we create the Exclusion Data ###########

# This must be included in all the OPS Mirror Priorities not just by rankset priority.

# df data is the main data matrix we will use to create the Exclusion data

exclusion_data_matrix = df %>% select(-retail_weight_value, -qver, -FUNCTION_GROUP_CODE, -identifier )

# Run this only on the ranksets from the "rankset_codes" variable. 
rankset_codes0.5 = rankset_codes + 0.5

exclusion_data_matrix = exclusion_data_matrix %>% filter( ACCOUNT_CODE %in% rankset_codes0.5)
#unique(exclusion_data_matrix$ACCOUNT_CODE)
#dim(exclusion_data_matrix)

# Create the practice or factor names we will use later. 
practice_names = names(select(exclusion_data_matrix , contains("X")))

# # Look at this to see what data looks like for X202 NaN rows. 
# head(exclusion_data_matrix[ is.na(exclusion_data_matrix$X202), practice_names] )


# For each row, if the X202 is NaN in that row, this will convert all the practices to NaN for that row.
exclusion_data_matrix[ is.na(exclusion_data_matrix$X202), practice_names] = NaN

# # Look at this to make sure it looks good. 
# head(exclusion_data_matrix[ is.na(exclusion_data_matrix$X202), practice_names] )


# Perform check with original data set to see no new NaN rows were created in the data. 
length(is.na(df$X202)) == length(is.na(exclusion_data_matrix$X202)) # This should be TRUE

# Now do not need the X202 practice. 
exclusion_data_matrix = exclusion_data_matrix %>% select( -contains("X202"))
#unique(exclusion_data_matrix$ACCOUNT_CODE)



excl_data_var = names(select(exclusion_data_matrix , -contains("X")))

excl_data = melt(exclusion_data_matrix , id = excl_data_var, variable.name = "FACTOR_NUMBER", value.name = "rating" )

excl_data$EXCLUDED = is.na(excl_data$rating)
excl_data$ACCOUNT_CODE = floor(excl_data$ACCOUNT_CODE)
excl_data$FACTOR_NUMBER = as.numeric(gsub("X", "", excl_data$FACTOR_NUMBER))
excl_data = excl_data %>%  select(-rating)

# Now some times this excl_data gets very big. So only keep the rows that are TRUE under EXCLUDED.
excl_data_all_data = excl_data
excl_data = excl_data %>% filter(EXCLUDED == TRUE) # Much more manageable now. But still too much (5.8 million)

# Perhaps there are duplicates
#excl_data_distinct = distinct(excl_data) 

dim(excl_data)
dim(df)
dim(PROG)

#write.csv(excl_data, file = "UBASE_Exclusion_Data.csv", row.names = FALSE)


# Non-convergent accounts were stored IN accounts_that_fail_to_converge
# For non-convergence keep only distinct rows. Drop all others. 

accounts_that_fail_to_converge = distinct(accounts_that_fail_to_converge)

# Now only keep the distinct account codes.
# distinct_accounts_that_fail_to_converge = distinct(select(accounts_that_fail_to_converge, nonconvergent_accounts))


# Creating the proper name formatting for the files. 
# THE PROPER NAMING FORMAT THAT THE FILE NEEDS TO BE DELIVERED TO THE DATA PROCESSING TEAM (IS team)
# THE NAME SHOULD BE OF THE FORMAT: "CHLGRHO18 RET_P5725 - OPS Final Run - 07262018.xlsx"


todaysData = format(Sys.time(), "%m%d%Y")
#todaysData
Run
#OPS_Run = paste("OPS", Run)
#OPS_Run
#FILENAME
FINAL_NAME1 = gsub("Specs", "Run", FILENAME)
#FINAL_NAME1
FINAL_NAME2 = substr(FINAL_NAME1, 1, nchar(FINAL_NAME1)-23)
FINAL_NAME2
OUTPUTNAME3 = paste0(FINAL_NAME2, " ", Mirror_run , " - " , Run, " - ", todaysData, ".xlsx") 
OUTPUTNAME3

#### Write an if statemnet for when the excl_data has more than 800,000 rows of data.
if ( nrow(excl_data) > 800000) {
  

  list_of_datasets <- list(  "OPS" = AggR 
                           , "Cutoffs" = Cutoff_data 
                           , "Nonconvergent Accounts UBASE" = accounts_that_fail_to_converge
                           )

  excl_data_name = paste0(FINAL_NAME2," exclusion_data ", todaysData, ".csv")
  write.csv(excl_data, file = excl_data_name, row.names = FALSE)
                           
  
} else {
  
  # SAVE all the datasets  as a list of data sets to export them as an excel (.xlsx) file. 
  list_of_datasets <- list(  "OPS" = AggR 
                           , "Cutoffs" = Cutoff_data 
                           , "UBASE Exclusion Data" = excl_data
                           , "Nonconvergent Accounts UBASE" = accounts_that_fail_to_converge
                           )
}


# HERE WE CREATE RUSULTS FILE WITH THE PROPER NAMING FORMAT 

write.xlsx(list_of_datasets, file = OUTPUTNAME3)

# fOR THIS program, for now, do not produce "UBASE Exclusion Data"
# list_of_datasets <- list("OPS" = AggR, "Cutoffs" = Cutoff_data, "UBASE Exclusion Data" = excl_data,
#                          "Nonconvergent Accounts" = distinct_accounts_that_fail_to_converge ,
#                          "Nonconvergent Accounts UBASE" = accounts_that_fail_to_converge)
# 
# 
# write.xlsx(list_of_datasets, file = "thebackup.xlsx")


# Creating Analytics Data
t14 = Sys.time() - t0

# Outputting the time and dataset variables. 
t15 = Sys.time() - t0

Time = as.numeric(c(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12,t13, t14, t15))
Time_Interval = 1:15
nTime = length(Time)
time1 = c(0, Time[-length(Time)] )
Time_Difference = Time - time1
Time.data = data.frame(time_interval = 1:length(Time), Time, Time_Difference)

# Dataset dimensions/variables
Data_Analytics = data.frame("ID" = rep("", nTime),
                            "DataSet"= rep(FILENAME, nTime),
                            "Sys.time" = rep(t0, nTime),
                            Time_Interval,
                             "nFDATA" = rep(nrow(PROG), nTime),
                             "nParent" = rep(nrow(Parent), nTime), 
                             "Number of Parent Codes" = rep(length(unique(Parent$Parent)), nTime), 
                             "nPractice" = rep(nrow(Practice), nTime), 
                             "Number of Practices" = rep(length(unique(Practice$FACTOR_NUMBER)), nTime),
                             "nOPS" = rep(nrow(AggR), nTime),
                             "TotalRunTime" = rep(Time[nTime], nTime),
                             Time, 
                             Time_Difference, 
                             "Code Version" = rep(CodeVersion, nTime),
                            "ComputerUsed" = rep(Computer, nTime),
                            "OPS_request_by" = OPS_request_by, 
                            "Run" = Run, 
                            "Program_type" = Program_type,
                            "Mirror_run" = Mirror_run,
                            "Mirror_code_ver" = Mirror_by_rankset_version,
                             Program_year
                            
)


#### Save file to Analytics2018 Super file. 

Analytics = read_excel("Q:/MARKET DEVELOPMENT/OPS Runs 2018/Analytics2018/Analytics.xlsx")
Analytics1 = rbind(Analytics, Data_Analytics)

## Checks if introduce new variables in the Analytics Data.

# # Debugging
# str(Analytics)
# str(Data_Analytics)
# head(Analytics1)
# head(Analytics)
# head(Data_Analytics)
# all(names(Analytics) == names(Data_Analytics))
# #Analytics1 = Data_Analytics # This was the seed file to make rbind work.

list_of_datasets <- list("Analytics" = Analytics1)
write.xlsx(list_of_datasets, file = "Q:/MARKET DEVELOPMENT/OPS Runs 2018/Analytics2018/Analytics.xlsx")

# Save file to Analytics2018backup.
path = paste("Q:/MARKET DEVELOPMENT/OPS Runs 2018/Analytics2018/Analytics2018backup/Analytics ", FILENAME, ".xlsx", sep = "")
write.xlsx(list_of_datasets, file = path)

# Save file to the currect directory
path2 = paste("Analytics of ", FILENAME, ".xlsx", sep = "")
write.xlsx(Data_Analytics, file = path2)

time_final = Sys.time() - t0

# END OF OPS Mirror


# OPS System time Completion
a = Sys.time()
a 
t10
t12
time_final
print(paste("OPS took a total of", a - t0, "to complete"))

