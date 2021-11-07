require("IBrokers");require("quantmod");require("pbapply")
source("sendAllSyntheticOrders_IB.R")
# *************************************************************************************
# read in Options Data to extract Strikes/expirations available
FILES = list.files("/Volumes/3TB/CBOE/ALL",full.names = TRUE)
FILES = tail(FILES,1) # recent
ops = readRDS(FILES)
# *************************************************************************************
#                   create table with STRIKES/EXPIRATIONs available
# *************************************************************************************
STK = "BBIG"
ops = subset(ops,ops$Symbol == STK & ops$flag == "C")
#ops = subset(ops, ops$expiry > Sys.Date())
ops = subset(ops, ops$expiry == "2021-11-19")
op_df = ops[,c("Symbol","strike","expiry")]
View(op_df)
# *************************************************************************************
#                             get Contract Ids from IB
# *************************************************************************************
op_df$stkID  = NA
op_df$callID = NA
op_df$putID  = NA

for(ii in 1:nrow(op_df)){
  stk    = op_df[ii,1] %>% as.character
  strike = op_df[ii,2] %>% as.numeric
  exp    = op_df[ii,3] %>% as.character
  IDS = try(getContratIds(STK=stk,STRIKE = strike,EXP = exp))
  if(inherits(IDS, 'try-error')){
    op_df$stkID[ii]  = NA
    op_df$callID[ii] = NA
    op_df$putID[ii]  = NA
    
  }else{
    op_df$stkID[ii]  = IDS$stkID
    op_df$callID[ii] = IDS$callID
    op_df$putID[ii]  = IDS$putID
  }
}

# exclude contracts that we didn't get contract IDs for
op_df = na.omit(op_df)
#saveRDS(op_df,"BBIG_exIDs.rds")
#op_df = readRDS("BBIG_exIDs.rds")
# *************************************************************************************
#                             sendALL orders
# *************************************************************************************
# Manually sending orders fails when they are continuous:
# buySynthetic_shortSTK(STK="BBIG",STRIKE=5.5,EXP="2021-11-19",minProfit = 0.15) 
# buySynthetic_shortSTK(STK="BBIG",STRIKE=6,EXP="2021-11-19",minProfit = 0.15) 
# buySynthetic_shortSTK(STK="BBIG",STRIKE=6.5,EXP="2021-11-19",minProfit = 0.15) 
# *************************************************************************************
#                             sendALL orders
# *************************************************************************************
# op_df    : data frame containing stock symbol, strike, expiration, & IDs
# minProfit: min profit to be accepted
sendAllSyntheticOrders(op_df=op_df,minProfit = 0.1) 








