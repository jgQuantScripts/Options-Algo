# *************************************************************************************
# STK      : Underlying stock symbol
# STRIKE   : Strike(s) for call/put
# EXP      : option expiration in the following format: 
# minProfit: min profit to be accepted
# *************************************************************************************
buySynthetic_shortSTK = function(STK,STRIKE,EXP,minProfit,orderID){
  require("IBrokers")
  tws = twsConnect(port=7497)
  ac <- reqAccountUpdates(tws)
  # format EXP date
  EXP = format(as.Date(EXP,format="%Y-%m-%d"), "%Y%m%d")
  
  # gather stock information
  stk_det = reqContractDetails(tws,twsSTK(STK,exch="SMART",primary="NASDAQ",currency="USD"))
  
  # create contract for option: CALL
  contract_call <- twsContract()
  contract_call$symbol <- STK
  contract_call$currency <- "USD"
  contract_call$sectype <- "OPT"
  contract_call$expiry <- paste(EXP) # "20211119" for "2021-11-19"
  contract_call$right  <- "C"
  contract_call$strike <- paste(STRIKE)
  contract_call$primary<- "NASDAQ"
  CALL <- reqContractDetails(tws,contract_call)
  
  # create contract for option: PUT
  contract_put <- twsContract()
  contract_put$symbol <- STK
  contract_put$currency <- "USD"
  contract_put$sectype <- "OPT"
  contract_put$expiry <- paste(EXP) # "20211119" for "2021-11-19"
  contract_put$right  <- "P"
  contract_put$strike <- paste(STRIKE)
  contract_put$primary<- "NASDAQ"
  PUT <- reqContractDetails(tws,contract_put)
  
  # create legs for spread order
  leg1 = twsComboLeg(conId = stk_det[[1]]$conId,ratio = "100",action = "SELL",exchange = "SMART")   # sell- stock
  leg2 = twsComboLeg(conId = CALL[[1]]$contract$conId,ratio = "1",action = "BUY",exchange = "SMART")# buy - call
  leg3 = twsComboLeg(conId = PUT[[1]]$contract$conId,ratio = "1",action = "SELL",exchange = "SMART")# sell- put
  
  # we now have to wrap it in a twsBAG()
  cont = twsBAG()
  cont$conId = stk_det[[1]]$conId
  cont$symbol = STK
  cont$sectype = "BAG"
  cont$exch = "SMART"
  cont$primary = "NASDAQ"
  cont$comboleg = list(leg1,leg2,leg3)
  
  # calculate limit price for spread
  limitPRC = STRIKE + minProfit
  # # getOrderID
  # orderID = as.numeric(reqIds(tws))
  cat("\n",orderID)
  # while(orderID < 0  | !IBrokers::isConnected(tws))
  # {
  #   tws = twsConnect(port=7497)
  #   ac <- reqAccountUpdates(tws)
  #   Sys.sleep(2)
  #   orderID = as.numeric(reqIds(tws))
  # }
  # create TWSorder 
  myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                          outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
  myorder
  # place combo order
  placeOrder(tws,cont, myorder)
  # disconnect
  twsDisconnect(tws)
}
# *************************************************************************************
# STK      : Underlying stock symbol
# STRIKE   : Strike(s) for call/put
# EXP      : option expiration in the following format: 
# *************************************************************************************
getContratIds = function(STK,STRIKE,EXP){
  Sys.sleep(3)
  require("IBrokers")
  tws = twsConnect(port=7497)
  ac <- reqAccountUpdates(tws)
  # format EXP date
  EXP = format(as.Date(EXP,format="%Y-%m-%d"), "%Y%m%d")
  
  # gather stock information
  stk_det = reqContractDetails(tws,twsSTK(STK,exch="SMART",primary="NASDAQ",currency="USD"))
  
  # create contract for option: CALL
  contract_call <- twsContract()
  contract_call$symbol <- STK
  contract_call$currency <- "USD"
  contract_call$sectype <- "OPT"
  contract_call$expiry <- paste(EXP) # "20211119" for "2021-11-19"
  contract_call$right  <- "C"
  contract_call$strike <- paste(STRIKE)
  contract_call$primary<- "NASDAQ"
  CALL <- reqContractDetails(tws,contract_call)
  
  # create contract for option: PUT
  contract_put <- twsContract()
  contract_put$symbol <- STK
  contract_put$currency <- "USD"
  contract_put$sectype <- "OPT"
  contract_put$expiry <- paste(EXP) # "20211119" for "2021-11-19"
  contract_put$right  <- "P"
  contract_put$strike <- paste(STRIKE)
  contract_put$primary<- "NASDAQ"
  PUT <- reqContractDetails(tws,contract_put)
  
  IDS  = as.data.frame(cbind(stk_det[[1]]$conId,CALL[[1]]$contract$conId,PUT[[1]]$contract$conId))
  colnames(IDS) = c("stkID","callID","putID")
  IDS
}
# *************************************************************************************
# *************************************************************************************
# send all orders
sendAllSyntheticOrders = function(op_df, minProfit){
  require("IBrokers")
  tws = twsConnect(port=7497)
  ac <- reqAccountUpdates(tws)
  
  if(nrow(op_df) == 1){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii]) + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 2){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii]) + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii]) + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 3){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=TRUE)
    # place combo order
    placeOrder(tws,cont, myorder)

    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    
    ii = ii + 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii]) + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 4){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    
    ii = ii + 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii]) + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii + 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii]) + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 5){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 6){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 7){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 8){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 9){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 10){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 11){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 12){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 13){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 14){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 15){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 16){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
   
  }
  if(nrow(op_df) == 17){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 18){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 19){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 20){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 21){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 22){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 23){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 24){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 25){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 26){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 27){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 28){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 29){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  if(nrow(op_df) == 30){
    ii = 1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii +1;Sys.sleep(1)
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
    ii = ii+1
    # create legs for spread order
    leg1 = twsComboLeg(conId = op_df$stkID[ii],ratio = "100",action = "SELL",exchange = "SMART")# sell- stock
    leg2 = twsComboLeg(conId = op_df$callID[ii],ratio = "1",action = "BUY",exchange = "SMART")  # buy - call
    leg3 = twsComboLeg(conId = op_df$putID[ii],ratio = "1",action = "SELL",exchange = "SMART")  # sell- put
    
    # we now have to wrap it in a twsBAG()
    cont = twsBAG()
    cont$conId = op_df$stkID[ii]
    cont$symbol = op_df$Symbol[1]
    cont$sectype = "BAG"
    cont$exch = "SMART"
    cont$primary = "NASDAQ"
    cont$comboleg = list(leg1,leg2,leg3)
    
    # calculate limit price for spread
    limitPRC = as.numeric(op_df$strike[ii])  + minProfit
    # getOrderID
    orderID = as.numeric(reqIds(tws))
    
    # create TWSorder 
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(limitPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    # place combo order
    placeOrder(tws,cont, myorder)
  }
  # disconnect
  twsDisconnect(tws)
}

