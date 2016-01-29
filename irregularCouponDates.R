getBondDefinition <- function(ISIN, jdbcDriverPath, dbHost, dbUsername, dbPassword){
  library(RJDBC)
  
  drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",
              jdbcDriverPath)
  conn <- tryCatch(dbConnect(drv, dbHost,
                             username= dbUsername,password= dbPassword),
                   error=function(e) "Database credential error")
  
  queryText <- paste( "SELECT * FROM BONDDEFINITION WHERE ISIN IN ('", 
                      paste(ISIN,collapse ="','"),"')" , sep="")
  queryResponse <- dbSendQuery(conn,queryText)
  queryResponse <- dbFetch(queryResponse)
  queryResponse[as.numeric(sapply(ISIN, function(x) which(queryResponse[,1]==x))),]
}

getYieldFromBondPriceISIN <- function(ISIN, price, evaluationDate = Sys.Date(), calcDayConvention=NULL, jdbcDriverPath, dbHost, dbUsername, dbPassword){
  
    ISIN <- "TRDTFVK51610"
    evaluationDate <- as.Date("2016-01-26")
    price <- 96.292
    jdbcDriverPath <- "/opt/sqljdbc_4.2/enu/sqljdbc4.jar"
    dbHost <- "jdbc:sqlserver://212.15.8.153;databaseName=OSMANLIBOND"
    dbUsername <- "osmanlibond_usr"
    dbPassword <-"osmanlibond1*"
    x<-1
  
  queryResponse <- getBondDefinition(ISIN, jdbcDriverPath, dbHost, dbUsername, dbPassword)
  
  tmp <- do.call(rbind,
                 lapply(1:nrow(queryResponse),function(x){
                   bondDef <- queryResponse[x,]
                   myIssueDate<-as.Date(bondDef$ISSUEDATE)
                   myMaturityDate<-as.Date(bondDef$MATURITY)
                   myCouponRate<-(bondDef$COUPONRATE)
                   myToday<- evaluationDate
                   setEvaluationDate(myToday)
                   myDayCounter<-'Actual365NoLeap'
                   myCouponPeriod<-bondDef$COUPONPERIOD
                   myCleanPrice<- price[x]
                   
                   if(any(is.na(c(myIssueDate,myMaturityDate,myCouponRate,myToday,myDayCounter,myCouponPeriod,myCleanPrice)))){
                     data.frame(ISIN = as.character(ISIN[x]), yield = NA, dayCounter = NA)
                   }else{
                     if(is.null(calcDayConvention)){
                       if(bondDef$DAYCOUNTCONVENTION==3) myDayConvention <- 8
                       else if(bondDef$DAYCOUNTCONVENTION==4) myDayConvention <- 1
                     }else myDayConvention <- calcDayConvention
                     
                     if(bondDef$COUPONPERIOD==0){
                       myFaceAmount<- 100 * (1 + bondDef$COUPONRATE)
                       myYield <- ZeroYield(myCleanPrice, myFaceAmount, myToday, myMaturityDate,
                                            dayCounter=1, frequency=1,compound=1,businessDayConvention=5)
                     }else
                     {
                       myFaceAmount <- 100
                       bond <- list(settlementDays=0,
                                    issueDate=myIssueDate,
                                    faceAmount=myFaceAmount,
                                    dayCounter='ActualActual.ISMA',
                                    paymentConvention='Unadjusted')
                       
                       myEffectiveDate<-ifelse(as.Date(bondDef$NEXTCOUPONDATE)==as.Date(bondDef$MATURITY),
                                               bondDef$ISSUEDATE,bondDef$NEXTCOUPONDATE)
                       
                       schedule <- list(effectiveDate=myIssueDate,#as.Date(myEffectiveDate),
                                        maturityDate=myMaturityDate,
                                        period=myCouponPeriod,
                                        calendar='Turkey',
                                        businessDayConvention='Unadjusted',
                                        terminationDateConvention='Unadjusted',
                                        dateGeneration='Forward',endOfMonth=0)
                       
                       cDates <- c(as.Date("2015-11-27"),as.Date("2016-02-24"),as.Date("2016-05-24"))
                       
                       schedule <- list(dates = cDates,
                                        isRegular = rep(F,length(cDates)-1),
                                        calendar='Turkey',
                                        businessDayConvention='Unadjusted',
                                        terminationDateConvention='Unadjusted',
                                        dateGeneration='Forward',
                                        endOfMonth=0)
                       
                       
                       
                       # 'arg' should be one of “Actual360”, “ActualFixed”, 
                       # “ActualActual”, “Business252”, “OneDayCounter”, “SimpleDayCounter”, 
                       # “Thirty360”, “Actual365NoLeap”, “ActualActual.ISMA”, “ActualActual.Bond”, 
                       # “ActualActual.ISDA”, “ActualActual.Historical”, “ActualActual.AFB”, “ActualActual.Euro”
                       
                       calc=list(dayCounter=8,
                                 compounding='Compounded',
                                 freq='Annual',
                                 durationType='Modified')
                       
                       myCouponRate <- c(0.0273,0.0276)
                       coupon.rate <- myCouponRate * round(365/bondDef$COUPONPERIOD,0)
                       
                       # coupon.rate <- 0.1121
                       
                       setEvaluationDate(myToday)
                       
                       
                       fit<-FixedRateBond(bond,
                                          coupon.rate,
                                          schedule,
                                          calc,
                                          price=myCleanPrice
                                          #yield=yield
                       )
                       (fit)
                       
                       coupon.rate <- coupon.rate * 100 *myCouponRate / head(fit$cashFlow$Amount,-1 )
                       #
                       
                       fit<-FixedRateBond(bond,
                                          coupon.rate,
                                          schedule,
                                          calc,
                                          price=myCleanPrice
                                          #yield=yield
                       )
                       (fit)
                       
                       tail(fit$cashFlow$Amount,2)[1]
                       myYield <- fit$yield
                     }
                     
                     data.frame(ISIN = as.character(bondDef$ISIN), yield = as.numeric(myYield*100), dayCounter = as.numeric(bondDef$DAYCOUNTCONVENTION))
                   }
                 })
  )
  tmp[,1] <- as.character(tmp[,1])
  tmp
}

