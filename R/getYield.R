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
  dbFetch(queryResponse)
}

getYieldFromBondPriceISIN <- function(ISIN, price, evaluationDate = Sys.Date(), jdbcDriverPath, dbHost, dbUsername, dbPassword){
  
  queryResponse <- getBondDefinition(ISIN, jdbcDriverPath, dbHost, dbUsername, dbPassword)
    
  
  apply(queryResponse,1,function(bondDef){
    myIssueDate<-as.Date(bondDef$ISSUEDATE)
    myMaturityDate<-as.Date(bondDef$MATURITY)
    myCouponRate<-(bondDef$COUPONRATE)
    myToday<- evaluationDate
    setEvaluationDate(myToday)
    myDayCounter<-'Actual365NoLeap'
    myCouponPeriod<-bondDef$COUPONPERIOD
    myCleanPrice<- price
    
    if(bondDef$COUPONPERIOD==0){
      myFaceAmount<- 100 + bondDef$COUPONRATE
      myYield <- ZeroYield(myCleanPrice, myFaceAmount, myToday, myMaturityDate,
                           dayCounter=1, frequency=1,compound=1,businessDayConvention=5)
    }else
    {
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
      # 'arg' should be one of “Actual360”, “ActualFixed”, 
      # “ActualActual”, “Business252”, “OneDayCounter”, “SimpleDayCounter”, 
      # “Thirty360”, “Actual365NoLeap”, “ActualActual.ISMA”, “ActualActual.Bond”, 
      # “ActualActual.ISDA”, “ActualActual.Historical”, “ActualActual.AFB”, “ActualActual.Euro”
      
      calc=list(dayCounter=1,
                compounding='Compounded',
                freq='Annual',
                durationType='Modified')
      coupon.rate <- myCouponRate * round(365/bondDef$COUPONPERIOD,0)
      
      setEvaluationDate(myToday)
      
      
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
    
    c(bondDef$ISIN, myYield*100)
  })
  
}

tmp <- getYieldFromBondPriceISIN(c("TRDTFVK51610","TRFGDKM41617"),c(92.69567056,98.028),as.Date("2016-01-18"),
                          "/opt/sqljdbc_4.0/enu/sqljdbc4.jar","jdbc:sqlserver://212.15.8.153;databaseName=OSMANLIBOND",
                          "osmanlibond_usr", "osmanlibond1*")
