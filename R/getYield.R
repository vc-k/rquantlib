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
                # 'arg' should be one of “Actual360”, “ActualFixed”, 
                # “ActualActual”, “Business252”, “OneDayCounter”, “SimpleDayCounter”, 
                # “Thirty360”, “Actual365NoLeap”, “ActualActual.ISMA”, “ActualActual.Bond”, 
                # “ActualActual.ISDA”, “ActualActual.Historical”, “ActualActual.AFB”, “ActualActual.Euro”
                
                calc=list(dayCounter=myDayConvention,
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
              
              data.frame(ISIN = as.character(bondDef$ISIN), yield = as.numeric(myYield*100), dayCounter = as.numeric(bondDef$DAYCOUNTCONVENTION))
            }
          })
  )
  tmp[,1] <- as.character(tmp[,1])
  tmp
}

