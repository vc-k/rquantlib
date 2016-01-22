\name{getYieldFromBondPriceISIN}
\alias{getYieldFromBondPriceISIN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate yield from price with ISIN code from DB}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
asdasd
asdasdas
asdasd
asdasdasd
}
\usage{
getYieldFromBondPriceISIN(ISIN, price, evaluationDate = Sys.Date(), jdbcDriverPath, dbHost, dbUsername, dbPassword)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ISIN}{
%%     ~~Describe \code{ISIN} here~~
asdasdasd
}
  \item{price}{
%%     ~~Describe \code{price} here~~
asdasdasd
}
  \item{evaluationDate}{
%%     ~~Describe \code{evaluationDate} here~~
asdasdasd
}
  \item{jdbcDriverPath}{
%%     ~~Describe \code{jdbcDriverPath} here~~
asdasdasd
}
  \item{dbHost}{
%%     ~~Describe \code{dbHost} here~~
asdasdasd
}
  \item{dbUsername}{
%%     ~~Describe \code{dbUsername} here~~
asdasdasd
}
  \item{dbPassword}{
%%     ~~Describe \code{dbPassword} here~~
asdasdasd
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{kamil}{asdasdasd}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (ISIN, price, evaluationDate = Sys.Date(), jdbcDriverPath, 
    dbHost, dbUsername, dbPassword) 
{
    queryResponse <- getBondDefinition(ISIN, jdbcDriverPath, 
        dbHost, dbUsername, dbPassword)
    apply(queryResponse, 1, function(bondDef) {
        myIssueDate <- as.Date(bondDef$ISSUEDATE)
        myMaturityDate <- as.Date(bondDef$MATURITY)
        myCouponRate <- (bondDef$COUPONRATE)
        myToday <- evaluationDate
        setEvaluationDate(myToday)
        myDayCounter <- "Actual365NoLeap"
        myCouponPeriod <- bondDef$COUPONPERIOD
        myCleanPrice <- price
        if (bondDef$COUPONPERIOD == 0) {
            myFaceAmount <- 100 + bondDef$COUPONRATE
            myYield <- ZeroYield(myCleanPrice, myFaceAmount, 
                myToday, myMaturityDate, dayCounter = 1, frequency = 1, 
                compound = 1, businessDayConvention = 5)
        }
        else {
            bond <- list(settlementDays = 0, issueDate = myIssueDate, 
                faceAmount = myFaceAmount, dayCounter = "ActualActual.ISMA", 
                paymentConvention = "Unadjusted")
            myEffectiveDate <- ifelse(as.Date(bondDef$NEXTCOUPONDATE) == 
                as.Date(bondDef$MATURITY), bondDef$ISSUEDATE, 
                bondDef$NEXTCOUPONDATE)
            schedule <- list(effectiveDate = myIssueDate, maturityDate = myMaturityDate, 
                period = myCouponPeriod, calendar = "Turkey", 
                businessDayConvention = "Unadjusted", terminationDateConvention = "Unadjusted", 
                dateGeneration = "Forward", endOfMonth = 0)
            calc = list(dayCounter = 1, compounding = "Compounded", 
                freq = "Annual", durationType = "Modified")
            coupon.rate <- myCouponRate * round(365/bondDef$COUPONPERIOD, 
                0)
            setEvaluationDate(myToday)
            fit <- FixedRateBond(bond, coupon.rate, schedule, 
                calc, price = myCleanPrice)
            (fit)
            tail(fit$cashFlow$Amount, 2)[1]
            myYield <- fit$yield
        }
        c(bondDef$ISIN, myYield * 100)
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
