// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- 
//
// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright (C) 2002 - 2009 Dirk Eddelbuettel <edd@debian.org>
// Copyright (C) 2005 - 2006  Dominick Samperi
//
// $Id$
//
// This file is part of the RQuantLib library for GNU R.
// It is made available under the terms of the GNU General Public
// License, version 2, or at your option, any later version,
// incorporated herein by reference.
//
// This program is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public
// License along with this program; if not, write to the Free
// Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA

#include "rquantlib.hpp"

// cf QuantLib-0.9.0/test-suite/europeanoption.cpp
boost::shared_ptr<VanillaOption>
makeOption(const boost::shared_ptr<StrikedTypePayoff>& payoff,
           const boost::shared_ptr<Exercise>& exercise,
           const boost::shared_ptr<Quote>& u,
           const boost::shared_ptr<YieldTermStructure>& q,
           const boost::shared_ptr<YieldTermStructure>& r,
           const boost::shared_ptr<BlackVolTermStructure>& vol,
           EngineType engineType,
           Size binomialSteps,
           Size samples) {
  
    boost::shared_ptr<GeneralizedBlackScholesProcess> stochProcess = makeProcess(u,q,r,vol);
    boost::shared_ptr<PricingEngine> engine;

    switch (engineType) {
    case Analytic:
        engine = boost::shared_ptr<PricingEngine>(new AnalyticEuropeanEngine(stochProcess));
        break;
    case JR:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<JarrowRudd>(stochProcess, binomialSteps));
        break;
    case CRR:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<CoxRossRubinstein>(stochProcess, binomialSteps));
    case EQP:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<AdditiveEQPBinomialTree>(stochProcess, binomialSteps));
        break;
    case TGEO:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<Trigeorgis>(stochProcess, binomialSteps));
        break;
    case TIAN:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<Tian>(stochProcess, binomialSteps));
        break;
    case LR:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<LeisenReimer>(stochProcess, binomialSteps));
        break;
    case JOSHI:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<Joshi4>(stochProcess, binomialSteps));
        break;
    case FiniteDifferences:
        engine = boost::shared_ptr<PricingEngine>(new FDEuropeanEngine<CrankNicolson>(stochProcess, binomialSteps, samples));
        break;
    case Integral:
        engine = boost::shared_ptr<PricingEngine>(new IntegralEngine(stochProcess));
        break;
    case PseudoMonteCarlo:
        engine = MakeMCEuropeanEngine<PseudoRandom>(stochProcess)
            .withStepsPerYear(1)
            .withSamples(samples)
            .withSeed(42);
        break;
    case QuasiMonteCarlo:
        engine = MakeMCEuropeanEngine<LowDiscrepancy>(stochProcess)
            .withStepsPerYear(1)
            .withSamples(samples);
        break;
    default:
        QL_FAIL("Unknown engine type");
    }
    boost::shared_ptr<VanillaOption> option(new EuropeanOption(payoff, exercise));
    option->setPricingEngine(engine);
    return option;
}

// QuantLib option setup utils, copied from the test-suite sources

boost::shared_ptr<YieldTermStructure> buildTermStructure(SEXP params,
                                                         SEXP tsQuotes,
                                                         SEXP times){
    char* exceptionMesg = NULL;
    boost::shared_ptr<YieldTermStructure> curve;
    try {
        
        // Parameter wrapper classes.
        RcppParams rparam(params);
        RcppNumList tslist(tsQuotes);
        
        int i;
        
        Date todaysDate( dateFromR(rparam.getDateValue("tradeDate") )); 
        Date settlementDate( dateFromR(rparam.getDateValue("settleDate") )); 
        // cout << "TradeDate: " << todaysDate << endl << "Settle: " << settlementDate << endl;
        
        RQLContext::instance().settleDate = settlementDate;
        Settings::instance().evaluationDate() = todaysDate;
        std::string firstQuoteName = tslist.getName(0);
        
        //double dt = rparam.getDoubleValue("dt");
        
        std::string interpWhat, interpHow;
        bool flatQuotes = true;
        if(firstQuoteName.compare("flat") != 0) {
            
            // Get interpolation method (not needed for "flat" case)
            interpWhat = rparam.getStringValue("interpWhat");
            interpHow  = rparam.getStringValue("interpHow");
            flatQuotes = false;
        }
        
        Calendar calendar = TARGET();
        RQLContext::instance().calendar = calendar;
        Integer fixingDays = 2;
        RQLContext::instance().fixingDays = fixingDays;
        
        // Any DayCounter would be fine.
        // ActualActual::ISDA ensures that 30 years is 30.0
        DayCounter termStructureDayCounter =
            ActualActual(ActualActual::ISDA);
        double tolerance = 1.0e-15;
        

        if(firstQuoteName.compare("flat") == 0) {
            // Create a flat term structure.
            double rateQuote = tslist.getValue(0);
            boost::shared_ptr<Quote> flatRate(new SimpleQuote(rateQuote));
            boost::shared_ptr<FlatForward> ts(new FlatForward(settlementDate,
                                                              Handle<Quote>(flatRate),
                                                              Actual365Fixed()));
            curve =  ts;
        }
        else {
            // Build curve based on a set of observed rates and/or prices.
            std::vector<boost::shared_ptr<RateHelper> > curveInput;
            for(i = 0; i < tslist.size(); i++) {
                std::string name = tslist.getName(i);
                double val = tslist.getValue(i);
                boost::shared_ptr<RateHelper> rh = 
                    ObservableDB::instance().getRateHelper(name, val);
                // edd 2009-11-01 FIXME NULL_RateHelper no longer builds under 0.9.9
                // if (rh == NULL_RateHelper)
                if (rh.get() == NULL)
                    throw std::range_error("Unknown rate in getRateHelper");
                curveInput.push_back(rh);
            }
            boost::shared_ptr<YieldTermStructure> ts =
                getTermStructure(interpWhat, interpHow, 
                                 settlementDate, curveInput,
                                 termStructureDayCounter, tolerance);
            
            curve = ts;
        }
        
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }
    return curve;    
}

Schedule getSchedule(SEXP sch){
   
    RcppParams rparam(sch);
    RcppDate iDate = rparam.getDateValue("effectiveDate");
    QuantLib::Date effectiveDate(dateFromR(iDate));
    RcppDate mDate = rparam.getDateValue("maturityDate");
    QuantLib::Date maturityDate(dateFromR(mDate));      
    double frequency = rparam.getDoubleValue("period");
    std::string cal = rparam.getStringValue("calendar");
    double businessDayConvention = rparam.getDoubleValue("businessDayConvention");
    double terminationDateConvention = rparam.getDoubleValue("terminationDateConvention");
    Calendar calendar = UnitedStates(UnitedStates::GovernmentBond);
    if (cal == "us"){
        calendar = UnitedStates(UnitedStates::GovernmentBond);
    }
    else if (cal == "uk"){
        calendar = UnitedKingdom(UnitedKingdom::Exchange);
    }
    BusinessDayConvention bdc = getBusinessDayConvention(businessDayConvention);   
    BusinessDayConvention t_bdc = getBusinessDayConvention(terminationDateConvention);
    Schedule schedule(effectiveDate,
                      maturityDate,
                      Period(getFrequency(frequency)),
                      calendar, bdc, t_bdc, 
                      DateGeneration::Backward, false);
    return schedule;
    
}

boost::shared_ptr<YieldTermStructure> rebuildCurveFromZeroRates(
                                                                SEXP dateSexp,
                                                                SEXP zeroSexp){
    RcppDateVector rcppdates  = RcppDateVector(dateSexp);
    int n = rcppdates.size();
    std::vector<QuantLib::Date> dates(rcppdates.size());
    for (int i = 0;i<n;i++){
        QuantLib::Date day(dateFromR(rcppdates(i)) );
        dates[i] = day;
        
    }
    //extract coupon rates vector
    RcppVector<double> RcppVec(zeroSexp); 
    std::vector<double> zeros(RcppVec.stlVector());
    
    boost::shared_ptr<YieldTermStructure>  
        rebuilt_curve(new 
                      InterpolatedZeroCurve<LogLinear>(                        
                                                       dates,
                                                       zeros,
                                                       ActualActual()));
    return rebuilt_curve;
}

boost::shared_ptr<YieldTermStructure> getFlatCurve(SEXP flatcurve){
    RcppParams curve(flatcurve);
    Rate riskFreeRate = curve.getDoubleValue("riskFreeRate");
    RcppDate today_Date = curve.getDateValue("todayDate");       
    QuantLib::Date today(dateFromR(today_Date));
    
    boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(riskFreeRate));
    Settings::instance().evaluationDate() = today;
    return flatRate(today,rRate,Actual360());
}

boost::shared_ptr<IborIndex> getIborIndex(SEXP index, const Date today){
    RcppParams rparam(index);
    std::string type = rparam.getStringValue("type");

    if (type == "USDLibor"){
        double riskFreeRate = rparam.getDoubleValue("riskFreeRate");
        double period = rparam.getDoubleValue("period");
        boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(riskFreeRate));
        Settings::instance().evaluationDate() = today;
        Handle<YieldTermStructure> curve(flatRate(today,rRate,Actual360()));
        boost::shared_ptr<IborIndex> iindex(new USDLibor(period * Months, curve));
        return iindex;
    }
    else return boost::shared_ptr<IborIndex>();
}

std::vector<double> getDoubleVector(SEXP vector){
    try {
        RcppVector<double> RcppVec(vector);
        if (RcppVec.size() > 0){
            return  std::vector<double>(RcppVec.stlVector());
        }
        else return std::vector<double>();
    }
    catch (std::exception& e){       
        return std::vector<double>();
    }
}

boost::shared_ptr<YieldTermStructure>
makeFlatCurve(const Date& today,
	      const boost::shared_ptr<Quote>& forward,
	      const DayCounter& dc) {
    return boost::shared_ptr<YieldTermStructure>(
	   new FlatForward(today, Handle<Quote>(forward), dc));
}

boost::shared_ptr<YieldTermStructure>
flatRate(const Date& today,
	 const boost::shared_ptr<Quote>& forward,
	 const DayCounter& dc) {
  return boost::shared_ptr<YieldTermStructure>(
	       new FlatForward(today, Handle<Quote>(forward), dc));
}
  
boost::shared_ptr<BlackVolTermStructure> 
makeFlatVolatility(const Date& today,
                   const boost::shared_ptr<Quote>& vol,
                   const DayCounter dc) {
    return boost::shared_ptr<BlackVolTermStructure>(
           new BlackConstantVol(today, NullCalendar(), Handle<Quote>(vol), dc));
}

boost::shared_ptr<BlackVolTermStructure>
flatVol(const Date& today,
	const boost::shared_ptr<Quote>& vol,
	const DayCounter& dc) {
  return boost::shared_ptr<BlackVolTermStructure>(new
            BlackConstantVol(today, NullCalendar(), Handle<Quote>(vol), dc));
}

boost::shared_ptr<GeneralizedBlackScholesProcess>
makeProcess(const boost::shared_ptr<Quote>& u,
            const boost::shared_ptr<YieldTermStructure>& q,
            const boost::shared_ptr<YieldTermStructure>& r,
            const boost::shared_ptr<BlackVolTermStructure>& vol) {
    return boost::shared_ptr<BlackScholesMertonProcess>(
           new BlackScholesMertonProcess(Handle<Quote>(u),
                                         Handle<YieldTermStructure>(q),
                                         Handle<YieldTermStructure>(r),
                                         Handle<BlackVolTermStructure>(vol)));
}

// R uses dates indexed to Jan 1, 1970. Rcpp uses an internal Julian Date representation,
// but Quantlib uses the 'spreadsheet' format indexed to 1905 so we need to adjust
int dateFromR(const RcppDate &d) {
    return(d.getJDN() - RcppDate::Jan1970Offset + RcppDate::QLtoJan1970Offset);
}
DayCounter getDayCounter(const double n){
    if (n==0) return Actual360();
    else if (n==1) return Actual365Fixed();
    else if (n==2) return ActualActual();
    else if (n==3) return Business252();
    else if (n==4) return OneDayCounter();
    else if (n==5) return SimpleDayCounter();
    else  return Thirty360();
}
BusinessDayConvention getBusinessDayConvention(const double n){
    if (n==0) return Following;
    else if (n==1) return ModifiedFollowing;
    else if (n==2) return Preceding;
    else if (n==3) return ModifiedPreceding;
    else  return Unadjusted;
}
Compounding getCompounding(const double n){
    if (n==0) return Simple;
    else if (n==1) return Compounded;
    else if (n==2) return Continuous;
    else return SimpleThenCompounded;
}
Frequency getFrequency(const double n){

    Frequency f;

    if (n==-1) f = NoFrequency;
    else if (n==0) f = Once;
    else if (n==1) f = Annual;
    else if (n==2) f = Semiannual;
    else if (n==3) f = EveryFourthMonth;
    else if (n==4) f = Quarterly;
    else if (n==6) f = Bimonthly;
    else if (n==12) f = Monthly;
    else if (n==13) f = EveryFourthWeek;
    else if (n==26) f = Biweekly;
    else if (n==52) f = Weekly;
    else if (n==365) f = Daily;
    else f = OtherFrequency;   

    return f;


}
TimeUnit getTimeUnit(const double n){
    if (n==0) return Days;
    else if (n==1) return Weeks;
    else if (n==2) return Months;
    else return Years;
}
DateGeneration::Rule getDateGenerationRule(const double n){
    if (n==0) return DateGeneration::Backward;
    else if (n==1) return DateGeneration::Forward;
    else if (n==2) return DateGeneration::Zero;
    else if (n==3) return DateGeneration::ThirdWednesday;
    else if (n==4) return DateGeneration::Twentieth;
    else return DateGeneration::TwentiethIMM;

}
Calendar* getCalendar(SEXP calParameters){
    RcppParams rparam(calParameters);
    std::string    calstr = rparam.getStringValue("calendar");
    Calendar* pcal = NULL;
    if (calstr == "TARGET") { 		// generic calendar 
        pcal = new TARGET();
        
    } else if (calstr == "Canada" || calstr == "Canada/Settlement") {
        pcal = new Canada(Canada::Settlement);
    } else if (calstr == "Canada/TSX") {
        pcal = new Canada(Canada::TSX);
        
    } else if (calstr == "Germany" || calstr == "Germany/FrankfurtStockExchange") {
        pcal = new Germany(Germany::FrankfurtStockExchange);
    } else if (calstr == "Germany/Settlement") {
        pcal = new Germany(Germany::Settlement);
    } else if (calstr == "Germany/Xetra") {
        pcal = new Germany(Germany::Xetra);
        } else if (calstr == "Germany/Eurex") {
        pcal = new Germany(Germany::Eurex);
        
    } else if (calstr == "Italy" || calstr == "Italy/Settlement") {
        pcal = new Italy(Italy::Settlement);
        } else if (calstr == "Italy/Exchange") {
        pcal = new Italy(Italy::Exchange);
        
    } else if (calstr == "Japan") {
        pcal = new Japan();
        
    } else if (calstr == "UnitedKingdom" || calstr == "UnitedKingdom/Settlement") {
        pcal = new UnitedKingdom(UnitedKingdom::Settlement);
        } else if (calstr == "UnitedKingdom/Exchange") {
        pcal = new UnitedKingdom(UnitedKingdom::Exchange);
    } else if (calstr == "UnitedKingdom/Metals") {
        pcal = new UnitedKingdom(UnitedKingdom::Metals);
        
    } else if (calstr == "UnitedStates" || calstr == "UnitedStates/Settlement") {
        pcal = new UnitedStates(UnitedStates::Settlement);
    } else if (calstr == "UnitedStates/NYSE") {
        pcal = new UnitedStates(UnitedStates::NYSE);
        } else if (calstr == "UnitedStates/GovernmentBond") {
        pcal = new UnitedStates(UnitedStates::GovernmentBond);
    } else if (calstr == "UnitedStates/NERC") {
        pcal = new UnitedStates(UnitedStates::NERC);
    }

    return pcal;
}