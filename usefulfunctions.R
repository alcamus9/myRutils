# ----------------------------------------------------------
# All useful funtions defined here
#-----------------------------------------------------------
# when adding together return don't use sum, use this retsum
# takes care of compounding
retsum<-function(vecofpercrets) {
(prod(1+(vecofpercrets/100))-1)*100
}
#-----------------------------------------------------------
add_two_rets<-function(percret1, percret2){
((1+percret1/100)*(1+percret2/100)-1)*100
}
#-----------------------------------------------------------

# to get cumulative return series
cumretvec<-function(retvec) {
cumulative_vec=list()
previous_retsum<-0
for (i in 1:length(retvec)) {
cumulative_vec[[i]]=add_two_rets(previous_retsum, retvec[i])
previous_retsum=cumulative_vec[[i]]
}
unlist(cumulative_vec)
}

#-----------------------------------------------------------
# to compute drawdown from a cumulative return series

compute_drawdown<-function(cumretvec) {
  max=max(cumretvec)
  min=min(cumretvec)
  if(which(cumretvec==min)-which(cumretvec==max)>0) {
    return(max-min)
  } else { 
    newcumretvec=cumretvec[which(cumretvec==max):length(cumretvec)]
    compute_drawdown(newcumretvec)
  }
}

#-----------------------------------------------------------
prevmonth<-function(givenmonth) {
if (givenmonth==1) {prevmth=12}
else {prevmth<-givenmonth-1}
prevmth
}
#-----------------------------------------------------------
require(timeDate)
PrevBD <- function(dt) {
  if(format(dt-1,'%w') %in% c(0,6)) Recall(dt-1) 
  else if(isBizday(as.timeDate(dt-1), holidays = holidayNYSE(), wday = 1:5)) dt-1
        else Recall(dt-1)
    }
#-----------------------------------------------------------
# what was the date on the day which was "days" number of 
# business days before the date "dt"

LagBD <-function(dt,days) {
  for (i in 1:days) {
  dt2 <- PrevBD(dt)
  dt<-dt2
  }
dt2
}
#-----------------------------------------------------------
# convert vector of words to a single string that can 
# be embedded in SQL queries' "IN" clause

get_sql_string <-function(names) {  
  sql_string="('"
  i=1
  while (i < length(names) ){
    sql_string=paste0(sql_string,names[i],"','")
    i=i+1
  }
  sql_string<-paste0(sql_string,names[i],"')")
  sql_string
}
#-----------------------------------------------------------
# get a vector containing all dates between 2 given dates
getDates<-function(dateString1,dateString2){
# date string should be in format "yyyy-mm-dd"
as.Date(as.Date(dateString1):as.Date(dateString2))
}

#-----------------------------------------------------------

# get last month-end date before a given date

monthStartDate<-function(referenceDate) {
as.Date(paste0(format(referenceDate,'%Y'),"-",format(referenceDate,'%m'),"-01"))
}

library(lubridate)
prevMonthEnd<-function(referenceDate) {
monthStartDate(referenceDate)-1
}


library(purrr)
library(bizdays)
library(RQuantLib)

getWeekEndingDates <- function (endDate, nobs) {
  most_recent_friday<- endDate - wday(endDate + 1)
  datelist<-seq(most_recent_friday- ((nobs-1)*7), most_recent_friday, by=7)
  datevec<-as.Date(unlist(datelist), origin="1970-01-01")
  datevec
}

getWeekEndingBusinessDates <- function(endDate, nobs){
  rawDates<- getWeekEndingDates(endDate, nobs)
  cal=load_quantlib_calendars("UnitedStates/NYSE", from=Sys.Date()-300*7, to=Sys.Date())
  corrected_dates<-adjust.previous(rawDates, "QuantLib/UnitedStates/NYSE")
  corrected_dates
}

getMonthEndingDates <- function (endDate, nobs) {
  datevec<-seq(monthStartDate(endDate),length=nobs,by="-1 months")-1
  datevec
}


getMonthEndingBusinessDates <- function (endDate, nobs) {
  rawDates<-getMonthEndingDates(endDate, nobs)
  cal=load_quantlib_calendars("UnitedStates/NYSE", from=Sys.Date()-300*7, to=Sys.Date())
  corrected_dates<-adjust.previous(rawDates, "QuantLib/UnitedStates/NYSE")
  corrected_dates
}