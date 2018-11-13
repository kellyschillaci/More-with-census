#Read in the census data set
readCensus <- function(){
  urlToRead <-
    "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  
  #read the data from the web
  testframe<-read.csv(url(urlToRead))
  #remove the first 8 rows ('header information')
  testframe<-testframe[-1:-8,]
  #only keep first 5 columns
  testframe<-testframe[,1:5]
  #rename the first column
  testframe$stateName<-testframe[,1]
  testframe<-testframe[,-1]
  #remove the last rows (tail info)
  testframe<-testframe[-52:-58,]
  #remove the 'dot' from state name
  testframe$stateName <-gsub("\\.","", testframe$stateName)
  #convert the columns to actual numbers and rename
  #columns
  testframe$april10census <-Numberize(testframe$X)
  testframe$april10base <-Numberize(testframe$X.1)
  testframe$july10pop <-Numberize(testframe$X.2)
  testframe$july11pop <-Numberize(testframe$X.3)
  testframe<-testframe[,-1:-4]
  #remove the old rownames, which are now confusing
  rownames(testframe)<-NULL
  
  
  return(testframe)
}
Numberize<-function(inputVector)
{
  inputVector<-gsub(",","",inputVector)
  inputVector<-gsub(" ","",inputVector)
  return(as.numeric(inputVector))
}

dfStates <- readCensus()

dfStates$april10census[1:3]

mean(dfStates$july11pop)
#mean = 6109645
max(dfStates$july11pop)
#max = 37691912
index <- which.max(dfStates$july11pop)
#California has the highest population in July 2011
#Sort data in increasing order July 2011
July2011Sort <- dfStates[order(dfStates$july11pop),]


#Function returns percent below mean
BelowMean <- function(input_vector, min_value){
  return((sum(input_vector <= min_value))/(length(input_vector)))
  
}
BelowMean(dfStates$july11pop,mean(dfStates$july11pop))

###Source An Introduction to Data Science by Jeffery S.Saltz and Jeffrey M. Stanton


