library(devtools) #loading all the required libraries and functions 
library(twitteR)
library(plyr) 
library(stringr)
library(gdata)
source("C:\\Users\\Ryanm\\Documents\\score_sentimentfn.R")

#automates the authorisation process for using the Twitter API 
api_key <- "Dxkeaeo90xqvJojTjv0thFk8q" 
api_secret <- "tCOOKiR2vW9Yw1fbShJfpebkWt3Cay8esHIeJ7K56GWzMdI85z"
access_token <- "1483135261-nxXJfMeCYYQmuYbPRbrRZ9YPd833XY1ZFpNxAlA"
access_token_secret <- "KcgiaiEAU8mU4OJFWUwIjNffGdpHfr681FbRylAIVqcYt"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#loads the positive and negative lexicon
pos.words <- scan('C:/Users/Ryanm/Desktop/College/sentiment_lexicon/positive-words.txt', what='character', comment.char=';')
neg.words <- scan('C:/Users/Ryanm/Desktop/College/sentiment_lexicon/negative-words.txt', what='character', comment.char=';')

#clean text function removes unicode 
clean.text <- function(text)
{
	#removes forbidden characters
	ct<-str_replace_all(text,"[^[:graph:]]", " ")

	#remove usernames
	ct<-gsub('(@)(\\w+)','',ct)

	#remove RT
	ct<-gsub('RT ','',ct)

	#remove hyperlinks
	ct<-gsub('(http)(\\:)(\\/)(\\w+)','',ct)

	#map to lower case
	ct<-tolower(ct)

	#remove unicode
	ct<-gsub('(\\<U\\+)(\\w+)','',ct)
	ct<-gsub('(\\<)(ed)(\\>)','',ct)

	#remove whitespace
	#ct<-gsub('^\\s+|\\s+$','',ct)
	ct<-gsub('^\\s+|\\s+$','',ct)

	return(ct)
}

overall.sentiment <- function(scores) #this takes the scores over a day/series of days 
{
	num.rows <- length(scores)

	positive.tweets=0 
	neutral.tweets=0
	negative.tweets=0
	
	for( i in 1:num.rows )
	{
		if( (scores[i] >= 1) )
		{
			positive.tweets=positive.tweets+1
		}

		if( (scores[i] == 0))
		{
			neutral.tweets=neutral.tweets+1
		}

		if( (scores[i] <= -1) )
		{
			negative.tweets=negative.tweets+1
		}
	}
	results = c(positive.tweets,neutral.tweets,negative.tweets)
	return(results)
}
 
classify.tweets <- function(string,start,end,num.tweets)
{
	start<-toString(start) #R doesn't initially recognise these in the correct format
	end<-toString(end)

	tweets<-searchTwitter(string, since=start, until=end, n=num.tweets)
	tweets.text<-laply(tweets, function(t) t$getText() ) #extracts just the text from the tweets  
	tweets.text<-clean.text(tweets.text) #clean up the text remove unicode etc

	scores<-score.sentiment(tweets.text,pos.words,neg.words) #note that this includes the tweets themselves, remove this 
	scores<-scores$score
	
	sentiment<-overall.sentiment(scores) #gives the vector with 3 entries (POS,NEU,NEG)
	return(sentiment)
}

load.market.data<-function(start,end) #Would be nice to have checking of structure of the table included 
{
	start<-toString(start) #R doesn't initially recognise these in the correct format
	end<-toString(end)
	days<-seq(from=as.Date(start),to=as.Date(end),by='days' ) #gives all of the days between the start and end
	num.days<-length(days)

	market.dataframe <- read.csv(file="C:\\Users\\Ryanm\\Documents\\table.csv",header=TRUE) #change this to have a general file handle
	dimensions <- dim(market.dataframe)
	n_rows <- dimensions[1]
	day_count<-1 #this will be used to keep track of the entries successfully extracted from the dataframe 
	high.vec<-NULL #these will be the y variables in our linear regression 
	low.vec<-NULL

	for (i in n_rows:1) #note that the excel spreadsheet has dates in reverse order 
	{
		entry<-market.dataframe[i,1] #this indexing just gives the date entry 
		
		if( toString(entry) == days[day_count] ) #this means we have found the correct row of the dataframe. Note this array (days) is sorted chronologically 
		{
			row<-market.dataframe[i,] #this gives the whole row from the dataframe 
			high.vec<-c(high.vec,row[[3]] )
			low.vec<-c(low.vec,row[[4]] )
						
			if(day_count==num.days) #this means we're finished
			{
				market.vec<-c(high.vec,low.vec)
				market.matrix = matrix( market.vec, nrow = num.days, ncol = 2)	
				return(market.matrix)
			}
			day_count<-day_count+1
		}	
	}
	if(day_count!=num.days) #this means we weren't able to get all the desired dates from the spreadsheet
	{
		print("Error: missing market days from spreadsheet")
		return(0)
	}
}
	
#when regressing market share against sentiment, we use (for a given market share day) the previous consec.days sentiment 
get.apple.sentiment<-function(string,start,end,consec.days,num.tweets) #load vector of all dates and pick the relevant ones 
{
	start<-toString(start) #R doesn't initially recognise these in the correct format
	end<-toString(end)											
	days<-seq(from=as.Date(start),to=as.Date(end),by='days' ) 
	num.days<-length(days)
	market.start<-days[1+consec.days] 
	market.end<-days[length(days)] #last element of vector 
	num.market.days<- length(days)-consec.days #we iterate through the excel table here 
	
	#loads all the required market data for the whole period 
	stock.matrix<-load.market.data(market.start,market.end) 
	
	#forming element of data frame 
	tweet.dates <- NULL 
	positive.vector<-NULL
	neutral.vector<-NULL
	negative.vector<-NULL
	market.dates<-NULL
	high.vec<-stock.matrix[,1]
	low.vec<-stock.matrix[,2]
	
	for(i in 1:num.market.days ) 
	{
		#making column of data frame 
		day1<-days[i]
		day2<-days[i + consec.days - 1] #these are the predictor days 
		entry<-paste(day1,'to',day2,sep=" ") 
		tweet.dates <-c(tweet.dates,entry)
		figure<-toString(days[i+consec.days])
		market.dates<-c(market.dates,figure)
		
		#getting the tweet score used to "predict" market share 
		consec.days.score <- classify.tweets(string,day1,day2,num.tweets)
				
		#classifying tweets 
		positive.vector<-c(positive.vector, consec.days.score[1] ) 
		neutral.vector<-c(neutral.vector, consec.days.score[2] )   
		negative.vector<-c(negative.vector, consec.days.score[3] )
	}

	search.string<-replicate(num.market.days,string)
	num.tweets<-replicate(num.market.days,num.tweets)
	apple.df <- data.frame(search.string,tweet.dates,market.dates,num.tweets,positive.vector,neutral.vector,negative.vector,high.vec,low.vec) 
	write.table(apple.df,'results.csv', sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
	return(apple.df)
}

sentiment.regression<-function()

#get latest apple data from this link http://finance.yahoo.com/q/hp?s=AAPL&a=10&b=02&c=2014&d=10&e=14&f=2014&g=d




