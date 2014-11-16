library(devtools)
library(twitteR)
library(plyr) #loading all the required libraries and functions 
library(stringr)
library(gdata)
source("C:\\Users\\Ryanm\\Documents\\score_sentimentfn.R")

api_key <- "Dxkeaeo90xqvJojTjv0thFk8q" #automates the authorisation process for using the Twitter API 
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
 
classify.apple.tweets <- function(string,start,end,num_tweets)
{
	start<-toString(start) #R doesn't initially recognise these in the correct format
	end<-toString(end)

	apple.tweets<-searchTwitter(string, since=start, until=end, n=num_tweets)
	apple.tweets.text<-laply(apple.tweets, function(t) t$getText() ) #extracts just the text from the tweets  
	apple.tweets.text<-clean.text(apple.tweets.text) #clean up the text #remove unicode etc

	apple.scores<-score.sentiment(apple.tweets.text,pos.words,neg.words) #note that this includes the tweets themselves, remove this 
	apple.scores<-apple.scores$score
	
	sentiment<-overall.sentiment(apple.scores) #gives the vector with 3 entries (POS,NEU,NEG)
	return(sentiment)
}

load.apple.data<-function(start,end) #FIX THIS FUNCTION SO IT CAN DEAL WITH SPREADSHEETS WHICH MAY BE MISSING REQUIRED DATES (FIX SEARCH ALGORITHM)
{
	start<-toString(start) #R doesn't initially recognise these in the correct format
	end<-toString(end)
	days<-seq(from=as.Date(start),to=as.Date(end),by='days' ) #gives all of the days between the start and end
	num.days<-length(days)

	apple.dataframe <- read.xls("C:\\Users\\Ryanm\\Documents\\table.xlsx",header=TRUE)
	dimensions <- dim(apple.dataframe)
	n_rows <- dimensions[1]
	
	day_count<-1 #this will be used to keep track of the entries successfully extracted from the dataframe 
	value_array<-NULL #we will fill this with the market share extracted from the spreadsheet 
	day.average<-NULL
		
	for (i in n_rows:1) #note that the excel spreadsheet has dates in reverse order 
	{
		entry<-apple.dataframe[i,1] #this indexing just gives the date entry 
		
		if( toString(entry) == days[day_count] ) #this means we have found the correct row of the dataframe. Note this array (days) is sorted chronologically 
		{
			row <- apple.dataframe[i,] #the comma acts like a wildcard and tells you to isolate just that row
			
			day.high<- row[3] #perhaps take the average of the high and the low? FIX THIS PART!!
			day.low<- row[4]			
			day.average[day_count] = (day.high + day.low)/2
			print( day.average[day_count] )

			if(day_count == num.days) #this means we have successfully extracted the dates 
			{	
				break
			}	
			day_count<-day_count+1
		}		
	}
	
	if(day_count != num.days) #this means we have an error
	{	
			return("day missing from spreadsheet")
	}
	else
	{
		return(day.average)
	}
}

get.apple.sentiment<-function(string,start,end,consec.days,num_tweets,filename) #note the furthest that we can go back is present date - 6 days 
{
	start<-toString(start) #R doesn't initially recognise these in the correct format
	end<-toString(end)													#e.g. if consec.days is 3 we takes tweets in groups of 3 days
	days<-seq(from=as.Date(start),to=as.Date(end),by='days' ) #gives all of the days between the start and end
	num.days<-length(days)
	prices<-load.apple.data(start,end)
		
	day.string <- NULL #day.string will form a column in the data frame
	positive.vector<-NULL
	neutral.vector<-NULL
	negative.vector<-NULL

	for(i in 1:(num.days+1-consec.days) ) #this is because we can only mine tweets from between two dates 
	{					   #also, last cluster of dates is from (lastday+1-consec.days to lastday)
		day1<-days[i]
		day2<-days[i + consec.days - 1] #i.e. between day1 and day2 we have consec.days consecutive days
		
		entry<-paste(day1,'to ',day2,sep=" ") #this will be one form of entry in the data frame 
		day.string <-c(day.string,entry)  

		consec.days.score <- classify.apple.tweets(string,day1,day2,num_tweets)
			
		positive.vector<-c(positive.vector, consec.days.score[1]) #forming another column for the data frame 
		neutral.vector<-c(neutral.vector, consec.days.score[2])   #note R uses 1-based indexing 
		negative.vector<-c(negative.vector, consec.days.score[3])
	}

	search.string<-replicate(num.days+1-consec.days,string)
	num.tweets<-replicate(num.days+1-consec.days,num_tweets)
	
	apple.df <- data.frame(search.string,num.tweets,positive.vector,neutral.vector,negative.vector,prices) #note we have added prices here!!
	save(apple.df,file=filename)

	return(apple.df)
}

#get latest apple data from this link http://finance.yahoo.com/q/hp?s=AAPL&a=10&b=02&c=2014&d=10&e=14&f=2014&g=d



