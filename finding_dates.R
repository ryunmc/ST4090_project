dates.index = function(start,end)
{
	dates <- read.xls("C:\\Users\\Ryanm\\Desktop\\ST4090\\apple_dates.xlsx", header=FALSE) #these are the dates in a column
	num_rows <- nrow(dates)	

	i <- 1
      date <- "2015-01-08"

      while(date != end)
	{
		date <- toString(dates[i,1])
		i<-i+1
		
		if(i >= num_rows)
		{
			print("Error: start date not found")
			return(0)
		}
      }

	end_index<-i-1

	while(date != start)
	{
		date <- toString(dates[i,1])
		i<-i+1

		if(i >= num_rows)
		{
			print("Error: end date not found")
			return(0)
		}
	}
	
	start_index<-i-1
	
	return( c(start_index,end_index) ) #this is used to isolate the appropiate dates 
	
}
	
	