clean_text <- function(text)
{

#remove usernames
ct<-gsub('(@)(\\w+)','',text)

#remove RT
ct<-gsub('RT ','',ct)

#remove hyperlinks
ct<-gsub('(http)(\\:)(\\/)(\\w+)','',ct)

#remove unicode
ct<-gsub('(\\<U\\+)(\\w+)','',ct)
ct<-gsub('(\\<)(ed)(\\>)','',ct)

#map to lower case
#ct<-tolower(ct)

#remove whitespace
#ct<-gsub('^\\s+|\\s+$','',ct)
ct<-gsub('^\\s+|\\s+$','',ct)

return(ct)
} 