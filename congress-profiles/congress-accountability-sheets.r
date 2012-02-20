# You will need an API key from both opensecrets.org and nytimes.com

install.packages("XML")
install.packages("reshape")

library("XML")
library("reshape")

work.dir<-""
# Put your working directory here


for ( i in 1:nrow(ca.df)) {

	try( 	
    download.file(url=paste("http://www.govtrack.us/data/photos/", ca.df$GT.ID[i], "-200px.jpeg", sep=""), 
      destfile=paste(work.dir, "Cong photos/", ca.df$GT.ID[i], ".jpeg", sep=""))
  )
  
} 

ph.file<-list.files(path = paste(work.dir,  "Cong photos/", sep="") )

for ( i in ca.df$GT.ID[!ca.df$GT.ID %in% gsub("[.]jpeg", "", ph.file) ] ) {

  file.copy(from="", 
    to=paste(work.dir, "Cong photos/", i, ".jpeg", sep="") )
# Need to fix this
}



ca.df<-xmlToList(xmlParse("http://www.govtrack.us/data/us/112/people.xml"))
ca.df<-lapply(ca.df, unlist)
ca.df<-lapply(ca.df, t)
ca.df<-lapply(ca.df, data.frame, stringsAsFactors =FALSE)
ca.df<-do.call(rbind.fill, ca.df)
ca.df<-moc.xml.df[ moc.xml.df$role.type=="rep" & moc.xml.df$role.current=="1" &
	as.Date(moc.xml.df$role.enddate) >= as.Date(Sys.Date()), 
  c(".attrs.id", ".attrs.osid", ".attrs.bioguideid",
    ".attrs.firstname", ".attrs.lastname", "role.party", "role.state", "role.district")
  ]	
# moc.xml.df$role.enddate is insurance against mid-term appointments to Senate



colnames(ca.df)<-c("GT.ID", "OS.ID", "BG.ID", "first.name", "last.name", "party", "state", "district")									 

ca.df<-ca.df[!is.na(ca.df$GT.ID) & !ca.df$state %in% c("AS", "GU", "MP", "PR", "VI"), ]

ca.df$party[ca.df$party=="Republican"]<-"R"
ca.df$party[ca.df$party=="Democrat"]<-"D"
ca.df$party[ca.df$party=="Independent"]<-"I"

ca.df$ph.num<-NA
ca.df$office<-NA

for ( i in 1:nrow(ca.df)) {

  contact.xml<-xmlToList(xmlParse(
    paste("http://api.opencongress.org/people?district=", ca.df$district[i], 
      "&state=", ca.df$state[i], sep="") ) )

  ca.df$ph.num[i]<-contact.xml$people$person$phone

  ca.df$office[i]<-gsub(" House Office Building", "", contact.xml$people$person[["congress-office"]])
cat(i, "\n")
}


OS.col.names<-c("OS.contrib.1", "OS.amount.1", "OS.contrib.2", "OS.amount.2", 
  "OS.contrib.3", "OS.amount.3", "OS.contrib.4", "OS.amount.4", "OS.contrib.5", "OS.amount.5" )
	
for ( i in OS.col.names) {
	ca.df[, i]<-NA
}


nrow(ca.df)
# 1:200
# 201:400
# 401:435


for ( i in 1:nrow(ca.df)) {

	moc.xml<-tryCatch(
    moc.xml<-xmlToList(xmlParse(
      paste("http://www.opensecrets.org/api/?method=candContrib&cid=", 
        ca.df$OS.ID[i], "&cycle=2010&apikey=YOUR_API_KEY", sep=""))),
    error = function(e) { "Data unavailable" }
  )
  
  if (moc.xml=="Data unavailable") { ca.df[i, OS.col.names]<-moc.xml; next }
  
  ca.df[i, OS.col.names]<-unlist(moc.xml$contributors[1:5])
  cat(paste(i, " "))

}
			


ca.df$OS.net.worth<-NA


for ( i in 401:435) {

  net.worth.v<-tryCatch(
    strsplit(readLines(paste("http://www.opensecrets.org/api/?method=memPFDprofile&year=2010&cid=", 
      ca.df$OS.ID[i], "&output=xml&apikey=YOUR_API_KEY", sep="")), " ")[[1]],
    error = function(e) "Data unavailable"
  )
  
  if (length(net.worth.v)==1) { ca.df[i, "OS.net.worth"]<-"Data Unavailable"; next }

  ca.df[i, "OS.net.worth"]<-mean(
    as.numeric(
      c(
        gsub("[^0-9]", "", net.worth.v[grepl("net_low", net.worth.v)]), 
        gsub("[^0-9]", "", net.worth.v[grepl("net_high", net.worth.v)])
      )
    )
  )
  ca.df[i, "OS.net.worth"]<-round(ca.df[i, "OS.net.worth"])
  cat(paste(i, " "))
}




vote.nums<-c("2011-932", "2010-413", "2008-681", "2010-165", "2010-647")
			

#NDAA: 2011-932
#Dodd Frank 2010-413
#Bailout: 2008-681
#Healthcare: 2010-165
#Bush tax cuts: 2010-647

for ( i in 1:5) {

  vote.df<- read.csv(paste("http://www.govtrack.us/congress/vote_download.xpd?vote=h", vote.nums[i], sep=""), 
    skip=1, stringsAsFactors=FALSE)
  
  colnames(vote.df)[1]<-"GT.ID"
  
  colnames(vote.df)[colnames(vote.df)=="VOTE"]<-paste("vote.", i, sep="")

  ca.df<-merge(ca.df, vote.df[, c("GT.ID", paste("vote.", i, sep=""))], all.x=TRUE)

}



# NYT years in office
ca.df$years<-NA

for ( i in 1:nrow(ca.df)) {

  nyt.xml<-xmlToList(xmlParse(paste("http://api.nytimes.com/svc/politics/v3/us/legislative/congress/members/", 
    ca.df$BG.ID[i], ".xml?api-key=YOUR_API_KEY", sep="")))

  ca.df$years[i]<-nyt.xml$results$member$roles[[1]]$seniority

  Sys.sleep(.6)
  cat(i, " ")
  #2 calls per second with NYT
}


			
#TO DO:
	# Truncate OS donors at 33 characters
	# Add SOPA co-sponsorship

write.csv(ca.df, paste(work.dir, "Cong Accountability output.csv" sep=""), row.names=FALSE)

			#SOPA co-sponsorship
			# http://www.washingtonpost.com/wp-dyn/content/article/2010/12/16/AR2010121606200.html
			# http://www.govtrack.us/congress/bill.xpd?bill=h112-3261
			# https://www.popvox.com/bills/us/112/hr3261



