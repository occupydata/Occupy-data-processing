

contribs.df

target.corp





parse.dcra<-function(x) {
	
	#	ret.df<-as.data.frame(matrix("", nrow=1, ncol=34), stringsAsFactors=FALSE)
	
	ret.df<-vector(mode="list", length=34)
	
	names(ret.df)<-c("corp.name",
									 "file.num",
									 "entity.id",
									 "model.type",
									 "locale",
									 "qualifier",
									 "business.name",
									 "suffix",
									 "effective.date",
									 "registration.date",
									 "is.perpetual",
									 "end.date",
									 "entity.status",
									 "entity.status.date",
									 "foreign.name",
									 "date.of.organization",
									 "state",
									 "country",
									 "suffix2",
									 "bus.addr.line.1",
									 "bus.addr.line.2",
									 "bus.addr.city",
									 "bus.addr.state",
									 "bus.addr.zip",
									 "is.non.comm.reg.agent",
									 "reg.agent.name",
									 "reg.agent.addr.line.1",
									 "reg.agent.addr.line.2",
									 "reg.agent.addr.city",
									 "reg.agent.addr.state",
									 "reg.agent.addr.zip",
									 "reg.agent.ph.num",
									 "reg.agent.fax.num",
									 "reg.agent.email")
	
	temp.v<-x[grepl("Initial File Number:", x)]
	ret.df$corp.name<-gsub("&nbsp- Initial File Number:.+", "", temp.v)
	ret.df$file.num<-gsub(".+&nbsp- Initial File Number: ", "", temp.v)
	
	ret.df$entity.id<-x[which(x=="Entity Id")+1]
	
	ret.df$model.type<-x[which(x=="Model Type")+1]
	
	ret.df$locale<-x[which(x=="Locale")+1]
	
	ret.df$qualifier<-x[which(x=="Qualifier")+1] 
	
	ret.df$business.name<-gsub("^Business Name", "", 
														 x[grepl("^Business Name[A-Za-z]", x)])
	
	ret.df$suffix<-gsub("^Suffix", "", 
											x[grep("^Business Name[A-Za-z]", x)+1])
	
	ret.df$effective.date<-gsub("^Effective Date", "", 
															x[grepl("^Effective Date[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)])
	
	ret.df$registration.date<-gsub("^Registration Date", "", 
																 x[grepl("^Registration Date[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)])
	
	ret.df$is.perpetual<-gsub("^Is Perpetual[?]", "", 
														x[grepl("^Is Perpetual[?][a-zA-Z]", x)])
	
	ret.df$end.date<-gsub("^End Date", "", 
												x[grepl("^End Date[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)])
	
	ret.df$entity.status<-gsub("^Entity Status", "", 
														 x[grepl("^Entity Status[a-zA-Z]", x)])
	
	ret.df$entity.status.date<-gsub("^Entity Status Date", "", 
																	x[grepl("^Entity Status Date[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)])
	
	ret.df$foreign.name<-gsub("^Foreign Name", "", 
														x[grepl("^Foreign Name", x)])
	
	ret.df$date.of.organization<-gsub("^Date of Organization", "", 
																		x[grepl("^Date of Organization[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)])
	
	ret.df$state<-gsub("^State", "", 
										 x[grep("^Date of Organization[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)+1])
	
	ret.df$country<-gsub("^Country", "", 
											 x[grep("^Date of Organization[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)+2])
	
	ret.df$suffix2<-gsub("^Suffix", "", 
											 x[grep("^Date of Organization[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)+3])
	
	business.addr.v<-x[
		which(x=="Business Address"):grep("Is non-commercial Registered Agent", x)]
	
	ret.df$bus.addr.line.1<-business.addr.v[which(business.addr.v=="Line1")+1]
	
	temp.v<-business.addr.v[which(business.addr.v=="Line2")+1]
	
	#	cat(business.addr.v, sep="\n")
	
	if (temp.v=="City") {
		ret.df$bus.addr.line.2<-""
	} else {
		ret.df$bus.addr.line.2<-temp.v
	}
	
	ret.df$bus.addr.city<-business.addr.v[which(business.addr.v=="City")+1]
	ret.df$bus.addr.state<-business.addr.v[which(business.addr.v=="State")+1]
	ret.df$bus.addr.zip<-business.addr.v[which(business.addr.v=="Zip")+1]
	
	reg.agent.v<-x[which(x=="Agent"):which(x=="Report List")]
	
	ret.df$is.non.comm.reg.agent<-gsub("^Is non-commercial Registered Agent[?]", "", 
																		 reg.agent.v[grepl("^Is non-commercial Registered Agent[?]", reg.agent.v)])
	
	ret.df$reg.agent.name<-gsub("^Name", "", 
															reg.agent.v[grepl("^Name[A-Za-z]", reg.agent.v)])
	
	ret.df$reg.agent.addr.line.1<-reg.agent.v[which(reg.agent.v=="Line1")+1]
	
	temp.v<-reg.agent.v[which(reg.agent.v=="Line2")+1]
	
	if (temp.v=="City") {
		ret.df$reg.agent.addr.line.2<-""
	} else {
		ret.df$reg.agent.addr.line.2<-temp.v
	}
	
	ret.df$reg.agent.addr.city<-reg.agent.v[which(reg.agent.v=="City")+1]
	ret.df$reg.agent.addr.state<-reg.agent.v[which(reg.agent.v=="State")+1]
	ret.df$reg.agent.addr.zip<-reg.agent.v[which(reg.agent.v=="Zip")+1]
	
	ret.df$reg.agent.ph.num<-gsub("^Phone Number", "", 
																reg.agent.v[grepl("^Phone Number", reg.agent.v)])
	
	ret.df$reg.agent.fax.num<-gsub("^Fax Number", "", 
																 reg.agent.v[grepl("^Fax Number", reg.agent.v)])
	
	ret.df$reg.agent.email<-gsub("^Email", "", 
															 reg.agent.v[grepl("^Email", reg.agent.v)])
	
	
	
	ret.df[sapply(ret.df, length)!=1]<-""
	
	ret.df<-as.data.frame(ret.df, stringsAsFactors=FALSE)
	
	return(ret.df)
	
}

contribs.df$contribution.id

dcra.data.to.merge.ls<-list()

# for (target.contrib in suspected.bundled.contrib.ids) {

# target.corp<-contribs.df$Contributor[contribution.id==target.contrib]

target.contrib<-100

target.corp<-"Big Bear, LLC"

target.corp.query<-gsub("[[:punct:]]", " ",  target.corp)
target.corp.query<-gsub("[[:space:]]+", " ",  target.corp.query)
target.corp.query<-gsub("(^[[:space:]]+)|([[:space:]]+$)", "",  target.corp.query)

target.corp.query<-toupper(target.corp.query)

target.corp.suffix<-str_extract_all(target.corp.query, 
  "( PC$)|( INC$)|( LLC$)|( INCORPORATED$)|( PLLC$)|( LLP$)|( LTD$)|( LLLP$)|( LIMITED PARTNERSHIP$)|( PARTNERSHIP$)|( LP$)|( GP$)")[[1]]

target.corp.query<-gsub( "( PC$)|( INC$)|( LLC$)|( INCORPORATED$)|( PLLC$)|( LLP$)|( LTD$)|( LLLP$)|( LIMITED PARTNERSHIP$)|( PARTNERSHIP$)|( LP$)|( GP$)", "", target.corp.query)

cat(target.corp.query, file=paste(code.dir, "dcraQuery.txt", sep=""))

system(paste("cd ", "\"", code.dir, "\"", "\n", "php ", "\"", code.dir, "dcra-scrape.php\"", sep=""), ignore.stdout = TRUE, ignore.stderr = TRUE)


scrape.output.v<-readLines(paste(code.dir, "dcra_temp_data.txt", sep=""))

unlink(paste(code.dir, "dcra_temp_data.txt", sep=""))

scrape.output.v<-strsplit(scrape.output.v, "<SEPARATOR>")

scrape.output.ls<-lapply(scrape.output.v, parse.dcra)

scrape.output.df<-do.call(rbind, scrape.output.ls)

scrape.output.df$effective.date<-as.Date(scrape.output.df$effective.date, "%m/%d/%Y")
scrape.output.df$registration.date<-as.Date(scrape.output.df$registration.date, "%m/%d/%Y")
scrape.output.df$entity.status.date<-as.Date(scrape.output.df$entity.status.date, "%m/%d/%Y")
scrape.output.df$date.of.organization<-as.Date(scrape.output.df$date.of.organization, "%m/%d/%Y")


scrape.output.df$likely.match<-FALSE
scrape.output.df$birth.date<-scrape.output.df$effective.date
scrape.output.df$death.date<-NA

for ( i in 1:nrow(scrape.output.df)) {
	
  if (scrape.output.df$entity.status[i]=="Active") {
    death.date<-as.Date("12/31/2012", "%m/%d/%Y")
  } else {
  	death.date<-scrape.output.df$entity.status.date[i]
  }
  
}

scrape.output.df$likely.match<-
  scrape.output.df$birth.date <= contribs.df$Date.of.Receipt[contribution.id==target.contrib] &
  scrape.output.df$death.date >= contribs.df$Date.of.Receipt[contribution.id==target.contrib]

scrape.output.df$likely.match[is.na(scrape.output.df$likely.match)]<-TRUE

# install.packages("RecordLinkage")
library("RecordLinkage")

scrape.output.df$edit.dist<-levenshteinDist(target.corp.query, scrape.output.df$business.name)

scrape.output.df$chosen.match<-FALSE

scrape.output.df$chosen.match[
  which.min(scrape.output.df$edit.dist[scrape.output.df$likely.match])]<-TRUE
  

scrape.output.df$contribution.id<-target.contrib


#if (sum(scrape.output.df$chosen.match)==1) {
	dcra.data.to.merge.ls[[as.character(target.contrib)]]<-
		scrape.output.df[scrape.output.df$chosen.match, ]
#}

# if (sum(scrape.output.df$chosen.match)>1) {
#	dcra.data.to.merge.ls[[as.character(target.contrib)]]<-
#		scrape.output.df[sample(which(scrape.output.df$chosen.match), size=1), ]
#}





Date.of.Receipt
effective.date
nd.date
!="Active"

narrow down possible corps by dates
And narrow by type of corp
use text matching to get the right one

