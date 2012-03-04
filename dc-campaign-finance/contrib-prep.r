
work.dir<-""
# Put your working directory here

download.file(
  "http://commondatastorage.googleapis.com/ckannet-storage/2012-02-16T040627/DC_campaign_contribs_99_part12.zip",
  paste(work.dir, "DC_campaign_contribs_99_part12.zip", sep="")
  )
  
unzip( paste(work.dir, "DC_campaign_contribs_99_part12.zip", sep=""), exdir=paste(work.dir, "raw/", sep="") )

files.to.stack<-list.files(path=paste(work.dir, "raw/", sep=""))

files.to.stack<-files.to.stack[grepl("DC_contribs_", files.to.stack)]

contribs.df<-read.csv(paste(work.dir, "raw/", files.to.stack[1], sep=""), stringsAsFactors=FALSE)

for ( targ.file in files.to.stack[-1] ) {

  contribs.temp.df<-read.csv(paste(work.dir, "raw/", targ.file, sep=""), stringsAsFactors=FALSE)

  contribs.df<-rbind(contribs.df, contribs.temp.df)

}

contribs.to.geocode.df<-contribs.df[!duplicated(contribs.df[, c("Address", "city", "state", "Zip")]), 
  c("Address", "city", "state", "Zip")]

contribs.to.geocode.df$geocode.id<-1:nrow(contribs.to.geocode.df)

contribs.df<-merge(contribs.df, contribs.to.geocode.df)

geocode.output.ls<-vector("list", length=nrow(contribs.to.geocode.df))

URLencode.vec <- Vectorize(URLencode)
# Find a fix for slow operation:
# http://r.789695.n4.nabble.com/RFE-vectorize-URLdecode-td901435.html

contribs.to.geocode.df$api.url<-paste(
	"http://webgis.usc.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsedAdvanced_V02_96.aspx?streetAddress=",
	URLencode.vec(contribs.to.geocode.df$Address, reserved = TRUE),
	"&city=",
	URLencode.vec(contribs.to.geocode.df$city, reserved = TRUE),
	"&state=",
	URLencode.vec(contribs.to.geocode.df$state, reserved = TRUE),
	"&zip=",
	URLencode.vec(contribs.to.geocode.df$Zip, reserved = TRUE),
	"&apikey=YOUR_API_KEY_HERE&format=tsv&census=true&censusYear=2010&notStore=false&verbose=true&h=u&geom=false&version=2.96",
	sep=""
	)


# observs<-sample(1:nrow(contribs.to.geocode.df), 2000)
# observs[i]

check.integer <- function(N){
	!length(grep("[^[:digit:]]", format(N, scientific = FALSE)))
}

library(sendmailR)
library(stringr)

credits<-2000

for ( i in 24300:nrow(contribs.to.geocode.df)) {
	
	geocode.output.ls[[i]]<-
		tryCatch(read.delim(contribs.to.geocode.df$api.url[i], header=FALSE, stringsAsFactors=FALSE),
						 error = function(e) { "Error retrieving geocode" })
	
	cat(date(), "\n")
	flush.console()
	
	credits <- credits - 1
	
	if (check.integer(i/50)) {
		
		credits<- tryCatch(
			read.table("http://webgis.usc.edu/UserServices/Payments/AccountBalanceWebServiceHttp.aspx?version=1.0&apikey=YOUR_API_KEY_HERE&format=csv", stringsAsFactors=FALSE, sep=",")[, 2],
			error = function(e) { credits })
		
		junk.v<-tryCatch(save(geocode.output.ls, file=paste(work.dir, "Geocode output list.Rdata")),
										 error = function(e) { "Error" })
		
	}
	
	if (credits<=10) {
		
		from <- sprintf("<sendmailR@%s>", Sys.info()[4])
		to <- "<travis.d.mcarthur@gmail.com>"
		subject <- "ADD MORE WEBGIS CREDITS"
		
		junk.v<-tryCatch( sendmail(from, to, subject, paste("Hi Travis, \n\nYou need to add more credits to WebGIS"),
															 control=list(smtpServer="ASPMX.L.GOOGLE.COM")), error = function(e) { "Error" })
		
		while (credits<=10) {
			
			Sys.sleep(10)
			
			credits<- tryCatch(
				read.table("http://webgis.usc.edu/UserServices/Payments/AccountBalanceWebServiceHttp.aspx?version=1.0&apikey=YOUR_API_KEY_HERE&format=csv", stringsAsFactors=FALSE, sep=",")[, 2],
				error = function(e) { credits })
			
		}
		
	}
	
}

load("/Users/travismcarthur/Desktop/Occupy/DC MOP UP/Ballot initiative/Geocode output list.Rdata")

# geocode.output.ls<-geocode.output.ls[1:31441]
successful.geocode.v<-sapply(geocode.output.ls, FUN=function(x) {
	if(is.data.frame(x)) {
	  ret<-ncol(x)==122 
	} else {
	  ret<-FALSE
	} 
	ret})

# <need to add second attempt code in here >

# this is inefficient geocode.output.df<-do.call( rbind, geocode.output.ls[successful.geocode.v])

geocode.output.mat<-matrix(unlist(geocode.output.ls[successful.geocode.v]), ncol=122, byrow=TRUE)
geocode.output.df<-as.data.frame(geocode.output.mat, stringsAsFactors=FALSE)

colnames(geocode.output.df)<-c("Transaction.Id", "API.Version", "Query.Status.Code", "Latitude", "Longitude", "Match.Score", "Match.Type", "Matching.Geography.Type", "Interpolation.Type", "Interpolation.Sub.Type", "Matched.Location.Type", "Feature.Matching.Result.Type", "FeatureMatchingResultCount", "FeatureMatchingResultTypeNotes", "TieHandlingStrategyType", "FeatureMatchingResultTypeTieBreakingNotes", "FeatureMatchingSelectionMethod", "FeatureMatchingSelectionMethodNotes", "Time.Taken", "Census.Year", "Census.Block", "Census.Block.Group", "Census.Tract", "Census.County.Fips", "Census.CBSA.Fips", "Census.CBSA.Micro", "Census.MCD.Fips", "Census.MetDiv.Fips", "Census.MSA.Fips", "Census.Place.Fips", "Census.State.Fips", "MNumber", "MNumberFractional", "MPreDirectional", "MPreQualifier", "MPreType", "MPreArticle", "MName", "MPostArticle", "MPostQualifier", "MSuffix", "MPostDirectional", "MSuiteType", "MSuiteNumber", "MPostOfficeBoxType", "MPostOfficeBoxNumber", "MCity", "MConsolidatedCity", "MMinorCivilDivision", "MCountySubRegion", "MCounty", "MState", "MZip", "MZipPlus1", "MZipPlus2", "MZipPlus3", "MZipPlus4", "MZipPlus5", "PNumber", "PNumberFractional", "PPreDirectional", "PPreQualifier", "PPreType", "PPreArticle", "PName", "PPostArticle", "PPostQualifier", "PSuffix", "PPostDirectional", "PSuiteType", "PSuiteNumber", "PPostOfficeBoxType", "PPostOfficeBoxNumber", "PCity", "PConsolidatedCity", "PMinorCivilDivision", "PCountySubRegion", "PCounty", "PState", "PZip", "PZipPlus1", "PZipPlus2", "PZipPlus3", "PZipPlus4", "PZipPlus5", "FNumber", "FNumberFractional", "FPreDirectional", "FPreQualifier", "FPreType", "FPreArticle", "FName", "FPostArticle", "FPostQualifier", "FSuffix", "FPostDirectional", "FSuiteType", "FSuiteNumber", "FPostOfficeBoxType", "FPostOfficeBoxNumber", "FCity", "FConsolidatedCity", "FMinorCivilDivision", "FCountySubRegion", "FCounty", "FState", "FZip", "FZipPlus1", "FZipPlus2", "FZipPlus3", "FZipPlus4", "FZipPlus5", "FArea", "FAreaType", "FGeometrySRID", "FGeometry", "FSource", "FVintage", "FPrimaryIdField", "FPrimaryIdValue", "FSecondaryIdField", "FSecondaryIdValue")

geocode.output.df$geocode.id<-which(successful.geocode.v)

geocode.output.df<-geocode.output.df[, 
  apply(geocode.output.df, MARGIN=2, FUN=function(x) {!all(is.na(x))})
  ]


contribs.geocoded.df<-merge(contribs.df, geocode.output.df)

#contribs.geocoded.df<-geocode.output.df

contribs.geocoded.df$geocode.success<-contribs.geocoded.df$Feature.Matching.Result Type=="Success"
contribs.geocoded.df$geocode.exact<-geocode.output.df$Match.Type=="Exact"

contribs.geocoded.df[contribs.geocoded.df$Matching.Geography.Type=="USPSZipPlus4", c("FArea", "FAreaType")]

head(
contribs.geocoded.df[contribs.geocoded.df$Matching.Geography.Type=="CountySubRegion", c("FArea", "FAreaType")]
)

Take these:
Parcel StreetSegment    USPSZipPlus4

contribs.geocoded.df$PSuiteNumber.cleaned<-gsub("[^0-9]", "", contribs.geocoded.df$PSuiteNumber)

address.cols<- c("PNumber", "PNumberFractional", "FPreDirectional", "FPreQualifier", "FName", "FPostQualifier", "FPostQualifier", "FSuffix", "FPostDirectional", "PSuiteNumber.cleaned")

for (i in address.cols) {
	contribs.geocoded.df[is.na(contribs.geocoded.df[, i]), i]<-"empty.remove.indicator"
}

contribs.geocoded.df$address.clean<-paste(
	contribs.geocoded.df[, address.cols[1]],
  contribs.geocoded.df[, address.cols[2]],
  contribs.geocoded.df[, address.cols[3]],
  contribs.geocoded.df[, address.cols[4]],
  contribs.geocoded.df[, address.cols[5]],
  contribs.geocoded.df[, address.cols[6]],
  contribs.geocoded.df[, address.cols[7]],
  contribs.geocoded.df[, address.cols[8]],
  contribs.geocoded.df[, address.cols[9]],
	"No.",
  contribs.geocoded.df[, address.cols[10]],
	sep=" "
)

contribs.geocoded.df$address.clean<-gsub("No[.] empty[.]remove[.]indicator", "", contribs.geocoded.df$address.clean )

contribs.geocoded.df$address.clean<-gsub("empty[.]remove[.]indicator", "", contribs.geocoded.df$address.clean )

contribs.geocoded.df$address.clean<-gsub("( ){2,}", " ",  contribs.geocoded.df$address.clean )

contribs.geocoded.df$address.clean<-gsub("( ){2,}", " ",  contribs.geocoded.df$address.clean )

contribs.geocoded.df$address.clean<-gsub("(^ *)|( *$)", "", contribs.geocoded.df$address.clean )

contribs.geocoded.df$PNumber[is.na(contribs.geocoded.df$PNumber)<-"empty.remove.indicator"
contribs.geocoded.df$PNumberFractional[is.na(contribs.geocoded.df$PNumberFractional)<-"empty.remove.indicator"

for (i in address.cols) {
  contribs.geocoded.df[contribs.geocoded.df[, i]=="empty.remove.indicator", i]<-NA
}
							 
																			 Think about removing: PNumberFractional
FPreDirectional	FPreQualifier	FName	FPostQualifier	FSuffix	FPostDirectional
Preprocess to remove anything but numbers: PSuiteNumber
contribs.geocoded.df$[is.na(contribs.geocoded.df$)<-"empty.remove.indicator"

Take the first of these that is not NA: FCity	FCountySubRegion	FCounty

table(geocode.output.df$Match.Score==100, geocode.output.df$Match.Type)


contribs.df[contribs.df$geocode.id==1,]

contribs.to.geocode.df[contribs.to.geocode.df$geocode.id==1,]

test.df<-read.csv("http://webgis.usc.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V02_96.aspx?streetAddress=9355%20Burton%20Way&city=Beverly%20Hills&state=ca&zip=90210&apikey=YOUR_API_KEY_HERE&format=csv&census=true&censusYear=2010&notStore=false&version=2.96")

sapply(successful.geocode.1.v, FUN=length)

geocode.output.leftover.ls<-geocode.output.ls[successful.geocode.v]
table(sapply(geocode.output.leftover.ls, FUN=ncol))


write.csv(contribs.geocoded.df[1:1000, ], "/Users/travismcarthur/Desktop/Occupy/DC MOP UP/Ballot initiative/R processing/test output.csv")
  
  
# Examples for WebGIS:
  https://webgis.usc.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsedAdvanced_V02_96.aspx?streetAddress=PO%20Box%20123&city=Beverly%20Hills
  &state=ca
  &zip=90210&apikey=demo&format=csv&census=true&censusYear=2010&notStore=false&verbose=true&h=u&geom=false&version=2.96
  
  
  
  
  http://webgis.usc.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsedAdvanced_V02_96.aspx?streetAddress=
  	9355%20Berton%20Way
  &city=Bevirrly%20Hills&state=ca&zip=90210&apikey=demo&format=csv&census=true&censusYear=2010&notStore=false&verbose=true&sou=true&souatts=city&geom=true&version=2.96
  
library(lattice)

install.packages("rgl")
library(rgl)
attach(mtcars)
plot3d(wt, disp, mpg, col="red", size=3) 


