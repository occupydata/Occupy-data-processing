
work.dir<-""
# Put your working directory here

download.file(
  "http://commondatastorage.googleapis.com/ckannet-storage/2012-02-16T040627/DC_campaign_contribs_99_part12.zip",
  paste(work.dir, "DC_campaign_contribs_99_part12.zip", sep="")
  )
  
unzip( paste(work.dir, "DC_campaign_contribs_99_part12.zip", sep=""), exdir=paste(work.dir, "raw/", sep="") )

files.to.stack<-list.files(path=paste(work.dir, "raw/", sep=""))

files.to.stack<-files.to.stack[grepl("DC_contribs_", files.to.stack)]

contribs.raw.df<-read.csv(paste(work.dir, "raw/", files.to.stack[1], sep=""), stringsAsFactors=FALSE)

for ( targ.file in files.to.stack[-1] ) {

  contribs.temp.df<-read.csv(paste(work.dir, "raw/", targ.file, sep=""), stringsAsFactors=FALSE)

  contribs.raw.df<-rbind(contribs.raw.df, contribs.temp.df)

}




contribs.to.geocode.df<-contribs.raw.df[!duplicated(contribs.raw.df[, c("Address", "city", "state", "Zip")]), 
  c("Address", "city", "state", "Zip")]

contribs.to.geocode.df$geocode.id<-1:nrow(contribs.to.geocode.df)

contribs.raw.df<-merge(contribs.raw.df, contribs.to.geocode.df)

for ( i in 1:ncol(contribs.to.geocode.df)) {

  contribs.to.geocode.df[, i]<-gsub("(^\t)|(\t$)", "", contribs.to.geocode.df[, i])
  contribs.to.geocode.df[, i]<-gsub("\t", " ", contribs.to.geocode.df[, i])

}


URLencode.vec <- Vectorize(URLencode)
# Find a fix for slow operation:
# http://r.789695.n4.nabble.com/RFE-vectorize-URLdecode-td901435.html

contribs.to.geocode.df$DC.geocoder<-FALSE

contribs.to.geocode.df$DC.geocoder[
  grepl("(^200)|(^202)|(^203)|(^204)|(^205)", contribs.to.geocode.df$Zip)]<-TRUE

contribs.to.geocode.df$DC.geocoder[
  contribs.to.geocode.df$state=="DC" & nchar(contribs.to.geocode.df$Zip)<4]<-TRUE
# Anything with city?


contribs.to.geocode.df$DC.geocoder[
  contribs.to.geocode.df$Address=="" | is.na(contribs.to.geocode.df$Address)]<-FALSE

DC.contribs.to.geocode.df<-contribs.to.geocode.df[contribs.to.geocode.df$DC.geocoder, ]

DC.contribs.to.geocode.df$DC.api.url<-paste(
  "http://citizenatlas.dc.gov/newwebservices/locationverifier.asmx/verifyDCAddressThrouString?Address=",
  URLencode.vec(DC.contribs.to.geocode.df$Address, reserved = TRUE),
  sep=""
  )
# TO DO: Deal with this warning message: 1: In strsplit(URL, "") : input string 1 is invalid in this locale



install.packages("sendmailR")
install.packages("stringr")
install.packages("XML")
install.packages("reshape")

library(sendmailR)
library(stringr)
library(XML)
library(reshape)

check.integer <- function(N){
  !length(grep("[^[:digit:]]", format(N, scientific = FALSE)))
}

DC.geocoded.df<-data.frame(a="a", stringsAsFactors=FALSE)[FALSE, FALSE]
DC.geocode.output.ls<-vector("list", length=2500)

# nrow(DC.contribs.to.geocode.df)

k<-1

for ( i in 1:nrow(DC.contribs.to.geocode.df)) {
  
  DC.geocode.output.ls[[k]]<-
    tryCatch(DC.geocode.output.ls[[k]]<-unlist(xmlToList(xmlParse(DC.contribs.to.geocode.df$DC.api.url[i]))),
             error = function(e) { "Error retrieving geocode" })
  
  DC.geocode.output.ls[[k]]<-DC.geocode.output.ls[[k]][
    !grepl("(^returnDataset.schema)|(^returnCDDataSet.schema)|(returnCDDataSet.diffgram)",
    names(DC.geocode.output.ls[[k]])) & !duplicated(names(DC.geocode.output.ls[[k]])) ]
    
  DC.geocode.output.ls[[k]]<-c(geocode.id=DC.contribs.to.geocode.df$geocode.id[i], DC.geocode.output.ls[[k]])
  
  k<-k+1
  
  cat(i, date(), "\n")
  flush.console()

  if (check.integer(i/2500) | i==nrow(DC.contribs.to.geocode.df) ) {

    DC.geocode.output.ls<-lapply(DC.geocode.output.ls, t)
    DC.geocode.output.ls<-lapply(DC.geocode.output.ls, data.frame, stringsAsFactors=FALSE)
    DC.geocode.output.ls<-do.call(rbind.fill, DC.geocode.output.ls)
    DC.geocoded.df<-rbind.fill(DC.geocoded.df, DC.geocode.output.ls)
    DC.geocode.output.ls<-vector("list", length=2500)
    junk.v<-tryCatch(save(DC.geocoded.df, file=paste(work.dir, "DC Geocode output list.Rdata")),
                     error = function(e) { "Error" })
    k<-1
        
  }
  
}

names(DC.geocoded.df)<-gsub("returnDataset[.]diffgram[.]NewDataSet[.]Table1[.]", "", names(DC.geocoded.df))

names(DC.geocoded.df)[names(DC.geocoded.df) != "geocode.id"]<-paste("DC.geocoder.", names(DC.geocoded.df)[names(DC.geocoded.df) != "geocode.id"], sep="")

contribs.to.geocode.df$DC.geocoder.ConfidenceLevel<-as.numeric(contribs.to.geocode.df$DC.geocoder.ConfidenceLevel)

contribs.to.geocode.df<-merge(contribs.to.geocode.df, DC.geocoded.df, all=TRUE)

contribs.to.geocode.df$USA.geocoder<-FALSE

contribs.to.geocode.df$USA.geocoder[!contribs.to.geocode.df$DC.geocoder]<-TRUE
contribs.to.geocode.df$USA.geocoder[!is.na(contribs.to.geocode.df$DC.geocoder.ConfidenceLevel) &
  contribs.to.geocode.df$DC.geocoder.ConfidenceLevel<80]<-TRUE
contribs.to.geocode.df$USA.geocoder[is.na(contribs.to.geocode.df$DC.geocoder.ConfidenceLevel)]<-TRUE

USA.contribs.to.geocode.df<-contribs.to.geocode.df[contribs.to.geocode.df$USA.geocoder, ]

#Keep?
#returnCDDataSet.diffgram.NewDataSet.Address_x0020_Return_x0020_Codes.Parsed_x0020__x0026__x0020_Normalized



geocode.output.ls<-vector("list", length=nrow(USA.contribs.to.geocode.df))


contribs.to.geocode.df$api.url<-paste(
  "http://webgis.usc.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsedAdvanced_V02_96.aspx?streetAddress=",
  URLencode.vec(USA.contribs.to.geocode.df$Address, reserved = TRUE),
  "&city=",
  URLencode.vec(USA.contribs.to.geocode.df$city, reserved = TRUE),
  "&state=",
  URLencode.vec(USA.contribs.to.geocode.df$state, reserved = TRUE),
  "&zip=",
  URLencode.vec(USA.contribs.to.geocode.df$Zip, reserved = TRUE),
  "&apikey=YOUR_API_KEY_HERE&format=tsv&census=true&censusYear=2010&notStore=false&verbose=true&h=u&geom=false&version=2.96",
  sep=""
  )


# observs<-sample(1:nrow(contribs.to.geocode.df), 2000)
# observs[i]

library(sendmailR)
library(stringr)

credits<-2000

for ( i in 24300:nrow(USA.contribs.to.geocode.df)) {
  
  geocode.output.ls[[i]]<-
    tryCatch(cbind(data.frame(
      geocode.id=USA.contribs.to.geocode.df$geocode.id[i], stringsAsFactor=FALSE),
      read.delim(USA.contribs.to.geocode.df$api.url[i], header=FALSE, stringsAsFactors=FALSE)),
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
    to <- ""
    # email address above
    subject <- "ADD MORE WEBGIS CREDITS"
    
    junk.v<-tryCatch( sendmail(from, to, subject, paste("Hi, \n\nYou need to add more credits to WebGIS"),
                               control=list(smtpServer="ASPMX.L.GOOGLE.COM")), error = function(e) { "Error" })
    
    while (credits<=10) {
      
      Sys.sleep(10)
      
      credits<- tryCatch(
        read.table("http://webgis.usc.edu/UserServices/Payments/AccountBalanceWebServiceHttp.aspx?version=1.0&apikey=YOUR_API_KEY_HERE&format=csv", stringsAsFactors=FALSE, sep=",")[, 2],
        error = function(e) { credits })
      
    }
    
  }
  
}


# geocode.output.ls<-geocode.output.ls[1:31441]
successful.geocode.v<-sapply(geocode.output.ls, FUN=function(x) {
  if(is.data.frame(x)) {
    ret<-ncol(x)==122 
  } else {
    ret<-FALSE
  } 
  ret})


geocode.output.mat<-matrix(unlist(geocode.output.ls[successful.geocode.v]), ncol=122, byrow=TRUE)
USA.geocode.output.df<-as.data.frame(geocode.output.mat, stringsAsFactors=FALSE)

colnames(USA.geocode.output.df)<-c("Transaction.Id", "API.Version", "Query.Status.Code", "Latitude", "Longitude", "Match.Score", "Match.Type", "Matching.Geography.Type", "Interpolation.Type", "Interpolation.Sub.Type", "Matched.Location.Type", "Feature.Matching.Result.Type", "FeatureMatchingResultCount", "FeatureMatchingResultTypeNotes", "TieHandlingStrategyType", "FeatureMatchingResultTypeTieBreakingNotes", "FeatureMatchingSelectionMethod", "FeatureMatchingSelectionMethodNotes", "Time.Taken", "Census.Year", "Census.Block", "Census.Block.Group", "Census.Tract", "Census.County.Fips", "Census.CBSA.Fips", "Census.CBSA.Micro", "Census.MCD.Fips", "Census.MetDiv.Fips", "Census.MSA.Fips", "Census.Place.Fips", "Census.State.Fips", "MNumber", "MNumberFractional", "MPreDirectional", "MPreQualifier", "MPreType", "MPreArticle", "MName", "MPostArticle", "MPostQualifier", "MSuffix", "MPostDirectional", "MSuiteType", "MSuiteNumber", "MPostOfficeBoxType", "MPostOfficeBoxNumber", "MCity", "MConsolidatedCity", "MMinorCivilDivision", "MCountySubRegion", "MCounty", "MState", "MZip", "MZipPlus1", "MZipPlus2", "MZipPlus3", "MZipPlus4", "MZipPlus5", "PNumber", "PNumberFractional", "PPreDirectional", "PPreQualifier", "PPreType", "PPreArticle", "PName", "PPostArticle", "PPostQualifier", "PSuffix", "PPostDirectional", "PSuiteType", "PSuiteNumber", "PPostOfficeBoxType", "PPostOfficeBoxNumber", "PCity", "PConsolidatedCity", "PMinorCivilDivision", "PCountySubRegion", "PCounty", "PState", "PZip", "PZipPlus1", "PZipPlus2", "PZipPlus3", "PZipPlus4", "PZipPlus5", "FNumber", "FNumberFractional", "FPreDirectional", "FPreQualifier", "FPreType", "FPreArticle", "FName", "FPostArticle", "FPostQualifier", "FSuffix", "FPostDirectional", "FSuiteType", "FSuiteNumber", "FPostOfficeBoxType", "FPostOfficeBoxNumber", "FCity", "FConsolidatedCity", "FMinorCivilDivision", "FCountySubRegion", "FCounty", "FState", "FZip", "FZipPlus1", "FZipPlus2", "FZipPlus3", "FZipPlus4", "FZipPlus5", "FArea", "FAreaType", "FGeometrySRID", "FGeometry", "FSource", "FVintage", "FPrimaryIdField", "FPrimaryIdValue", "FSecondaryIdField", "FSecondaryIdValue")


USA.geocode.output.df<-USA.geocode.output.df[, 
  apply(geocode.output.df, MARGIN=2, FUN=function(x) {!all(is.na(x))})
  ]




contribs.geocoded.df<-merge(contribs.raw.df, USA.geocode.output.df)

#contribs.geocoded.df<-geocode.output.df

contribs.geocoded.df$geocode.success<-contribs.geocoded.df$Feature.Matching.Result.Type=="Success"
contribs.geocoded.df$geocode.exact<-contribs.geocoded.df$Match.Type=="Exact"

contribs.geocoded.df$street.precise<-contribs.geocoded.df$Matching.Geography.Type %in% 
  c("Parcel", "StreetSegment", "USPSZipPlus4")


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

contribs.geocoded.df$PNumber[is.na(contribs.geocoded.df$PNumber)]<-"empty.remove.indicator"
contribs.geocoded.df$PNumberFractional[is.na(contribs.geocoded.df$PNumberFractional)]<-"empty.remove.indicator"

for (i in address.cols) {
  contribs.geocoded.df[is.na(contribs.geocoded.df[, i]) | 
    contribs.geocoded.df[, i]=="empty.remove.indicator", i]<-NA
}





# Take the first of these that is not NA: FCity  FCountySubRegion  FCounty

contribs.geocoded.df$city.clean[is.na(contribs.geocoded.df$FCountySubRegion) & is.na(contribs.geocoded.df$FCity)]<-
  contribs.geocoded.df$FCounty[is.na(contribs.geocoded.df$FCountySubRegion) & is.na(contribs.geocoded.df$FCity)]

contribs.geocoded.df$city.clean[is.na(contribs.geocoded.df$city.clean) & is.na(contribs.geocoded.df$FCity)]<-
  contribs.geocoded.df$FCountySubRegion[is.na(contribs.geocoded.df$city.clean) & is.na(contribs.geocoded.df$FCity)]

contribs.geocoded.df$city.clean[is.na(contribs.geocoded.df$city.clean)]<-
  contribs.geocoded.df$FCity[is.na(contribs.geocoded.df$city.clean)]

contribs.geocoded.df$state.clean<-contribs.geocoded.df$FState

contribs.geocoded.df$zip.clean<-contribs.geocoded.df$FZipPlus4
contribs.geocoded.df$zip.clean[is.na(contribs.geocoded.df$zip.clean)]<-contribs.geocoded.df$FZip[is.na(contribs.geocoded.df$zip.clean)]

contribs.geocoded.df$Date.of.Receipt<-as.Date(contribs.geocoded.df$Date.of.Receipt, format="%m/%d/%Y")
contribs.geocoded.df$Amount<-gsub("([$]|(,))", "", contribs.geocoded.df$Amount)
neg.amounts.v<-grepl("^[(].*[)]$", contribs.geocoded.df$Amount)

contribs.geocoded.df$Amount[neg.amounts.v]<-
  gsub("[()]", "", contribs.geocoded.df$Amount[neg.amounts.v])

contribs.geocoded.df$Amount[neg.amounts.v]<- 
  paste("-", contribs.geocoded.df$Amount[neg.amounts.v], sep="")

contribs.geocoded.df$Amount<-as.numeric(contribs.geocoded.df$Amount)

contribs.geocoded.df$Amount[is.na(as.numeric(contribs.geocoded.df$Amount))]

contribs.geocoded.df<-merge(contribs.geocoded.df, 
  contribs.to.geocode.df[, c("geocode.id", "DC.geocoder", "USA.geocoder")], all=TRUE)


contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "address.clean"]<-
  contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "DC.geocoder.FULLADDRESS"]

contribs.geocoded.df[!is.na(contribs.geocoded.df$UNITNUMBER)], "address.clean"]<-
  paste(contribs.geocoded.df[!is.na(contribs.geocoded.df$UNITNUMBER)], "address.clean"],
    " No. ", contribs.geocoded.df[!is.na(contribs.geocoded.df$UNITNUMBER)], "UNITNUMBER"],
    sep="")

contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "zip.clean"]<-
  contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "DC.geocoder.ZIPCODE"]

contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "city.clean"]<-"Washington"
contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "state.clean"]<-"DC"




# DC.geocoded.df$returnDataset.diffgram.NewDataSet.Table1.FULLADDRESS[!is.na(DC.geocoded.df$returnDataset.diffgram.NewDataSet.Table1.ADDRNUMSUFFIX)]

# TO DO: Must do something with this: returnDataset.diffgram.NewDataSet.Table1.ADDRNUMSUFFIX


contribs.df<-contribs.geocoded.df

# DC.contribs.to.geocode.df[1:720, ][!is.na(DC.geocoded.df$returnDataset.diffgram.NewDataSet.Table1.ADDRNUMSUFFIX), ]

gsub("[()]", "", "(67878)")

                                       
Think about removing: PNumberFractional

library(lattice)

install.packages("rgl")
library(rgl)
attach(mtcars)
plot3d(wt, disp, mpg, col="red", size=3) 


