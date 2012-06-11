# Copyright (c) 2012 Data Committee of Occupy DC
# 
# Licensed under the MIT License:
# Permission is hereby granted, free of charge, to any person obtaining a copy of 
# this software and associated documentation files (the "Software"), to deal in 
# the Software without restriction, including without limitation the rights to 
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
# of the Software, and to permit persons to whom the Software is furnished to do 
# so, subject to the following conditions:
# 	
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# Contact: data at occupydc dot org

work.dir<-""
# Put your working directory here

code.dir<-""
# Put your code directory here. (i.e. where is this R script file?)

usc.api.key<-""
# Put your USC WebGIS API key here

email.addr<-""
# Your email address for the USC WebGIS low-credit alert



install.packages("sendmailR")
install.packages("stringr")
install.packages("XML")
install.packages("reshape")
install.packages("foreign")
install.packages("RecordLinkage")
install.packages("network")
install.packages("igraph")
install.packages("maptools")
install.packages("splancs")
install.packages("PBSmapping")
install.packages("sp")
install.packages("geosphere")
install.packages("shapefiles")
setRepositories(ind=1:2)
install.packages("rgdal")

library(sendmailR)
library(stringr)
library(XML)
library(reshape)
library(foreign)
library(RecordLinkage)
library(network)
library(igraph)
library(maptools)
library(splancs)
library(PBSmapping)
library(sp)
library(geosphere)
library(shapefiles)
library(rgdal)





download.file(
  "http://commondatastorage.googleapis.com/ckannet-storage/2012-05-01T132354/DC_campaign_contribs_99_part12.zip",
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

contribs.raw.df$contribution.id<-1:nrow(contribs.raw.df)


contribs.to.geocode.df<-contribs.raw.df[!duplicated(contribs.raw.df[, c("Address", "city", "state", "Zip")]), 
  c("Address", "city", "state", "Zip")]

contribs.to.geocode.df$geocode.id<-1:nrow(contribs.to.geocode.df)

contribs.raw.df<-merge(contribs.raw.df, contribs.to.geocode.df)

for ( i in 1:ncol(contribs.to.geocode.df)) {

  contribs.to.geocode.df[, i]<-gsub("(^\t)|(\t$)", "", contribs.to.geocode.df[, i])
  contribs.to.geocode.df[, i]<-gsub("\t", " ", contribs.to.geocode.df[, i])

}





cat("autoid\tAddr", paste(1:nrow(contribs.to.geocode.df), "\t", gsub(",", " ", contribs.to.geocode.df$Address), " ", contribs.to.geocode.df$city, ", ", contribs.to.geocode.df$state, " ", contribs.to.geocode.df$Zip, sep=""), file=paste(work.dir, "raw/temp perl input.txt", sep=""), sep="\n")

system(paste("perl ", "\"", code.dir, "parseAddr.pl\" ","\"", work.dir, "raw/temp perl input.txt\" \"", work.dir, "raw/temp perl output.txt\" tab", sep=""), ignore.stdout = TRUE, ignore.stderr = TRUE)

perl.parsed.df<-read.table(paste(work.dir, "raw/temp perl output.txt", sep=""), sep="\t", quote="", header=TRUE, comment.char = "", stringsAsFactors=FALSE)


perl.parsed.df$perl_parsed_combined_no_city<-
	paste(perl.parsed.df$perl_parsed_number, perl.parsed.df$perl_parsed_prefix, perl.parsed.df$perl_parsed_street, perl.parsed.df$perl_parsed_type, perl.parsed.df$perl_parsed_suffix, perl.parsed.df$perl_parsed_sec_unit_type, perl.parsed.df$perl_parsed_sec_unit_num)

perl.parsed.df$perl_parsed_combined_no_city<-gsub(" {2,}", " ", perl.parsed.df$perl_parsed_combined_no_city)
perl.parsed.df$perl_parsed_combined_no_city<-gsub("(^ ))|( $)", "", perl.parsed.df$perl_parsed_combined_no_city)


contribs.to.geocode.df<-cbind(contribs.to.geocode.df, perl.parsed.df[, !colnames(perl.parsed.df) %in% c("autoid", "Addr") ])




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
  
  if (i==nrow(DC.contribs.to.geocode.df)) {
    DC.geocode.output.ls<-DC.geocode.output.ls[lapply(DC.geocode.output.ls, length)!=0]
  }

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

DC.geocoded.df$DC.geocoder.ConfidenceLevel<-as.numeric(DC.geocoded.df$DC.geocoder.ConfidenceLevel)

contribs.to.geocode.df<-merge(contribs.to.geocode.df, DC.geocoded.df, all=TRUE)


contribs.to.geocode.df$DC.geocoder.address.clean<-contribs.to.geocode.df$DC.geocoder.FULLADDRESS

addr.suffix.v<-which(!is.na(contribs.to.geocode.df$DC.geocoder.ADDRNUMSUFFIX))


for ( i in length(addr.suffix.v):1) {
	
  if (grepl(paste("^[0-9]+ ", contribs.to.geocode.df$DC.geocoder.ADDRNUMSUFFIX[addr.suffix.v][i], sep=""),
    contribs.to.geocode.df$DC.geocoder.FULLADDRESS[addr.suffix.v][i]) ) {
    
    addr.suffix.v<-addr.suffix.v[-i]
    
  }
  
}

contribs.to.geocode.df$DC.geocoder.address.clean[addr.suffix.v]<-paste(
  sapply(strsplit( contribs.to.geocode.df$DC.geocoder.FULLADDRESS[addr.suffix.v], " "), 
    FUN=function(x) { x[1]} ),
  contribs.to.geocode.df$DC.geocoder.ADDRNUMSUFFIX[addr.suffix.v],
  sapply(strsplit( contribs.to.geocode.df$DC.geocoder.FULLADDRESS[addr.suffix.v], "^[0-9]+ "), 
    FUN=function(x) { x[2]} ),
  sep=" "
)

contribs.to.geocode.df[!is.na(contribs.to.geocode.df$DC.geocoder.UNITNUMBER) & is.na(contribs.to.geocode.df$DC.geocoder.address.clean), ][1, ]


contribs.to.geocode.df$DC.geocoder.address.clean[!is.na(contribs.to.geocode.df$DC.geocoder.UNITNUMBER) & !is.na(contribs.to.geocode.df$DC.geocoder.address.clean)]<-
	paste(contribs.to.geocode.df$DC.geocoder.address.clean[!is.na(contribs.to.geocode.df$DC.geocoder.UNITNUMBER) & !is.na(contribs.to.geocode.df$DC.geocoder.address.clean)],
  "No.",
  contribs.to.geocode.df$DC.geocoder.UNITNUMBER[!is.na(contribs.to.geocode.df$DC.geocoder.UNITNUMBER) & !is.na(contribs.to.geocode.df$DC.geocoder.address.clean)],
  sep=" " )

contribs.to.geocode.df$DC.geocoder.city.clean<-contribs.to.geocode.df$DC.geocoder.CITY
contribs.to.geocode.df$DC.geocoder.state.clean<-contribs.to.geocode.df$DC.geocoder.STATE
contribs.to.geocode.df$DC.geocoder.zip.clean<-contribs.to.geocode.df$DC.geocoder.ZIPCODE

contribs.to.geocode.df$USA.geocoder<-FALSE

contribs.to.geocode.df$USA.geocoder[!contribs.to.geocode.df$DC.geocoder]<-TRUE
contribs.to.geocode.df$USA.geocoder[!is.na(contribs.to.geocode.df$DC.geocoder.ConfidenceLevel) &
  contribs.to.geocode.df$DC.geocoder.ConfidenceLevel<80]<-TRUE
contribs.to.geocode.df$USA.geocoder[is.na(contribs.to.geocode.df$DC.geocoder.ConfidenceLevel)]<-TRUE


USA.contribs.to.geocode.df<-contribs.to.geocode.df[contribs.to.geocode.df$USA.geocoder, ]

#Keep?
#returnCDDataSet.diffgram.NewDataSet.Address_x0020_Return_x0020_Codes.Parsed_x0020__x0026__x0020_Normalized



geocode.output.ls<-vector("list", length=nrow(USA.contribs.to.geocode.df))




USA.contribs.to.geocode.df$api.url<-paste(
  "http://webgis.usc.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsedAdvanced_V02_96.aspx?streetAddress=",
  URLencode.vec(USA.contribs.to.geocode.df$perl_parsed_combined_no_city, reserved = TRUE),
  "&city=",
  URLencode.vec(USA.contribs.to.geocode.df$city, reserved = TRUE),
  "&state=",
  URLencode.vec(USA.contribs.to.geocode.df$state, reserved = TRUE),
  "&zip=",
  URLencode.vec(USA.contribs.to.geocode.df$Zip, reserved = TRUE),
  "&apikey=", 
  usc.api.key, "&format=tsv&census=true&censusYear=2010&notStore=false&verbose=true&h=u&geom=false&version=2.96",
  sep=""
  )



credits<-2000

for ( i in 1:nrow(USA.contribs.to.geocode.df)) {
  
  geocode.output.ls[[i]]<-
    tryCatch(cbind(data.frame(
      geocode.id=USA.contribs.to.geocode.df$geocode.id[i], stringsAsFactors=FALSE),
      read.delim(USA.contribs.to.geocode.df$api.url[i], header=FALSE, stringsAsFactors=FALSE)),
             error = function(e) { "Error retrieving geocode" })
  
  cat(i , "of", nrow(USA.contribs.to.geocode.df), date(), "\n")
  flush.console()
  
  credits <- credits - 1
  
  if (check.integer(i/50)) {
    
    credits<- tryCatch(
      read.table(paste("http://webgis.usc.edu/UserServices/Payments/AccountBalanceWebServiceHttp.aspx?version=1.0&apikey=", usc.api.key, "&format=csv", sep=""), stringsAsFactors=FALSE, sep=",")[, 2],
      error = function(e) { credits })
    
    junk.v<-tryCatch(save(geocode.output.ls, file=paste(work.dir, "Geocode output list.Rdata")),
                     error = function(e) { "Error" })
    
  }
  
  if (credits<=10) {
    
    from <- sprintf("<sendmailR@%s>", Sys.info()[4])
    to <- email.addr
    # email address above
    subject <- "ADD MORE WEBGIS CREDITS"
    
    junk.v<-tryCatch( sendmail(from, to, subject, paste("Hi, \n\nYou need to add more credits to WebGIS"),
                               control=list(smtpServer="ASPMX.L.GOOGLE.COM")), error = function(e) { "Error" })
    
    while (credits<=10) {
      
      Sys.sleep(10)
      
      credits<- tryCatch(
        read.table(paste("http://webgis.usc.edu/UserServices/Payments/AccountBalanceWebServiceHttp.aspx?version=1.0&apikey=", usc.api.key, "&format=csv" sep=""), stringsAsFactors=FALSE, sep=",")[, 2],
        error = function(e) { credits })
      
    }
    
  }
  
}


# geocode.output.ls<-geocode.output.ls[1:31441]
successful.geocode.v<-sapply(geocode.output.ls, FUN=function(x) {
  if(is.data.frame(x)) {
    ret<-ncol(x)==123
  } else {
    ret<-FALSE
  } 
  ret})

# geocode.output.ls<-lapply(geocode.output.ls, FUN=function(x) { if (is.data.frame(x) && ncol(x)==124) {x[, 1]<-as.character(x[, 1])}; x})

geocode.output.mat<-matrix(unlist(geocode.output.ls[successful.geocode.v]), ncol=123, byrow=TRUE)
# geocode.output.mat<-geocode.output.mat[, -2]
USA.geocode.output.df<-as.data.frame(geocode.output.mat, stringsAsFactors=FALSE)

colnames(USA.geocode.output.df)<-c("geocode.id", "Transaction.Id", "API.Version", "Query.Status.Code", "Latitude", "Longitude", "Match.Score", "Match.Type", "Matching.Geography.Type", "Interpolation.Type", "Interpolation.Sub.Type", "Matched.Location.Type", "Feature.Matching.Result.Type", "FeatureMatchingResultCount", "FeatureMatchingResultTypeNotes", "TieHandlingStrategyType", "FeatureMatchingResultTypeTieBreakingNotes", "FeatureMatchingSelectionMethod", "FeatureMatchingSelectionMethodNotes", "Time.Taken", "Census.Year", "Census.Block", "Census.Block.Group", "Census.Tract", "Census.County.Fips", "Census.CBSA.Fips", "Census.CBSA.Micro", "Census.MCD.Fips", "Census.MetDiv.Fips", "Census.MSA.Fips", "Census.Place.Fips", "Census.State.Fips", "MNumber", "MNumberFractional", "MPreDirectional", "MPreQualifier", "MPreType", "MPreArticle", "MName", "MPostArticle", "MPostQualifier", "MSuffix", "MPostDirectional", "MSuiteType", "MSuiteNumber", "MPostOfficeBoxType", "MPostOfficeBoxNumber", "MCity", "MConsolidatedCity", "MMinorCivilDivision", "MCountySubRegion", "MCounty", "MState", "MZip", "MZipPlus1", "MZipPlus2", "MZipPlus3", "MZipPlus4", "MZipPlus5", "PNumber", "PNumberFractional", "PPreDirectional", "PPreQualifier", "PPreType", "PPreArticle", "PName", "PPostArticle", "PPostQualifier", "PSuffix", "PPostDirectional", "PSuiteType", "PSuiteNumber", "PPostOfficeBoxType", "PPostOfficeBoxNumber", "PCity", "PConsolidatedCity", "PMinorCivilDivision", "PCountySubRegion", "PCounty", "PState", "PZip", "PZipPlus1", "PZipPlus2", "PZipPlus3", "PZipPlus4", "PZipPlus5", "FNumber", "FNumberFractional", "FPreDirectional", "FPreQualifier", "FPreType", "FPreArticle", "FName", "FPostArticle", "FPostQualifier", "FSuffix", "FPostDirectional", "FSuiteType", "FSuiteNumber", "FPostOfficeBoxType", "FPostOfficeBoxNumber", "FCity", "FConsolidatedCity", "FMinorCivilDivision", "FCountySubRegion", "FCounty", "FState", "FZip", "FZipPlus1", "FZipPlus2", "FZipPlus3", "FZipPlus4", "FZipPlus5", "FArea", "FAreaType", "FGeometrySRID", "FGeometry", "FSource", "FVintage", "FPrimaryIdField", "FPrimaryIdValue", "FSecondaryIdField", "FSecondaryIdValue")

colnames(USA.geocode.output.df)<-paste("USA.geocoder.", colnames(USA.geocode.output.df), sep="")

colnames(USA.geocode.output.df)[colnames(USA.geocode.output.df)=="USA.geocoder.geocode.id"]<-"geocode.id"


USA.geocode.output.df<-USA.geocode.output.df[, 
  apply(USA.geocode.output.df, MARGIN=2, FUN=function(x) {!all(is.na(x))})
  ]




contribs.geocoded.df<-merge(contribs.raw.df, USA.geocode.output.df, all=TRUE)

#contribs.geocoded.df<-geocode.output.df

contribs.geocoded.df$USA.geocoder.geocode.success<-contribs.geocoded.df$USA.geocoder.Feature.Matching.Result.Type=="Success"
contribs.geocoded.df$USA.geocoder.geocode.exact<-contribs.geocoded.df$USA.geocoder.Match.Type=="Exact"

contribs.geocoded.df$USA.geocoder.street.precise<-contribs.geocoded.df$USA.geocoder.Matching.Geography.Type %in% 
  c("Parcel", "StreetSegment", "USPSZipPlus4")


contribs.geocoded.df$USA.geocoder.PSuiteNumber.cleaned<-gsub("[^0-9]", "", contribs.geocoded.df$USA.geocoder.PSuiteNumber)

address.cols<- c("USA.geocoder.PNumber", "USA.geocoder.PNumberFractional", "USA.geocoder.FPreDirectional", "USA.geocoder.FPreQualifier", "USA.geocoder.FName", "USA.geocoder.FPostQualifier", "USA.geocoder.FPostQualifier", "USA.geocoder.FSuffix", "USA.geocoder.FPostDirectional", "USA.geocoder.PSuiteNumber.cleaned")

for (i in address.cols) {
  contribs.geocoded.df[is.na(contribs.geocoded.df[, i]), i]<-"empty.remove.indicator"
}

contribs.geocoded.df$USA.geocoder.address.clean<-paste(
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

contribs.geocoded.df$USA.geocoder.address.clean<-gsub("No[.] empty[.]remove[.]indicator", "", contribs.geocoded.df$USA.geocoder.address.clean )

contribs.geocoded.df$USA.geocoder.address.clean<-gsub("empty[.]remove[.]indicator", "", contribs.geocoded.df$USA.geocoder.address.clean )

contribs.geocoded.df$USA.geocoder.address.clean<-gsub("( ){2,}", " ",  contribs.geocoded.df$USA.geocoder.address.clean )

contribs.geocoded.df$USA.geocoder.address.clean<-gsub("( ){2,}", " ",  contribs.geocoded.df$USA.geocoder.address.clean )

contribs.geocoded.df$USA.geocoder.address.clean<-gsub("(^ *)|( *$)", "", contribs.geocoded.df$USA.geocoder.address.clean )

contribs.geocoded.df$USA.geocoder.PNumber[is.na(contribs.geocoded.df$USA.geocoder.PNumber)]<-"empty.remove.indicator"
contribs.geocoded.df$USA.geocoder.PNumberFractional[is.na(contribs.geocoded.df$USA.geocoder.PNumberFractional)]<-"empty.remove.indicator"

for (i in address.cols) {
  contribs.geocoded.df[is.na(contribs.geocoded.df[, i]) | 
    contribs.geocoded.df[, i]=="empty.remove.indicator", i]<-NA
}





# Take the first of these that is not NA: FCity  FCountySubRegion  FCounty

contribs.geocoded.df$USA.geocoder.city.clean[is.na(contribs.geocoded.df$USA.geocoder.FCountySubRegion) & is.na(contribs.geocoded.df$USA.geocoder.FCity)]<-
  contribs.geocoded.df$USA.geocoder.FCounty[is.na(contribs.geocoded.df$USA.geocoder.FCountySubRegion) & is.na(contribs.geocoded.df$USA.geocoder.FCity)]

contribs.geocoded.df$USA.geocoder.city.clean[is.na(contribs.geocoded.df$USA.geocoder.city.clean) & is.na(contribs.geocoded.df$USA.geocoder.FCity)]<-
  contribs.geocoded.df$USA.geocoder.FCountySubRegion[is.na(contribs.geocoded.df$USA.geocoder.city.clean) & is.na(contribs.geocoded.df$USA.geocoder.FCity)]

contribs.geocoded.df$USA.geocoder.city.clean[is.na(contribs.geocoded.df$USA.geocoder.city.clean)]<-
  contribs.geocoded.df$USA.geocoder.FCity[is.na(contribs.geocoded.df$USA.geocoder.city.clean)]

contribs.geocoded.df$USA.geocoder.state.clean<-contribs.geocoded.df$USA.geocoder.FState

contribs.geocoded.df$USA.geocoder.zip.clean<-contribs.geocoded.df$USA.geocoder.FZipPlus4
contribs.geocoded.df$USA.geocoder.zip.clean[is.na(contribs.geocoded.df$USA.geocoder.zip.clean)]<-contribs.geocoded.df$USA.geocoder.FZip[is.na(contribs.geocoded.df$USA.geocoder.zip.clean)]

contribs.geocoded.df$Date.of.Receipt<-as.Date(contribs.geocoded.df$Date.of.Receipt, format="%m/%d/%Y")
contribs.geocoded.df$Amount<-gsub("([$]|(,))", "", contribs.geocoded.df$Amount)
neg.amounts.v<-grepl("^[(].*[)]$", contribs.geocoded.df$Amount)

contribs.geocoded.df$Amount[neg.amounts.v]<-
  gsub("[()]", "", contribs.geocoded.df$Amount[neg.amounts.v])

contribs.geocoded.df$Amount[neg.amounts.v]<- 
  paste("-", contribs.geocoded.df$Amount[neg.amounts.v], sep="")

contribs.geocoded.df$Amount<-as.numeric(contribs.geocoded.df$Amount)

# contribs.geocoded.df$Amount[is.na(as.numeric(contribs.geocoded.df$Amount))]

contribs.geocoded.df<-merge(contribs.geocoded.df, 
  contribs.to.geocode.df[, !colnames(contribs.to.geocode.df) %in% c("Address", "city", "state", "Zip")], all=TRUE)

contribs.geocoded.df$address.clean<-contribs.geocoded.df$USA.geocoder.address.clean
contribs.geocoded.df$city.clean<-contribs.geocoded.df$USA.geocoder.city.clean
contribs.geocoded.df$state.clean<-contribs.geocoded.df$USA.geocoder.state.clean
contribs.geocoded.df$zip.clean<-contribs.geocoded.df$USA.geocoder.zip.clean

contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "address.clean"]<-
  contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "DC.geocoder.address.clean"]

contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "zip.clean"]<-
  contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "DC.geocoder.zip.clean"]

contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "city.clean"]<-"Washington"
contribs.geocoded.df[!contribs.geocoded.df$USA.geocoder, "state.clean"]<-"DC"

dim(contribs.geocoded.df)[1]==dim(contribs.raw.df)[1]

contribs.df<-contribs.geocoded.df

rm(contribs.geocoded.df)

contribs.df$address.is.unclean<-!contribs.df$USA.geocoder.street.precise & contribs.df$USA.geocoder


contribs.df$address.clean[!contribs.df$USA.geocoder.street.precise & contribs.df$USA.geocoder]<-
	contribs.df$perl_parsed_combined_no_city[!contribs.df$USA.geocoder.street.precise & contribs.df$USA.geocoder]

contribs.df$city.is.unclean<-is.na(contribs.df$city.clean)
contribs.df$state.is.unclean<-is.na(contribs.df$state.clean)
contribs.df$zip.is.unclean<-is.na(contribs.df$zip.clean)

contribs.df$city.clean[is.na(contribs.df$city.clean)]<-contribs.df$city[is.na(contribs.df$city.clean)]
contribs.df$state.clean[is.na(contribs.df$state.clean)]<-contribs.df$perl_parsed_state[is.na(contribs.df$state.clean)]
contribs.df$zip.clean[is.na(contribs.df$zip.clean)]<-contribs.df$Zip[is.na(contribs.df$zip.clean)]

contribs.df$address.clean<-gsub("(^ +)|( +$)", "", contribs.df$address.clean)

unit.nums.to.fix.v<-str_extract(contribs.df$address.clean, " [^ ]+ [0-9]+-?[A-Za-z]?$")
no.units.index<-is.na(unit.nums.to.fix.v)
unit.nums.to.fix.v<-unit.nums.to.fix.v[!no.units.index]

unit.nums.to.fix.v<-gsub("( # )|( APARTMENT )|( APT )|( DEPT )|( FL )|( FLOOR )|( NO )|( STE )|( SUITE )|( UNIT )", " No. ", unit.nums.to.fix.v)


contribs.df$address.clean<-gsub(" [^ ]+ [0-9]+-?[A-Za-z]?$", "", contribs.df$address.clean)

contribs.df$address.clean[!no.units.index]<-paste(contribs.df$address.clean[!no.units.index],
  unit.nums.to.fix.v, sep="")

simple.cap <- function(x) {
	s <- strsplit(x, " ")[[1]]
	paste(toupper(substring(s, 1,1)), substring(s, 2),
				sep="", collapse=" ")
}

simple.cap<-Vectorize(simple.cap, USE.NAMES=FALSE)
contribs.df$address.clean<-simple.cap(tolower(contribs.df$address.clean))
contribs.df$city.clean<-simple.cap(tolower(contribs.df$city.clean))

contribs.df$address.clean<-gsub(" Nw$", " NW", contribs.df$address.clean)
contribs.df$address.clean<-gsub(" Ne$", " NE", contribs.df$address.clean)
contribs.df$address.clean<-gsub(" Sw$", " SW", contribs.df$address.clean)
contribs.df$address.clean<-gsub(" Se$", " SE", contribs.df$address.clean)
contribs.df$address.clean<-gsub(" Nw ", " NW ", contribs.df$address.clean)
contribs.df$address.clean<-gsub(" Ne ", " NE ", contribs.df$address.clean)
contribs.df$address.clean<-gsub(" Sw ", " SW ", contribs.df$address.clean)
contribs.df$address.clean<-gsub(" Se ", " SE ", contribs.df$address.clean)
contribs.df$address.clean<-gsub(" Po B", " PO B", contribs.df$address.clean)

contribs.df$address.no.unit.clean<-gsub(" No[.] [0-9]+[A-Za-z]?$", "", contribs.df$address.clean)



contribs.df$address.evidence.multiunit.building<-FALSE

inconsist.no.unit.v<-contribs.df$address.no.unit.clean[contribs.df$address.no.unit.clean!=contribs.df$address.clean]

contribs.df$address.evidence.multiunit.building[
	contribs.df$address.no.unit.clean %in% unique(inconsist.no.unit.v)]<-TRUE


contribs.df$latitude.consolidated<-NA
contribs.df$longitude.consolidated<-NA

contribs.df$latitude.consolidated[!contribs.df$USA.geocoder]<-
  contribs.df$DC.geocoder.LATITUDE[!contribs.df$USA.geocoder]
contribs.df$longitude.consolidated[!contribs.df$USA.geocoder]<-
  contribs.df$DC.geocoder.LONGITUDE[!contribs.df$USA.geocoder]

contribs.df$latitude.consolidated[contribs.df$USA.geocoder]<-
  contribs.df$USA.geocoder.Latitude[contribs.df$USA.geocoder]
contribs.df$longitude.consolidated[contribs.df$USA.geocoder]<-
  contribs.df$USA.geocoder.Longitude[contribs.df$USA.geocoder]

contribs.df$latitude.consolidated[contribs.df$latitude.consolidated==0]<-NA
contribs.df$longitude.consolidated[contribs.df$longitude.consolidated==0]<-NA

# output.cols.v<-c("Committee.Name", "Contributor", "Contribution.Type", "Amount", "Date.of.Receipt", "address.clean", "city.clean", "state.clean", "zip.clean", "latitude.consolidated", "longitude.consolidated")

#write.csv(contribs.df[, output.cols.v], file=paste(work.dir, "DC campaign contributions alpha version.csv", sep=""))

#save(contribs.df, file=paste(work.dir, "Geocoded contribs df.Rdata", sep=""))

#########################
#########################
#########################


contribs.df$contribution.type.clean<-contribs.df$Contribution.Type

contribs.df$contribution.type.clean[contribs.df$contribution.type.clean=="Corp"]<-"Corporation"
contribs.df$contribution.type.clean[contribs.df$contribution.type.clean=="Business"]<-"Corporation"
contribs.df$contribution.type.clean[contribs.df$contribution.type.clean=="Individual "]<-"Individual"
contribs.df$contribution.type.clean[contribs.df$contribution.type.clean=="individual"]<-"Individual"
contribs.df$contribution.type.clean[contribs.df$contribution.type.clean=="Labor Org"]<-"Labor"
contribs.df$contribution.type.clean[contribs.df$contribution.type.clean %in% 
	c("", "Organization", "PCC", "Republican PPC", "Democratic PPC", "Statehood Green Party PPC")]<-"Other"
contribs.df$contribution.type.clean[contribs.df$contribution.type.clean %in% 
	c("Labor Sponsored PAC", "PAC", "Corporate Sponsored PAC")]<-"Political Action Committee"
contribs.df$contribution.type.clean[contribs.df$contribution.type.clean =="Partnership"]<-"Business Partnership"
contribs.df$contribution.type.clean[contribs.df$contribution.type.clean =="Labor"]<-"Labor Union"

contribs.df$contributor.clean<-contribs.df$Contributor

contribs.df$contributor.clean<-gsub(",( )*,", ",", contribs.df$contributor.clean)
contribs.df$contributor.clean<-gsub("( ){2,}", " ", contribs.df$contributor.clean)
contribs.df$contributor.clean<-gsub("(\t+)|(\n+)", " ", contribs.df$contributor.clean)
contribs.df$contributor.clean<-gsub("(^ +)|( +$)", "", contribs.df$contributor.clean)
contribs.df$contributor.clean<-gsub(" [Ii][Nn][Cc]$", " Inc.", contribs.df$contributor.clean)







#######################
# BEGIN INCONSISTENT NAME FIXER
#######################

# input should be: contribs.df[, c("address.clean", "contributor.clean", "contribution.id")]



#test.df<-data.frame(incorrect=c("az", "aa", "bz", "bb"), correct=c("aa", "aa", "bb", "bb"), stringsAsFactors=FALSE)

#test.v<-sample(c("az", "aa", "bz", "bb"))
#test.v
#test.v[match( test.df[, 1], test.v)]<-test.df[, 2]
#test.v

source(paste(code.dir, "fix-inconsistent-names.r", sep=""))

all.contrib.types<-unique(contribs.df$contribution.type.clean)
all.contrib.types<-all.contrib.types[!all.contrib.types %in% c("Corporation", "Partnership")]

for ( target.type in all.contrib.types) {
	
	fixed.temp.df<-fix.inconsistent.contrib.names(
	  contribs.df[contribs.df$contribution.type.clean==target.type, 
	    c("address.clean", "contributor.clean", "contribution.id")])

	contribs.df$contributor.clean[
	  match(fixed.temp.df$contribution.id, contribs.df$contribution.id)]<-
	  	fixed.temp.df$contributor.replacement

}

fixed.temp.df<-fix.inconsistent.contrib.names(
	contribs.df[contribs.df$contribution.type.clean %in% c("Corporation", "Partnership"), 
	  c("address.clean", "contributor.clean", "contribution.id")], corps=TRUE)

contribs.df$contributor.clean[
	match(fixed.temp.df$contribution.id, contribs.df$contribution.id)]<-
		fixed.temp.df$contributor.replacement

#for (name.part in c("contributor.first.name", "contributor.last.name", "contributor.spouse.2", "contributor.spouse.1")) {

#  fixed.temp.df<-fix.inconsistent.contrib.names(
#  	contribs.df[contribs.df$Contribution.Type=="Individual" & !is.na(contribs.df[, name.part]), 
#  	  c("address.clean", name.part, "contribution.id")])

#  contribs.df[match(fixed.temp.df$contribution.id, contribs.df$contribution.id), name.part]<-
#		fixed.temp.df$contributor.replacement

#}



#######################
# END INCONSISTENT NAME FIXER
#######################


indiv.sep.ls<-strsplit(contribs.df$contributor.clean, ",")

contribs.df$contributor.first.name<-NA
contribs.df$contributor.last.name<-NA

contribs.df$contributor.first.name[contribs.df$contribution.type.clean=="Individual" & grepl( ",", contribs.df$contributor.clean) & sapply(indiv.sep.ls, FUN=length)==2]<-
	sapply(indiv.sep.ls[contribs.df$contribution.type.clean=="Individual" & grepl( ",", contribs.df$contributor.clean) & sapply(indiv.sep.ls, FUN=length)==2], FUN=function(x) {x[2]})

contribs.df$contributor.last.name[contribs.df$contribution.type.clean=="Individual" & grepl( ",", contribs.df$contributor.clean) & sapply(indiv.sep.ls, FUN=length)==2]<-
	sapply(indiv.sep.ls[contribs.df$contribution.type.clean=="Individual" & grepl( ",", contribs.df$contributor.clean) & sapply(indiv.sep.ls, FUN=length)==2], FUN=function(x) {x[1]})

contribs.df$contributor.spouse.1<-NA
contribs.df$contributor.spouse.2<-NA


indiv.sep.ls<-strsplit(contribs.df$contributor.first.name, "(&)|( [aA][nN][dD] )")

contribs.df$contributor.spouse.1[contribs.df$contribution.type.clean=="Individual" & grepl( "(&)|( [aA][nN][dD] )", contribs.df$contributor.clean) & sapply(indiv.sep.ls, FUN=length)==2]<-
	sapply(indiv.sep.ls[contribs.df$contribution.type.clean=="Individual" & grepl( "(&)|( [aA][nN][dD] )", contribs.df$contributor.clean) & sapply(indiv.sep.ls, FUN=length)==2], FUN=function(x) {x[1]})

contribs.df$contributor.spouse.2[contribs.df$contribution.type.clean=="Individual" & grepl( "(&)|( [aA][nN][dD] )", contribs.df$contributor.clean) & sapply(indiv.sep.ls, FUN=length)==2]<-
	sapply(indiv.sep.ls[contribs.df$contribution.type.clean=="Individual" & grepl( "(&)|( [aA][nN][dD] )", contribs.df$contributor.clean) & sapply(indiv.sep.ls, FUN=length)==2], FUN=function(x) {x[1]})

contribs.df$contributor.first.name<-gsub("(^ +)|( +$)", "", contribs.df$contributor.first.name)
contribs.df$contributor.last.name<-gsub("(^ +)|( +$)", "", contribs.df$contributor.last.name)
contribs.df$contributor.spouse.1<-gsub("(^ +)|( +$)", "", contribs.df$contributor.spouse.1)
contribs.df$contributor.spouse.2<-gsub("(^ +)|( +$)", "", contribs.df$contributor.spouse.2)




system(paste("cd \"", code.dir, "\"\n", "perl scrape-committees.pl",  sep=""))

file.copy(
	from=paste(code.dir,"committees.tsv", sep=""), 
  to=paste(work.dir,"DC candidate committees.tsv", sep=""),
	overwrite=TRUE
)

unlink(paste(code.dir,"committees.tsv", sep=""))
unlink(paste(code.dir,"lwp_cookies.txt", sep=""))



committees.df<-read.delim(paste(work.dir,"DC candidate committees.tsv", sep=""), stringsAsFactors=FALSE)

colnames(committees.df)[colnames(committees.df)=="committee"]<-"Committee.Name"
colnames(committees.df)[colnames(committees.df)=="office"]<-"electoral.office"

committees.df$Committee.Name<-gsub("(^ +)|( +$)", "", committees.df$Committee.Name)


contribs.df$Committee.Name<-gsub("(^ +)|( +$)", "", contribs.df$Committee.Name)

contribs.df$Committee.Name[contribs.df$Committee.Name=="Re-Elect Jim Graham  (2006)"]<-
	"Re-Elect Jim Graham (2006)"
committees.df$Committee.Name[committees.df$Committee.Name=="Committee to Elect Edward Donnie James"]<-
	"Committee to Elect Edward \"Donnie\" James"

committees.df$Committee.Name[committees.df$Committee.Name=="SS for Ward 8 City Council"]<-
	"\"SS\" for Ward 8 City Council"

committees.df$Committee.Name[committees.df$Committee.Name=="A Lot of People Supporting Erik Gaull for Ward 3 C"]<-
	"A Lot of People Supporting Erik Gaull for Ward 3 Council"

contribs.df$Committee.Name[contribs.df$Committee.Name=="A Lot of People Supporting Erik Gaull for Ward 3 C"]<-
	"A Lot of People Supporting Erik Gaull for Ward 3 Council"

committees.df$Committee.Name[committees.df$Committee.Name=="Committee to Re-elect Tom Wells for School Board-D"]<-
	"Committee to Re-elect Tom Wells for School Board-Dist 3"

contribs.df$Committee.Name[contribs.df$Committee.Name=="Committee to Re-elect Tom Wells for School Board-D"]<-
	"Committee to Re-elect Tom Wells for School Board-Dist 3"

contribs.df$Committee.Name[contribs.df$Committee.Name=="Committee to Re-elect Tony Williams"]<-
	"Committee to Re-Elect Tony Williams"

contribs.df$Committee.Name[contribs.df$Committee.Name=="Committee to Elect  Renee Bowser to Council Ward 4"]<-
	"Committee to Elect Renee Bowser to Council Ward 4"





committees.df[committees.df$Committee.Name=="Committee to Elect Cardell Shelton", ]

committees.same.name.df<-committees.df[duplicated(committees.df$Committee.Name) | duplicated(committees.df$Committee.Name, fromLast=TRUE),]

committees.same.name.df<-committees.same.name.df[!duplicated(committees.same.name.df[, c("Committee.Name", "year")], fromLast=TRUE) & 
  !duplicated(committees.same.name.df[, c("Committee.Name", "year")]), ]

#contribs.save.df<-contribs.df



for ( i in 1:nrow(committees.same.name.df)) {
	
	targ.yr<-committees.same.name.df$year[i]
	
	if (committees.same.name.df$Committee.Name[i]!="Friends of Calvin Gurley") {
	  targ.yr.set<-c(targ.yr-1, targ.yr)
	} else {
	  targ.yr.set<-targ.yr
	}

  contribs.df$Committee.Name[
    substr(contribs.df$Date.of.Receipt, 1, 4) %in% targ.yr.set &
    contribs.df$Committee.Name==committees.same.name.df$Committee.Name[i]
    ] <- paste(committees.same.name.df$Committee.Name[i], " [", targ.yr, "]", sep="")

}

# unique(contribs.df$Committee.Name)[grepl("[[]", unique(contribs.df$Committee.Name))]

committees.df<-committees.df[!committees.df$Committee.Name %in% committees.same.name.df$Committee.Name, ]

committees.same.name.df$Committee.Name<-paste(committees.same.name.df$Committee.Name,
  " [", committees.same.name.df$year, "]", sep="")

committees.df<-rbind(committees.df, committees.same.name.df)


#committees.df<-committees.df[!duplicated(committees.df[, c("Committee.Name", "electoral.office")]), ]

committees.df<-committees.df[!duplicated(committees.df$Committee.Name), ]

contribs.df<-merge(contribs.df, committees.df[, c("Committee.Name", "candidate", "electoral.office", "year", "party", "organization_date")], all.x=TRUE)

colnames(contribs.df)[colnames(contribs.df)=="year"]<-"election.cycle"
colnames(contribs.df)[colnames(contribs.df)=="party"]<-"candidate.party"
colnames(contribs.df)[colnames(contribs.df)=="organization_date"]<-"candidate.committee.organization.date"

contribs.df$electoral.office[is.na(contribs.df$electoral.office)]<-"DATA UNAVAILABLE"
contribs.df$max.legal.contrib<-NA


contribs.df$max.legal.contrib[grepl("(Council Ward)|(School Board President)|(School Board At-Large)", contribs.df$electoral.office)]<-500
contribs.df$max.legal.contrib[grepl("Council At-Large", contribs.df$electoral.office)]<-1000
contribs.df$max.legal.contrib[grepl("Council Chairman", contribs.df$electoral.office)]<-1500
contribs.df$max.legal.contrib[grepl("(Mayor)|(US Senator)|(US Representative)|(DATA UNAVAILABLE)", contribs.df$electoral.office)]<-2000
contribs.df$max.legal.contrib[grepl("(DC Democratic State Committee)|(School Board Ward)|(School Board District)|(National Committee)|(Delegates)", contribs.df$electoral.office)]<-300



read.dbf <-  foreign::read.dbf


download.file(
	"http://dcatlas.dcgis.dc.gov/download/WardPly.zip",
	paste(work.dir, "WardPly.zip", sep="")
	)
# http://data.dc.gov/Main_DataCatalog.aspx?id=2860

unzip( paste(work.dir, "WardPly.zip", sep=""), exdir=paste(work.dir, "raw/", sep="") )


ward.12.ogr<-readOGR(paste(work.dir, "raw/WardPly.shp", sep=""), layer="WardPly")


ward.12.shps<-importShapefile(paste(work.dir, "raw/WardPly.shp", sep=""))
coords.to.convert<-as.data.frame(ward.12.shps[, c("X", "Y")])
coordinates(coords.to.convert)<-~ X + Y 
proj4string(coords.to.convert)<-ward.12.ogr@proj4string
coords.to.convert<-spTransform(coords.to.convert, CRS("+proj=longlat +datum=WGS84"))
ward.12.shps[, c("X", "Y")]<-coords.to.convert@coords

contribs.event<-contribs.df[!duplicated(contribs.df$geocode.id) & 
	!is.na(contribs.df$longitude.consolidated) & (contribs.df$election.cycle==2012 & !is.na(contribs.df$election.cycle)), c("geocode.id", "longitude.consolidated", "latitude.consolidated")]
colnames(contribs.event)<-c("EID", "X", "Y")
contribs.event$X<-as.numeric(contribs.event$X)
contribs.event$Y<-as.numeric(contribs.event$Y)
contribs.event<-as.EventData(contribs.event)

contribs.in.wards.2012<-findPolys(contribs.event, ward.12.shps)



download.file(
	"http://dcatlas.dcgis.dc.gov/download/Ward02Ply.zip",
	paste(work.dir, "Ward02Ply.zip", sep="")
	)
# See http://data.dc.gov/Metadata.aspx?id=126

unzip( paste(work.dir, "Ward02Ply.zip", sep=""), exdir=paste(work.dir, "raw/", sep="") )

ward.02.ogr<-readOGR(paste(work.dir, "raw/Ward02Ply.shp", sep=""), layer="Ward02Ply")

ward.02.shps<-importShapefile(paste(work.dir, "raw/Ward02Ply.shp", sep=""))
coords.to.convert<-as.data.frame(ward.02.shps[, c("X", "Y")])
coordinates(coords.to.convert)<-~ X + Y 
proj4string(coords.to.convert)<-ward.02.ogr@proj4string
coords.to.convert<-spTransform(coords.to.convert, CRS("+proj=longlat +datum=WGS84"))
ward.02.shps[, c("X", "Y")]<-coords.to.convert@coords

contribs.event<-contribs.df[!duplicated(contribs.df$geocode.id) & 
	!is.na(contribs.df$longitude.consolidated) & (contribs.df$election.cycle!=2012 | is.na(contribs.df$election.cycle)), 
	c("geocode.id", "longitude.consolidated", "latitude.consolidated")]
colnames(contribs.event)<-c("EID", "X", "Y")
contribs.event$X<-as.numeric(contribs.event$X)
contribs.event$Y<-as.numeric(contribs.event$Y)
contribs.event<-as.EventData(contribs.event)

contribs.in.wards.2002<-findPolys(contribs.event, ward.02.shps)

contribs.matched.ward.df<-data.frame(
	geocode.id=c(contribs.in.wards.2012$EID, contribs.in.wards.2002$EID),
	contributor.ward=c(
	  as.character(attr(ward.12.shps, "PolyData")$NAME[contribs.in.wards.2012$PID]),
	  as.character(attr(ward.02.shps, "PolyData")$NAME[contribs.in.wards.2002$PID])),
	stringsAsFactors=FALSE
	)

contribs.matched.ward.df$contributor.ward<-
	gsub("(Ward )|( [(][0-9]*[)])", "", contribs.matched.ward.df$contributor.ward)

contribs.df<-merge(contribs.df, contribs.matched.ward.df, all.x=TRUE)

contribs.df$contributor.ward[is.na(contribs.df$contributor.ward)]<-"Non-DC"




contribs.df$recipient.ward<-NA

contribs.df$recipient.ward[grepl("[Ww]ard[[:space:]]*1", contribs.df$electoral.office)] <-"1"
contribs.df$recipient.ward[grepl("[Ww]ard[[:space:]]*2", contribs.df$electoral.office)] <-"2"
contribs.df$recipient.ward[grepl("[Ww]ard[[:space:]]*3", contribs.df$electoral.office)] <-"3"
contribs.df$recipient.ward[grepl("[Ww]ard[[:space:]]*4", contribs.df$electoral.office)] <-"4"
contribs.df$recipient.ward[grepl("[Ww]ard[[:space:]]*5", contribs.df$electoral.office)] <-"5"
contribs.df$recipient.ward[grepl("[Ww]ard[[:space:]]*6", contribs.df$electoral.office)] <-"6"
contribs.df$recipient.ward[grepl("[Ww]ard[[:space:]]*7", contribs.df$electoral.office)] <-"7"
contribs.df$recipient.ward[grepl("[Ww]ard[[:space:]]*8", contribs.df$electoral.office)] <-"8"

contribs.df$recipient.ward[is.na(contribs.df$recipient.ward)]<-"Citywide"

contribs.df$contributor.recipient.same.geo<-NA

contribs.df$contributor.recipient.same.geo<-contribs.df$recipient.ward==contribs.df$contributor.ward

contribs.df$contributor.recipient.same.geo[contribs.df$contributor.ward!="Non-DC" &
	contribs.df$recipient.ward=="Citywide"]<-TRUE





download.file("http://dcatlas.dcgis.dc.gov/download/CamaCommPt.ZIP", 
							paste(work.dir, "raw/CamaCommPt.ZIP", sep="") )
# See http://data.dc.gov/Metadata.aspx?id=144

unzip( paste(work.dir, "raw/CamaCommPt.ZIP", sep=""), exdir=paste(work.dir, "raw/", sep="") )

cama.comm.df<-read.dbf(paste(work.dir, "raw/CamaCommPt.dbf", sep=""), as.is=TRUE)
cama.comm.df$database_source<-"Comm"


download.file("http://dcatlas.dcgis.dc.gov/download/CamaResPt.ZIP", 
							paste(work.dir, "raw/CamaResPt.ZIP", sep="") )
# See http://data.dc.gov/Metadata.aspx?id=42

unzip( paste(work.dir, "raw/CamaResPt.ZIP", sep=""), exdir=paste(work.dir, "raw/", sep="") )

cama.res.df<-read.dbf(paste(work.dir, "raw/CamaResPt.dbf", sep=""), as.is=TRUE)
cama.res.df$database_source<-"Res"


download.file("http://dcatlas.dcgis.dc.gov/download/CamaCondoPt.ZIP", 
							paste(work.dir, "raw/CamaCondoPt.ZIP", sep="") )
# See http://data.dc.gov/Metadata.aspx?id=41

unzip( paste(work.dir, "raw/CamaCondoPt.ZIP", sep=""), exdir=paste(work.dir, "raw/", sep="") )

cama.condo.df<-read.dbf(paste(work.dir, "raw/CamaCondoPt.dbf", sep=""), as.is=TRUE)
cama.condo.df$database_source<-"Condo"

cama.df<-rbind.fill(cama.comm.df, cama.res.df)

cama.df<-rbind.fill(cama.df, cama.condo.df)

rm(cama.condo.df)
rm(cama.res.df)
rm(cama.comm.df)

cama.df$NUM_UNITS<-as.numeric(cama.df$NUM_UNITS)


cama.dups.df<-cama.df[duplicated(cama.df$SSL, fromLast=TRUE) | duplicated(cama.df$SSL), ] 

cama.dups.ls<-vector(mode = "list", length = length(unique(cama.dups.df$SSL)) )

names(cama.dups.ls)<-unique(cama.dups.df$SSL)

for (target.ssl in unique(cama.dups.df$SSL)) {
	
	cama.dups.temp.df<-cama.dups.df[cama.dups.df$SSL==target.ssl, ]
	
	num.units.temp<-cama.dups.temp.df$NUM_UNITS
	
	num.units.temp[is.na(num.units.temp)]<-0
	
	if (any(num.units.temp==0) & any(num.units.temp==1)) {
		num.units.temp<-1
	}
	
	if (any(num.units.temp>1) ) {
		num.units.temp<-sum(num.units.temp)
	}
	
	cama.dups.temp.df$NUM_UNITS<-num.units.temp	
	
	for ( i in 1:ncol(cama.dups.temp.df)) {
		
		if (!all( duplicated(cama.dups.temp.df[, i], fromLast=TRUE) | duplicated(cama.dups.temp.df[, i]) )) {
			cama.dups.temp.df[, i]<-"<MULTIPLE VALUES--SEE CAMA FILE>"
		}
		
	}
	
	cama.dups.ls[[target.ssl]]<-cama.dups.temp.df[1, ]
	
#	cat(target.ssl, "\n")
	
} 


cama.df<-cama.df[!cama.df$SSL %in% unique(cama.dups.df$SSL), ]

cama.dups.finished.df<-do.call(rbind, cama.dups.ls)

cama.df<-rbind(cama.df, cama.dups.finished.df)

colnames(cama.df)<-paste("DC.property.", colnames(cama.df), sep="")

colnames(cama.df)[colnames(cama.df)=="DC.property.SSL"]<-"DC.geocoder.SSL"

contribs.df<-merge(contribs.df, cama.df, all.x=TRUE)
			
rm(cama.df)

contribs.df$DC.property.multiunit.building<-contribs.df$DC.property.NUM_UNITS>1 &
  !is.na(contribs.df$DC.property.NUM_UNITS)


### This point is where it is saved
# save(contribs.df, file=paste(work.dir, "Geocoded contribs df may 1 after cama.Rdata", sep=""))
# save(committees.df, file=paste(work.dir, "committees finished df may 1 after cama.Rdata", sep=""))

puppet.id.run<-1

source(paste(code.dir, "shell-corp-identification.r", sep=""))

source(paste(code.dir, "corp-data-gathering.r", sep=""))
# save(contribs.df, file=paste(work.dir, "Geocoded contribs df may 1 after DCRA data gathering.Rdata", sep=""))

puppet.grouping.df<-contribs.df[contribs.df$contrib.timing.puppet.flag, ]

puppet.grouping.df<-puppet.grouping.df[order(
	puppet.grouping.df$address.clean,
	puppet.grouping.df$Committee.Name, 
	puppet.grouping.df$Date.of.Receipt), ]

puppet.grouping.df$group.distinguisher<-
	c(7<puppet.grouping.df$Date.of.Receipt[-1]-puppet.grouping.df$Date.of.Receipt[-nrow(puppet.grouping.df)] |
	puppet.grouping.df$Committee.Name[-1]!=puppet.grouping.df$Committee.Name[-nrow(puppet.grouping.df)] |
	puppet.grouping.df$address.clean[-1]!=puppet.grouping.df$address.clean[-nrow(puppet.grouping.df)], FALSE)

puppet.grouping.df$bundling.instance<-0
group.num<-1

for ( i in 1:nrow(puppet.grouping.df)) {
	puppet.grouping.df$bundling.instance[i]<-group.num
	if (puppet.grouping.df$group.distinguisher[i]) {group.num<-group.num+1}	
}

contribs.df<-merge(contribs.df, puppet.grouping.df[, c("contribution.id", "bundling.instance")], all.x=TRUE)

reg.agent.test.df<-contribs.df[contribs.df$contrib.timing.puppet.flag & 
	(contribs.df$DC.property.multiunit.building | contribs.df$address.evidence.multiunit.building) &
	contribs.df$address.no.unit.clean == contribs.df$address.clean, c("DCRA.reg.agent.name", "bundling.instance", "contribution.id")]

reg.agent.test.big.df<-contribs.df[!is.na(contribs.df$bundling.instance), c("DCRA.reg.agent.name", "bundling.instance", "contribution.id")]

contribs.df$reg.agent.puppet.flag<-TRUE

set.reg.agent.to.false<-c()

for ( i in 1:nrow(reg.agent.test.df)) {
	
#	contribs.df$reg.agent.puppet.flag[
#		which(contribs.df$contribution.id==reg.agent.test.df$contribution.id[i])]<-
	temp.v<-
			!is.na(reg.agent.test.df$DCRA.reg.agent.name[i]) & 
			reg.agent.test.df$DCRA.reg.agent.name[i] %in% 
			reg.agent.test.big.df$DCRA.reg.agent.name[ 
			  reg.agent.test.big.df$bundling.instance==reg.agent.test.df$bundling.instance[i] &
			    reg.agent.test.big.df$contribution.id!=reg.agent.test.df$contribution.id[i]
				]

  if(!temp.v) {
    set.reg.agent.to.false<-c(
    	set.reg.agent.to.false, reg.agent.test.df$contribution.id[i])
  }

}


contribs.df$reg.agent.puppet.flag[
  contribs.df$contribution.id %in% set.reg.agent.to.false]<-FALSE

contribs.df$bundling.instance[
	contribs.df$contribution.id %in% set.reg.agent.to.false]<-NA

contribs.df$contrib.timing.puppet.flag[contribs.df$address.clean %in% c("", " ADDRESS", " N A", " REQUESTED", "Requested", "X", "N A", "Address")]<-FALSE

contribs.df$reg.agent.puppet.flag[contribs.df$address.clean %in% c("", " ADDRESS", " N A", " REQUESTED", "Requested", "X", "N A", "Address")]<-FALSE

contribs.df$bundling.instance[contribs.df$address.clean %in% c("", " ADDRESS", " N A", " REQUESTED", "Requested", "X", "N A", "Address")]<-NA	 


more.than.one.shell.corp<-unique(contribs.df$bundling.instance[
  duplicated(contribs.df$bundling.instance) & contribs.df$reg.agent.puppet.flag])

contribs.df$reg.agent.puppet.flag[!(contribs.df$bundling.instance %in% more.than.one.shell.corp) &
	contribs.df$reg.agent.puppet.flag & contribs.df$contrib.timing.puppet.flag]<-FALSE

contribs.df$contrib.timing.puppet.flag[!(contribs.df$bundling.instance %in% more.than.one.shell.corp) &
	contribs.df$reg.agent.puppet.flag & contribs.df$contrib.timing.puppet.flag]<-FALSE

contribs.df$bundling.instance[!(contribs.df$bundling.instance %in% more.than.one.shell.corp) &
	contribs.df$reg.agent.puppet.flag & contribs.df$contrib.timing.puppet.flag]<-NA

contribs.df$bundling.instance<-as.numeric(as.factor(contribs.df$bundling.instance))

contribs.df$final.puppet.flag <- contribs.df$contrib.timing.puppet.flag &
  contribs.df$reg.agent.puppet.flag

#puppet.id.run<-2

#source(paste(code.dir, "shell-corp-identification.r", sep=""))





save(contribs.df, file=paste(work.dir, "DC_campaign_contributions_all_columns.Rdata", sep=""))

write.csv(contribs.df, file=paste(work.dir, "DC_campaign_contributions_all_columns.csv", sep=""), row.names=FALSE, na="")

wd.saved<-getwd()
setwd(work.dir)
zip(zipfile="DC_campaign_contributions_all_columns.zip", files= "DC_campaign_contributions_all_columns.csv")
setwd(wd.saved)


journalist.cut.columns<-c(	"contribution.id","contributor.clean","Committee.Name","Amount","Date.of.Receipt","address.clean","city.clean","state.clean","zip.clean","contribution.type.clean","contributor.first.name","contributor.last.name","contributor.spouse.1","contributor.spouse.2","candidate","electoral.office","election.cycle","Address","city","state","Zip","Contributor","Contribution.Type","address.no.unit.clean","address.evidence.multiunit.building","latitude.consolidated","longitude.consolidated","candidate.party","candidate.committee.organization.date","contributor.ward","recipient.ward","contributor.recipient.same.geo","DC.property.NUM_UNITS","DC.property.multiunit.building","same.address.puppet.flag","max.contrib.puppet.flag","contrib.timing.puppet.flag","DCRA.business.full.name","DCRA.file.num","DCRA.entity.id","DCRA.reg.agent.name","bundling.instance","reg.agent.puppet.flag","final.puppet.flag","max.legal.contrib","DC.geocoder.SSL","geocode.id")

write.csv(contribs.df[, journalist.cut.columns], 
					file=paste(work.dir, "DC_campaign_contributions_journalist_cut.csv", sep=""), row.names=FALSE, na="")

wd.saved<-getwd()
setwd(work.dir)
zip(zipfile="DC_campaign_contributions_journalist_cut.zip", files= "DC_campaign_contributions_journalist_cut.csv")
setwd(wd.saved)


# write.csv(file=paste(work.dir, "search form.csv", sep=""), row.names=FALSE, na="")


write.csv(contribs.df[, c("candidate", "election.cycle", "contributor.clean", "address.clean", "city.clean", "state.clean", "Amount", "Date.of.Receipt")], file=paste(work.dir, "DC_campaign_contributions_online_search_form.csv", sep=""), row.names=FALSE, na="")

wd.saved<-getwd()
setwd(work.dir)
zip(zipfile="DC_campaign_contributions_online_search_form.zip", files= "DC_campaign_contributions_online_search_form.csv")
setwd(wd.saved)






puppets.to.output.df<-contribs.df[contribs.df$final.puppet.flag, ]

puppets.to.output.df<-puppets.to.output.df[order(
	puppets.to.output.df$address.clean,
	puppets.to.output.df$Committee.Name, 
	puppets.to.output.df$Date.of.Receipt), ]

write.csv(puppets.to.output.df[, c("contributor.clean", "address.clean", "DCRA.reg.agent.name", "Amount", "Date.of.Receipt", "Committee.Name", "recipient.ward", "election.cycle", "latitude.consolidated", "longitude.consolidated")], file=paste(work.dir, "DC_campaign_contributions_puppet_corps_for_json.csv", sep=""), row.names=FALSE, na="")


write.table(puppets.to.output.df[, c("candidate", "election.cycle", "contributor.clean", "DCRA.reg.agent.name", "address.clean", "city.clean", "state.clean", "Amount", "Date.of.Receipt")], file=paste(work.dir, "DC_campaign_contributions_puppets_for_webpage_search.csv", sep=""), col.names=c("Candidate", "Election Year", "Contributor", "Registered Agent", "Address", "City", "State", "Amount", "Date"),  row.names=FALSE, qmethod = "double", sep=",")

puppets.to.output.df$manyeyes.address<-paste("Addr: ", puppets.to.output.df$address.clean, ", ", puppets.to.output.df$city.clean, ", ", puppets.to.output.df$state.clean, sep="")

puppets.to.output.df$manyeyes.corp<-paste("Corp: ", puppets.to.output.df$contributor.clean, sep="")

puppets.to.output.df$manyeyes.amount<-paste("Amount: $", puppets.to.output.df$Amount, sep="")

for (target.year in sort(unique(puppets.to.output.df$election.cycle))) {
	
	write.table(puppets.to.output.df[puppets.to.output.df$election.cycle==target.year, c("Committee.Name", "manyeyes.address", "manyeyes.corp", "manyeyes.amount")], 
    file=paste(work.dir, "Many Eyes Visualization ", target.year, ".csv", sep=""), 
    col.names=c("Committee", "Address", "Contributor", "Amount"),
    row.names=FALSE, qmethod = "double", sep=",")
	
}




webpage.df<-contribs.df[!is.na(contribs.df$candidate) & !is.na(contribs.df$election.cycle), c("candidate", "election.cycle", "contributor.clean", "address.clean", "city.clean", "state.clean", "Amount", "Date.of.Receipt")]

write.table(webpage.df[order(webpage.df$Date.of.Receipt, webpage.df$Amount, decreasing=TRUE),], file=paste(work.dir, "DC_campaign_contributions_for_webpage_search.csv", sep=""), col.names=c("Candidate", "Election Year", "Contributor", "Address", "City", "State", "Amount", "Date"),  row.names=FALSE, qmethod = "double", sep=",")

wd.saved<-getwd()
setwd(work.dir)
zip(zipfile="DC_campaign_contributions_for_webpage_search.zip", files= "DC_campaign_contributions_for_webpage_search.csv")
setwd(wd.saved)

