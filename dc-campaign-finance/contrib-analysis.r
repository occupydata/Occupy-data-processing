


by.comm.year.agg<-aggregate(contribs.df$Amount[contribs.df$Contribution.Type=="Corporation"],
	by=list(electoral.office=contribs.df$electoral.office[contribs.df$Contribution.Type=="Corporation"],
	Committee.Name=contribs.df$Committee.Name[contribs.df$Contribution.Type=="Corporation"],
	year=substr(contribs.df$Date.of.Receipt[contribs.df$Contribution.Type=="Corporation"], 1, 4)),
	FUN=sum, na.rm=TRUE)

write.csv(by.comm.year.agg, file=paste(work.dir, "Aggregation by committee and year - corps.csv", sep=""), row.names=FALSE)




# suspected bundling
# 3 tests:
# 1. same address
# 2. within 7 days
# 3. maximum amount

% of corp contribs that are suspected bundling

contribs.df$contribution.id<-1:nrow(contribs.df)

same.address.combined.v<-c()
max.contrib.detect.combined.v<-c()

contribs.df$electoral.office[is.na(contribs.df$electoral.office)]<-"placeholder"

for ( target.committee in unique(contribs.df$Committee.Name)) {

  contribs.one.cand.df<-contribs.df[contribs.df$Committee.Name==target.committee, ]
  
  contribs.one.cand.df<-contribs.one.cand.df[contribs.one.cand.df$Contribution.Type=="Corporation", ]
  
  address.clean.tab<-table(contribs.one.cand.df$address.clean)
  
  same.address.v<-names(address.clean.tab)[address.clean.tab>=2]
  same.address.combined.v<-c( same.address.combined.v, same.address.v)
  
  for (target.address in same.address.v) {
  
    bundlers.temp.df<-contribs.one.cand.df[contribs.one.cand.df$address.clean==target.address, ]
    
    contrib.office<-contribs.one.cand.df$electoral.office[1]
    
    if (grepl("(Council Ward)|(School Board President)|(School Board At-Large)|(placeholder)", contrib.office)) {max.legal.contrib<-500}
    if (grepl("Council At-Large", contrib.office)) {max.legal.contrib<-1000}
    if (grepl("Council Chairman", contrib.office)) {max.legal.contrib<-1500}
    if (grepl("(Mayor)|(US Senator)|(US Representative)", contrib.office)) {max.legal.contrib<-2000}
    if (grepl("(DC Democratic State Committee)|(School Board Ward)|(School Board District)|(National Committee)", contrib.office)) {max.legal.contrib<-300}
    
    max.contrib.detect.v<-contribs.one.cand.df$contribution.id[bundlers.temp.df$Amount==max.legal.contrib]
    
    max.contrib.detect.combined.v<-c(max.contrib.detect.combined.v, max.contrib.detect.v)
  }
  cat(target.committee, "\n")
}

same.address.combined.v<-unique(same.address.combined.v)
max.contrib.detect.combined.v<-unique(max.contrib.detect.combined.v)

contribs.df$same.address.shell.flag<-FALSE
contribs.df$max.contrib.shell.flag<-FALSE

contribs.df$same.address.shell.flag[contribs.df$address.clean %in% same.address.combined.v]<-TRUE
contribs.df$max.contrib.shell.flag[contribs.df$contribution.id %in% max.contrib.detect.combined.v]<-TRUE

# shp2kml 2.1b:

shell.contribs.df<-contribs.df[contribs.df$max.contrib.shell.flag, ]
# perhaps contribs.df$same.address.shell.flag

shell.contribs.temp.df<-data.frame(address.clean=unique(shell.contribs.df$address.clean),
  shell.address.id=1:length(unique(shell.contribs.df$address.clean)), stringsAsFactors=FALSE)

shell.contribs.df<-merge(shell.contribs.df, shell.contribs.temp.df)

shell.ls<-vector(mode="list", length=length(unique(shell.contribs.df$shell.address.id)))

names(shell.ls)<-unique(shell.contribs.df$shell.address.id)

for ( i in unique(shell.contribs.df$shell.address.id)) {
	
  shell.mat<-matrix(t(shell.contribs.df[shell.contribs.df$shell.address.id==i, 
    c("Contributor", "Amount", "Date.of.Receipt", "Committee.Name")]), nrow=1)
  
  shell.col.names<-data.frame(aa=paste("contributor", 1:(ncol(shell.mat)/4), sep=""), 
    bb=paste("amount", 1:(ncol(shell.mat)/4), sep=""), 
    cc=paste("date", 1:(ncol(shell.mat)/4), sep=""),
    dd=paste("committee", 1:(ncol(shell.mat)/4), sep=""), stringsAsFactors=FALSE)
  
  shell.mat.df<-as.data.frame(shell.mat, stringsAsFactors=FALSE)
  
  names(shell.mat.df)<-c((t(shell.col.names)))
  
  shell.mat.df<-cbind(data.frame(address=shell.contribs.df$address.clean[shell.contribs.df$shell.address.id==i][1],
  	TotalAmount=sum(shell.contribs.df$Amount[shell.contribs.df$shell.address.id==i], na.rm=TRUE),
  	stringsAsFactors=FALSE),
  	shell.mat.df)
  
  shell.ls[[i]]<-shell.mat.df

}

library(reshape)
shell.dbf<-do.call(rbind.fill, shell.ls)
# will want to order by committee


shp.points.df<-contribs.df
	
library(geosphere)
library(shapefiles)

coords.shp.df<-data.frame(
	Id=coords.shp.df$Id,
	X=coords.shp.df$long,
	Y=coords.shp.df$lat
)

line.data.df<-line.data.df[, !colnames(line.data.df) == "Id"]

coords.shp.dbf<-cbind(
	data.frame(Id=1:nrow(coords.df), Item=paste("Item", 1:nrow(coords.df), sep="")),
	line.data.df)

coords.output.shp <- convert.to.shapefile(coords.shp.df, coords.shp.dbf, "Id", 1)

write.shapefile(coords.output.shp, output.shp.path, arcgis=T)




  same.address.v

# $2,000 for Mayor, Shadow Senator and Shadow Representative 
# $1,500 for Chairman of the Council 
# $1,000 for an At-Large Council member 
# $500 for President of the Board of Education, At-Large Member, Board of Education, or for a Ward Council member
# $300 for a member of the Board of Education elected from a school district or for an official of a political party
# $25 for a member of an Advisory Neighborhood Commission 


names(DC.geocoded.df)[grepl("[.][0-9]$", names(DC.geocoded.df))]

table(!is.na(DC.geocoded.df$returnDataset.diffgram.NewDataSet.Table1.FULLADDRESS.1))



contribs.df[!is.na(contribs.df[contribs.df$state.clean=="DC", "MPreDirectional"]), "address.clean"]

contribs.df[!is.na(contribs.df[contribs.df$state.clean=="DC", "MPreDirectional"]), "Address"]

contribs.df[!is.na(contribs.df[, "MPreDirectional"]) & contribs.df$state.clean=="DC", "MPreDirectional"]
head(
contribs.df[!is.na(contribs.df[, "MPreDirectional"]) & contribs.df$state.clean=="DC", "Address"]
)
str(contribs.df[contribs.df$Address=="9102 E. Pershing Ave",] )



install.packages("chron")

source("http://blog.revolution-computing.com/downloads/calendarHeat.R")

contribs.date.agg<-aggregate(contribs.df$Amount, by=list(contribs.df$Date.of.Receipt),
 FUN=sum, na.rm=TRUE)

contribs.date.agg<-contribs.date.agg[as.Date(contribs.date.agg$Group.1) >
	as.Date("2007-01-01"), 	]

calendarHeat(contribs.date.agg$Group.1, log(contribs.date.agg$x), color="r2b")

#See:
#https://code.google.com/p/hackystat-ui-trajectory/source/browse/trunk/Rcode/?r=612#Rcode%2Fe4.heatmaps
# http://greatergreaterwashington.org/post/13968/most-sitting-councilmembers-absent-on-campaign-finance/


# Liscenced home improvement contractors: http://government.dc.gov/DC/Government/Publication%20Files/Consumer/home_improvement_contractors_list_1_2010.pdf

# DC liccensed general contractors: http://government.dc.gov/DC/Government/Publication%20Files/Consumer/general_contractors_list_12_2010.pdf

# http://lsdbe.dslbd.dc.gov/public/certification/search.aspx

# http://pivs.dcra.dc.gov/property/search
# http://cpms.dcra.dc.gov/BBLV/default.aspx

# http://dcra.dc.gov/DC/DCRA/About+DCRA/News+Room/Press+Releases/DCRA+launches+Corp+Online,+a+new+online+corporate+registration+and+filing+system
# https://corp.dcra.dc.gov
# http://dcatlas.dcgis.dc.gov/catalog/results.asp
# http://dcatlas.dcgis.dc.gov/metadata/AddressPt.html

# Biking: http://dcatlas.dcgis.dc.gov/metadata/TopoLn.html
# http://dcatlas.dcgis.dc.gov/metadata/Topo_20ft.html
# http://dcatlas.dcgis.dc.gov/catalog/

# http://dcatlas.dcgis.dc.gov/metadata/NbhClusPly.html
# http://dcatlas.dcgis.dc.gov/metadata/DCPropertyPt.html DC govt property
# http://dcatlas.dcgis.dc.gov/metadata/CamaCommPt.html

# Basic Business licenses: http://data.dc.gov/Metadata.aspx?id=1520


# http://octo.dc.gov/DC/OCTO/Maps+and+Apps/Online+Mapping/All+Online+Maps

# http://geospatial.dcgis.dc.gov/ocf/

# http://www.city-data.com

# http://otr.cfo.dc.gov/otr/cwp/view,a,1330,q,594345.asp

# http://otr.cfo.dc.gov/otr/lib/otr/tax/property/pdf/usecodes.pdf
# https://www.taxpayerservicecenter.com/PropertyDetailTips.pdf



contribs.df$DUPS<-duplicated(contribs.df[, c("Address", "city", "state", "Zip")])

k<-1

for ( i in 1:nrow(contribs.df)) {
  if (contribs.df$DUPS[i]) {
    contribs.df$geocode.id[i]<-k
  } else {
    k<-k+1
    contribs.df$geocode.id[i]<-k
  }
    
}

#### THIS IS NECESSARY SINCE SOMEHOW THE GEOCODES WERE SCRAMBLED. MUST FIX IN FINAL
#### VERSION OF CODE ABOVE


View(contribs.geocoded.df[90000:91000, c("Address", "address.clean")])

View(contribs.geocoded.df[90020, ])

contribs.geocoded.df[contribs.geocoded.df$Matching.Geography.Type=="USPSZipPlus4", c("FArea", "FAreaType")]

head(
  contribs.geocoded.df[contribs.geocoded.df$Matching.Geography.Type=="CountySubRegion", c("FArea", "FAreaType")]
  )