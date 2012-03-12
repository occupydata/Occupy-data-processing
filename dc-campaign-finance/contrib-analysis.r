

names(DC.geocoded.df)[grepl("[.][0-9]$", names(DC.geocoded.df))]

table(!is.na(DC.geocoded.df$returnDataset.diffgram.NewDataSet.Table1.FULLADDRESS.1))

gyuhui<-
edit(DC.geocoded.df[!is.na(DC.geocoded.df$returnDataset.diffgram.NewDataSet.Table1.FULLADDRESS.1), ])




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