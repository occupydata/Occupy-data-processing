
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

contribs.to.geocode.df

geocode.output.ls<-vector("list", length=nrow(contribs.to.geocode.df))


for ( i in 1:nrow(contribs.df)) {

  
}

