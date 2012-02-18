
work.dir<-""
# Put your working directory here

download.file(
  "http://commondatastorage.googleapis.com/ckannet-storage/2012-02-16T040627/DC_campaign_contribs_99_part12.zip",
  paste(work.dir, "DC_campaign_contribs_99_part12.zip", sep="")
  )
  
unzip( paste(work.dir, "DC_campaign_contribs_99_part12.zip", sep=""), exdir=paste(work.dir, "raw/", sep="") )

files.to.stack<-list.files(path=paste(work.dir, "raw/", sep=""))

targ.file<-targ.file[grepl("DC_contribs_" targ.file)]

contribs.df<-read.csv(paste(work.dir, "raw/", files.to.stack[1], sep=""), stringsAsFactors=FALSE)

for ( targ.file in files.to.stack[-1] ) {

  contribs.temp.df<-read.csv(paste(work.dir, "raw/", targ.file, sep=""), stringsAsFactors=FALSE)

  contribs.temp.df<-rbind(contribs.df, contribs.temp.df)

}

