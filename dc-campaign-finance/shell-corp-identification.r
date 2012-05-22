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

# suspected bundling
# 3 tests:
# 1. same address
# 2. within 7 days
# 3. maximum amount

#% of corp contribs that are suspected bundling


contribs.df$same.address.puppet.flag<-FALSE
contribs.df$max.contrib.puppet.flag<-FALSE
contribs.df$contrib.timing.puppet.flag<-FALSE

same.address.combined.v<-c()
max.contrib.detect.combined.v<-c()
contrib.timing.detect.combined.v<-c()


# map.races<-"All 2010"

map.races<-c(
	"All All Years",
	"All 2012",
	"All 2010",
	"All 2008",
	"All 2006",
	"All 2004",
	"All 2002",
	"Citywide 2012",
	"Citywide 2010",
	"Citywide 2008",
	"Citywide 2006",
	"Citywide 2004",
	"Citywide 2002",
	"Ward 1 2010",
	"Ward 1 2006",
	"Ward 1 2002",
	"Ward 2 2012",
	"Ward 2 2008",
	"Ward 2 2004",
	"Ward 3 2010",
	"Ward 3 2006",
	"Ward 3 2002",
	"Ward 4 2012",
	"Ward 4 2008",
	"Ward 4 2004",
	"Ward 5 2010",
	"Ward 5 2006",
	"Ward 5 2002",
	"Ward 6 2010",
	"Ward 6 2006",
	"Ward 6 2002",
	"Ward 7 2012",
	"Ward 7 2008",
	"Ward 7 2004",
	"Ward 8 2012",
	"Ward 8 2008",
	"Ward 8 2004")


if(puppet.id.run==1) {map.races<-"All All Years"}


all.committees<-unique(contribs.df$Committee.Name)


for (target.races in map.races) {
	
	
	target.year<-str_extract_all(target.races, "[0-9]{4}")[[1]]
	target.geog<-str_extract_all(target.races, "(All)|(Citywide)|(Ward [0-8])")[[1]]
	
	if (target.geog=="All") {
		
		comm.temp<-committees.df$Committee.Name[committees.df$year==target.year]
		target.committees<-all.committees[all.committees %in% comm.temp]
		
	}
	
	if (target.geog=="Citywide") {
		
		comm.temp<-committees.df$Committee.Name[committees.df$year==target.year &
			!grepl("(Ward)|(District)", committees.df$electoral.office) ]
		target.committees<-all.committees[all.committees %in% comm.temp]
		
	}
	
	if (grepl("Ward", target.geog)) {
		
		comm.temp<-committees.df$Committee.Name[committees.df$year==target.year &
			grepl(target.geog, committees.df$electoral.office) ]
		target.committees<-all.committees[all.committees %in% comm.temp]
		
	}
	
	if (target.races=="All All Years") {
	  
		comm.temp<-committees.df$Committee.Name
		target.committees<-all.committees
		
	}
	
	
	same.address.combined.v<-c()
	max.contrib.detect.combined.v<-c()
	contrib.timing.detect.combined.v<-c()
	
	for ( target.committee in target.committees) {
		
		contribs.one.cand.df<-contribs.df[contribs.df$Committee.Name==target.committee, ]
		
		contribs.one.cand.df<-contribs.one.cand.df[contribs.one.cand.df$contribution.type.clean %in% c("Corporation"), ]
		
		address.clean.tab<-table(contribs.one.cand.df$address.clean)
		
		same.address.v<-names(address.clean.tab)[address.clean.tab>=2]
		same.address.combined.v<-c( same.address.combined.v, same.address.v)
		
		for (target.address in same.address.v) {
			
			bundlers.temp.df<-contribs.one.cand.df[contribs.one.cand.df$address.clean==target.address, ]
			
			contrib.office<-contribs.one.cand.df$electoral.office[1]
			
			if (grepl("(Council Ward)|(School Board President)|(School Board At-Large)", contrib.office)) {max.legal.contrib<-500}
			if (grepl("Council At-Large", contrib.office)) {max.legal.contrib<-1000}
			if (grepl("Council Chairman", contrib.office)) {max.legal.contrib<-1500}
			if (grepl("(Mayor)|(US Senator)|(US Representative)|(DATA UNAVAILABLE)", contrib.office)) {max.legal.contrib<-2000}
			if (grepl("(DC Democratic State Committee)|(School Board Ward)|(School Board District)|(National Committee)|(Delegates)", contrib.office)) {max.legal.contrib<-300}
			
			max.contrib.detect.v<-bundlers.temp.df$contribution.id[bundlers.temp.df$Amount==max.legal.contrib]
			
			max.contrib.detect.combined.v<-c(max.contrib.detect.combined.v, max.contrib.detect.v)
			
			if (sum(bundlers.temp.df$contribution.id %in% max.contrib.detect.v)>=2) {
				
				contrib.timing.mat<-matrix(bundlers.temp.df$Date.of.Receipt[
					bundlers.temp.df$contribution.id %in% max.contrib.detect.v])
				rownames(contrib.timing.mat)<-bundlers.temp.df$contribution.id[
					bundlers.temp.df$contribution.id %in% max.contrib.detect.v]
				contrib.timing.mat<-dist(contrib.timing.mat)
				contrib.timing.mat<-as.matrix(contrib.timing.mat)<=7
				# contrib.timing.mat[upper.tri(contrib.timing.mat, diag = TRUE)]<-FALSE
				diag(contrib.timing.mat)<-FALSE
				contrib.timing.v<-apply(contrib.timing.mat, MARGIN=2, FUN=any)
				
				contrib.timing.detect.combined.v<-
					c(contrib.timing.detect.combined.v, colnames(contrib.timing.mat)[contrib.timing.v])
			}
			
		}
		cat(target.committee, "\n")
	}
	
	same.address.combined.v<-unique(same.address.combined.v)
	max.contrib.detect.combined.v<-unique(max.contrib.detect.combined.v)
	contrib.timing.detect.combined.v<-unique(contrib.timing.detect.combined.v)
	
	contribs.df$same.address.puppet.flag[contribs.df$address.clean %in% same.address.combined.v]<-TRUE
	contribs.df$max.contrib.puppet.flag[
		contribs.df$contribution.id %in% max.contrib.detect.combined.v & contribs.df$same.address.puppet.flag]<-TRUE
	
	contribs.df$contrib.timing.puppet.flag[
		contribs.df$contribution.id %in% contrib.timing.detect.combined.v &
			contribs.df$same.address.puppet.flag &
			contribs.df$max.contrib.puppet.flag]<-TRUE
	
	# shp2kml 2.1b:
	
	if(puppet.id.run==1) {
	  next
#    puppet.contribs.df<-contribs.df[contribs.df$contrib.timing.puppet.flag & 
#	    contribs.df$Committee.Name %in% comm.temp, ]
	}
	
	if(puppet.id.run==2) {
		puppet.contribs.df<-contribs.df[contribs.df$final.puppet.flag &
			contribs.df$Committee.Name %in% comm.temp, ]
	}
	
	if(nrow(puppet.contribs.df)==0) {
		write.csv(puppet.contribs.df, file=paste(work.dir, "NO puppet CORPS ", target.races, ".csv"), row.names=FALSE)
		next
	}
	
	puppet.contribs.temp.df<-data.frame(address.clean=unique(puppet.contribs.df$address.clean),
	  puppet.address.id=1:length(unique(puppet.contribs.df$address.clean)), stringsAsFactors=FALSE)
	
	puppet.contribs.df<-merge(puppet.contribs.df, puppet.contribs.temp.df)
	
	puppet.ls<-vector(mode="list", length=length(unique(puppet.contribs.df$puppet.address.id)))
	
	names(puppet.ls)<-unique(puppet.contribs.df$puppet.address.id)
	
	i<-unique(puppet.contribs.df$puppet.address.id)[1]
	i<-1
	
	for ( i in unique(puppet.contribs.df$puppet.address.id)) {
		
		puppet.order.tab<-sort(table(puppet.contribs.df[puppet.contribs.df$puppet.address.id==i, "Committee.Name"]), decreasing = TRUE)
		
		puppet.order.df<-data.frame(Committee.Name=names(puppet.order.tab), record.order=1:length(puppet.order.tab),
															 stringsAsFactors=FALSE)
		
		puppet.temp.df<-merge(puppet.contribs.df[puppet.contribs.df$puppet.address.id==i, 
																					 c("Contributor", "DCRA.reg.agent.name", "Amount", "Date.of.Receipt", "Committee.Name")], puppet.order.df)
		
		puppet.temp.df<-puppet.temp.df[order(puppet.temp.df$record.order, puppet.temp.df$Date.of.Receipt, -puppet.temp.df$Amount), ]
		
		puppet.temp.df$temporal.relation<-
			c(7<puppet.temp.df$Date.of.Receipt[-1]-puppet.temp.df$Date.of.Receipt[-nrow(puppet.temp.df)] |
			  puppet.temp.df$Committee.Name[-1]!=puppet.temp.df$Committee.Name[-nrow(puppet.temp.df)], FALSE)
		
		puppet.temp.df$temporal.relation[!puppet.temp.df$temporal.relation]<-""
		
		puppet.mat<-matrix(t(puppet.temp.df[, c("Contributor", "DCRA.reg.agent.name", "Amount", "Date.of.Receipt", "Committee.Name", "temporal.relation")]), nrow=1)
		
		puppet.col.names<-data.frame(aa=paste("contributor", 1:(ncol(puppet.mat)/6), sep=""), 
                                ab=paste("regagent", 1:(ncol(puppet.mat)/6), sep=""),
																bb=paste("amount", 1:(ncol(puppet.mat)/6), sep=""), 
																cc=paste("date", 1:(ncol(puppet.mat)/6), sep=""),
																dd=paste("committee", 1:(ncol(puppet.mat)/6), sep=""), 
																ee=paste("space", 1:(ncol(puppet.mat)/6), sep=""),
																stringsAsFactors=FALSE)
		
		puppet.mat.df<-as.data.frame(puppet.mat, stringsAsFactors=FALSE)
		
		names(puppet.mat.df)<-c(t(puppet.col.names))
		
		puppet.mat.df<-cbind(
			data.frame(address=puppet.contribs.df$address.clean[puppet.contribs.df$puppet.address.id==i][1],
								 lat=puppet.contribs.df$latitude.consolidated[puppet.contribs.df$puppet.address.id==i][1],
								 long=puppet.contribs.df$longitude.consolidated[puppet.contribs.df$puppet.address.id==i][1],
								 TotalAmount=sum(puppet.contribs.df$Amount[puppet.contribs.df$puppet.address.id==i], na.rm=TRUE),
								 SSL=puppet.contribs.df$DC.geocoder.SSL[puppet.contribs.df$puppet.address.id==i][1],
								 MARid=puppet.contribs.df$DC.geocoder.ADDRESS_ID[puppet.contribs.df$puppet.address.id==i][1],
								 stringsAsFactors=FALSE),
			puppet.mat.df)
		
		puppet.ls[[i]]<-puppet.mat.df
		
	}
	

	puppet.dbf<-do.call(rbind.fill, puppet.ls)
	# will want to order by committee
	
	
	puppet.dbf<-puppet.dbf[order(puppet.dbf$TotalAmount, decreasing=TRUE), ]
	
	puppet.dbf$rankCat<-5
	
	if (nrow(puppet.dbf)>=10) {
		puppet.dbf$rankCat[1:10]<-1
	}
	
	if (nrow(puppet.dbf)>=20) {
		puppet.dbf$rankCat[11:20]<-2
	}
	
	if (nrow(puppet.dbf)>=60) {
		puppet.dbf$rankCat[21:60]<-3
	}
	
	if (nrow(puppet.dbf)>=140) {
		puppet.dbf$rankCat[61:140]<-4
	}
	
	puppet.dbf<-cbind(data.frame(Id=1:nrow(puppet.dbf)), puppet.dbf)
	
	puppet.shp.df<-data.frame(
		Id=puppet.dbf$Id,
		X=as.numeric(puppet.dbf$long),
		Y=as.numeric(puppet.dbf$lat)
		)
	
	puppet.output.shp <- convert.to.shapefile(puppet.shp.df, puppet.dbf, "Id", 1)
	
	write.shapefile(puppet.output.shp, paste(work.dir, "puppet points time criteria ", target.races, sep=""), arcgis=T)
	
#	write.csv(puppet.dbf, file=paste(work.dir, "puppet points time criteria ", target.races, ".csv", sep=""), row.names=FALSE)
	
	write.table(puppet.dbf, file=paste(work.dir, "puppet points time criteria ", target.races, ".tab", sep=""), row.names=FALSE, sep="\t", na="", quote=FALSE)

	
	
}


# View(puppet.grouping.df[, c("address.clean", "Committee.Name","bundling.instance")])


### This point is where it is saved
# save(contribs.df, file=paste(work.dir, "Geocoded contribs df may 1 after puppet ID.Rdata", sep=""))
# save(committees.df, file=paste(work.dir, "committees finished df may 1 after puppet ID.Rdata", sep=""))
# save.image(file=paste(work.dir, "full workspace image after puppet ID.Rdata", sep=""))

# write.csv(contribs.df[contribs.df$contrib.timing.puppet.flag, c("contribution.id", "Contributor", "Committee.Name", "Amount", "address.clean", "city.clean", "state.clean", "longitude.consolidated", "latitude.consolidated")], file=paste(work.dir, "puppet points time criteria flat file 4-21-12.csv", sep=""),  row.names=FALSE )






# http://ocf.dc.gov/intop/opinions/op_96_12.shtm
# $2,000 for Mayor, Shadow Senator and Shadow Representative 
# $1,500 for Chairman of the Council 
# $1,000 for an At-Large Council member 
# $500 for President of the Board of Education, At-Large Member, Board of Education, or for a Ward Council member
# $300 for a member of the Board of Education elected from a school district or for an official of a political party
# $25 for a member of an Advisory Neighborhood Commission 
