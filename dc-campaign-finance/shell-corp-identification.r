# Copyright (c) 2012 Data Committee of Occupy DC
# 	
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

library("stringr")

contribs.df$contribution.id<-1:nrow(contribs.df)

contribs.df$same.address.shell.flag<-FALSE
contribs.df$max.contrib.shell.flag<-FALSE
contribs.df$contrib.timing.shell.flag<-FALSE

same.address.combined.v<-c()
max.contrib.detect.combined.v<-c()
contrib.timing.detect.combined.v<-c()

contribs.df$electoral.office[is.na(contribs.df$electoral.office)]<-"placeholder"




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



all.committees<-unique(contribs.df$Committee.Name)

target.races<-map.races[3]

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
		
		contribs.one.cand.df<-contribs.one.cand.df[contribs.one.cand.df$Contribution.Type %in% c("Business", "Corporation"), ]
		
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
	
	contribs.df$same.address.shell.flag[contribs.df$address.clean %in% same.address.combined.v]<-TRUE
	contribs.df$max.contrib.shell.flag[
		contribs.df$contribution.id %in% max.contrib.detect.combined.v & contribs.df$same.address.shell.flag]<-TRUE
	
	contribs.df$contrib.timing.shell.flag[
		contribs.df$contribution.id %in% contrib.timing.detect.combined.v &
			contribs.df$same.address.shell.flag &
			contribs.df$max.contrib.shell.flag]<-TRUE
	
	# shp2kml 2.1b:
	
	shell.contribs.df<-contribs.df[contribs.df$contrib.timing.shell.flag & 
	  contribs.df$Committee.Name %in% comm.temp, ]
	# perhaps contribs.df$same.address.shell.flag
	
	if(nrow(shell.contribs.df)==0) {
		write.csv(shell.contribs.df, file=paste(work.dir, "NO SHELL CORPS ", target.races, ".csv"), row.names=FALSE)
		next
	}
	
	shell.contribs.temp.df<-data.frame(address.clean=unique(shell.contribs.df$address.clean),
																		 shell.address.id=1:length(unique(shell.contribs.df$address.clean)), stringsAsFactors=FALSE)
	
	shell.contribs.df<-merge(shell.contribs.df, shell.contribs.temp.df)
	
	shell.ls<-vector(mode="list", length=length(unique(shell.contribs.df$shell.address.id)))
	
	names(shell.ls)<-unique(shell.contribs.df$shell.address.id)
	
	i<-unique(shell.contribs.df$shell.address.id)[1]
	i<-1
	
	for ( i in unique(shell.contribs.df$shell.address.id)) {
		
		shell.order.tab<-sort(table(shell.contribs.df[shell.contribs.df$shell.address.id==i, "Committee.Name"]), decreasing = TRUE)
		
		shell.order.df<-data.frame(Committee.Name=names(shell.order.tab), record.order=1:length(shell.order.tab),
															 stringsAsFactors=FALSE)
		
		shell.temp.df<-merge(shell.contribs.df[shell.contribs.df$shell.address.id==i, 
																					 c("Contributor", "Amount", "Date.of.Receipt", "Committee.Name")], shell.order.df)
		
		shell.temp.df<-shell.temp.df[order(shell.temp.df$record.order, shell.temp.df$Date.of.Receipt, -shell.temp.df$Amount), ]
		
		shell.temp.df$temporal.relation<-
			c(7<shell.temp.df$Date.of.Receipt[-1]-shell.temp.df$Date.of.Receipt[-nrow(shell.temp.df)], FALSE)
		
		shell.temp.df$temporal.relation[!shell.temp.df$temporal.relation]<-""
		
		shell.mat<-matrix(t(shell.temp.df[, c("Contributor", "Amount", "Date.of.Receipt", "Committee.Name", "temporal.relation")]), nrow=1)
		
		shell.col.names<-data.frame(aa=paste("contributor", 1:(ncol(shell.mat)/5), sep=""), 
																bb=paste("amount", 1:(ncol(shell.mat)/5), sep=""), 
																cc=paste("date", 1:(ncol(shell.mat)/5), sep=""),
																dd=paste("committee", 1:(ncol(shell.mat)/5), sep=""), 
																ee=paste("space", 1:(ncol(shell.mat)/5), sep=""),
																stringsAsFactors=FALSE)
		
		shell.mat.df<-as.data.frame(shell.mat, stringsAsFactors=FALSE)
		
		names(shell.mat.df)<-c(t(shell.col.names))
		
		shell.mat.df<-cbind(
			data.frame(address=shell.contribs.df$address.clean[shell.contribs.df$shell.address.id==i][1],
								 lat=shell.contribs.df$latitude.consolidated[shell.contribs.df$shell.address.id==i][1],
								 long=shell.contribs.df$longitude.consolidated[shell.contribs.df$shell.address.id==i][1],
								 TotalAmount=sum(shell.contribs.df$Amount[shell.contribs.df$shell.address.id==i], na.rm=TRUE),
								 stringsAsFactors=FALSE),
			shell.mat.df)
		
		shell.ls[[i]]<-shell.mat.df
		
	}
	
	library(reshape)
	shell.dbf<-do.call(rbind.fill, shell.ls)
	# will want to order by committee
	
	
	shell.dbf<-shell.dbf[order(shell.dbf$TotalAmount, decreasing=TRUE), ]
	
	shell.dbf$rankCat<-5
	
	if (nrow(shell.dbf)>=10) {
		shell.dbf$rankCat[1:10]<-1
	}
	
	if (nrow(shell.dbf)>=20) {
		shell.dbf$rankCat[11:20]<-2
	}
	
	if (nrow(shell.dbf)>=60) {
		shell.dbf$rankCat[21:60]<-3
	}
	
	if (nrow(shell.dbf)>=140) {
		shell.dbf$rankCat[61:140]<-4
	}
	
	shell.dbf<-cbind(data.frame(Id=1:nrow(shell.dbf)), shell.dbf)
	
	library(geosphere)
	library(shapefiles)
	
	shell.shp.df<-data.frame(
		Id=shell.dbf$Id,
		X=as.numeric(shell.dbf$long),
		Y=as.numeric(shell.dbf$lat)
		)
	
	shell.output.shp <- convert.to.shapefile(shell.shp.df, shell.dbf, "Id", 1)
	
	write.shapefile(shell.output.shp, paste(work.dir, "shell points time criteria ", target.races, sep=""), arcgis=T)
	
	write.csv(shell.dbf, file=paste(work.dir, "shell points time criteria ", target.races, ".csv", sep=""), row.names=FALSE)
	
}


write.csv(contribs.df[contribs.df$contrib.timing.shell.flag, c("contribution.id", "Contributor", "Committee.Name", "Amount", "address.clean", "city.clean", "state.clean", "longitude.consolidated", "latitude.consolidated")], file=paste(work.dir, "shell points time criteria flat file 4-21-12.csv", sep=""),  row.names=FALSE )



# sudo port install php5-curl




# http://ocf.dc.gov/intop/opinions/op_96_12.shtm
# $2,000 for Mayor, Shadow Senator and Shadow Representative 
# $1,500 for Chairman of the Council 
# $1,000 for an At-Large Council member 
# $500 for President of the Board of Education, At-Large Member, Board of Education, or for a Ward Council member
# $300 for a member of the Board of Education elected from a school district or for an official of a political party
# $25 for a member of an Advisory Neighborhood Commission 
