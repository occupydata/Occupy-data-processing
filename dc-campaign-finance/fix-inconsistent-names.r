# input should be: contribs.df[, c("address.clean", "contributor.clean", "contribution.id")]

# Dependencies: RecordLinkage, network, igraph

fix.inconsistent.contrib.names<-function(inconsistent.recs, grouping.threshold=0.97) {
	
	colnames(inconsistent.recs)<-c("address.clean", "contributor.clean", "contribution.id")
	
	contributor.clean.save<-inconsistent.recs$contributor.clean
	
	inconsistent.recs$contributor.clean<-toupper(inconsistent.recs$contributor.clean)
	inconsistent.recs$contributor.clean<-gsub("(,)|([.])|(;)", "", inconsistent.recs$contributor.clean)
  inconsistent.recs$contributor.clean<-gsub("&", "AND", inconsistent.recs$contributor.clean)
	inconsistent.recs$contributor.clean<-gsub("( PC$)|( INC$)|( LLC$)|( INCORPORATED$)|( PLLC$)|( LLP$)|( LTD$)|( LLLP$)|( LIMITED PARTNERSHIP$)|( PARTNERSHIP$)|( LP$)|( GP$)", "", inconsistent.recs$contributor.clean)
	
	
	
	rpairs<-tryCatch(rpairs <- compare.dedup(inconsistent.recs[, c("address.clean", "contributor.clean")],
    blockfld = list("address.clean"),
    identity = inconsistent.recs$contribution.id, strcmp = TRUE),
														error = function(e) { "No results" })
	
	if (length(rpairs)==1) {return(data.frame(contributor.clean="", contributor.replacement="", stringsAsFactors=FALSE)[FALSE,])}
	
	inconsistent.recs$contributor.clean<-contributor.clean.save
	rpairs$data$contributor.clean<-contributor.clean.save
	
	rpairs.weights <- epiWeights(rpairs)
	
	rpairs.classified <- epiClassify(rpairs.weights, 
	  threshold.upper=.99999999999999999, threshold.lower=grouping.threshold)
	
	rpairs.classified<-rpairs.classified[rpairs.classified$prediction %in% c("P", "L")] 
	
#	unique.addr<-unique(rpairs.classified$data$address.clean)
#	unique.addr<-which(rpairs.classified$data$address.clean %in% unique.addr)
	
#	df.temp<-rpairs.classified$pairs[rpairs.classified$pairs$id1 %in% unique.addr |
#		rpairs.classified$pairs$id2 %in% unique.addr,]
	
	df.temp<-rpairs.classified$pairs
	
	m<-as.matrix(apply(df.temp[, c("id1", "id2")], MARGIN=2, FUN=as.character))
	if (ncol(m)==1) {m<-t(m)}
	dimnames(m)<-NULL
	
	g<-graph.edgelist(m)
	test<-clusters(g)
	
	g.df<-data.frame(indices=V(g)$name, membership=test$membership,
    contributor=rpairs.classified$data$contributor.clean[as.numeric(V(g)$name)],
	  stringsAsFactors=FALSE)
	
	contributor.freq.agg<-aggregate(g.df$membership, 
	  by=list(membership=g.df$membership, contributor=g.df$contributor),
	  FUN=length)
	
	unique.components<-unique(contributor.freq.agg$membership)
	
	name.replacements.df<-data.frame(component=unique.components, contributor=NA, stringsAsFactors=FALSE)
	
	for (i in unique.components) {
		
		contributor.agg.temp<-contributor.freq.agg[contributor.freq.agg$membership==i,]
		
		contributor.most.freq<-contributor.agg.temp$contributor[
			contributor.agg.temp$x == max(contributor.agg.temp$x) ]
		
		if (length(contributor.most.freq)>1) {
			contributor.most.freq<-contributor.most.freq[
				nchar(contributor.most.freq)==max(nchar(contributor.most.freq))]
		}
		
		if (length(contributor.most.freq)>1) {
			contributor.most.freq<-contributor.most.freq[1]
		}
		
		name.replacements.df$contributor[
		  name.replacements.df$component==i]<-contributor.most.freq
		
	}
	
	name.replacements.df<-merge(name.replacements.df, data.frame(component=test$membership,
    contribution.id=inconsistent.recs$contribution.id[as.numeric(V(g)$name)],
		  stringsAsFactors=FALSE))
	
	colnames(name.replacements.df)[
	  colnames(name.replacements.df)=="contributor"]<-"contributor.replacement"
	
	return(name.replacements.df[, c("contribution.id", "contributor.replacement")])
	
}