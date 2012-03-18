
# TO DO: Capitalization may be a problem (i.e. all caps)

fix.inconsistent.text<-function(inconsistent.strings, grouping.threshold=0.35, exact.digits=TRUE, plot.clusters=FALSE) {

inconsistent.strings.tab<-table(inconsistent.strings)

inconsistent.strings.uniq<-unique(names(inconsistent.strings.tab))
# inconsistent.strings.uniq<-inconsistent.strings

# next we need to process each string as a set of trigrams 
word2trigram = function(word){ 
	trigramatrix =  matrix(c(seq(1, nchar(word)-2), seq(1, nchar(word)-2)+2), ncol = 2, byrow = F) 
	trigram = c() 
	for (i in 1:nrow(trigramatrix)) { 
		trigram = append(trigram,substr(word,trigramatrix[i,1],trigramatrix[i,2])) 
	} 
	return(trigram) 
} 

Prov2trigram = lapply(inconsistent.strings.uniq, word2trigram) 

# every trigram in the sample 
Trigrams = levels(factor((unlist(Prov2trigram)))) 

# we get how many times a trigram appears in a string
ocrrnc.mtrx = matrix(rep(0,length(Trigrams)* length(Prov2trigram)), ncol = length(Prov2trigram)) 
for (i in 1:ncol(ocrrnc.mtrx)) { 
	ocrrnc.mtrx[,i] = as.integer(table(append(Prov2trigram[[i]], Trigrams))-1) 
} 

# calculate cosine (often used in NLP) 
matrizCos = function(X){ 
    X  = t(X ) 
    nterm = nrow(X ) 
    modulo = c() 
    cosen = matrix(rep(0,(nterm*nterm)),ncol = nterm) 
    for (i in 1:nterm){ 
        Vec = X [i,] 
        modulo[i] = sqrt(Vec%*%Vec) 
        cosen[,i] = (X  %*% Vec) 
    } 
    cosen = (cosen/modulo)/matrix(rep(modulo,nterm),ncol = nterm,byrow=T) 
    cosen[is.nan(cosen)] <- 0 
    return (cosen) 
} 
rslt.dst.mat = matrizCos(ocrrnc.mtrx) 

# and get the clusters 
attr(rslt.dst.mat , "dimnames")<-list(inconsistent.strings.uniq , inconsistent.strings.uniq ) 

hc <- hclust(as.dist(1-rslt.dst.mat), method = "single")

if (plot.clusters) { plot(hc, cex=.5) }
# TO DO: adjust cex according to length of input string

hc.cut<-cutree(hc, h=grouping.threshold)

ret.df<-data.frame(inconsistent=names(hc.cut), consistent=NA, stringsAsFactors=FALSE)

for ( i in 1:length(unique(hc.cut))) {

	match.strings.tab<-inconsistent.strings.tab[names(inconsistent.strings.tab) %in% 
	  names(hc.cut)[i==hc.cut]]

  if (length(which(match.strings.tab == max(match.strings.tab)))==1) {
  
  	ret.df$consistent[ret.df$inconsistent %in% names(match.strings.tab)]<-
  		names(match.strings.tab[which.max(match.strings.tab)])
  	
  } else {
  	
    max.names<-names(match.strings.tab)[which(match.strings.tab == max(match.strings.tab))]
    max.names.char<-max.names[nchar(max.names) == max(nchar(max.names))]
    ret.df$consistent[ret.df$inconsistent %in% names(match.strings.tab)]<-
    	  sample(max.names.char, 1)
  }
}

if (exact.digits) {
	
	revert.v<-gsub("[^0-9]", "", ret.df$inconsistent)!=gsub("[^0-9]", "", ret.df$consistent)
	
	ret.df$consistent[revert.v]<-ret.df$inconsistent[revert.v]
	
}

ret.df<-merge(data.frame(inconsistent=inconsistent.strings,
  orig.order=1:length(inconsistent.strings), stringsAsFactors=FALSE), ret.df, all=TRUE)

ret.df[order(ret.df$orig.order), c("orig.order", "inconsistent", "consistent")]

}

