
Poblacion_dist = matrix( 
c("MADRIZ", 0.3, 
"BARCCELONA", 0.25, 
"BILAO", 0.135, 
"SEVILA", 0.1, 
"VALENCCIA", 0.1, 
"CORUNA", 0.025, 
"ALACANNTE",0.025, 
"VALLADOLI", 0.025, 
"SANTIAGNO", 0.01, 
"SAN SEBASIAN", 0.01, 
"CADIZA", 0.01, 
"ZARAGOZ", 0.01), 
ncol = 2, byrow=T) 

# True locations 
Poblacion = matrix( 
c("MADRID", 0.3, 
"BARCELONA", 0.25, 
"BILBAO", 0.135, 
"SEVILLA", 0.1, 
"VALENCIA", 0.1, 
"CORUÑA", 0.025, 
"ALICANTE",0.025, 
"VALLADOLID", 0.025, 
"SANTIAGO", 0.01, 
"SAN_SEBASTIAN", 0.01, 
"CADIZ", 0.01, 
"ZARAGOZA", 0.01), 
ncol = 2, byrow=T) 

muestrear = function(que, cuantas_veces){ 
	sample(que[,1], prob = as.numeric(que[,2]), cuantas_veces) 
} 

Provincias = ((replicate(10,c(muestrear(Poblacion,1), c(muestrear(Poblacion_dist,1)))))) 


# now we have a list with 20 locations 
Provincias = Provincias[1:length(Provincias)] 

# next we need to process each location as a set of trigrams 
word2trigram = function(word){ 
	trigramatrix =  matrix(c(seq(1, nchar(word)-2), seq(1, nchar(word)-2)+2), ncol = 2, byrow = F) 
	trigram = c() 
	for (i in 1:nrow(trigramatrix)) { 
		trigram = append(trigram,substr(word,trigramatrix[i,1],trigramatrix[i,2])) 
	} 
	return(trigram) 
} 
Prov2trigram = lapply(Provincias, word2trigram) 

# every trigram in the sample 
Trigrams = levels(factor((unlist(Prov2trigram)))) 

# we get how many times appears a trigram in a location 
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
attr(rslt.dst.mat , "dimnames")<-list(Provincias , Provincias ) 
plot(hclust(as.dist(1-rslt.dst.mat),method = 'med')) 



hc <- hclust(as.dist(1-rslt.dst.mat)  ,method = 'med')

hc$height <- sort(jitter(hc$height))

#cutree(hc, k=1:5) #k = 1 is trivial
cutree(as.hclust(hc), h=.4)


