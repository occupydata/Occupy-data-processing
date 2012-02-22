

split.csv <- function(input.file, output.dir, row.num,  output.stem="output") {

  x <- read.csv(input.file, stringsAsFactors=FALSE)
  
  cutoffs <- c( seq(from=0, to=nrow(x), by=row.num), nrow(x) )
    
  for (i in 1:(length(cutoffs)-1)) {
    
    write.csv(x[(cutoffs[i]+1):cutoffs[i+1], ], 
      paste(output.dir, output.stem, i, ".csv", sep=""), 
    	row.names=FALSE)
  	
    cat (paste(output.dir, output.stem, i, ".csv\n", sep=""))
    
  }
	
  NULL
  
}

# EXAMPLE BELOW
split.csv("/YOUR/INPUT/PATH/AND/FILE/NAME/HERE.csv", "/YOUR/INPUT/DIRECTORY/HERE/", 200)

duplicate.row <- function(x, num.copies, output.file, which.rows="all", which.columns="all") {

  if (which.rows=="all") {which.rows<-1:nrow(x)}
  if (which.columns=="all") {which.columns<-TRUE}

  output.df<-x[FALSE, which.columns]

  for ( i in 1:length(which.rows)) {
  
  	output.df[(nrow(output.df)+1):(nrow(output.df)+num.copies[i]), which.columns] <-
  	  x[i, which.columns]
cat((nrow(output.df)+1):num.copies[i], "\n")
  }

  write.csv(output.df, output.file, row.names=FALSE)

NULL

}

# which.columns SHOULD BE CHARACTER VECTOR
# which.rows SHOULD BE NUMERICAL VECTOR OR DEFAULT OF "all"
# EXAMPLE BELOW

test.df<-read.csv("/YOUR/INPUT/PATH/AND/FILE/NAME/HERE.csv", stringsAsFactors=FALSE)

test.df<-test.df[1:10, ]

duplicate.row(test.df, test.df$geoID, "/YOUR/OUTPUT/PATH/AND/FILE/NAME/HERE.csv", which.columns=c("title", "latitude", "longitude"))

