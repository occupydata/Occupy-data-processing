
library(stringr)

journal.links.html<-readLines("http://www.mass.gov/legis/journal/187/12index.htm")

link.leaves<-unlist(str_extract_all(journal.links.html, "[^j]sj[0-9]{6}[.]htm"))
link.leaves<-gsub("\"", "", link.leaves)

links.to.follow<-paste("http://www.mass.gov/legis/journal/187/", link.leaves, sep="")



# vote.record.df<-data.frame(reps="", stringsAsFactors=FALSE)[FALSE, , drop=FALSE]

vote.record.df<-data.frame(reps="", vote="", roll.call="", stringsAsFactors=FALSE)[FALSE, ]

#target.link<-links.to.follow[17]
#target.link<-"http://www.mass.gov/legis/journal/187/sj020912.htm"

for ( target.link in links.to.follow) {

journal.html<-readLines(target.link)
# http://www.mass.gov/legis/journal/187/sj011212.htm
# http://www.mass.gov/legis/journal/187/sj030812.htm


#first.lines<-which(gsub("([[:space:]])|(<.*>)", "", journal.html)=="YEAS")
first.lines<-grep("YEAS", journal.html)

if (length(first.lines)==0) {next}

p.lines<-grep("</p>", journal.html)

tab.lines<-grep("</table>", journal.html)




bill.num.lines<-grep("[(]Senate, No[.] [0-9]+", journal.html)

roll.call.num.lines<-grep("[[]Yeas and Nays No[.] [0-9]+[]]", journal.html)

#grep("[[]Yeas and Nays No[.] [0-9]+[]]", journal.html[427])


session<-"test"

for ( i in first.lines) {
	
	target.bill.num<-str_extract_all(journal.html[
		max(bill.num.lines[bill.num.lines<i])], "[(]Senate, No[.] [0-9]+[)]")[[1]]
	target.bill.num<-gsub("[^0-9]", "", target.bill.num)
	
	if (length(target.bill.num)==0 | i-max(bill.num.lines[bill.num.lines<i]) > 10) {
		target.bill.num<-NA
	}
	
	target.roll.call.num<-str_extract_all(journal.html[
		max(roll.call.num.lines[roll.call.num.lines<i])], "[[]Yeas and Nays No[.] [0-9]+[]]")[[1]]
	target.roll.call.num<-gsub("[^0-9]", "", target.roll.call.num)
	
	vote.name<-paste("s", session, ".b", target.bill.num, ".r", target.roll.call.num, sep="")
	
	last.line<-min(p.lines[p.lines>i])
	
	table.format<-any(grepl("<tr", journal.html[i:last.line]) )
	
	if ( (last.line-i) < 10 | table.format ) {
	  last.line<-min(tab.lines[tab.lines>i]) 
	}
	
	roll.call<-journal.html[i:last.line]
	
	naked.line.num<-which(!grepl("<br>", roll.call) & grepl("^ +[A-Z]", roll.call))
	
	naked.lines<-roll.call[naked.line.num]
	naked.lines<-gsub("(^ )|( $)", "", naked.lines)
	
	roll.call[naked.line.num-1]<-
	  paste(roll.call[naked.line.num-1], naked.lines, sep=" ")
	
	roll.call<-roll.call[grepl("(<)|(>)", roll.call)]
	
	roll.call<-unlist(strsplit(roll.call, "\t"))
	
#	voting.sets<-grep("(YEAS)|(NAYS)|(ANSWERED.*PRESENT)|(ABSENT OR NOT VOTING)", roll.call)
	voting.sets<-grep("[A-Z]{4,}", roll.call)
	
	vote.record.temp.df<-data.frame(reps=roll.call[-voting.sets], stringsAsFactors=FALSE)
	
	vote.record.temp.df$vote<-NA
	
	for ( j in 1:length(voting.sets)) {
		
		voting.set.end<-voting.sets[j+1]
		if(j==length(voting.sets)) {voting.set.end<-length(roll.call)}
		
		voting.label<- roll.call[voting.sets[j]]
		vote.record.temp.df$vote[vote.record.temp.df$reps %in% roll.call[voting.sets[j]:voting.set.end]]<-voting.label
		
	}
	
	
	if (table.format) {
	
		vote.record.temp.df[, 1]<-
			sapply(str_extract_all(vote.record.temp.df[, 1], ">[^<]+"), FUN=function(x) {paste(x, collapse="")} )
		vote.record.temp.df[, 2]<-
			sapply(str_extract_all(vote.record.temp.df[, 2], ">[^<]+"), FUN=function(x) {x[1]} )
	
	} else {
		
  	vote.record.temp.df[, 1]<-
	    sapply(str_extract_all(vote.record.temp.df[, 1], "(>[^<]+)|([^>]+<)|(^[^<>]*$)"), FUN=function(x) {paste(x, collapse="")} )
  	vote.record.temp.df[, 2]<-
	  	sapply(str_extract_all(vote.record.temp.df[, 2], "(>[^<]+)|([^>]+<)"), FUN=function(x) {x[1]} )
	
	}
	
	vote.record.temp.df[, 1]<-gsub("(<)|(>)|(&[A-Za-z]+)", "", vote.record.temp.df[, 1])
	vote.record.temp.df[, 2]<-gsub("(<)|(>)|(&[A-Za-z]+)", "", vote.record.temp.df[, 2])
	
	vote.record.temp.df<-vote.record.temp.df[grepl("[[:alpha:]]", vote.record.temp.df[, 1]), ]
	
	vote.record.temp.df[, 1]<-gsub("([0-9]+[.])|(;)", "", vote.record.temp.df[, 1])	
	vote.record.temp.df[, 1]<-gsub("(^ +)|( +$)", "", vote.record.temp.df[, 1])
	vote.record.temp.df[, 1]<-gsub(" {2,}", " ", vote.record.temp.df[, 1])
	
#	names(vote.record.temp.df)[2]<-vote.name
	vote.record.temp.df$roll.call<-vote.name
	
#	vote.record.df<-merge(vote.record.df, vote.record.temp.df, all=TRUE)
	vote.record.df<-rbind(vote.record.df, vote.record.temp.df, all=TRUE)
	
}

}


#journal.html[gsub("([[:space:]])|(<.*>)", "", journal.html)=="YEAS"]

vote.record.no.dups.df<-vote.record.df[!duplicated(vote.record.df), ]



target.roll.call<-"00200"
target.roll.call<-"00020"
target.roll.call<-"00130"

download.file(url=paste("http://www.mass.gov/legis/journal/RollCallPdfs/187/", 
							target.roll.call, ".pdf", sep=""),
							destfile=paste(work.dir, "temp.pdf", sep=""))

system(paste("pdftotext -layout ", paste(work.dir, "temp.pdf", sep="")))

roll.call.txt<-readLines(paste(work.dir, "temp.txt", sep=""))

if (length(roll.call.txt)<40) {

  system(paste("convert -depth 8 -density 400 -units PixelsPerInch -type Grayscale +compress", 
  	paste(work.dir, "temp.pdf", sep=""),
    paste(work.dir, "temp.tif", sep="")))

  system(paste("tesseract ", work.dir, "temp.tif ", work.dir, "temp -l eng -psm 6", sep=""))
  
  roll.call.txt<-readLines(paste(work.dir, "temp.txt", sep=""))

}

roll.call.txt

House, No. 3535

for 

str_extract_all(

strsplit(roll.call.txt[20], "(^Y )|( Y )|(^N )|( N )|(^X )|( X )|(^P )|( P )")


600

brew tesseract
brew link tesseract

sudo port install tesseract
put http://tesseract-ocr.googlecode.com/files/eng.traineddata.gz
into /opt/local/share/tessdata/




system

10
