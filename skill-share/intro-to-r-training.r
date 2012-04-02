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


# CONCEPTUAL FRAMEWORK OF R

# There are two types of things in R:
# 1. Objects
# 2. Functions

# Functions are used to operate on objects, usually to create a new object
# or transform the original object

# Let's make a matrix to demonstrate how to use a function.
# The function c() is a basic function that groups together numbers 
# or character strings. 
# The "<-" sign ("less than" sign, minus sign) will assign our group
# of numbers to a object name. An object name is used to 
# succinctly refer to or "call" data. We'll call this "mat.input.v"

mat.input.v <- c(9, 56, 72, 4, 862, 263, 93, 49, 152)

c(9, 56, 72, 4, 862, 263, 93, 49, 152)

mat.input.v

# rm(mat.input.v)

# The matrix() function is used to form a matrix. Let's look at its help file

?matrix

sample.data.mat <- matrix(data=mat.input.v, nrow=3)

sample.data.mat

# Let's change the default argument for "byrow"

sample.data.mat <- matrix(data=mat.input.v, nrow=3, byrow=TRUE)

sample.data.mat

# TYPES OF OBJECTS
# There are four basic types of objects in R.
# You have already seen two of them: a vector and a matrix.
# Let's explore the vector in more detail.

# Much of the terminology is R is based on linear algebra concepts
# So an ordered set of numbers or character strings is called a vector
# There are many ways to create a vector or extract a vector from 
# another object. We used the c() function to create mat.input.v

mat.input.v

# We can use the ":" operator to create a vector of all the integers
# that exist between two other integers.

one.hundred.v<-1:100

one.hundred.v

# Let's look at a way to extract a vector from a dataset. 
# The read.csv() function is going to be your primary way to get
# A dataset into your R workspace

net.worth.df<-read.csv("http://www.opensecrets.org/db2dl/?q=PFDsWorth&cycle=2008&output=CSV", stringsAsFactors=FALSE)

# ?read.csv

# You do not want to deal with factors until you get good at R, so we will not
# convert character strings to factors.

# What did we just load? There are two main ways to examine what we have.

View(net.worth.df)

str(net.worth.df)

ls()

# What if we want to just extract one column and make it into a vector?
# There are two ways to do this. One way is:

congress.ppl.names.v<-net.worth.df$Name

# str() is indispensible when working your way through a data project.
# It can be used on any type of object, including a vector

str(congress.ppl.names.v)

# Another way to extract the column is:

congress.ppl.names.v<-net.worth.df[, "Name"]

# If we are unsure if two operations produce the exact same object,
# we can use identical()

identical(net.worth.df$Name, net.worth.df[, "Name"])

# What is up with these square brackets? Recall that R concepts are 
# usually based on linear algebra. This is subscript notation, and
# it is a key concept in R that you will use constantly. It is saying:
# object.name[select row(s), select column(s)]
# Rows are always before the comma and columns after.
# If the space before or after the comma is blank, R assumes that you
# want all the rows or all the columns

# Unlike with the "$" operator, subscripts can be used to get more than 
# one column from a dataframe, although this makes a dataframe
# rather than just a vector

name.and.chamber.df<-net.worth.df[, c("Name", "AvgValue")]
	
str(name.and.chamber.df)

# You can select rows and columns by using numeric vectors or 
# logical vectors. Character vectors as mostly just for getting columns

# First 5 rows
net.worth.df[1:5, c("Name", "AvgValue")]

# Only get those observations/records where the net worth is
# greater than 
net.worth.df[net.worth.df$AvgValue>100000000, c("Name", "AvgValue")]

net.worth.df[net.worth.df$AvgValue>100000000, ]

# What is going on here? First a logical vector is produced and then 
# it is fed into the subscript 
net.worth.df$AvgValue>100000000

# Not only will subscripting get you only the rows that you request,
# but if you use a numeric vector it will get you the rows in that order
net.worth.df[1:4, c("Name", "AvgValue")]
net.worth.df[4:1, c("Name", "AvgValue")]

net.worth.df[4:1, c("AvgValue", "Name")]

# This feature is used to sort datasets by columns, using order()
net.worth.df[order(net.worth.df$Name), c("Name", "AvgValue")]

# WARNING: sort() is something different

# You can also use the subset() function for these types of selection
# operations, but I don't recommend it.

# Almost every object or result of a function can be used as an input
# for a function. The standard parethetical order of operations prevails,
# i.e. the innermost function is evaluated first.

nrow(net.worth.df)
nrow(net.worth.df)/100
round(nrow(net.worth.df)/100)
rep("test", times=round(nrow(net.worth.df)/100))


tt<-nrow(net.worth.df)
mm<-tt/100
bb<-round(mm)
rep("test", times=bb)

# Let's stop for a sec to note some important details:
# 1. All object names and functions are case-sensitive
# sample.df does not equal Sample.df

one.hundred.v
One.Hundred.V
	
# 2. Allowable object and function names can include letters,
# numbers, periods, and underscores. No spaces are allowed in these names
# 3. Adding ".df", ".mat", or ".v" is unnecessary. This is my personal
# style. I find it useful since you often have the same data in different
#	forms and keeping track of what all your objects are can be hard without
# these identifiers
# 4. Spaces between object names and operators are ignored. Spaces
# are useful for making your code clear and readable to others and yourself

matrix(data=mat.input.v, nrow=3)
# is the same as
matrix(  data=mat.input.v  , nrow =  3)

# 5. If R thinks that a statement continues on the next line,
# it will interpret it as one statement on one line
matrix(data=mat.input.v, 
  nrow=3)

TRUE & TRUE & FALSE & 
	FALSE

2+2  log(6)

# Flow control features

if (nrow(net.worth.df)>400) {
  cat("yup")
}

if (nrow(net.worth.df)<400) {
	cat("yup")
} else {
  cat("nope")
}


for ( i in 1:nrow(net.worth.df)) {
  cat("Print this", i, "\n")
}

# R has a full suite of flow control features like while, break, next, etc.

# EXAMPLE ANALYSIS

table(net.worth.df$chamber)

net.worth.tab<-table(net.worth.df$chamber)

barplot(net.worth.tab, col="blue")

net.worth.df$more.than.10.million<-net.worth.df$AvgValue>10000000

multi.dem.tab<-table(net.worth.df$chamber, net.worth.df$more.than.10.million)

as.data.frame.table(multi.dem.tab)


net.worth.agg<-aggregate(net.worth.df$AvgValue/1000000, 
							   by=list(net.worth.df$chamber), FUN=sum)

net.worth.agg<-net.worth.agg[order(net.worth.agg$x, decreasing=TRUE), ]

barplot(height=net.worth.agg$x, names.arg=net.worth.agg$Group.1)

net.worth.agg<-aggregate(net.worth.df$AvgValue/1000000, 
												 by=list(net.worth.df$chamber), FUN=mean)

barplot(height=net.worth.agg$x, names.arg=net.worth.agg$Group.1)

??"standard deviation"

getwd()

setwd()


png("Net Worth by Chamber.png", height=900)

barplot(height=net.worth.agg$x, names.arg=net.worth.agg$Group.1)

dev.off()


		
chamber.worth.lm<-lm(net.worth.df$AvgValue ~ net.worth.df$chamber)

chamber.worth.lm

summary(chamber.worth.lm)



install.packages("geosphere")

library(geosphere)

??"standard deviation"

ls()

write.csv(net.worth.df, "net worth.csv")

# Other issues
# Scalar is a single number
# Do not use attach(), even though many intro manuals ask you to use it
# to get data from an xls, just save it as a csv in Excel

# WHERE TO GO FROM HERE:
# http://rseek.org/
# http://stackoverflow.com/questions/tagged/r
# http://www.statmethods.net/
# http://www.r-bloggers.com/
# http://stackoverflow.com/questions/192369/books-for-learning-the-r-language/2270793#2270793





