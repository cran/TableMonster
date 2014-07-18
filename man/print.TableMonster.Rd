\name{print.TableMonster}
\alias{print.TableMonster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Easy Generation of 'booktab' tables}
\description{Provides a user friendly interface to generation of booktab style
  tables using xtable.}
\usage{
\method{print}{TableMonster}(x, ...)
}
\arguments{
  \item{x}{an object of class 'TableMonster' -- see below}
  \item{\dots}{additional arguments -- currently unused}
}
\author{Grant Izmirlian}

\examples{
## Example 1: A table with a single heading
##
library(TableMonster)
tst <- as.data.frame(cbind(rep(c("Joe","Shmoe","Out","The","Doe"), 2),
                     rep(c("PFC","SGT","MJR","LTE","SHMK"), 2),
                     rep(c(12345, 54321, 46943, 23123, 51234), 2)))

attr(tst,"headings") <- list("","","")
names(attr(tst,"headings")) <- c("Name", "Rank", "Serial Number")
attr(tst,"ctypes") <- rep("n",3)
attr(tst,"digits") <- rep(0,3)
attr(tst,"caption")<-"This is JUST a TEST"
class(tst) <- "TableMonster"
tst

## Example 2: A table with a two level heading
##
library(TableMonster)
gp <- rep(1:2,each=5)
m1 <- rnorm(10)
s1 <- rchisq(10, df=1)/10
z1 <- m1/s1
m2 <- rnorm(10)
s2 <- rchisq(10, df=1)/10
z2 <- m2/s2
m3 <- rnorm(10)
s3 <- rchisq(10, df=1)/10
z3 <- m3/s3

foo <- as.data.frame(list(variable=letters[sample(10)], group=gp, model1=m1, se1=s1, Z1=z1,
                                                             model2=m2, se2=s2, Z2=z2,
                                                             model3=m3, se3=s3, Z3=z3))

attr(foo, "headings") <- list('Variable'="", 'Group'="",
                              'Model 1'=list('Estimate'="", 'Std Err'="", 'Wald Test'=""),
                              'Model 2'=list('Estimate'="", 'Std Err'="", 'Wald Test'=""),
                              'Model 3'=list('Estimate'="", 'Std Err'="", 'Wald Test'=""))
attr(foo, "caption") <- "This is TableMonster (TM)!!!"
                             
attr(foo,"ctypes") <- c("c","c",rep("n",9))
attr(foo,"digits") <- c(0, 0, rep(3, 9))

class(foo) <- "TableMonster"

foo

}
