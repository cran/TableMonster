\name{print.TableMonster}
\alias{print.TableMonster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Easy Generation of 'booktab' tables}
\description{Provides a user friendly interface to generation of booktab style
  tables using xtable.}
\usage{
\method{print}{TableMonster}(x, extra = NULL, special = NULL, simple = FALSE, dbg = FALSE, ...)
}
\arguments{
  \item{x}{an object of class 'TableMonster' -- see below}
  \item{extra}{If you want a solid line (tr) or centered midlines (cr) anywhere in
    the body of the table then use this list argument, with a 'tl' for
    each solid and a 'mr' for each centered midline in the same format
    ast the top, e.g.  extra=list(tr=11, cr=13) will put one of each
    after body lines 11 and 13.}
  \item{special}{Optionally, one of the following: 'aos' or 'jrss-b', to
    produce tables compatible with the style guid of the Annals of
    Statistics or JRSS-B, respectively.}
  \item{simple}{Set to 'TRUE' to override the default treatment of
    multi-level tables}
  \item{dbg}{Set to 'TRUE' and the routine will output intermediate
    results to a file 'debug.rda' containing the computed results of
    the list 'add.to.row' which is passed to the function print.xtable.}
  \item{\dots}{1. Optionally, \code{label}, of type character, giving the
    name of the latex label name associated with the table for
    crossreference within the latex document. 2. Optionally
    \code{special}, a charcter string taking the value "jrss-b" or
    "aos". 3. Optionally \code{rowcolor}, a list of the form
    list(color="yellow", rownum=5), for highlighting a particular
    row. You must remember to \\usepackage\{xcolor\} and include 'table'
    in your documentclass options, e.g. \\documentclass[table]\{beamer\},
    and of course, define the color 'yellow' in your preamble.
    Finally, any named arguments accepted by \code{print.xtable}
    are accepted.}
}
\author{Grant Izmirlian \email{izmirlian@astrazeneca.com}}
\examples{
## Example 1: A table with a single heading
##
library(TableMonster)
tst <- as.data.frame(cbind(rep(c("John","Joe","Mary","Jane","Alex"), 2),
                     rep(c("male","male","female","female","female"), 2),
                     rep(c(12345, 54321, 46943, 23123, 51234), 2)))


hdngs <- as.list(rep("", 3))
names(hdngs) <- c("Name", "Gender", "Student ID")

tmHeadings(tst) <- hdngs
tmCtypes(tst) <- rep("n",3)
tmDigits(tst) <- rep(0,3)
tmCaption(tst) <-"This is JUST a TEST"

class(tst) <- "TableMonster"

tst

print(tst, label="tbl:anexample")
print(tst, include.rownames=FALSE, sanitize.text.function=I)
print(tst, label="tbl:anexample", include.rownames=FALSE, sanitize.text.function=I)

## Example 2: A table with a two level heading
##
library(TableMonster)
gp <- rep(1:2,each=5)
m1 <- rnorm(10)
s1 <- (rchisq(10, df=1)/10)^0.5
z1 <- m1/s1
m2 <- rnorm(10)
s2 <- (rchisq(10, df=1)/10)^0.5
z2 <- m2/s2
m3 <- rnorm(10)
s3 <- (rchisq(10, df=1)/10)^0.5
z3 <- m3/s3

foo <- as.data.frame(list(variable=letters[sample(10)], group=gp, model1=m1, se1=s1, Z1=z1,
                                                             model2=m2, se2=s2, Z2=z2,
                                                             model3=m3, se3=s3, Z3=z3))

tmHeadings(foo) <- list('Variable'="", 'Group'="",
                        'Model 1'=list('Estimate'="", 'Std Err'="", 'Wald Test'=""),
                        'Model 2'=list('Estimate'="", 'Std Err'="", 'Wald Test'=""),
                        'Model 3'=list('Estimate'="", 'Std Err'="", 'Wald Test'=""))
tmCaption(foo) <- "This is TableMonster (TM)!!!"
                             
tmCtypes(foo) <- c("c","c",rep("n",9))
tmDigits(foo) <- c(0, 0, rep(3, 9))

class(foo) <- "TableMonster"

print(foo, rowcolor=list(color="yellow", rownum=7))
}
