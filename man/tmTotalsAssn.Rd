\name{tmTotals<-}
\alias{tmTotals<-}
\title{
  Assignment function for the 'totals' attribute
}
\description{
  Assignment function for the 'totals' attribute of an object of class
  'TableMonster' 
}
\usage{
  tmTotals(x) <- value
}
\arguments{
  \item{x}{
    An object of class 'TableMonster'
  }
  \item{value}{
    The 'totals' attribute, a numeric or character vector of length
    equal to the number of columns in the table.
  }
}
\details{
  This attribute is optional and is only used when you have a table
  in which you want to put a single row of column totals (or anything
  else) below the bottom line.
}
\author{Grant Izmirlian}
\references{
  <https://www.youtube.com/watch?v=CM1TaNVnh58>
}
