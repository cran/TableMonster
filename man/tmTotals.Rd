\name{tmTotals}
\alias{tmTotals}
\title{
  Gets the 'totals' attribute
}
\description{
  Gets the 'totals' attribute of an object of class 'TableMonster'
}
\usage{
  tmTotals(x)
}

\arguments{
  \item{x}{
    An object of class 'TableMonster'
  }
}
\details{
  This attribute is optional and is only used when you have a table
  in which you want to put a single row of column totals (or anything
  else) below the bottom line.
}
\value{
  A numeric or character vector of length equal to the number of columns
  in the table
}
\author{Grant Izmirlian}
