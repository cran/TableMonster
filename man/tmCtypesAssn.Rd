\name{tmCtypes<-}
\alias{tmCtypes<-}
\title{
  Assignment function for the 'ctypes' attribute
}
\description{
  Assignment function for the 'ctypes' attribute of an object of class
  'TableMonster'
}
\usage{
  tmCtypes(x) <-value
}
\arguments{
  \item{x}{
    An object of class 'TableMonster'
  }
  \item{value}{
    A vector of length equal to the number of columns in the table
    containing entries "n" or "c" meaning that the corresponding
    column is of mode "numeric" or "character"
  }
}
\details{
  This is a required attribute for an object of class 'TableMonster'
}
\author{Grant Izmirlian}
\references{
  <https://www.youtube.com/watch?v=CM1TaNVnh58>
}


