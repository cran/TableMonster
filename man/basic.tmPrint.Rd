\name{basic.tmPrint}
\alias{basic.tmPrint}
\title{
  Simple Call to print.TableMonster
}
\description{
  For a generic table ready data.frame, 'x', the call basic.tmPrint(x)
  produces booktabs style latex table suitable for publication
}
\usage{
  basic.tmPrint(x, special = NULL, simple = FALSE, dbg = FALSE, ...)
}
\arguments{
  \item{x}{
    A table ready data.frame
}
  \item{special}{
    Special arguments to print.TableMonster. See object documentation.
}
  \item{simple}{
    The remnants of a call to the print method, e.g. if x is a table
    ready data.frame then
    
    print(x, simple=TRUE)

    calls this function.
    
}
  \item{dbg}{
    set to a value >= 1 for debugging
}
  \item{\dots}{
    other arguments to print.TableMonster
}
}
\value{
  An invisible version of the argument 'x'
}
\author{
  Grant Izmirlian
}
