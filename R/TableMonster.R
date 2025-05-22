"%,%" <- function(x,y)paste0(x,y)

"if.na.x" <- function (x, x0 = FALSE) 
{
    d.x <- dim(x)
    x. <- c(as.matrix(x))
    y <- rep(x0, length(x.))
    y[!is.na(x.)] <- x.[!is.na(x.)]
    structure(y, dim = d.x)
}

print.TableMonster <-
function (x, extra = NULL, special = NULL, simple = FALSE, dbg = FALSE, ...) 
{
    if (dbg) 
        browser()
    spcl <- FALSE
    spcl.val <- NULL
    if (!missing(special)) {
        spcl.val <- special
        spcl <- TRUE
    }
    m <- match.call()
    m$simple <- m$dbg <- NULL
    ddd <- list()
    nmsddd <- names(m)[-(1:2)]
    n.ddd <- length(nmsddd)
    if (n.ddd > 0) 
        for (k in 1:n.ddd) ddd[[nmsddd[k]]] <- m[[2 + k]]
    x.df <- as.data.frame(x)
    nr <- nrow(x.df)
    nc <- ncol(x.df)
    headings <- tmHeadings(x)
    ctypes <- tmCtypes(x)
    digits <- tmDigits(x)
    displ <- tmDisplay(x)
    caption <- tmCaption(x)
    totals <- tmTotals(x)
    rc.idx <- grep("rowcolor", nmsddd)
    is.rc <- (length(rc.idx) > 0)
    is.extra <- !missing(extra)
    if (is.rc) {
        rowcolor <- ddd[[rc.idx]]
        ddd <- ddd[-rc.idx]
        n.ddd <- n.ddd - 1
        nmsddd <- names(ddd)
    }
    if (is.rc) {
        is.clr <- !is.null(rowcolor$color)
        is.clr.rnm <- !is.null(rowcolor$rownum)
        sum.is <- is.clr + is.clr.rnm
        if (sum.is > 0 && (sum.is < 2)) 
            stop("Specification of row color requires components 'color' and 'rownum' to be set")
        if (is.clr) 
            clr <- rowcolor$color
        if (is.clr.rnm) 
            clr.rnm <- eval(rowcolor$rownum, sys.parent())
    }
    is.tot <- !is.null(totals)
    if (is.tot) 
        if (!is.logical(totals)) 
            stop("Attribute 'totals' must be logical")
    leaves <- sapply(strsplit(names(unlist(headings, recursive = TRUE)), 
        split = ".", fixed = TRUE), FUN = function(x) x[length(x)])
    o <- unique(sapply(strsplit(names(unlist(headings, recursive = TRUE)), 
        split = ".", fixed = TRUE), FUN = function(x) x[1]))
    parents <- if.na.x(xtabs(~sapply(strsplit(names(unlist(headings, 
        recursive = TRUE)), split = ".", fixed = TRUE), FUN = function(x) x[1]))[o], 
        1)
    names(parents) <- o
    n.h <- length(parents)
    depth <- 0 * parents + 1
    is.dpth2 <- setdiff(names(parents), leaves)
    depth[is.dpth2] <- 2
    mxdpth <- max(depth)
    atmxdpth <- which(depth == mxdpth)
    dpth2 <- any(parents > 1)
    h1 <- h1a <- NULL
    if (dpth2) 
        simple <- FALSE
    if (dpth2) {
        h1 <- h1a <- NULL
        fff <- function(i, lns, nms) ifelse(lns[i] > 1, "\\multicolumn{" %,% 
            lns[i] %,% "}{c}{" %,% nms[i] %,% "}", nms[i])
        h1[atmxdpth] <- sapply(atmxdpth, FUN = fff, lns = parents, 
            nms = names(parents))
        h1[setdiff(1:n.h, atmxdpth)] <- ""
        h1 <- paste(h1, collapse = "&") %,% "\\\\\n"
        tt <- cumsum(parents)
        i0 <- tt[atmxdpth - 1] + 1
        i1 <- tt[atmxdpth]
        ni <- length(i0)
        prfx <- "\\cmidrule(r){" %,% i0[1] %,% "-" %,% i1[1] %,% 
            "}"
        bdy <- NULL
        sfx <- "\n"
        if (ni > 1) {
            bdy <- NULL
            if (ni > 2) {
                k.k <- apply(cbind(i0, i1)[2:(ni - 1), , drop = FALSE], 
                  1, FUN = function(x) x[1] %,% "-" %,% x[2])
                bdy <- paste("\\cmidrule(lr){" %,% k.k, collapse = "}") %,% 
                  "}"
            }
            sfx <- "\\cmidrule(l){" %,% i0[ni] %,% "-" %,% i1[ni] %,% 
                "}\n"
        }
        h1a <- prfx %,% bdy %,% sfx
    }
    h2 <- paste(leaves, collapse = "&") %,% "\\\\\n"
    nc2 <- length(leaves)
    prfx <- "\\cmidrule(r){" %,% 1 %,% "-" %,% 1 %,% "}"
    k.k <- sapply(2:(nc2 - 1), FUN = function(x) x %,% "-" %,% 
        x)
    bdy <- paste("\\cmidrule(lr){" %,% k.k, collapse = "}")
    sfx <- "}\\cmidrule(l){" %,% nc2 %,% "-" %,% nc2 %,% "}\n"
    h2a <- ftr <- prfx %,% bdy %,% sfx
    xtbl.call <- as.call(expression(xtable, as.data.frame(x), 
        digits = c(0, digits), align = "ll" %,% paste(rep("r", 
            nc - 1), collapse = "")))
    if (!is.null(displ)) 
        xtbl.call$display <- c("s", displ)
    pr.xtbl.call <- as.call(expression(print, xtbl, hline.after = NULL, 
        include.rownames = FALSE, include.colnames = FALSE, type = "latex"))
    is.lbl <- is.algn <- FALSE
    if (n.ddd > 0) {
        lbl.idx <- grep("label", nmsddd)
        is.lbl <- (length(lbl.idx) > 0)
        if (is.lbl) {
            lbl.val <- ddd[[lbl.idx]]
            ddd <- ddd[-lbl.idx]
            n.ddd <- n.ddd - 1
            nmsddd <- names(ddd)
        }
        algn.idx <- grep("align", nmsddd)
        is.algn <- (length(algn.idx) > 0)
        if (is.algn) {
            algn.val <- eval(ddd[[algn.idx]], sys.parent())
            ddd <- ddd[-algn.idx]
            n.ddd <- n.ddd - 1
            nmsddd <- names(ddd)
        }
        is.ddd <- (n.ddd > 0)
        if (is.ddd) 
            for (k in 1:n.ddd) pr.xtbl.call[[nmsddd[k]]] <- ddd[[nmsddd[k]]]
    }
    if (!spcl) {
        xtbl.call[["caption"]] <- as.name("caption")
        if (is.lbl) 
            xtbl.call[["label"]] <- lbl.val
        if (is.algn) 
            xtbl.call$align <- c("l", algn.val)
        atr <- c("\\toprule\n", h1, h1a, h2, h2a)
        if (is.rc) 
            atr <- c(atr, "\\rowcolor{" %,% clr %,% "}")
        if (is.tot) 
            atr <- c(atr, ftr)
        atr <- c(atr, "\\bottomrule\n")
        add.to.row <- list()
        add.to.row[["command"]] <- atr
        add.to.row[["pos"]] <- list()
        add.to.row[["pos"]][1:2] <- -1
        add.to.row[["pos"]][3:(3 + dpth2 * 2)] <- 0
        if (is.rc) 
            add.to.row[["pos"]][3 + dpth2 * 2 + 1] <- clr.rnm - 
                1
        if (is.tot) 
            add.to.row[["pos"]][3 + dpth2 * 2 + is.rc + 1] <- nr - 
                1
        add.to.row[["pos"]][3 + dpth2 * 2 + is.rc + is.tot + 
            1] <- nr
    }
    if (spcl) {
        if (spcl.val == "jrss-b") {
            btbl <- "\\begin{table}\n"
            cpn <- "\\caption{" %,% caption %,% "}\n"
            if (is.lbl) 
                cpn <- "\\caption{\\label{" %,% lbl.val %,% "}" %,% 
                  caption %,% "}\n"
            ctr <- NULL
            fb <- "\\fbox{%\n"
            btblr <- "l" %,% paste(rep("r", nc2 - 1), collapse = "")
            if (is.algn) 
                btblr <- paste(algn.val, collapse = "")
            btblr <- "\\begin{tabular}{" %,% btblr %,% "}\n"
            etblr <- "\\end{tabular}}\n"
            etbl <- "\\end{table}\n"
            tp <- btbl %,% cpn %,% ctr %,% fb %,% btblr %,% "\\toprule\n"
            atr <- c(tp, h1, h1a, h2, h2a)
            if (is.rc) 
                atr <- c(atr, "\\rowcolor{" %,% clr %,% "}")
            if (is.tot) 
                atr <- c(atr, ftr)
            atr <- c(atr, "\\bottomrule\n", etblr, etbl)
            add.to.row <- list()
            add.to.row[["command"]] <- atr
            add.to.row[["pos"]] <- list()
            add.to.row[["pos"]][1:(3 + dpth2 * 2)] <- 0
            if (is.rc) 
                add.to.row[["pos"]][3 + dpth2 * 2 + 1] <- clr.rnm - 
                  1
            if (is.tot) 
                add.to.row[["pos"]][3 + dpth2 * 2 + is.rc + 1] <- nr - 
                  1
            add.to.row[["pos"]][3 + dpth2 * 2 + is.rc + is.tot + 
                1] <- nr
            add.to.row[["pos"]][3 + dpth2 * 2 + is.rc + is.tot + 
                2] <- nr
            add.to.row[["pos"]][3 + dpth2 * 2 + is.rc + is.tot + 
                3] <- nr
            pr.xtbl.call$only.contents <- TRUE
        }
        if (spcl.val == "aos") {
            btbl <- "\\begin{table}\n"
            cpn <- "\\caption{" %,% caption %,% "}\n"
            if (is.lbl) 
                cpn <- "\\caption{\\label{" %,% lbl.val %,% "}" %,% 
                  caption %,% "}\n"
            ctr <- NULL
            btblr <- "l" %,% paste(rep("r", nc2 - 1), collapse = "")
            if (is.algn) 
                btblr <- paste(algn.val, collapse = "")
            btblr <- "\\begin{tabular}{" %,% btblr %,% "}\n"
            etblr <- "\\end{tabular}\n"
            etbl <- "\\end{table}\n"
            tp <- btbl %,% cpn %,% ctr %,% btblr %,% "\\toprule\n"
            atr <- c(tp, h1, h1a, h2, h2a)
            if (is.rc) 
                atr <- c(atr, "\\rowcolor{" %,% clr %,% "}")
            if (is.tot) 
                atr <- c(atr, ftr)
            atr <- c(atr, "\\bottomrule\n", etblr, etbl)
            add.to.row <- list()
            add.to.row[["command"]] <- atr
            add.to.row[["pos"]] <- list()
            add.to.row[["pos"]][1:(3 + dpth2 * 2)] <- 0
            if (is.rc) 
                add.to.row[["pos"]][3 + dpth2 * 2 + 1] <- clr.rnm - 
                  1
            if (is.tot) 
                add.to.row[["pos"]][3 + dpth2 * 2 + is.rc + 1] <- nr - 
                  1
            add.to.row[["pos"]][3 + dpth2 * 2 + is.rc + is.tot + 
                1] <- nr
            add.to.row[["pos"]][3 + dpth2 * 2 + is.rc + is.tot + 
                2] <- nr
            add.to.row[["pos"]][3 + dpth2 * 2 + is.rc + is.tot + 
                3] <- nr
            pr.xtbl.call$only.contents <- TRUE
        }
    }
    if (dbg) 
        save(list = "add.to.row", file = "debug.rda")
    
    if(is.extra)
    {
      atr <- add.to.row
      atr.c <- atr$command
      atr.p <- atr$pos
      tr.c <- atr.c[1]
      mr.c <- atr.c[substring(atr.c, 2, 5)=="cmid"][1]
      n.atr <- length(atr.c)

      atr.c.new <- atr.c[1:(n.atr-1)]
      atr.p.new <- atr.p[1:(n.atr-1)]
      if(any(names(extra)=="tr"))
      {
          atr.c.new <- c(atr.c.new, rep("\\midrule\n", sum(names(extra)=="tr")))
          atr.p.new <- c(atr.p.new, extra$tr)
      }
      if(any(names(extra)=="mr"))
      {
          atr.c.new <- c(atr.c.new, rep(mr.c, sum(names(extra)=="mr")))
          atr.p.new <- c(atr.p.new, extra$mr)
      }
      atr.c.new <- c(atr.c.new, atr.c[n.atr])
      atr.p.new <- c(atr.p.new, atr.p[n.atr])
      add.to.row$command <- atr.c.new
      add.to.row$pos <- atr.p.new
    }
    
    pr.xtbl.call$add.to.row <- as.name("add.to.row")
    if (is.rc) {
       cat(sprintf("%s\n", "%% Don't forget to \\usepackage{xcolor} and include 'table' in your documentclass options, "))
       cat(sprintf("%s\n", "%% e.g. \\documentclass[table]{beamer}, and remember to define the color, " %,% 
            clr %,% ", in your preamble"))
    }
    xtbl <- eval(xtbl.call)
    eval(pr.xtbl.call)
}

"basic.tmPrint" <-
function(x, special = NULL, simple = FALSE, dbg = FALSE, ...)
{
    mc <- match.call()
    is.cptn <- any(names(mc)=="cptn")
    is.lbl <- any(names(mc)=="lbl")
    nc <- ncol(x)
    m <- dt <- ct <- NULL
    lgnd1 <- c(`numeric`="numeric",`integer`="integer", `factor`="character",`AsIs`="character", `character`="character")
    lgnd2 <- c(`dbl`="n",`int`="n",`c`="c")
    for(k in 1:nc)
    {
      m <- c(m, lgnd1[class(x[,k])])
      dt <- c(dt, 
      switch(m[k],
             "numeric"=ifelse(all(round(x[,k])==x[,k]), "int", "dbl"),
             "integer"="int",
             "character"="c"
             ))
      ct <- lgnd2[dt]
    }
    dig <- c(`dbl`=3, `int`=0, `c`=0)
    hdr <- as.list(rep("", nc))
    names(hdr) <- names(x)
    tmHeadings(x) <- hdr
    tmCtypes(x) <- ct
    tmDigits(x) <- dig[dt]
    if(is.cptn) tmCaption(x) <- eval(mc$cptn)
    class(x) <- "TableMonster"
    pr.call <- as.call(expression(print, x))
    if(is.lbl) pr.call$label <- eval(mc$lbl)
    pr.call$sanitize.text.function <- I
    eval(pr.call)
}
    
"as.data.frame.TableMonster" <-
function(x, row.names = NULL, optional = FALSE, ...)
{
    attr(x, "headings") <- NULL
    attr(x, "ctypes") <- NULL
    attr(x, "digits") <- NULL
    attr(x, "caption") <- NULL
    attr(x, "totals") <- NULL
    class(x) <- "data.frame"
    x
}

"tmHeadings" <-
function(x)
{
  attr(x, "headings")
}

"tmCtypes" <-
function(x)
{
  attr(x, "ctypes")
}

"tmDigits" <-
function(x)
{
  attr(x, "digits")
}

"tmTotals" <-
function(x)
{
  attr(x, "totals")
}

"tmCaption" <-
function(x)
{
  attr(x, "caption")
}

"tmDisplay" <-
function(x)
{
  attr(x, "display")
}

"tmHeadings<-" <-
function(x, value)
{
  attr(x, "headings") <- value
  x
}

"tmCtypes<-" <-
function(x, value)
{
  attr(x, "ctypes") <- value
  x
}

"tmDigits<-" <-
function(x, value)
{
  attr(x, "digits") <- value
  x
}

"tmTotals<-" <-
function(x, value)
{
  attr(x, "totals") <- value
  x
}

"tmCaption<-" <-
function(x, value)
{
  attr(x, "caption") <- value
  x
}

"tmDisplay<-" <-
function(x, value)
{
  attr(x, "display") <- value
  x
}

.onAttach <- function(libname, pkgname)
{
    options(stringsAsFactors=FALSE)
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                    fields="Version")
    msg <- paste(pkgname, ver) %,% "\n\n" %,%
           "Type ?print.TableMonster"
    packageStartupMessage(msg)
}
