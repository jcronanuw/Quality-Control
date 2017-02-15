cliplot1 <- read_excel("~/Downloads/clipdata.xlsx")
summary.clipdata<-summary(cliplot1)
####Checking dataset dimensions 
dim(cliplot1)
####Cheking columns that I do not want to use
View(cliplot1)
####Function for the creation of data frames from raw data
empty.df<-function (..., stringsAsFactors = TRUE, Header=TRUE) {
  nargs <- length(args <- list(...))
  if (!nargs) 
    return(as.data.frame(list()))
  if (nargs == 0L) 
    return(as.data.frame(list()))
  Names <- function(x) {if(!is.null(names(x))) names(x) else rep("",length(x))}
  Paste <- function(...) {a<-list(...); r<-do.call("paste", c(list(sep="."),
                                                              a[sapply(a, function(x) !is.character(x) || any(nzchar(x)))]));
  nx <- max(sapply(a, length))
  if (length(r)) return(rep(r, length.out=nx)) else return(rep("", nx))
  }
  contribcols <- sapply(args, function(x) ifelse(class(x)=="data.frame", ncol(x), 1))
  outargs <- sum(contribcols)
  cargs <- vector("list", outargs)
  nmc <- paste0("Var", seq.int(sum(contribcols)))
  nm <- unlist(lapply(seq_along(args), function(x) if(class(args[[x]])=="data.frame") {
    Paste(Names(args)[x], Names(args[[x]])) } else {Names(args)[x]}))
  if (is.null(nm)) 
    nm <- nmc
  else if (any(ng0 <- !nzchar(nm))) 
    nm[ng0] <- nmc[ng0]
  names(cargs) <- make.unique(make.names(nm))
  rep.fac <- 1L
  d <- sapply(args, function(x) ifelse(class(x)=="data.frame", nrow(x), length(x)))
  orep <- prod(d)
  if (orep == 0L) {
    i<-1
    for (a in seq_along(args)) {
      if (contribcols[a]==1) {
        args[[a]]=list(a)
      }
      for(j in seq_len(contribcols[a])) {
        cargs[[i]] <- args[[a]][[j]][FALSE]
        i <- i+1
      }
    }
  } else {    
    i<-1
    for (a in seq_along(args)) {
      nx <- d[a]
      orep <- orep/nx
      x<-args[[a]]
      if (contribcols[a]==1) {
        x<-list(x)
      }
      for(j in seq_len(contribcols[a])) {
        y <- x[[j]]
        y <- y[rep.int(rep.int(seq_len(nx), rep.int(rep.fac, 
                                                    nx)), orep)]
        if (stringsAsFactors && !is.factor(y) && is.character(y)) 
          y <- factor(y, levels = unique(y))
        cargs[[i]] <- y
        i <- i+1
      }
      rep.fac <- rep.fac * nx
    }
  }
  rn <- .set_row_names(as.integer(prod(d)))
  structure(cargs, class = "data.frame", row.names = rn)}
#####Function parameters

  