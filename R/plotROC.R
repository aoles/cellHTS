ROC = function(x, positives, negatives) {
##'positives' and 'negatives' is a vector of characters specifying the name of the controls

  if(!"score" %in% names(x))
    stop("Please score 'x' (using for example the function 'calcZscore') before trying to calculate ROC.")


# default
assayType = "one-way assay"
score = x$score

### correct from this point forward (5.09.2006)
  if (!missing(positives)) {
## checks
      if(!is(positives, "list")){
        ## check
        if (!is(positives, "vector") | length(positives)!=1 | mode(positives)!="character") 
          stop("'positives' should be a vector of regular expressions with length one.")

    }else{
        if (length(positives)!=2 ||
            !identical(sort(names(positives)), c("act", "inh")) ||
            any(sapply(positives, length)!=1) ||
            any(sapply(positives, mode)!="character"))#* 
          stop(cat("'positives' should be a list with 
             two components: 'act' and 'inh'.\n Or a vector of regular expressions with lenght one.\n"))

        positives = paste(positives, collapse="|")
        score = abs(x$score) # because this is a two way assay
        assayType = "two-way assay"

}## else is list

    }else{## if !missing
## assumes the screen is a one-way assay
      positives = "^pos$"
    }


    if (!missing(negatives)) {
      ## check
      if (!is(negatives, "vector") | length(negatives)!=1 | mode(negatives)!="character") 
        stop("'negatives' should be a vector of regular expressions with length one")


    } else {
      negatives = "^neg$"
    }


  wellAnno = as.character(x$wellAnno)
  xpos =NULL
  xneg=NULL

  xpos = regexpr(positives, wellAnno, perl=TRUE)>0
  xneg = regexpr(negatives, wellAnno, perl=TRUE)>0

  if(!any(xneg))
    stop("Negative controls not found")
    #stop(sprintf("The 'wellAnno' slot does not contain any entries with value '%s'.", negatives))
  if(!any(xpos))
    stop("Positive controls not found")
    #stop(sprintf("The 'wellAnno' slot does not contain any entries with value '%s'.", positives))


  
  br = unique(quantile(score, probs=seq(0, 1, length=1001), na.rm=TRUE))
  ct  = cut(score, breaks=br)
  spNeg = split(xneg, ct)
  spPos = split(xpos, ct)
  nNeg = sapply(spNeg, sum)
  nPos = sapply(spPos, sum)
  stopifnot(all(names(nPos)==names(nNeg)))

  posNames = unique(wellAnno[xpos])
  posNames = x$plateConf$Content[match(posNames, tolower(x$plateConf$Content))]
  negNames = unique(wellAnno[xneg])
  negNames = x$plateConf$Content[match(negNames, tolower(x$plateConf$Content))]


  x = list(TP = cumsum(rev(nPos)),
           FP = cumsum(rev(nNeg)),
           positives = positives,
           negatives = negatives, 
           posNames = posNames,
           negNames = negNames,
           assayType = assayType)
  class(x) = "ROC"
  return(x) 
}


plot.ROC = function(x, col="darkblue", type="l", main="ROC curve", ...) {

if (length(x$negNames) > 1) xinfo=paste(x$negNames, collapse=", ") else xinfo=x$negNames
if (length(x$posNames) > 1) yinfo=paste(x$posNames, collapse=", ") else yinfo=x$posNames

plot(x$FP, x$TP, xlab="", ylab="", col=col, type=type, ...)
mtext(main, side = 3, line = 2, font=2)
mtext(x$assayType, side = 3, line = 1)
mtext("#FP", side = 1, line = 2)
mtext(xinfo, side = 1, line=3, font=3)
mtext("#TP", side = 2, line=3)
mtext(yinfo, side = 2, line=2, font=3)
}

lines.ROC = function(x, ...)
  lines(x$FP, x$TP, ...)
