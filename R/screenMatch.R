## Ligia Braz (September 2006)

## match the gene annotation of two screens in order to find common gene-perturbing reagents
## keyword: multi-screen comparison


screenMatch <- function(screens, ids){
  ## 1) Consistency checks
  ## 'screens must be a list of length 2, with the 2 cellHTS objects that we want to compare:
  if(!is(screens, "list") | length(screens)!=2 | !all(sapply(screens, is, "cellHTS")))
    stop("'screens' must be a list of 2 'cellHTS' objects.")

  if(!all(sapply(screens, function(x) x$state["annotated"])))
    stop("Please, first annotate the cellHTS objects in 'screens' using 'annotate.cellHTS'.")

  ## 'ids' must be a character vector of length 2 giving, for each cellHTS object, the name of the column of 'geneAnno' slot that should be used as the annotation IDs.
  ## By default, if 'ids' is not given, the function will take the column 'GeneID'.
  if(missing(ids)){
    ids <- rep("GeneID", 2)
  }else{
    ## checks
    if(!is(ids, "character") | length(ids)!=2 | !all(sapply(1:2,
                                       function(i)
                                       ids[i] %in% names(screens[[i]]$geneAnno))))
      stop("'ids' must be character vector of length 2 giving, \n for each cellHTS object, the name of the column of slot 'geneAnno' \n to consider for the annotation")
  }

  ## 2) Determine the overlap between the annotation of both data sets.
  anno <- lapply(1:2, function(i) {
    v <- unique(screens[[i]]$geneAnno[[ids[i]]])
    v <- v[!(v %in% c(NA, "NA", ""))]
  })


  cids <- intersect(anno[[1]], anno[[2]])

  ## proportion of overlap
  p.overlap <- sapply(anno, length)
  p.overlap <- signif(length(cids)/p.overlap, 3)

  #p.overlap <- min(p.overlap)

  ## Get a logical vector with the same length as the annotation vector, with the value TRUE for the indices that belong to the vector of common probes.
  vals <- lapply(1:2,
               function(i){
                 screens[[i]]$geneAnno[[ids[i]]] %in% cids }
               )
  #lapply(vals, table)
  names(p.overlap) <- names(vals) <- sapply(screens, function(x) x$name) 
  return(list(p.overlap=p.overlap, isInBoth=vals))
  }
