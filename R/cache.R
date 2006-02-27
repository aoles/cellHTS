## caching
cache = function(name, expr) {
  cachefile <- paste("tmp-", name, ".RData", sep="")
  if(file.exists(cachefile)) {
    load(cachefile)
  } else {
    assign(name, expr)
    save(list=name, file=cachefile)
  }
  get(name)
}
