## Creates the cellHTS object with the assembled data from 'KcViab' and 'KcViabSmall' experiment:


    fname <- c("KcViab", "KcViabSmall")
    for (f in fname){
      datadir <- paste("../", f, sep="")
      x <- readPlateData("Platelist.txt", f, path=datadir)
      confFile <- paste(datadir, "Plateconf.txt", sep="/")
      logFile  <- paste(datadir, "Screenlog.txt", sep="/")
      descripFile  <- paste(datadir, "Description.txt", sep="/")
      x <- configure(x, confFile, logFile, descripFile)
      if (f==fname[1]){
        geneIDFile <- paste(datadir, "GeneIDs_Dm_HFA_1.1.txt", sep="/")
        KcViab <- annotate(x, geneIDFile)
        save(KcViab, file=sprintf("../../data/%s.rda", f), compress=TRUE)
      }else{
        geneIDFile <- paste(datadir, "GeneIDs_Dm_HFAsubset_1.1.txt", sep="/")
        KcViabSmall <- annotate(x, geneIDFile)
        save(KcViabSmall, file=sprintf("../../data/%s.rda", f), compress=TRUE)
      }
    }