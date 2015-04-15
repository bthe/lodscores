##' @title Identify loduses
##' @name readLocus
##' @param dat dataframe where the columns indicates a locus
##' @param combined logical indicates wheather a column contains 
##' a single locus or a locus pair
##' @param na.action a function that deals with NA's in the data
##' @return data.frame with an ID, locus allele and 
##' allele frequency. 
##' @export
readLocus <- function(dat,
                      combined=FALSE,
                      na.action=na.omit){
  
  
  if(combined){
    ndat <- names(dat)
    dat <- dat[c(ndat[1],rep(ndat[-1],each=2))]
    
    dat[paste0(ndat[-1],'.1')] <-
      dat[paste0(ndat[-1],'.1')] -
      1000*round(dat[paste0(ndat[-1],'.1')]/1000)
    dat[ndat[-1]] <-
      (dat[ndat[-1]] - dat[paste0(ndat[-1],'.1')])/1000
    
  }
  
  names(dat) <- c('id',sprintf('loc.%s.%s',
                               rep(1:((ncol(dat)-1)/2), each = 2),
                               rep(1:2,((ncol(dat)-1)/2))))
  
  dat <- na.action(dat)
  
  freqs <-
    plyr::ldply(1:((ncol(dat)-1)/2),
          function(x){        
            tmp <-
              gstudio::frequencies(unlist(plyr::llply(1:nrow(dat),
                                                function(y) {      
                                                  gstudio::locus(c(dat[y,2*x],
                                                                   dat[y,2*x+1]),
                                                                 type = 'separated')
                                                })))
            names(tmp) <- tolower(names(tmp))
            plyr::mutate(tmp,
                   allele = as.numeric(as.character(allele)),
                   locus = x)
          })
  dat.m <- plyr::mutate(reshape2::melt(dat,id.vars='id',value.name = "allele"),
                  locus = sapply(strsplit(gsub('loc.','',variable),
                                          '.', fixed = TRUE),
                                 function(x) as.numeric(x[1])),
                  sub.locus = sapply(strsplit(gsub('loc.','',variable),
                                              '.', fixed = TRUE),
                                     function(x) as.numeric(x[2])))
  dat.m <- plyr::join(dat.m,freqs,by=c('allele','locus'))
  class(dat.m) <- c('LODdat',class(dat.m))
  return(list(dat.m=dat.m,freqs=freqs))
}