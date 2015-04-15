library(readr)
library(parallel)
library(doMC)
library(dplyr)
library(data.table)
registerDoMC(cores=detectCores(logical = TRUE))
findat <- read_csv(system.file('extdata','findat.csv',
                               package='lodscores'),
                   na='000000')

dat.p <- readLocus(findat,combined=TRUE)

## calculate the LOD score for the example
lod <- LODscores(dat.p$dat.m)

## generate pseudo populations to calculate the p-values
num.fins <- length(unique(dat.p$dat.m$id))
tmp <- genPop(dat.p$freqs,
              num.fins,100)

lod.tmp <- plyr::ldply(1:100,
                    function(x){
                      tmp <- LODscores(tmp[[x]])
                      tmp$iter <- x
                      return(tmp)
                    },.parallel=TRUE)


num.comp <- num.fins * (num.fins -1)/2


lod$p.fc <- sapply(lod$LOD_FC,function(x) mean(x>lod.tmp$LOD_FC)) 


p.vals <- ddply(lod,~ID1+ID2,function(x){
  laply(lod.tmp,function(y){
        
  })
})
