LODscores <- function(dat.m){
  freqARR <- reshape2::acast(dat.m,id~variable,value.var='frequency')
  allARR <- reshape2::acast(dat.m,id~variable,value.var='allele')
  num.ind <- nrow(allARR)
  ## calc parent offspring lod
  lod.po <- lod_scores(freqARR,allARR,num.ind,0,1) 
  dimnames(lod.po) <- list(X=dimnames(allARR)[[1]],
                        Y=dimnames(allARR)[[1]])
  lod.po[upper.tri(lod.po,diag=TRUE)] <- 0
  
  ## calc half-sibling lod
  lod.hs <- lod_scores(freqARR,allARR,num.ind,1/2,1/2) 
  dimnames(lod.hs) <- list(X=dimnames(allARR)[[1]],
                           Y=dimnames(allARR)[[1]])
  lod.hs[upper.tri(lod.hs,diag=TRUE)] <- 0

  ## calc first cousin lod
  lod.fc <- lod_scores(freqARR,allARR,num.ind,3/4,1/4) 
  dimnames(lod.fc) <- list(X=dimnames(allARR)[[1]],
                           Y=dimnames(allARR)[[1]])
  lod.fc[upper.tri(lod.fc,diag=TRUE)] <- 0
  
  ## remove uninteresting bits
  lod.po <- subset(as.data.frame.table(lod.po),
                !(is.na(Freq) | Freq==-999 | Freq == 0))
  names(lod.po) <- c('ID1','ID2','LOD_PO')

  lod.hs <- subset(as.data.frame.table(lod.hs),
                   !(is.na(Freq) | Freq==-999 | Freq == 0))
  names(lod.hs) <- c('ID1','ID2','LOD_HS')
  
  lod.fc <- subset(as.data.frame.table(lod.fc),
                   !(is.na(Freq) | Freq==-999 | Freq == 0))
  names(lod.fc) <- c('ID1','ID2','LOD_FC')
  
  lod <- plyr::join(plyr::join(lod.po,lod.hs, 
                               by=c('ID1','ID2'), type='full'),
                    lod.fc, by=c('ID1','ID2'), type='full')
  
  return(lod)
}
