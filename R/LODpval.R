LODpval <- function(dat.p,n,.parallel=TRUE){
  
  lod <- LODscores(dat.p$dat.m)
  num.fins <- length(unique(dat.p$dat.m$id))
  num.comp <- num.fins*(num.fins -1)/2
  tmp <- genPop(dat.p$freqs,
                num.fins,n)
  
  lod.tmp <- plyr::ldply(1:n,
                         function(x){
                           tmp <- LODscores(tmp[[x]])
                           tmp$iter <- x
                           return(tmp)
                         },.parallel=.parallel)
  
  plyr::ddply(lod,~ID1+ID2,mutate,
              p_PO = sum(lod.tmp$LOD_PO > LOD_PO[1],na.rm=TRUE)/(n*num.comp),
              p_HS = sum(lod.tmp$LOD_HS > LOD_HS[1],na.rm=TRUE)/(n*num.comp),
              p_FC = sum(lod.tmp$LOD_FC > LOD_FC[1],na.rm=TRUE)/(n*num.comp),
              p_POc = pmin(p_PO*num.comp,1),
              p_HSc = pmin(p_PO*num.comp,1),
              p_FCc = pmin(p_PO*num.comp,1),
              .parallel=.parallel)
  
}