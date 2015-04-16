LODpval <- function(dat.p,n,q=0.05,.parallel=TRUE){
  
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
  r <- 1:nrow(lod)
  
  lod <- arrange(lod,LOD_FC)
  lod$p_FC <- calc_pval(lod$LOD_FC,sort(lod.tmp$LOD_FC)) 
  lod$p_FCc <- pmin(1,num.comp*lod$p_FC)
  lod$FC_FDR <- ifelse(lod$p_FC<((r/num.comp)*q),'Related','Unrelated')
  
  lod <- arrange(lod,LOD_PO)
  lod$p_PO <- calc_pval(lod$LOD_PO,sort(lod.tmp$LOD_PO))
  lod$p_POc <- pmin(1,num.comp*lod$p_PO)
  lod$PO_FDR <- ifelse(lod$p_PO<((r/num.comp)*q),'Related','Unrelated')
  
  lod <- arrange(lod,LOD_HS)
  lod$p_HS <- calc_pval(lod$LOD_HS,sort(lod.tmp$LOD_HS))
  lod$p_HSc <- pmin(1,num.comp*lod$p_HS) 
  lod$HS_FDR <- ifelse(lod$p_HS<((r/num.comp)*q),'Related','Unrelated')
  
  return(lod)
}