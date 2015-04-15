genPop <- function(freqs,size,n){
  plyr::rlply(n,
              function(){
                allele <- plyr::ddply(freqs,~locus,function(x){
                  tmp <- sample(x$allele,2*size,
                                replace=TRUE,
                                prob=x$frequency)
                  data.frame(id=rep(1:size,2),
                             allele=tmp,
                             sub.locus=rep(1:2,each=size),
                             variable=sprintf('loc.%s.%s',x$locus[1],
                                              rep(1:2,each=size)))
                })
                plyr::join(allele,freqs,by=c('allele','locus'))  
              })
}