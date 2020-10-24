outCI <-function(x,l,h,n=2,type='OR'){
  if(type=='OR'){
    pt <- paste0('%.',n,'f')
    h[h>1000] <- Inf
    a=paste0(sprintf(pt,x),'(',sprintf(pt,l),',',sprintf(pt,h),')')
  }else if(type=='ER'){
    pt <- paste0('%.',n,'f')
    x=(x-1)*100
    l=(l-1)*100
    h=(h-1)*100
    h[h>10000] <- Inf
    a=paste0(sprintf(pt,x),'(',sprintf(pt,l),',',sprintf(pt,h),')')
  }
  return(a)
}
