expl_rr <- function(data,formula,low=0.01,high=0.99,ref='min'){
  var_name <- as.character(formula[3])
  pred_dat <- eval(parse(text=as.character(paste0('data$',formula[2]))))
  var <- eval(parse(text=paste0('data$',var_name)))
  myloess <- stats::loess(formula,data)
  l=stats::quantile(var,low)
  h=stats::quantile(var,high)
  x <- seq(l,h,by=0.01)
  pred <- stats::predict(myloess,x,type='response')
  if(ref=='min') min_pred <-min(pred)
  if(ref=='median') min_pred <-stats::median(pred)
  if(ref=='mean') min_pred <-mean(pred)
  if(!ref%in%c('min','median','mean')) min_pred <-ref
  min_pred <-mean(pred[pred>0])
  roc <- roc(data[,1],var)
  v <- pROC::coords(roc,x='best',best.method = 'youden',transpose = F)[,1]
  
  p=ggplot2::ggplot(mapping = ggplot2::aes(x=var,y=pred_dat/min_pred))+
    ggplot2::geom_smooth(method = 'gam',formula = y~s(x,bs='cs'),color='2')+
    ggplot2::xlim(l,h)+ggplot2::theme_classic()+ggplot2::geom_hline(yintercept = 1,linetype=2)+
    ggplot2::geom_vline(xintercept = v,linetype=2)+
    ggplot2::xlab(var_name)+ggplot2::ylab('Risk Ratio')
  out <- list(p=p,pred=pred,min_pred=min_pred,threshold=v)
  return(out)
}

