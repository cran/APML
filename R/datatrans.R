datatrans <- function(data,class_number=5,rescale=F,factor_dummy=NULL,ref=NULL, target=NULL,drop_ratio=0,missing_rate=0.5){
  message(' Attention: Make sure only numbers in data. No characters.')
  data <- as.data.frame(lapply(data,as.numeric))
  
  onec <- NULL
  for (i in 1:ncol(data)) {
    if(any(is.na(data[,i]))){
      if(length(unique(data[,i]))<=2){
        onec=c(onec,i)}
    }else{
      if(length(unique(data[,i]))==1){
        onec=c(onec,i)}
    }
  }
  if(!is.null(onec)) {
    message('\n drop one-categorical variables')
    data <- data[,-onec]
  }
  
  
  if(!is.null(target)){
    message('\n drop categorical variables with # of minority < # of target minority')
    n_target <- sort(table(data[,target]))[1]
    drops <- NULL
    for (i in 1:ncol(data)) {
      if(sort(table(data[,i]))[1]<=round(n_target*drop_ratio) & length(unique(data[,i]))<=class_number){
        drops=c(drops,i)}
    }
    if(!is.null(drops)) data <- data[,-drops]
  }
  if(any(is.na(data))){
    message('\n drop missing variables')
    missings=NULL
    for (i in 1:ncol(data)) {
      if(any(is.na(data[,i]))){
        a <- sum(is.na(data[,i]))/length(data[,i])
        if(a>=missing_rate){
          missings=c(missings,i)
        }
      }
    }
    if(!is.null(missings)) data <- data[,-missings]
  }
  
  categories <- NULL
  for (i in 1:ncol(data)) {
    if(length(unique(stats::na.omit(data[,i])))<=2){
      categories=c(categories,i)}
  }
  
  if(!is.null(ref)){
    message('\n set ref for dichotomous variables')
    if(ref=='s'){
      for (i in categories) {
        low <- sort(unique(data[,i]))[1]
        high <- sort(unique(data[,i]))[2]
        data[which(data[,i]==low),i] <- 0
        data[which(data[,i]==high),i] <- 1
      }
    }else if(ref=='b'){
      for (i in categories) {
        low <- sort(unique(data[,i]))[1]
        high <- sort(unique(data[,i]))[2]
        data[which(data[,i]==high),i] <- 0
        data[which(data[,i]==low),i] <- 1
      }
    }else{
      for (i in categories) {
        data[which(data[,i]==ref),i] <- 0
        data[!which(data[,i]==ref),i] <- 1
      }
    }
  }
  
  mcategories <- NULL
  for (i in 1:ncol(data)) {
    if(length(unique(stats::na.omit(data[,i])))<=class_number){
      mcategories=c(mcategories,i)}
  }
  mcategories <-  mcategories[!mcategories %in% categories]
  mnames <- names(data[mcategories])
  
  if(!is.null(factor_dummy)){
    if(factor_dummy=='dummy'){
      for (i in mnames) {
        suppressWarnings(data <- dummy.data.frame(data, names = i, sep = "_"))
      }}
    
    if(factor_dummy=='factor'){
      if(length(mcategories)==1){
        data[,mcategories] <- sapply(data[,mcategories],as.factor)
      }else{
        data[,mcategories] <- lapply(data[,mcategories],as.factor)
      }
    }
  }
  
  num <- NULL
  for (i in 1:ncol(data)) {
    if(length(unique(stats::na.omit(data[,i])) )>class_number){
      num=c(num,i)}
  }
  if(rescale==TRUE){
    message('\n rescaling numeric variables')
    data[,num] <- scale(data[,num])
  }
  message('\n')
  return(data)
}

