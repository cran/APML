uncertainty <- function(x,y,th,ref=0){
  group <- ifelse(x>th,1,0)
  g1 <- y[group==0]
  g2 <- y[group==1]
  g1_mean <- mean(g1)
  g1_std <- stats::sd(g1)
  g2_mean <- mean(g2)
  g2_std <- stats::sd(g2)
  if(ref==0){
    or <- g2_mean/g1_mean
    or_st_er <- or*sqrt((g1_std/g1_mean)^2+(g2_std/g2_mean)^2)/sqrt(length(x))
  }else{
    or <- g1_mean/g2_mean
    or_st_er <- or*sqrt((g1_std/g1_mean)^2+(g2_std/g2_mean)^2)/sqrt(length(x))
  }
  or_low <- or-1.96*or_st_er
  or_high <- or+1.96*or_st_er
  out <- c(or,or_low,or_high)
  return(out)
}
