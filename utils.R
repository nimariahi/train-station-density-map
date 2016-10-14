#' 
#' Quantize values based on percentile thresholds
#' 
#' This can be used to quantize a list of continuous values, e.g. to reduce the 
#' number of levels for mapping.
#' 
#' @param vals A list/vector of numeric values
#' @param ths Percentile thresholds defining quantization levels.
#'   
#' @return Named list, where \code{$qvals} are the quantized values, 
#'   \code{limits} are the quantization limits, and \code{LabelStr} is a string
#'   describing the quantizatio ranges (useful to label a colorbar).
#'   
#'   
quantileQuantize <- function(vals,ths=c(0,0.25,.5,.75,1)){
  
  # Create field with percentile entries
  qs <- quantile(vals,ths,na.rm=T)
  
  densQ <- vals
  
  for (i in 1:length(qs)-1){
    sel <- vals>=qs[i] & vals<qs[i+1]
    densQ[sel] <- i
  }
  densQ[vals>=qs[length(qs)]] = length(qs)-1
  
  qs <- as.numeric(qs)
  Q <- (cbind(qs[1:length(qs)-1],qs[2:length(qs)]))
  
  # Given a 2-el numeric vector return a dash-separated string with the two numbers
  a.lab <- function(a,fmt='%f') {
    fmt_ <- sprintf('%s-%s',fmt,fmt)
    sprintf(fmt_,a[1],a[2])
  }
  
  label.str <- apply(Q,1,a.lab,'%04.2f')
  
  return(list('qvals'=densQ,'limits'=cbind(ths[1:length(ths)-1],ths[2:length(ths)]),'LabelStr'=label.str))
  
}
