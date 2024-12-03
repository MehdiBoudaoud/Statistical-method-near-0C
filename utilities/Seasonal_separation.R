


get_Seasons <- function(x, Sep_dates){
  
  x <- x |> as.data.frame()
  
  Years <- x$Year |> unique()
  x$Saison <- "S1"
  x$Date <- x$Date |> as.timeDate()
  
  for (year in Years) {
    df <- x[x[,"Year"] == year, ]
    df[,"Saison"] <- "S1"
    
    sepd <- year |> paste(
      "-", Sep_dates[2,], sep = ""
    ) |> as.timeDate()
    idx1 <- (df$Date < sepd[1]) |> which()
    idx2 <- (df$Date > sepd[2]) |> which()
    
    if(length(idx1) > 0) df[idx1, "Saison"] <- "S2"
    if(length(idx2) > 0) df[idx2, "Saison"] <- "S2"
    
    x[x[,"Year"] == year, ] <- df
  }
  return(x)
}

sep_modes <- function(x, sep_dates, bw = NULL, get_freqs = FALSE, get_data = FALSE) {
  df <- get_Seasons(x, sep_dates)
  
  d <- "2000-" |> paste0(sep_dates[1,]) |> as.Date()
  l1 <- ((d[1] - d[2])/365) |> as.numeric() 
  l2 <- 1 - l1
  
  if(is.null(bw)){
    bw1 <- bw_crt(df[df$Saison == "S1",]$Temp, 1)
    bw2 <- bw_crt(df[df$Saison == "S2",]$Temp, 1)
  }else{
    bw1 <- bw
    bw2 <- bw
  }
  
  m1 <- find_modes(df[df$Saison == "S1",]$Temp, bw1, get_freq = get_freqs, get_data = get_data)
  m2 <- find_modes(df[df$Saison == "S2",]$Temp, bw2, get_freq = get_freqs, get_data = get_data)
  
  ret <- list(modes = c(m2$modes, m1$modes))
  if(get_freqs == TRUE){
    ret$freqs <- c(m1$freqs, m2$freqs)
  }
  if(get_data == TRUE){
    m1$data$y <- m1$data$y * .5
    m2$data$y <- m2$data$y * .5
    #Y <- m1$data$y + m2$data$y
    l <- which(m2$data$x > m1$data$x[1])[1]
    
    x = c(m2$data$x[1:l-1], m1$data$x)
    y1 = c(rep(0, l-1), m1$data$y)
    y2 = c(m2$data$y, rep(0, l-1))

    ret$data <- data.frame(
      x = x,
      y1 = y1,
      y2 = y2,
      Y = y1 + y2
    )
    
  }
  
  return(ret)
}


