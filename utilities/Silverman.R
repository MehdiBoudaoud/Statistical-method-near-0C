

find_modes <- function(x, bw, get_freq = FALSE, get_data = FALSE, ...) {
  bw_h <- bw/2
  kdde_0 <- ks::kdde(x = x, h = bw_h, deriv.order = 0)
  kdde_1 <- ks::kdde(x = x, h = bw_h, deriv.order = 1)
  kdde_2 <- ks::kdde(x = x, h = bw_h, deriv.order = 2)
  
  df_1 <- data.frame(
    x = kdde_1$eval.points,
    y = kdde_0$estimate,
    y_1 = kdde_1$estimate,
    y_2 = kdde_2$estimate
  )
  
  modes <- c()
  freqs <- c()
  for (i in 1:400) {
    x_i <- df_1[i,]
    x_i_1 <- df_1[(i+1),]
    if(x_i$y_1 > 0){
      if(x_i_1$y_1 < 0){
        if(x_i$y > 0.01){
          modes <- c(
            modes,
            (df_1[(i-1),"x"] + df_1[i,"x"] + df_1[(i+1),"x"])/3
          )
          freqs <- c(
            freqs,
            (df_1[(i-1),"y"] + df_1[i,"y"] + df_1[(i+1),"y"])/3
          )
        }
      }
    }
  }
  
  ret <- list(modes = modes)
  if(get_freq == TRUE) {
    ret[["freqs"]] <- freqs
  }
  if(get_data == TRUE) {
    ret[["data"]] <- df_1
  }
  return(ret)
}

bw_crt <- function(x, nmodes, threshold = .1){
  bw <- c(.1, .25, .5, 1, 2, 5, 10, 15, 20)
  n <- c(
    find_modes(x, bw[1])$modes |> length(),
    find_modes(x, bw[2])$modes |> length(),
    find_modes(x, bw[3])$modes |> length(),
    find_modes(x, bw[4])$modes |> length(),
    find_modes(x, bw[5])$modes |> length(),
    find_modes(x, bw[6])$modes |> length(),
    find_modes(x, bw[7])$modes |> length(),
    find_modes(x, bw[8])$modes |> length(),
    find_modes(x, bw[9])$modes |> length()
  )
  
  
  cond <- which(n <= nmodes)
  if(length(cond) == 0){
    bw <- tail(bw, n = 1)
  }else{
    l <- cond[1]
    bw <- bw[l - 1]
  }
  
  n <- find_modes(x, bw)$modes |> length()
  
  while (n != nmodes) {
    #print(n)
    #print(bw)
    bw <- bw + threshold
    n <- find_modes(x, bw)$modes |> length()
    if(n < nmodes) {
      bw <- "error"
      break
    }
  }
  
  return(bw)
}
