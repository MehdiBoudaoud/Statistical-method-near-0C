
mixt_density <- function(x, m) {
  g <- m$G
  Mu <- m$mean
  Var <- m$variance
  Pro <- m$pro
  
  if(g == 1){
    d <- dnorm(x, mean = Mu, sd = sqrt(Var))
  }else{
    d <- (dnorm(x, mean = Mu[1], sd = sqrt(Var[1]))) * Pro[1]
    for (i in 2:g) {
      d <- d + (dnorm(x, mean = Mu[i], sd = sqrt(Var[i]))) * Pro[i]
    }
  }
  
  return(d)
}

mixt_modes <- function(x, g = 2, n_modes = 2, get_data = FALSE) {
  x <- x |> na.omit()
  m <- Mclust(x, G = g, modelNames = "E")|> 
    summary(parameters = T)
  ord_pros <- (m$pro |> order(decreasing = T)) |> head(n=n_modes)
  
  modes <- m$mean[ord_pros] |> sort() |> as.numeric()
  
  xx <- x |> unique() |> sort()
  df <- matrix(ncol = (g + 2), nrow = length(xx)) |> 
    as.data.frame()
  colnames(df) <- c(
    "x", "Y", paste0("y", seq(1, g))
  )
  df$x <- xx
  df$Y <- mixt_density(xx, m)
  for (i in 1:g) {
    df[,paste0("y", i)] <- m$pro[i] * dnorm(
      xx, mean = m$mean[i], sd = sqrt(m$variance[i])
    )
  }
  
  ret <- list(modes = modes)
  if(get_data == TRUE) {
    ret$data <- df
  }
  
  return(ret)
}

