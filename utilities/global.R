

# setwd("D:/Fabrice/temp_pics")
source("Gaussians_Mixture.R", echo=TRUE)
source("Silverman.R", echo=TRUE)
source("Seasonal_separation.R", echo=TRUE)

library(reshape2)
library(gridExtra)
library(mclust)
library(data.table)
library(dplyr)
library(tidyverse)
library(readxl)
library(timeSeries)
library(maps)


load("Data/gauss2.RData")
load("Data/gauss3.RData")
load("Data/gauss4.RData")
load("Data/silv.RData")
load("Data/seas.RData")

load("Data/Gaussian2.RData")
load("Data/Gaussian3.RData")
load("Data/Gaussian4.RData")
load("Data/Silverman.RData")
load("Data/Seasonal.RData")

load("Data/nmodes.RData")

estimates <- list(
  `Method 1` = silv,
  `Method 2` = seas,
  `Method 3` = gaus2,
  `Method 4` = gaus3,
  `Method 5` = gaus4
)

estimates_yearly <- list(
  `Method 1` = Silverman,
  `Method 2` = Seasonal,
  `Method 3` = Gaussian2,
  `Method 4` = Gaussian3,
  `Method 5` = Gaussian4
)

get_means <- function(data){
  data |> 
    group_by(Station) |> 
    summarise(
      Pic1 = mean(Pic1), 
      Pic2 = mean(Pic2)
    ) |> 
    as.data.frame()
}

estimates_mean <- list(
  `Method 1` = Silverman |> get_means(),
  `Method 2` = Seasonal |> get_means(),
  `Method 3` = Gaussian2 |> get_means(),
  `Method 4` = Gaussian3 |> get_means(),
  `Method 5` = Gaussian4 |> get_means()
)

pics <- data.frame(
  `Pic 1` = "Pic1",
  `Pic 2` = "Pic2"
)

est_methods <- c("Method 1", "Method 2", 
                 "Method 3", "Method 4", 
                 "Method 5")

X92st_list <- read_excel("Data/92st_list.xlsx")
#View(X92st_list)
St_Ids <- X92st_list$stnid
X92st_list$St_Name[c(65,66,67)] <- c(
  "QUEBEC_JEAN LESAGE INTL A",
  "MONTREAL_PIERRE ELLIOTT TRUDEAU INTL A",
  "MONTREAL_MIRABEL INT'L A"
)
St_names <- X92st_list$St_Name
colnames(X92st_list)[9:10] <- c('x', 'y')

plot_map <- function(data, pic = '1 pic', title = ''){
  
  df <- X92st_list[c('St_Name', 'PR', 'Elev', 'Lat', 'Long')]
  df['bw'] = 0
  St_names <- X92st_list$St_Name
  for (st in St_names) {
    df[
      df$St_Name == st,
      'bw'
    ] <- data[data$Station == st,][[pic]]
  }
  
  wmap <- map_data("world")
  ca <- wmap |> dplyr::filter(region == "Canada")
  mp92 <- ca |> ggplot(aes(long, lat)) +
    geom_path(aes(x = long, y = lat, group = group),
              color = "#737373") +
    geom_polygon(aes(group = group, subgroup = subregion),
                 fill = "#f0f0f0", color = "#525252") +
    scale_color_brewer(palette = "Paired") +
    theme_void() +
    geom_point(
      data = df, 
      aes(x = Long, y = Lat, size = bw, fill = PR, label = St_Name), 
      shape = 21
    ) +
    labs(
      color = 'Province',
      size = 'Bandwith',
      title = title
    ) 
  
  return(mp92)
}

# df <- "Data/92st_RData/" |> 
#   paste0(input$st_sel, ".csv") |> 
#   fread()
# 
# for (st in St_names) {
#   print(st)
#   df <- "Data/92st_csvData/" |> 
#     paste0(st, ".csv") |> 
#     fread()
#   
#   save(df, file = paste0("Data/92st_RData/", st, ".RData"))
# }


sepd2 <- matrix(
  c("09-21", "03-20", 
    "03-21", "09-20"),
  ncol = 2, nrow = 2, byrow = T
)

plot_hist <- function(data, bw){
  #dens <- density(data$Temp, bw = bw/2)
  data |> 
    ggplot(aes(x = Temp)) +
    geom_histogram(aes(y=..density..), binwidth = bw,
                   colour="black", fill="grey") +
    #geom_density(alpha=.2, fill="#FF6666")
  #  geom_line(aes(x, y), linewidth = 1.2, color = "red",
  #            data = data.frame(x = dens$x, y = dens$y)) +
    theme_classic()
}

local.min.max <- function(x, dev=mean, plot=TRUE, add.points=FALSE,  ...) {
  x <- stats::na.omit(x)
  r <- rle(x) 
  minima <- which(rep(x=diff(sign(diff(c(-Inf, r$values, -Inf)))) == 2, times=r$lengths))
  maxima <- which(rep(x=diff(sign(diff(c(-Inf, r$values, -Inf)))) == -2, times=r$lengths)) 
  if (plot == TRUE) {				 
    plot(x,type="l", ...)
    graphics::points(x[minima]~minima,pch=19,col="blue") 
    graphics::points(x[maxima]~maxima,pch=19,col="red")
    graphics::abline(h=dev(x, na.rm=TRUE), col="grey")
    if (add.points == TRUE) graphics::points(x, col="grey")
    graphics::legend("topleft", legend=c("Minima","Maxima"), pch=c(19,19), 
                     col=c("blue","red"), bg="white")
  }
  return( list(minima=x[minima], maxima=x[maxima],
               devmin=abs(dev(x) - x[minima]), 
               devmax=abs(dev(x) - x[maxima])) )
}	



comparison_plot <- function(estimates, meth1, meth2, pic = "Pic1", yearly = 0){
  
  if(yearly != 0){
    x <- estimates_yearly[[meth1]] |> dplyr::filter(Year == yearly)
    x <- x[,pic]
    y <- estimates_yearly[[meth2]] |> 
      dplyr::filter(Year == yearly) |>
      dplyr::distinct()
    y <- y[,pic]
  } else {
    x <- estimates[[meth1]][,pic]
    y <- estimates[[meth2]][,pic]
  }
  #print(y)
  df <- data.frame(
    Station = St_names,
    x = x,
    y = y
  )
  
  df |> ggplot(aes(x, y, label = Station)) +
    geom_point() +
    theme_classic() +
    labs(
      title = paste0(meth1, " (vs) ", 
                     meth2),
      x = meth1,
      y = meth2
    ) +
    geom_abline(intercept=0, slope=1, show.legend = TRUE)
}

#df_sub <- "Data/92st_RData/" |> 
#  paste0(input$St_select, ".csv") |> 
#  fread()


#Years <- df_sub$Date |>
#  format("%Y")

#df_sub$Year <- Years

#df_sub <- df_sub |> 
#  dplyr::filter(Year > 1980) |> 
#  dplyr::filter(Year < 2012) |> 
#  dplyr::filter(Temp > -100)


get_bw_diff <- function(lim1 = 'max1', by_bw = TRUE){
  
  diff_bw <- nmodes
  if(lim1 == 'max1'){
    max1 <- nmodes$`1 pic` |> max()
  }else{
    if (by_bw == TRUE){
      max1 <- lim1
    } else {
      max1 = (nmodes$max - nmodes$min)/lim1
    }
    
  }
  
  diff_bw$`1 pic` <- (max1 - nmodes$`1 pic`)
  diff_bw$`2 pics` <- nmodes$`1 pic` - nmodes$`2 pics`
  diff_bw$`3 pics` <- nmodes$`2 pics` - nmodes$`3 pics`
  diff_bw$`4 pics` <- nmodes$`3 pics` - nmodes$`4 pics`
  diff_bw$`5 pics` <- nmodes$`4 pics` - nmodes$`5 pics`
  
  return(diff_bw)
}



opt_n_modes <- function(gap){
  df <- X92st_list[c('St_Name', 'PR', 'Elev', 'Lat', 'Long')]
  df['N_modes'] = 0
  St_names <- X92st_list$St_Name
  for (st in St_names) {
    df[
      df$St_Name == st,
      'N_modes'
    ] <- gap[
      gap$Station == st, 
      c('1 pic', '2 pics', '3 pics', '4 pics', '5 pics')
    ] |>
      which.max() |>
      as.numeric()
  }
  
  df$N_modes <- df$N_modes |> as.factor()
  
  return(df)
}
