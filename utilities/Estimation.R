
##------------------------------------------------------------------------------
## Silverman 
## 1981-2011

silv <- matrix(ncol = 3, nrow = 92) |> 
  as.data.frame()
colnames(silv) <- c("Station", "Pic1", "Pic2")
silv["Station"] <- St_names
for (st in St_names) {
  print("------------------------")
  print(paste0("Station :", st))
  df_sub <- "Data/92st_RData/" |> 
    paste0(st, ".csv") |> 
    fread() 
  
  df_sub$Year <- df_sub$Date |> 
    format("%Y")
  df_sub <- df_sub |> 
    dplyr::filter(Year >= 1981) |> 
    dplyr::filter(Year <= 2011) |> 
    dplyr::filter(Temp > -100)
  
  bw <- bw_crt(df_sub$Temp, 2)
  modes <- find_modes(df_sub$Temp, bw)$modes
  
  silv[silv$Station == st, c("Pic1", "Pic2")] <- modes
}
silv
save(silv, file = "Data/silv.RData")


##------------------------------------------------------------------------------
## Silverman 
## Yearly

Silverman <- matrix(ncol = 4, nrow = 31) |> 
  as.data.frame()
colnames(Silverman) <- c("Station", "Year", "Pic1", "Pic2")
Silverman["Station"] <- St_names[1]
Silverman["Year"] <- seq.int(1981, 2011)
print("------------------------")
print(paste0("Station :", St_names[1]))
df_sub <- "Data/92st_RData/" |> 
  paste0(St_names[1], ".csv") |> 
  fread() 
df_sub$Year <- df_sub$Date |> 
  format("%Y")
for (y in Silverman$Year) {
  print(y)
  df_sub_y <- df_sub |> 
    dplyr::filter(Temp > -100) |> 
    dplyr::filter(Year == y)
  thr <- .1
  bw <- bw_crt(df_sub_y$Temp, 2, thr)
  while (bw == "error") {
    thr <- thr/10
    bw <- bw_crt(df_sub_y$Temp, 2, thr)
  }
  #bw <- multimode::bw.crit(df_sub_y$Temp, mod0 = 2)*2
  modes <- find_modes(df_sub_y$Temp, bw)$modes
  
  Silverman[Silverman$Year == y, c("Pic1", "Pic2")] <- modes
}

for (st in St_names[-1]) {
  silv <- matrix(ncol = 4, nrow = 31) |> 
    as.data.frame()
  colnames(silv) <- c("Station", "Year", "Pic1", "Pic2")
  silv["Station"] <- st
  silv["Year"] <- seq.int(1981, 2011)
  print("------------------------")
  print(paste0("Station :", st))
  df_sub <- "Data/92st_RData/" |> 
    paste0(st, ".csv") |> 
    fread() 
  df_sub$Year <- df_sub$Date |> 
    format("%Y")
  for (y in silv$Year) {
    print(y)
    df_sub_y <- df_sub |> 
      dplyr::filter(Temp > -100) |> 
      dplyr::filter(Year == y)
    if(nrow(df_sub_y) == 0) next
    
    thr <- .1
    bw <- bw_crt(df_sub_y$Temp, 2, thr)
    while (bw == "error") {
      thr <- thr/10
      bw <- bw_crt(df_sub_y$Temp, 2, thr)
    }
    modes <- find_modes(df_sub_y$Temp, bw)$modes
    
    silv[silv$Year == y, c("Pic1", "Pic2")] <- modes
  }
  
  Silverman <- Silverman |> 
    rbind(silv)
}

save(Silverman, file = "Data/Silverman.RData")



## Gaussian

gaus <- matrix(ncol = 3, nrow = 92) |> 
  as.data.frame()
colnames(gaus) <- c("Station", "Pic1", "Pic2")
gaus["Station"] <- St_names
for (st in St_names) {
  print("------------------------")
  print(paste0("Station :", st))
  df_sub <- "Data/92st_RData/" |> 
    paste0(st, ".csv") |> 
    fread() 
  
  df_sub$Year <- df_sub$Date |> 
    format("%Y")
  df_sub <- df_sub |> 
    dplyr::filter(Year >= 1981) |> 
    dplyr::filter(Year <= 2011) |> 
    dplyr::filter(Temp > -100)
  
  #bw <- bw_crt(df_sub$Temp, 2)
  modes <- mixt_modes(df_sub$Temp, 4, 2)$modes
  
  gaus[gaus$Station == st, c("Pic1", "Pic2")] <- modes
}
gaus3 <- gaus
save(gaus3, file = "Data/gauss3.RData")



##------------------------------------------------------------------------------
## Gaussian Mixture 
## Yearly

Gaussian <- matrix(ncol = 4, nrow = 31) |> 
  as.data.frame()
colnames(Gaussian) <- c("Station", "Year", "Pic1", "Pic2")
Gaussian["Station"] <- St_names[1]
Gaussian["Year"] <- seq.int(1981, 2011)
print("------------------------")
print(paste0("Station :", St_names[1]))
df_sub <- "Data/92st_RData/" |> 
  paste0(St_names[1], ".csv") |> 
  fread() 
df_sub$Year <- df_sub$Date |> 
  format("%Y")
for (y in Gaussian$Year) {
  print(y)
  df_sub_y <- df_sub |> 
    dplyr::filter(Temp > -100) |> 
    dplyr::filter(Year == y)

  modes <- mixt_modes(df_sub_y$Temp, 4, 2)$modes
  
  Gaussian[Gaussian$Year == y, c("Pic1", "Pic2")] <- modes
}

for (st in St_names[-1]) {
  gauss <- matrix(ncol = 4, nrow = 31) |> 
    as.data.frame()
  colnames(gauss) <- c("Station", "Year", "Pic1", "Pic2")
  gauss["Station"] <- st
  gauss["Year"] <- seq.int(1981, 2011)
  print("------------------------")
  print(paste0("Station :", st))
  df_sub <- "Data/92st_RData/" |> 
    paste0(st, ".csv") |> 
    fread() 
  df_sub$Year <- df_sub$Date |> 
    format("%Y")
  for (y in gauss$Year) {
    print(y)
    df_sub_y <- df_sub |> 
      dplyr::filter(Temp > -100) |> 
      dplyr::filter(Year == y)
    if(nrow(df_sub_y) == 0) next
    
    modes <- mixt_modes(df_sub_y$Temp, 4, 2)$modes
    
    gauss[gauss$Year == y, c("Pic1", "Pic2")] <- modes
  }
  
  Gaussian <- Gaussian |> 
    rbind(gauss)
}

Gaussian4 <- Gaussian
save(Gaussian4, file = "Data/Gaussian4.RData")


## Seasonal separation

sepd2 <- matrix(
  c("09-21", "03-20", 
    "03-21", "09-20"),
  ncol = 2, nrow = 2, byrow = T
)


seas <- matrix(ncol = 3, nrow = 92) |> 
  as.data.frame()
colnames(seas) <- c("Station", "Pic1", "Pic2")
seas["Station"] <- St_names
for (st in St_names) {
  print("------------------------")
  print(paste0("Station :", st))
  df_sub <- "Data/92st_RData/" |> 
    paste0(st, ".csv") |> 
    fread() 
  
  df_sub$Year <- df_sub$Date |> 
    format("%Y")
  df_sub <- df_sub |> 
    dplyr::filter(Year >= 1981) |> 
    dplyr::filter(Year <= 2011) |> 
    dplyr::filter(Temp > -100)
  
  #bw <- bw_crt(df_sub$Temp, 2)
  modes <- sep_modes(df_sub, sepd2, get_data = TRUE)
  
  seas[seas$Station == st, c("Pic1", "Pic2")] <- modes
}
seas
save(seas, file = "Data/seas.RData")


##------------------------------------------------------------------------------
## Seasonal Separation 
## Yearly

Seasonal <- matrix(ncol = 4, nrow = 31) |> 
  as.data.frame()
colnames(Seasonal) <- c("Station", "Year", "Pic1", "Pic2")
Seasonal["Station"] <- St_names[1]
Seasonal["Year"] <- seq.int(1981, 2011)
print("------------------------")
print(paste0("Station :", St_names[1]))
df_sub <- "Data/92st_RData/" |> 
  paste0(St_names[1], ".csv") |> 
  fread() 
df_sub$Year <- df_sub$Date |> 
  format("%Y")
for (y in Seasonal$Year) {
  print(y)
  df_sub_y <- df_sub |> 
    dplyr::filter(Temp > -100) |> 
    dplyr::filter(Year == y)
  
  modes <- sep_modes(df_sub_y, sepd2)$modes
  
  Seasonal[Seasonal$Year == y, c("Pic1", "Pic2")] <- modes
}

for (st in St_names[76:92]) {
  seas <- matrix(ncol = 4, nrow = 31) |> 
    as.data.frame()
  colnames(seas) <- c("Station", "Year", "Pic1", "Pic2")
  seas["Station"] <- st
  seas["Year"] <- seq.int(1981, 2011)
  print("------------------------")
  print(paste0("Station :", st))
  df_sub <- "Data/92st_RData/" |> 
    paste0(st, ".csv") |> 
    fread() 
  df_sub$Year <- df_sub$Date |> 
    format("%Y")
  for (y in seas$Year) {
    print(y)
    df_sub_y <- df_sub |> 
      dplyr::filter(Temp > -100) |> 
      dplyr::filter(Year == y)
    if(nrow(df_sub_y) < 5000) next
    
    modes <- sep_modes(df_sub_y, sepd2)$modes
    
    seas[seas$Year == y, c("Pic1", "Pic2")] <- modes
  }
  
  Seasonal <- Seasonal |> 
    rbind(seas)
}

Seasonal <- Seasonal
save(Seasonal, file = "Data/Seasonal.RData")
