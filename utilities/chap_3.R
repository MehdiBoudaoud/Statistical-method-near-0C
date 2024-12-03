
N_MODES <- 65 |> 
  get_bw_diff(by_bw = FALSE) |> 
  opt_n_modes() |> 
  dplyr::select(St_Name, N_modes) |> 
  dplyr::rename(Station = St_Name)

# sts <- x$St_Name[x$N_modes == 2]
## LOAD ANNUALLY ESTIMATED MODES
## -----------------------------
load("D:/Fabrice/temp_pics/Data/rapport_data/mode_list.RData")

names(mode_list) <- St_names
modesDF.orgl <- mode_list[[1]]
for (St in St_names[-1]) {
  modesDF.orgl <- modesDF.orgl |> 
    rbind(
      mode_list[[St]]
    )
}
modesDF.orgl$Dm <- modesDF.orgl$Mode2 - modesDF.orgl$Mode1
modesDF.orgl$Df <- modesDF.orgl$Freq2 - modesDF.orgl$Freq1

mode.K.orgl <- modesDF.orgl |> 
  as.data.table() |> 
  melt.data.table(id.vars = c("Year", "Station"),
                  measure.vars = c("Mode1", "Mode2"),
                  variable.name = "Classe",
                  value.name = "Mode")
levels(mode.K.orgl$Classe) <- c("1", "2")

freq.K.orgl <- modesDF.orgl |> 
  as.data.table() |> 
  melt.data.table(id.vars = c("Year", "Station"),
                  measure.vars = c("Freq1", "Freq2"),
                  variable.name = "Classe",
                  value.name = "Freq")
levels(freq.K.orgl$Classe) <- c("1", "2")

dfm.orgl <- merge(
  mode.K.orgl, 
  freq.K.orgl, 
  by = c("Year", "Station", "Classe")
)
dfm.orgl


func1 <- function(dfann, dfsaiso = NULL){
  x1 <- dfann |> 
    group_by(Station) |> 
    summarise(m1 = mean(Mode1),
              m2 = mean(Mode2),
              m1.max = mean(Mode1) + (sqrt(var(Mode1))/2),
              m1.min = mean(Mode1) - (sqrt(var(Mode1))/2),
              m2.max = mean(Mode2) + (sqrt(var(Mode2))/2),
              m2.min = mean(Mode2) - (sqrt(var(Mode2))/2)) |> 
    add_column(Estimation = "Annuelle")
  
  if(is.null(dfsaiso)){
    x <- x1
  } else {
    x2 <- dfsaiso |> 
      group_by(Station) |> 
      summarise(m1 = mean(Mode1),
                m2 = mean(Mode2),
                m1.max = mean(Mode1) + (sqrt(var(Mode1))/2),
                m1.min = mean(Mode1) - (sqrt(var(Mode1))/2),
                m2.max = mean(Mode2) + (sqrt(var(Mode2))/2),
                m2.min = mean(Mode2) - (sqrt(var(Mode2))/2)) |> 
      add_column(Estimation = "Saisonnier")
    
    x <- rbind(x1, x2)
  }
  
  x
}

xmode <- func1(modesDF.orgl)


func2 <- function(dfann, dfsaiso = NULL){
  x1 <- dfann |> 
    group_by(Station) |> 
    summarise(F1 = mean(Freq1),
              F2 = mean(Freq2),
              F1.max = mean(Freq1) + (sqrt(var(Freq1))/2),
              F1.min = mean(Freq1) - (sqrt(var(Freq1))/2),
              F2.max = mean(Freq2) + (sqrt(var(Freq2))/2),
              F2.min = mean(Freq2) - (sqrt(var(Freq2))/2)) |> 
    add_column(Estimation = "Annuelle")
  if(is.null(dfsaiso)){
    x <- x1
  } else {
    x2 <- dfsaiso |> 
      group_by(Station) |> 
      summarise(F1 = mean(Freq1),
                F2 = mean(Freq2),
                F1.max = mean(Freq1) + (sqrt(var(Freq1))/2),
                F1.min = mean(Freq1) - (sqrt(var(Freq1))/2),
                F2.max = mean(Freq2) + (sqrt(var(Freq2))/2),
                F2.min = mean(Freq2) - (sqrt(var(Freq2))/2)) |> 
      add_column(Estimation = "Saisonnier")
    
    x <- rbind(x1, x2)
  }
  x
}

xfreq <- func2(modesDF.orgl)

X92 <- X92st_list
X92$Region <- ""
X92[X92$PR == "BC", "Region"] <- 
  "British Columbia"
X92[X92$PR == "AB" | X92$PR == "SK" | X92$PR == "MB", "Region"] <- 
  "Prairies"
X92[X92$PR == "ON", "Region"] <- 
  "Ontario"
X92[X92$PR == "QC", "Region"] <- 
  "Quebec"
X92[X92$PR == "NB" | X92$PR == "PE" | X92$PR == "NS" | X92$PR == "NL", "Region"] <- 
  "Atlantic"
X92[X92$PR == "YK" | X92$PR == "NT" | X92$PR == "NU", "Region"] <- 
  "Northern"
colnames(X92)[4] <- "Station"
X92m <- merge(X92, 
              xmode |> dplyr::filter(Estimation == "Annuelle"),
              by = "Station")
X92m <- X92m |> 
  merge(N_MODES)

X92f <- merge(X92, 
              xfreq |> dplyr::filter(Estimation == "Annuelle"),
              by = "Station")

func3 <- function(dfann, dfsaiso = NULL){
  x1 <- dfann |> 
    group_by(Station, Classe) |> 
    summarise(m = mean(Mode),
              f = mean(Freq),
              m.max = mean(Mode) + (sqrt(var(Mode))/2),
              m.min = mean(Mode) - (sqrt(var(Mode))/2),
              f.max = mean(Freq) + (sqrt(var(Freq))/2),
              f.min = mean(Freq) - (sqrt(var(Freq))/2)) |> 
    add_column(Estimation = "Annuelle")
  if(is.null(dfsaiso)) {
    x <- x1
  } else {
    x2 <- dfsaiso |> 
      group_by(Station, Classe) |> 
      summarise(m = mean(Mode),
                f = mean(Freq),
                m.max = mean(Mode) + (sqrt(var(Mode))/2),
                m.min = mean(Mode) - (sqrt(var(Mode))/2),
                f.max = mean(Freq) + (sqrt(var(Freq))/2),
                f.min = mean(Freq) - (sqrt(var(Freq))/2)) |> 
      add_column(Estimation = "Saisonnier")
    
    x <- rbind(x1, x2)
  }
  x
}

x <- func3(dfm.orgl)
X <- merge(X92, 
           x |> dplyr::filter(Estimation == "Annuelle"),
           by = "Station")

X <- X |> merge(
  N_MODES
)

wmap <- map_data("world")
ca <- wmap |> dplyr::filter(region == "Canada")
mp92 <- ca |> ggplot(aes(long, lat)) +
  geom_path(aes(x = long, y = lat, group = group),
            color = "black") +
  geom_polygon(aes(group = group, subgroup = subregion),
               fill = "#e0e0e0", color = "black")

fig.a <-
  mp92 +
  geom_point(
    data = dplyr::filter(X, Classe == "1") |> dplyr::filter(N_modes == 2), 
    aes(x = Long, y = Lat, color = m, label = Station),
    size = 3.5
  )  +
  labs(
    #title = "Mode 1",
    colour = "Mean"
    # size = "??cart-type"
  )  +
  #paletteer::scale_fill_paletteer_c("viridis::plasma") +
  scale_color_continuous(type = "viridis",
                         limits = c(-30, 20))+
  theme_void()

fig.a

ggsave("temp_pics/results_pdf/M1.pdf", width = 8, height = 4.5)

fig.b <-
  mp92 +
  geom_point(
    data = dplyr::filter(X, Classe == "2") |> dplyr::filter(N_modes == 2), 
    aes(x = Long, y = Lat, color = m, label = Station),
    size = 3.5
  )  +
  labs(
    #title = "Mode 1",
    colour = "Mean"
    # size = "??cart-type"
  )  +
  #paletteer::scale_fill_paletteer_c("viridis::plasma") +
  scale_color_continuous(type = "viridis",
                         limits = c(-30, 20))+
  theme_void()

fig.b

ggsave("temp_pics/results_pdf/M2.pdf", width = 8, height = 4.5)


X92m$Difference <- 
  X92m$m2 - X92m$m1

fig.c <-
  mp92 +
  geom_point(
    data = X92m |> dplyr::filter(N_modes == 2), 
    aes(x = Long, y = Lat, color = Difference, label = Station),
    size = 3.5
  ) +
  labs(
    #title = "Mode 2",
    colour = "Moyenne",
    size = "??cart-type"
  ) +
  scale_color_continuous(type = "viridis")+
  theme_void()

fig.c

ggsave("temp_pics/results_pdf/M2 - M1.pdf", width = 8, height = 4.5)


fig1 <- 
  X92m |> 
  dplyr::filter(N_modes == 2) |>
  as.data.frame() |> 
  #xmode |> 
  #dplyr::filter(Estimation == "Annuelle") |> 
  ggplot(aes(x = m1, y = m2, labels = Station, color = Region)) +
  geom_point(size = 2)+
  geom_vline(xintercept = c(-2, 2), linetype=2) +
  geom_hline(yintercept = c(-2, 2), linetype=2) +
  geom_segment(aes(
    x = m1.min, y = m2, xend = m1.max, yend = m2
  ), linewidth = .5) +
  geom_segment(aes(
    x = m1, y = m2.min, xend = m1, yend = m2.max
  ), linewidth = .5) +
  #annotate("vline", x = -2)
  stat_cor(method="pearson") +
  #geom_line(aes(x = x, y = y), data = dfy) 
  labs(
    #title = "Mode 1 vs Mode 2",
    x = "Mode 1", #TeX(r'($M_1$)'),
    y = "Mode 2" #TeX(r'($M_2$)')
  ) +
  
  #  xlim(-30, 20) +
  #  ylim(-30, 20) +
  theme_classic() +
  scale_color_brewer(palette="Dark2") 
#facet_wrap(~Estimation)
fig1

ggsave("temp_pics/results_pdf/M1 vs M2.pdf", width = 8, height = 4.5)

fig1.. <- ggarrange(fig1, legend = "right")
fig1. <- fig1 + grids(linetype = "twodash", size = 1)
g1 <- ggarrange(fig.a, fig.b,
                labels = c("(a) Mode 1", "(b) Mode 2"), hjust = 0)
g1

ggarrange(g1, fig.c, fig1., ncol = 1,
          heights = c(6, 13, 15),
          widths = c(9, 9, 9),
          labels = c("", "(c) Mode2 - Mode1", "(d) Mode1 vs Mode2"))

ggarrange(fig.a, fig.b, fig.c, fig1,
          labels = c("(a) Mode 1", "(b) Mode 2", "(c) Mode2 - Mode1", "(d) Mode1 vs Mode2"), hjust = 0)

ggsave("temp_pics/results_pdf/fig311.pdf", width = 7, height = 10)

fig21 <- 
  X |> 
  dplyr::filter(N_modes == 2) |> 
  dplyr::filter(Classe == "1") |> 
  ggplot(aes(x = m, y = f, color = Region, labels = Station)) +
  geom_point(size = 2) +
  geom_vline(xintercept = c(-2, 2), linetype=2) +
  geom_segment(aes(
    x = m.min, y = f, xend = m.max, yend = f
  ), linetype = 1, linewidth = .5) +
  geom_segment(aes(
    x = m, y = f.min, xend = m, yend = f.max
  ), linetype = 1, linewidth = .5) +
  scale_y_continuous(limits = c(0, .1)) +
  #geom_hline(yintercept = c(.025, .035, .045, .055, .065), linetype = 2) +
  labs(
    #title = "Mode 2 vs Frequence 2",
    x = "Mode ", 
    y = "Frequence ",
    color = "Region"
  ) +
  #paletteer::scale_fill_paletteer_c("viridis::plasma") +
  scale_color_brewer(palette="Dark2") +
  theme(legend.position="top") +
  # stat_cor(method="pearson") +
  theme_classic()# +
#  facet_wrap(~ Classe, ncol = 2, switch = "y")
fig22 <- 
  X |> 
  dplyr::filter(N_modes == 2) |> 
  dplyr::filter(Classe == "2") |> 
  ggplot(aes(x = m, y = f, color = Region, labels = Station)) +
  geom_point(size = 2) +
  geom_vline(xintercept = c(-2, 2), linetype=2) +
  geom_segment(aes(
    x = m.min, y = f, xend = m.max, yend = f
  ), linetype = 1, linewidth = .5) +
  geom_segment(aes(
    x = m, y = f.min, xend = m, yend = f.max
  ), linetype = 1, linewidth = .5) +
  scale_y_continuous(limits = c(0, .1)) +
  #  geom_hline(yintercept = .025) +
  labs(
    #title = "Mode 2 vs Frequence 2",
    x = "Mode ", 
    y = "",
    color = "Region"
  ) +
  #paletteer::scale_fill_paletteer_c("viridis::plasma") +
  scale_color_brewer(palette="Dark2") +
  theme(legend.position="top") +
  # stat_cor(method="pearson") +
  theme_classic()

library(ggpubr)

ggarrange(fig21, fig22,
          common.legend = T,
          labels = c("(a) Mode1", "(b) Mode 2"),
          hjust = -1.3)


