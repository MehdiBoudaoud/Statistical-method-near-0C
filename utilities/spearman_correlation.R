

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

spearman_2C <- 
  modesDF.orgl |> 
  mutate(Year = as.numeric(Year)) |> 
  group_by(Station) |> 
  summarise(
    mean_M1 = mean(Mode1),
    mean_M2 = mean(Mode2),
    CorDM = cor(Dm, Year, method = "spearman"),
    CorDF = cor(Df, Year, method = "spearman"),
    Corm1 = cor(Mode1, Year, method = "spearman"),
    Corm2 = cor(Mode2, Year, method = "spearman"),
    Corf1 = cor(Freq1, Year, method = "spearman"),
    Corf2 = cor(Freq2, Year, method = "spearman")
  ) |> 
  dplyr::filter(
    mean_M1 < 2 & mean_M1 > -2
  )

spearman_2C <-
  spearman_2C |> 
  left_join(
    X92st_list |> 
      rename(
        Station = St_Name
      ) |> 
      dplyr::select(
        Station, PR, Elev, Lat, Long
      )
  ) |> 
  mutate(
    Sign_m1 = ifelse(
      Corm1 > 0, 1, 0
    ) |> as.factor(),
    Sign_m2 = ifelse(
      Corm2 > 0, 1, 0
    ) |> as.factor(),
    Sign_Dm = ifelse(
      CorDM > 0, 1, 0
    ) |> as.factor()
  ) |> 
  mutate(
    abs_cor_m1 = abs(Corm1),
    abs_cor_m2 = abs(Corm2),
    abs_cor_Dm = abs(CorDM)
  )

wmap <- map_data("world")
ca <- wmap |> dplyr::filter(region == "Canada")
mp92 <- ca |> ggplot(aes(long, lat)) +
  geom_path(aes(x = long, y = lat, group = group),
            color = "black") +
  geom_polygon(aes(group = group, subgroup = subregion),
               fill = "#e0e0e0", color = "black")

mp921 <- 
  mp92 +
  geom_point(
    data = spearman_2C,
    aes(x = Long, 
        y = Lat, 
        size = abs_cor_m1, 
        shape = Sign_m1,
        fill = Sign_m1)
  ) +
  labs(
    x = "Long",
    y = "",
    colour = "Mode",
    size = "Correlation"
  ) +
  scale_shape_manual(values = c(25, 24)) +
  guides(fill = FALSE, shape = FALSE) +
  scale_fill_brewer(palette="Dark2") +
  theme_void()
mp921

mp922 <- 
  mp92 +
  geom_point(
    data = spearman_2C,
    aes(x = Long, 
        y = Lat, 
        size = abs_cor_m2, 
        shape = Sign_m2,
        fill = Sign_m2)
  ) +
  labs(
    x = "Long",
    y = "",
    colour = "Mode",
    size = "Correlation"
  ) +
  scale_shape_manual(values = c(25, 24)) +
  guides(fill = FALSE, shape = FALSE) +
  scale_fill_brewer(palette="Dark2") +
  theme_void()
mp922

ggpubr::ggarrange(mp921, mp922,
          common.legend = T,
          labels = c("(a) Mode 1", "(b) Mode 2"),
          hjust = 0,
          legend = "right")
ggsave("temp_pics/results_pdf/fig3_3.pdf", width = 7, height = 2.8)


mp92D <- 
  mp92 +
  geom_point(
    data = spearman_2C,
    aes(x = Long, 
        y = Lat, 
        size = abs_cor_Dm, 
        shape = Sign_Dm,
        fill = Sign_Dm)
  ) +
  labs(
    x = "Long",
    y = "",
    colour = "Mode",
    size = "Correlation"
  ) +
  scale_shape_manual(values = c(25, 24)) +
  guides(fill = FALSE, shape = FALSE) +
  scale_fill_brewer(palette="Dark2") +
  theme_void()
mp92D

ggsave("temp_pics/results_pdf/fig3_4.pdf", width = 7, height = 4)
