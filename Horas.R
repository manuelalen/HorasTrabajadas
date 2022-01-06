library(ggplot2)
total <- 20*11
for (i in 1:total){

  horas[i] <- runif(1, 6.0,8) 
  
} 
df_horas <- data.frame("Día" <- 1:total,
                       "Horas" <- horas,
                       "SD" <- sd(horas),
                       "Límite Inferior"<- rep(mean(horas)-1.96*(sd(horas)/sqrt(length(total))),total),
                       "Límite Superior"<- rep(mean(horas)+1.96*(sd(horas)/sqrt(length(total))),total))
colnames(df_horas)<- c("Día","Horas","SD", "Límite Inferior","Límite Superior")
###Grafica
ggplot(df_horas, aes(x = Día, group = 1)) +
  geom_line(aes(y = Horas, colour = "Horas"))+
  geom_line(aes(y = `Límite Inferior`, colour = "Límite Inferior"))+
  geom_line(aes(y = `Límite Superior`, colour = "Límite Superior"))+
  scale_color_manual("Leyenda",
                     values= c("Horas" = "blue","Límite Inferior" = "red", "Límite Superior" = "green")) +
  theme(panel.background = element_rect(color = "black", # Border color
                                        size = 2, fill = "#d5f1f3"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Horas Trabajadas con intervalos de confianza al 95%")+
  scale_y_continuous(expand = c(0, 0), limits = c(3.5, 9))