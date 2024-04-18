library(readr)
library(ggpubr)
library(showtext)
library(plyr)
library(ggplot2)
library(ggimage)

# This is the source to use the icons
font_add("wmpeople1", "D:/varios/wmpeople1.TTF")
showtext_auto()

# Loading the ENSU file
tabla <- openxlsx::read.xlsx("D:/OneDrive - INEGI/Respaldo/varios/Medioambiente/demografia/poblacion_CDMX_ensu/XI_acoso_personal_violencia_sexual_dic_2023_est.xlsx",3)
tabla <- tabla[69:120,]

quitar <- which(stringr::str_detect(tabla[,1],"Región"))
tabla <- tabla[-quitar,]

# Now I need the names of the municipalities
alcaldias <- tabla[!tabla[,1]%in%c('Hombres', 'Mujeres'), 1]

# And now take the each one position
pos_alcaldias <- which(tabla[,1]%in%alcaldias)
pos_h <- pos_alcaldias + 1
pos_m <- pos_alcaldias + 2

# In order to assign the right name of the locality I create a new column with the labels
tabla$alcaldia <- NA
for(i in 1:length(pos_h)){
  tabla[pos_h[i],"alcaldia"] <- alcaldias[i]
  tabla[pos_m[i],"alcaldia"] <- alcaldias[i]
}

# And now put the gender
tabla$gender <- NA
for(i in 1:length(pos_h)){
  tabla[pos_h[i],"gender"] <- "m"
  tabla[pos_m[i],"gender"] <- "f"
}

# Since I'm interested only in the gender not NA
tabla <- tabla[!is.na(tabla$gender),]


# Finally 
tabla$geocode <- NA
for(i in 1:length(alcaldias)){
  tabla[tabla$alcaldia%in%alcaldias[i],"geocode"] <- i

}

dat <- tabla[!tabla$alcaldia%in%NA, c(2,6,7,8)]

# Note that X2 is the column of population
dat$int = round(as.numeric(dat$X2)/100000)
gdat = ddply(dat, "geocode", function(d) {
  male = d$int[d$gender == "m"]
  female = d$int[d$gender == "f"]
  data.frame(gender = c(rep("m", male), rep("f", female)),
             x = 1:(male + female))
})

gdat$char = ifelse(gdat$gender == "m", "p", "u")
ggplot(gdat, aes(x = x, y = factor(geocode))) +
  geom_text(aes(label = char, colour = gender),
            family = "wmpeople1", size = 8) +
  scale_x_continuous("Población）") +
  scale_y_discrete("Población",
                   labels = unique(dat$alcaldia[order(dat$geocode)])) +
  scale_colour_hue(guide = FALSE) +
  ggtitle("Población de la CDMX por alcaldía. ENSU 2023, INEGI.") +xlim(c(0, 20))
