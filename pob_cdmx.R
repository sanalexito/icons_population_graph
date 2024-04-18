library(readr)
library(ggpubr)
library(showtext)
library(plyr)
library(ggplot2)
library(ggimage)


font_add("wmpeople1", "D:/varios/wmpeople1.TTF")
showtext_auto()

dat = read.csv(textConnection('
      edu,educode,gender,population
      No School,1,m,17464
      No School,1,f,41268
      Primary School,2,m,139378
      Primary School,2,f,154854
      Middle School,3,m,236369
      Middle School,3,f,205537
      High School,4,m,94528
      High School,4,f,70521
      Bacherlor or above,5,m,57013
      Bacherlor or above,5,f,50334
'))

dat$int = round(dat$population / 10000)
gdat = ddply(dat, "educode", function(d) {
  male = d$int[d$gender == "m"]
  female = d$int[d$gender == "f"]
  data.frame(gender = c(rep("m", male), rep("f", female)),
             x = 1:(male + female))
})

gdat$char = ifelse(gdat$gender == "m", "p", "u")
ggplot(gdat, aes(x = x, y = factor(educode))) +
  geom_text(aes(label = char, colour = gender),
            family = "wmpeople1", size = 8) +
  scale_x_continuous("Population（10 million）") +
  scale_y_discrete("Education Level",
                   labels = unique(dat$edu[order(dat$educode)])) +
  scale_colour_hue(guide = FALSE) +
  ggtitle("2012 Demographics Data")



#-----------------------------------------------------------------
#datos de la ENSU
# edu <- c("Tla","Tla",
#          "Ags", "Ags",
#          "Izt", "Izt")
# educode <- c(1,1, 2,2, 3,3)
# gender <- c("m","f", "m","f", "m","f")
# population <- c( 127710,166166,   324537,359297,   722063,765654)
#dat <- as.data.frame(cbind(edu, educode, gender, population))
dat <- openxlsx::read.xlsx("D:/OneDrive - INEGI/Respaldo/varios/Medioambiente/demografia/poblacion_CDMX_ensu/pob_cdmx.xlsx", sheet = 1)
dat$int = round(as.numeric(dat$population)/100000)
gdat = ddply(dat, "educode", function(d) {
  male = d$int[d$gender == "m"]
  female = d$int[d$gender == "f"]
  data.frame(gender = c(rep("m", male), rep("f", female)),
             x = 1:(male + female))
})

gdat$char = ifelse(gdat$gender == "m", "p", "u")
ggplot(gdat, aes(x = x, y = factor(educode))) +
  geom_text(aes(label = char, colour = gender),
            family = "wmpeople1", size = 8) +
  scale_x_continuous("Población）") +
  scale_y_discrete("Población",
                   labels = unique(dat$edu[order(dat$educode)])) +
  scale_colour_hue(guide = FALSE) +
  ggtitle("Población de la CDMX por alcaldía. ENSU 2023, INEGI.") +xlim(c(0, 20))
