library(ggmap)
library(readxl)
library(tidyverse)
library(broom)
library(MuMIn)
library(knitr)
library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)

CMadres<- read_excel("C:/Users/Julio César/Desktop/2018-02.13 Datos Alejandra Herrerías/Documents/TesisM.C/Datos Ale-2015.xlsx",sheet = "CMadres")

CHijas <- read_excel("C:/Users/Julio César/Desktop/2018-02.13 Datos Alejandra Herrerías/Documents/TesisM.C/Datos Ale-2015.xlsx",sheet = "Fragmentos")

CoorCM<- read_excel("C:/Users/Julio César/Desktop/2018-02.13 Datos Alejandra Herrerías/Documents/TesisM.C/Datos Ale-2015.xlsx",sheet = "Mapa")


CM<-CMadres %>% filter(!is.na(`DiamMin`),!is.na(Altura),!is.na(`PorcentMuerto`),
                       !is.na(PorcentVivo),!is.na(`AreaMuerta`)) 


##########area de estudio##########

mapa <- get_map(location = c(lon = -86.8788, lat = 20.826),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 15)
ggmap(mapa,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")
+ 
  geom_point(aes(x= "Longitud",y= "Latitud", colour="yellow"),data= CoorCM)



mapa + scale_color_discrete (name= "Localización de los Parques Naturales",
                               breaks= c("FALSE", "TRUE"))


###ejemplo###

mapImageData1 <- get_map(location = c(lon = -0.016179, lat = 51.538525),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 17)

ggmap(mapImageData1,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")

########## modelos ############



mod1<-lm(AreFrag.m2 ~ frag,data=CM)
View(tidy(mod1))
View(augment(mod1))




####imagen#####
```{r, echo=FALSE}
knitr::include_graphics("nombre de la imagen")
```


##############3
ggplot(bodysize, aes(`Edad del clado(mya)`,`log(Riqueza de especies)`)) + 
  geom_point(aes(size =`log(Tamaño corporal)`)) + stat_smooth(method = "lm", 
                  formula = y ~ I(x^2) + x, alpha = 0.5)



#######2da.base de datos####

CM2<-CMadres %>% filter(!is.na(`DiamMin`),!is.na(Altura),!is.na(`PorcentMuerto`),
                       !is.na(PorcentVivo),!is.na(`AreaMuerta`)) %>% select(Grupo,Col,
                       frag,AreFrag.m2,TAS,Signo,Signo2,algas)




CM2$C.A2015



size<- c(2104, 1416, 1534, 852)
price <- c(460, 232, 315, 178)
data1 <- data.frame(size, price)


plot(size, price, col = 2, pch = 16,
     xlab = "Size in feet²", ylab = "Price ($) in 1000", main = "Size versus Price")


#makes a linear regression of the data
nn<- lm(AreFrag ~ 0.87, data = CM)


#uses the lm_prices' coef to plot the best line
abline(coef(lm_prices), lwd = 1)
lines(size[order(size)], price[order(price)], col = "blue")


#predic the price for 1200 feet²
size_1200 <- data.frame(size = 1200)
predict(lm_prices, size_1200)


ggplot(CM)+geom_histogram(aes(DiamMax),bins = 20,colour="light blue",
            fill="dark blue")+theme_classic()




ge```{r, fig.width=350, fig.height=250,echo=FALSE}
knitr::include_graphics("C:/Users/Julio César/Pictures/perfil del arrecife.png")

```




Sindrome = c("Signo Amarillo","Signo Blanco","Banda Negra","Puntos Negros","Blanqueamiento","Tumores")

Abreviacion = c("YS","WS","BB","DS","Bp/Bs/Be","TUM")

Caracteristicas = c("Presencia de anillos o bandas de color amarillo brillante translúcido; 
          en etapas tempranas las manchas amarillas rodean parches de tejido muerto del coral", 
          "Comienza en un punto de infección, e irradia hacia el exterior a una velocidad constante; 
          en el límite de la lesión se forma un arco uniforme, mientras avanza, una sucesión de 
          algas coloniza la banda blanca del esqueleto expuesto", "Conjunto microbiano que forma una 
        banda que se mueve a través de las colonias de corales, destruyendo activamente tejido del coral y 
        dejando atrás el esqueleto del mismo desnudo, blanco y recién expuesto rodeado por dicha banda. 
        El espesor de la banda puede ir de 1 a 30 mm", "Pequeñas manchas redondas, 
        más oscuras que el tejido normal. Con el tiempo crecen en tamaño, pueden estar asociadas a una 
        depresión en la superficie de los corales o expandirse en forma de anillo rodeando la porción de 
        coral muerto",	"Pérdida del color característico, tornándose pálido o incluso tomando la apariencia
        de un blanco brillante; debido a la reducción en el número de zooxanthellae", "Crecimiento anormal
      del tejido coralino y del esqueleto. Presenta adelgazamiento del tejido, pérdida de células secretoras
      de mucus y nematocistos, disminución de zooxantelas")
Tasa_de_avance = c("0.1-0.4 mm/día", "3.1 mm/día","3.1 mm/día", "NA","NA","NA")

Sindromes<-cbind.data.frame(Sindrome,Abreviacion,Caracteristicas,Tasa_de_avance)












