# Primero selecciono la carpeta de trabajo, cargo las librerías a utilizar y cargo la base de datos original.
setwd("~/Desktop/Modelos de Demanda")
install.packages("tidyverse")
library(tidyverse)
df1 <- read_csv("./datos.csv")
# En segundo lugar, selecciono los datos según las comunas asignadas.
df2 <- df1[df1$ComunaOrigen=="SAN BERNARDO" | df1$ComunaOrigen=="RECOLETA" | df1$ComunaOrigen=="EL BOSQUE" | df1$ComunaDestino=="EL BOSQUE" | df1$ComunaDestino=="RECOLETA" | df1$ComunaDestino=="SAN BERNARDO",]
#table(df2$ComunaDestino)
dim(df2)

# Ahora corrijo por datos inconsistentes

df3 <- df2[df2$`Tiempo de Viaje`<=180,]
df4 <- df3[!(df3$ComunaOrigen == df3$ComunaDestino & df3$`Tiempo de Viaje` >90),]

df5 <- df4[!(df4$Discapacidad=="D" & df4$Modo==18),]

df <- df5[!(df5$Ingreso<20000 & df5$Ingreso>=0 & df5$Actividad=="A"),]


# Ahora calculos la cantidad de viajes en cada comuna por tipo de viaje (origen, destino, dentro y fuera de comuna)
df_sb0 <- df[df$ComunaOrigen=="SAN BERNARDO",]
dim(df_sb0)
df_eb0 <- df[df$ComunaOrigen=="EL BOSQUE",]
dim(df_eb0)
df_rec0 <- df[df$ComunaOrigen=="RECOLETA",]
dim(df_rec0)


df_sb <- df[df$ComunaOrigen=="SAN BERNARDO" & df$ComunaDestino=="SAN BERNARDO",]
dim(df_sb)
df_eb <- df[df$ComunaOrigen=="EL BOSQUE" & df$ComunaDestino=="EL BOSQUE",]
dim(df_eb)
df_rec <- df[df$ComunaOrigen=="RECOLETA" & df$ComunaDestino=="RECOLETA",]
dim(df_rec)
df_sb2 <- df[df$ComunaOrigen=="SAN BERNARDO" & df$ComunaDestino!="SAN BERNARDO",]
dim(df_sb2)
df_eb2 <- df[df$ComunaOrigen=="EL BOSQUE" & df$ComunaDestino!="EL BOSQUE",]
dim(df_eb2)
df_rec2 <- df[df$ComunaOrigen=="RECOLETA" & df$ComunaDestino!="RECOLETA",]
dim(df_rec2)
df_sb3 <- df[df$ComunaDestino=="SAN BERNARDO",]
dim(df_sb3)
df_eb3 <- df[df$ComunaDestino=="EL BOSQUE",]
dim(df_eb3)
df_rec3 <- df[df$ComunaDestino=="RECOLETA",]
dim(df_rec3)
df_sb4 <- df[df$ComunaOrigen!="SAN BERNARDO" & df$ComunaDestino=="SAN BERNARDO",]
dim(df_sb4)
df_eb4 <- df[df$ComunaOrigen!="EL BOSQUE" & df$ComunaDestino=="EL BOSQUE",]
dim(df_eb4)
df_rec4 <- df[df$ComunaOrigen!="RECOLETA" & df$ComunaDestino=="RECOLETA",]
dim(df_rec4)

#summary(df2$`Tiempo de Viaje`)
#hist(df2$`Tiempo de Viaje`)

#### ahora vemos a qué comunas se viaja comunas viajamos.

reorder <- function(x) {
  factor(x,levels = names(sort(table(x),decreasing = TRUE)))
}

ggplot(df[df$ComunaOrigen=="SAN BERNARDO" & df$ComunaDestino!="SAN BERNARDO",], aes(x=reorder(ComunaDestino))) +
  geom_bar(aes(y = ((..count..)/sum(..count..)))) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(y="Destinos Viajes", x="")

ggplot(df[df$ComunaOrigen=="RECOLETA" & df$ComunaDestino!="RECOLETA",], aes(x=reorder(ComunaDestino))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(y="Destinos Viajes", x="")

ggplot(df[df$ComunaOrigen=="EL BOSQUE" & df$ComunaDestino!="EL BOSQUE",], aes(x=reorder(ComunaDestino))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(y="Destinos Viajes", x="")


#### ahora veremos los tiempos de viajes

ggplot(df[df$ComunaOrigen=="SAN BERNARDO" | df$ComunaOrigen=="RECOLETA" | df$ComunaOrigen=="EL BOSQUE",], aes(x=`Tiempo de Viaje`, fill=ComunaOrigen)) +
  geom_histogram(bins = 25, alpha=0.8) + 
  geom_vline(aes(xintercept=median(`Tiempo de Viaje`)),
             color="red", linetype="dashed", size=0.6) +
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")+
  scale_x_continuous(breaks = seq(0, 155, 30), lim = c(0, 155)) +
  labs(title="Histograma de Tiempos de Viaje", x="Tiempo de Viaje", y="Cantidad")



#ggplot(df[df$ComunaOrigen=="EL BOSQUE",], aes(x=`Tiempo de Viaje`)) +
#  geom_histogram(bins = 25)

#ggplot(df[df$ComunaOrigen=="RECOLETA",], aes(x=`Tiempo de Viaje`)) +
#  geom_histogram(aes(y = (..count..)/sum(..count..)),bins = 25)


### ahora veremos los modos de transporte

df$Modo[df$Modo==18] <- "Bicicleta"
df$Modo[df$Modo==17] <- "Caminata"
df$Modo[df$Modo==16] <- "Otros"
df$Modo[df$Modo==15] <- "Otros - Bus TS - Metro"
df$Modo[df$Modo==14] <- "Otros - Bus TS"
df$Modo[df$Modo==13] <- "Otros - Metro"
df$Modo[df$Modo==12] <- "Taxi - Metro"
df$Modo[df$Modo==11] <- "Taxi Colectivo - Metro"
df$Modo[df$Modo==10] <- "Bus no TS - Metro"
df$Modo[df$Modo==9] <- "Bus TS - Metro"
df$Modo[df$Modo==8] <- "Auto -  Metro"
df$Modo[df$Modo==7] <- "Bus TS - Bus no TS"
df$Modo[df$Modo==6] <- "Taxi"
df$Modo[df$Modo==5] <- "Taxi Colectivo"
df$Modo[df$Modo==4] <- "Metro"
df$Modo[df$Modo==3] <- "Bus no TS"
df$Modo[df$Modo==2] <- "Bus TS"
df$Modo[df$Modo==1] <- "Auto"



ggplot(df[df$ComunaOrigen=="SAN BERNARDO" | df$ComunaDestino=="SAN BERNARDO",], aes(x=reorder(Modo))) +
  geom_bar(aes(y = ((..count..)/sum(..count..)))) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(x="Modo de transporte", y="Porcentaje de viajes")

ggplot(df[df$ComunaOrigen=="RECOLETA" | df$ComunaDestino=="RECOLETA",], aes(x=reorder(Modo))) +
  geom_bar(aes(y = ((..count..)/sum(..count..)))) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(x="Modo de transporte", y="Porcentaje de viajes")

ggplot(df[df$ComunaOrigen=="EL BOSQUE" | df$ComunaDestino=="EL BOSQUE",], aes(x=reorder(Modo))) +
  geom_bar(aes(y = ((..count..)/sum(..count..)))) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(x="Modo de transporte", y="Porcentaje de viajes")


##### ahora cruzamos modo con tiempo de viaje

ggplot(df, aes(x=reorder(Modo),y=`Tiempo de Viaje`)) +
  geom_bar(stat="summary",fun="mean") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(x="Modo de transporte", y="Tiempo de Viaje Promedio (mins.)")


### ahora vemos los horarios de viaje

df$Periodo[df$Periodo==1] <- "Punta Mañana 1 (6:01 - 7:30)"
df$Periodo[df$Periodo==2] <- "Punta Mañana 2 (7:31 - 9:00)"
df$Periodo[df$Periodo==3] <- "Fuera de Punta 1 (10:01 - 12:00)"
df$Periodo[df$Periodo==4] <- "Punta Tarde (17:31 - 20:30)"
df$Periodo[df$Periodo==5] <- "Fuera de Punta 2 (9:01 - 10:00, 12:01 - 17:30, 20:31 - 23:00)"
df$Periodo[df$Periodo==6] <- "Noche (23:01 - 06:00)"


ggplot(df, aes(x=reorder(Periodo))) +
  geom_bar(aes(y = ((..count..)/sum(..count..)))) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(x="",y="Proporció de viajes")

#### por último veo el cruce entre propósito y horario

df$PropositoAgregado[df$PropositoAgregado==1] <- "Trabajo"
df$PropositoAgregado[df$PropositoAgregado==2] <- "Estudio"
df$PropositoAgregado[df$PropositoAgregado==3] <- "Otro"

ggplot(df, aes(x=PropositoAgregado,y=`Tiempo de Viaje`)) +
  geom_bar(stat="summary",fun="mean") +
  theme(axis.text.x=element_text(angle=45,hjust=1))

devtools::install_github("haleyjeppson/ggmosaic")
library(ggmosaic)

ggplot(data=df) +
  geom_mosaic(aes(x=product(Modo,PropositoAgregado), fill=Modo)) +
  labs(x="Modo de Transporte", x="Propósito de viaje") + 
  theme(legend.position = "none") +
  theme(text = element_text(size=6),
        axis.text.y = element_text(hjust=1)) 

dim(df[df$PropositoAgregado == "Trabajo",])/dim(df)
dim(df[df$PropositoAgregado == "Estudio",])/dim(df)
dim(df[df$PropositoAgregado == "Otro",])/dim(df)

##### ESTRATEGIAS DE MUESTREO

df$`Tiempo de Viaje`[df$`Tiempo de Viaje`<30] <- "Corto"
df$`Tiempo de Viaje`[df$`Tiempo de Viaje`!="Corto"] <- "Largo"


df$Ingreso[df$Ingreso<100000] <- "Bajo"
df$Ingreso[df$Ingreso<=350000] <- "Medio"
df$Ingreso[df$Ingreso!= "Medio" & df$Ingreso!="Bajo"] <- "Alto"


df$Modo [df$Modo=="Auto" | df$Modo=="Auto -  Metro"] <- "Privado"
df$Modo [df$Modo=="Bicicleta" | df$Modo=="Caminata"] <- "No Motorizado"
df$Modo [df$Modo!="Privado" & df$Modo!="No Motorizado"] <- "Público"

set.seed(42)
library(dplyr)
al_rec <- sample_n(df[df$ComunaOrigen=="RECOLETA" | df$ComunaDestino=="RECOLETA",], 250)
set.seed(52)
al_eb <- sample_n(df[df$ComunaOrigen=="EL BOSQUE" | df$ComunaDestino=="EL BOSQUE",], 250)
set.seed(62)
al_sb <- sample_n(df[df$ComunaOrigen=="SAN BERNARDO" | df$ComunaDestino=="SAN BERNARDO",], 250)


dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Medio" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Medio" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Alto" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Alto" & al_rec$Modo=="No Motorizado",])

### comuna el bosque

dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Medio" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Medio" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Alto" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Alto" & al_eb$Modo=="No Motorizado",])

#### san bernardo

dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Medio" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Medio" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Alto" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Alto" & al_sb$Modo=="No Motorizado",])


### ahora estratificado

set.seed(72)
df_recoleta <- df[df$ComunaOrigen=="RECOLETA" | df$ComunaDestino=="RECOLETA",]

est_rec1 <- sample_n(df_recoleta[df_recoleta$Ingreso=="Bajo",], 125)
est_rec2 <- sample_n(df_recoleta[df_recoleta$Ingreso=="Medio",], 95)
est_rec3 <- sample_n(df_recoleta[df_recoleta$Ingreso=="Alto",], 30)

al_rec <- rbind(est_rec1,est_rec2,est_rec3)
dim(al_rec)

dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Medio" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Medio" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Alto" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Alto" & al_rec$Modo=="No Motorizado",])


set.seed(82)
df_bosque <- df[df$ComunaOrigen=="EL BOSQUE" | df$ComunaDestino=="EL BOSQUE",]

est_eb1 <- sample_n(df_bosque[df_bosque$Ingreso=="Bajo",], 125)
est_eb2 <- sample_n(df_bosque[df_bosque$Ingreso=="Medio",], 95)
est_eb3 <- sample_n(df_bosque[df_bosque$Ingreso=="Alto",], 30)

al_eb <- rbind(est_eb1,est_eb2,est_eb3)
dim(al_eb)

dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Medio" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Medio" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Alto" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Alto" & al_eb$Modo=="No Motorizado",])


set.seed(92)
df_bernardo <- df[df$ComunaOrigen=="SAN BERNARDO" | df$ComunaDestino=="SAN BERNARDO",]

est_sb1 <- sample_n(df_bernardo[df_bernardo$Ingreso=="Bajo",], 125)
est_sb2 <- sample_n(df_bernardo[df_bernardo$Ingreso=="Medio",], 95)
est_sb3 <- sample_n(df_bernardo[df_bernardo$Ingreso=="Alto",], 30)

al_sb <- rbind(est_sb1,est_sb2,est_sb3)
dim(al_sb)

dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Medio" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Medio" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Alto" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Alto" & al_sb$Modo=="No Motorizado",])


#### Ahora muestreo por elección


set.seed(102)
df_recoleta <- df[df$ComunaOrigen=="RECOLETA" | df$ComunaDestino=="RECOLETA",]

est_rec1 <- sample_n(df_recoleta[df_recoleta$Modo=="Público",], 100)
est_rec2 <- sample_n(df_recoleta[df_recoleta$Modo=="Privado",], 50)
est_rec3 <- sample_n(df_recoleta[df_recoleta$Modo=="No Motorizado",], 100)

al_rec <- rbind(est_rec1,est_rec2,est_rec3)
dim(al_rec)

dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Bajo" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Medio" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Medio" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Medio" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Corto" & al_rec$Ingreso=="Alto" & al_rec$Modo=="No Motorizado",])

dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Público",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Alto" & al_rec$Modo=="Privado",])
dim(al_rec[al_rec$`Tiempo de Viaje`=="Largo" & al_rec$Ingreso=="Alto" & al_rec$Modo=="No Motorizado",])


set.seed(112)
df_bosque <- df[df$ComunaOrigen=="EL BOSQUE" | df$ComunaDestino=="EL BOSQUE",]

est_eb1 <- sample_n(df_bosque[df_bosque$Modo=="Público",], 100)
est_eb2 <- sample_n(df_bosque[df_bosque$Modo=="Privado",], 50)
est_eb3 <- sample_n(df_bosque[df_bosque$Modo=="No Motorizado",], 100)

al_eb <- rbind(est_eb1,est_eb2,est_eb3)
dim(al_eb)

dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Bajo" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Medio" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Medio" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Medio" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Corto" & al_eb$Ingreso=="Alto" & al_eb$Modo=="No Motorizado",])

dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Público",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Alto" & al_eb$Modo=="Privado",])
dim(al_eb[al_eb$`Tiempo de Viaje`=="Largo" & al_eb$Ingreso=="Alto" & al_eb$Modo=="No Motorizado",])


set.seed(122)
df_bernardo <- df[df$ComunaOrigen=="SAN BERNARDO" | df$ComunaDestino=="SAN BERNARDO",]

est_sb1 <- sample_n(df_bernardo[df_bernardo$Modo=="Público",], 100)
est_sb2 <- sample_n(df_bernardo[df_bernardo$Modo=="Privado",], 50)
est_sb3 <- sample_n(df_bernardo[df_bernardo$Modo=="No Motorizado",], 100)

al_sb <- rbind(est_sb1,est_sb2,est_sb3)
dim(al_sb)

dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Bajo" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Medio" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Medio" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Medio" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Corto" & al_sb$Ingreso=="Alto" & al_sb$Modo=="No Motorizado",])

dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Público",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Alto" & al_sb$Modo=="Privado",])
dim(al_sb[al_sb$`Tiempo de Viaje`=="Largo" & al_sb$Ingreso=="Alto" & al_sb$Modo=="No Motorizado",])

#### ahora calculo los tatales poblacionales (fui cambiando de comuna para no escribir todo de nuevo)

re <-df[df$ComunaOrigen=="SAN BERNARDO" | df$ComunaDestino=="SAN BERNARDO",]

dim(re[re$`Tiempo de Viaje`=="Corto" & re$Ingreso=="Bajo" & re$Modo=="Público",])
dim(re[re$`Tiempo de Viaje`=="Corto" & re$Ingreso=="Bajo" & re$Modo=="Privado",])
dim(re[re$`Tiempo de Viaje`=="Corto" & re$Ingreso=="Bajo" & re$Modo=="No Motorizado",])

dim(re[re$`Tiempo de Viaje`=="Largo" & re$Ingreso=="Bajo" & re$Modo=="Público",])
dim(re[re$`Tiempo de Viaje`=="Largo" & re$Ingreso=="Bajo" & re$Modo=="Privado",])
dim(re[re$`Tiempo de Viaje`=="Largo" & re$Ingreso=="Bajo" & re$Modo=="No Motorizado",])

dim(re[re$`Tiempo de Viaje`=="Corto" & re$Ingreso=="Medio" & re$Modo=="Público",])
dim(re[re$`Tiempo de Viaje`=="Corto" & re$Ingreso=="Medio" & re$Modo=="Privado",])
dim(re[re$`Tiempo de Viaje`=="Corto" & re$Ingreso=="Medio" & re$Modo=="No Motorizado",])

dim(re[re$`Tiempo de Viaje`=="Largo" & re$Ingreso=="Medio" & re$Modo=="Público",])
dim(re[re$`Tiempo de Viaje`=="Largo" & re$Ingreso=="Medio" & re$Modo=="Privado",])
dim(re[re$`Tiempo de Viaje`=="Largo" & re$Ingreso=="Medio" & re$Modo=="No Motorizado",])

dim(re[re$`Tiempo de Viaje`=="Corto" & re$Ingreso=="Alto" & re$Modo=="Público",])
dim(re[re$`Tiempo de Viaje`=="Corto" & re$Ingreso=="Alto" & re$Modo=="Privado",])
dim(re[re$`Tiempo de Viaje`=="Corto" & re$Ingreso=="Alto" & re$Modo=="No Motorizado",])

dim(re[re$`Tiempo de Viaje`=="Largo" & re$Ingreso=="Alto" & re$Modo=="Público",])
dim(re[re$`Tiempo de Viaje`=="Largo" & re$Ingreso=="Alto" & re$Modo=="Privado",])
dim(re[re$`Tiempo de Viaje`=="Largo" & re$Ingreso=="Alto" & re$Modo=="No Motorizado",])




