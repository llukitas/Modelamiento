
rm(list=ls())
gc()


##### -- Clustering  -- ####



########## 1) Librerias a utilizar ################# 


#install.packages(c("useful","cluster"),dependencies = TRUE)

install.packages("WDI")
library(WDI)

########## 2) Carga de data ################# 

indicators <- c( "NY.GDP.DEFL.KD.ZG", "NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG" ,"NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG" , "TG.VAL.TOTL.GD.ZS") 


 # extraiga información sobre estos indicadores para todos los países de nuestra lista
 # no todos los países tienen información para todos los indicadores
 # algunos países no tienen datos
wbInfo <- WDI(country="all", indicator=indicators, start=2011,  end=2011, extra=TRUE) # deshacerse de la información agregada
 

########## 3) Exploratorio #################

########## 3.1) Limpieza ################# 

wbInfo <- wbInfo[wbInfo$region != "Aggregates", ] # deshacerse de los países donde todos los indicadores son NA
wbInfo <- wbInfo[which(rowSums(!is.na(wbInfo[, indicators])) > 0), ] 
# deshacerse de las filas donde falta la iso
wbInfo <- wbInfo[!is.na(wbInfo$iso2c), ]

# establecer nombres de filas para que sepamos el país sin usarlo para la agrupación
rownames(wbInfo) <- wbInfo$iso2c # refactorizar región, ingresos y préstamos
# esto tiene en cuenta cualquier cambio en los niveles

wbInfo$region <- factor(wbInfo$region) 
wbInfo$income <- factor(wbInfo$income) 
wbInfo$lending <- factor(wbInfo$lending)


# Numero de cnglomerados raiz cuadara del numero de filas

sqrt(nrow(wbInfo))



# encontrar qué columnas conservar
# no los de este vector
 keep.cols <- which(!names(wbInfo) %in% c("iso2c", "country", "year",  "capital", "iso3c")) # encajan en la agrupación

 x=wbInfo[, keep.cols]
 
 
 x=na.omit(x)
 
 wbPam <- pam(x, k=12, keep.diss=TRUE, keep.data=TRUE) 
  # mostrar las observaciones medoides
wbPam$medoids

# hacer un gráfico de silueta
plot(wbPam, which.plots=2, main="")

