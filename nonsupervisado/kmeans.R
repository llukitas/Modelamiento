rm(list=ls())


##### -- Clustering  -- ####



########## 1) Librerias a utilizar ################# 


#install.packages(c("useful","cluster"),dependencies = TRUE)

install.packages("cluster")
library(useful)
library(cluster)



########## 2) Carga de data ################# 
wineUrl <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data' 

wine <- read.table(wineUrl, header=FALSE, sep=',',stringsAsFactors=FALSE, 
         col.names=c('Cultivar', 'Alcohol', 'Malic.acid', 
        'Ash', 'Alcalinity.of.ash', 'Magnesium', 'Total.phenols', 'Flavanoids', 'Nonflavanoid.phenols', 

        
                'Proanthocyanin', 'Color.intensity', 'Hue', 'OD280.OD315.of.diluted.wines', 'Proline' )) 


########## 3) Exploratorio ################# 
head(wine)
str(wine)




########## 3.1) Exploratorio ################# 
wineTrain <- wine[, which(names(wine) != "Cultivar")]



########## 4) Clustering ################# 
########## 4.1) Clustering sin inicio aleatorio  ################# 

set.seed(278613) ###semilla
wineK3 <- kmeans(x=wineTrain, centers=3)

wineK3$cluster
wineK3$size


plot(wineK3, data=wineTrain)

plot(wineK3, data=wine, class="Cultivar")

########## 4.2) Clustering con inicio aleatorio  ################# 

set.seed(278613)
wineK3N25 <- kmeans(wineTrain, centers=3, nstart=25)  # see the cluster sizes with 1 start

# see the cluster sizes with 25 starts 
wineK3N25$size


plot(wineK3N25, data=wineTrain)
plot(wineK3N25, data=wine, class="Cultivar")



########## 5) Numero de clustering  ################# 

wineBest <- FitKMeans(wineTrain, max.clusters=20, nstart=25,  seed=278613) 
wineBest
PlotHartigan(wineBest)






########## 6) Matriz de confusion  ################# 
table(wine$Cultivar, wineK3N25$cluster)

plot(table(wine$Cultivar, wineK3N25$cluster),
            main="Confusion Matrix for Wine Clustering",
            xlab="Cultivar", ylab="Cluster")




########## 7) Estadistica de GAPS  ################# 
theGap <- clusGap(wineTrain, FUNcluster=pam, K.max=20) 
gapDF <- as.data.frame(theGap$Tab) 
gapDF


# logW curves
  ggplot(gapDF, aes(x=1:nrow(gapDF))) +
      geom_line(aes(y=logW), color="blue") +
       geom_point(aes(y=logW), color="blue") +
       geom_line(aes(y=E.logW), color="green") +
       geom_point(aes(y=E.logW), color="green") +
      labs(x="Number of Clusters") 

 # gap curve
 ggplot(gapDF, aes(x=1:nrow(gapDF))) +
       geom_line(aes(y=gap), color="red") +
     geom_point(aes(y=gap), color="red") +
       geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), color="red") +
       labs(x="Number of Clusters", y="Gap")










