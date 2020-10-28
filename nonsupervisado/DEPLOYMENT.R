
rm(list=ls())
gc()


########## 1) Librerias a utilizar ################# 


install.packages(c("XML","pmml"),dependencies = TRUE)

install.packages("WDI")
library(XML)
library(pmml)

 library(datasets)
 data(iris)
 
 
 iris2 <- iris
  iris2$virginica <- iris$Species == "virginica"
  
  
  
 
   logitModel <- glm(virginica ~ Petal.Width + Petal.Length + Sepal.Length + Sepal.Width, data=iris2, family=binomial)
   
   
   plot(logitModel)
 
   
   logitModel 

   
   
   pmml(logitModel)     
   
   
   
   saveXML(pmml(logitModel), file="logitModel.xml")
   
   
   
   