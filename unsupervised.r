library(ggplot2)

library(corrplot)
library(lattice)
library(caret)
library(clusterSim)
library(Rtsne)
library("fpc")
library(scatterplot3d)
library(dplyr)

library(heatmaply)

set.seed(123)



####### import de data ######


#read.table(file, header = FALSE, sep = "", dec = ".")

my_data <- read.table("C:\\Users\\GIGABYTE\\Downloads\\data2.txt", header = FALSE, sep = "", dec = ".")
my_data2 <- read.table("C:\\Users\\GIGABYTE\\Downloads\\data2.txt", header = FALSE, sep = "", dec = ".")
labels <- read.table("C:\\Users\\GIGABYTE\\Downloads\\labels.txt", header = FALSE, sep = "", dec = ".")

#### name labels#####

label2 = as.data.frame(labels)
names(label2) = c("label")

label2 <-label2 %>% 
  mutate(class=case_when(
    label==1 ~ 'marcher',
    label==2 ~ 'monter',
    label==3 ~ 'descendre',
    label==4 ~ 'assis',
    label==5 ~ 'se lever',
    label==6 ~ 'allonger'
  ))

my_data2$label=labels
my_data2<-my_data2 %>% 
  mutate(class=case_when(
    label==1 ~ 'marcher',
    label==2 ~ 'monter',
    label==3 ~ 'descendre',
    label==4 ~ 'assis',
    label==5 ~ 'se lever',
    label==6 ~ 'allonger'
  ))


#####################
#look for na
sum(is.na(my_data))


#####################
########## dendogram function
myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){

  y <- rep(hclust$height,2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]
  x <- x[which(x<0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], 
        col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )}

#############BAR PLOT ###############
b<-barplot(table(my_data2$class),col= c("green","red","black","blue", "yellow","brown"),
           ylim=c(0,2000), cex.names=0.8, space=02)
text(b, table(my_data2$class)+50, table(my_data2$class))
######################


#Heatmap for dependent variables
heatmaply_cor(
  cor(my_data),
  xlab = "Features", 
  ylab = "Features",
  k_col = 2, 
  k_row = 2
)



#################" Removing linearly correlated variables  #################
correlationMatrix <- cor(my_data)
dim(as.matrix(highlyCorrelated))

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.9)

my_data_select <- my_data[,-c(highlyCorrelated)]

dim(my_data_select)

##############################################################################

############################ PCA Dimensionality reduction ###################

pca_result <- prcomp(my_data,rank = 2)
summary(pca_result)
df_pca = as.data.frame(pca_result$x)
df_pca = cbind(df_pca,labels)
names(df_pca)<- c("PC1","PC2","lab")


####pca visualization####
ggplot(df_pca, aes(PC1, y=PC2, color=as.factor(label2$class)) ) + geom_point()


#############################################################################






scatterplot3d(x=tsne5$Y[,1],y=tsne5$Y[,2],z=tsne5$Y[,3],
              color = c("red","green","blue", "orange", "brown", "purple")[as.matrix(labels)])



############### TSNE CONFIGURATIONS ##################
tsne1 <- Rtsne(my_data, dims = 2, perplexity=30, verbose=TRUE, max_iter = 1500)
tsne2 <- Rtsne(my_data, dims = 2, perplexity=30, verbose=TRUE, max_iter = 2500)
tsne3 <- Rtsne(my_data, dims = 2, perplexity=50, verbose=TRUE, max_iter = 1500)
tsne4 <- Rtsne(my_data, dims = 2, perplexity=50, verbose=TRUE, max_iter = 2500)


tsne5 <- Rtsne(my_data, dims = 3, perplexity=30, verbose=TRUE, max_iter = 1500)
tsne6 <- Rtsne(my_data, dims = 3, perplexity=30, verbose=TRUE, max_iter = 2500)
tsne7 <- Rtsne(my_data, dims = 3, perplexity=50, verbose=TRUE, max_iter = 1500)
tsne8 <- Rtsne(my_data, dims = 3, perplexity=50, verbose=TRUE, max_iter = 2500)


####tsne 2D visualization####
tnse4_df = as.data.frame(tsne4$Y)
tnse4_df = cbind(tnse4_df,labels)
names(tnse4_df) = c("c1", "c2", "lab")
ggplot(tnse4_df, aes(c1, y=c2, color=as.factor(label2$class)) ) + geom_point()



####TSNE 3D plot######
#tsne5
s3d <- scatterplot3d(x=tsne5$Y[,1],y=tsne5$Y[,2],z=tsne5$Y[,3],
                     color = c("red","green","blue", "orange", "brown", "purple")[as.matrix(labels)])



legend(s3d$xyz.convert(50, 0, 25), col= c("red","green","blue", "orange", "brown", "purple"), bg="white", lty=c(1,1), lwd=2, yjust=0, legend = c("marcher", "monter", "descendre", "assis", "se lever", "allonger"), cex = 1.1)



  #tsne8
s3d <- scatterplot3d(x=tsne8$Y[,1],y=tsne8$Y[,2],z=tsne8$Y[,3],
                     color = c("red","green","blue", "orange", "brown", "purple")[as.matrix(labels)])



legend(s3d$xyz.convert(50, 0, 25), col= c("red","green","blue", "orange", "brown", "purple"), bg="white", lty=c(1,1), lwd=2, yjust=0, legend = c("marcher", "monter", "descendre", "assis", "se lever", "allonger"), cex = 1.1)


########################################################################################





##################EXPERIMENT comparing Kmeans results#####################################

######################Without feature selecton#######################"
km = kmeans(my_data,6,iter.max = 50,nstart=50)
length(km$cluster)
print(comparing.Partitions(km$cluster,as.vector(as.matrix(labels))))
#######################################################################

######################With dependent variable removal#######################"
km = kmeans(my_data_select,6,iter.max = 50,nstart=50)
length(km$cluster)
print(comparing.Partitions(km$cluster,as.vector(as.matrix(labels))))
#######################################################################


######################With PCA dimensionality reduction#######################"
km = kmeans(pca_result$x,6,iter.max = 50,nstart=50)
length(km$cluster)
print(comparing.Partitions(km$cluster,as.vector(as.matrix(labels))))
#######################################################################


######################With tsne dimensionality reduction#######################"
km = kmeans(tsne4$Y,6,iter.max = 50,nstart=50)
length(km$cluster)
print(comparing.Partitions(km$cluster,as.vector(as.matrix(labels))))
#######################################################################

########################################################################################




#########################TESTING KMEAN WITH DIFF CONFIGURATIONS #################"
km1 = kmeans(tsne1$Y,6,iter.max = 50,nstart=50)
km2 = kmeans(tsne2$Y,6,iter.max = 50,nstart=50)
km3 = kmeans(tsne3$Y,6,iter.max = 50,nstart=50)
km4 = kmeans(tsne4$Y,6,iter.max = 50,nstart=50)
km5 = kmeans(tsne5$Y,6,iter.max = 50,nstart=50)
km6 = kmeans(tsne6$Y,6,iter.max = 50,nstart=50)
km7 = kmeans(tsne7$Y,6,iter.max = 50,nstart=50)
km8 = kmeans(tsne8$Y,6,iter.max = 50,nstart=50)


print(comparing.Partitions(km1$cluster,as.vector(as.matrix(labels))))
print(comparing.Partitions(km2$cluster,as.vector(as.matrix(labels))))
print(comparing.Partitions(km3$cluster,as.vector(as.matrix(labels))))
print(comparing.Partitions(km4$cluster,as.vector(as.matrix(labels))))
print(comparing.Partitions(km5$cluster,as.vector(as.matrix(labels))))
print(comparing.Partitions(km6$cluster,as.vector(as.matrix(labels))))
print(comparing.Partitions(km7$cluster,as.vector(as.matrix(labels))))
print(comparing.Partitions(km8$cluster,as.vector(as.matrix(labels))))

### confusion matrix for best result
table(km4$cluster,my_data2$class)

###################################################################################





###################################TESTING CAH WITH DIFFERENT CONFIGURATIONS ##########"
D1 = dist(tsne1$Y, method="euclidean")
H1 = hclust(D1, method="ward.D2")

classes1 <- cutree(H1, k=6)
vec_pred1 = as.vector(as.matrix(classes1))
vec_lab1 = as.vector(as.matrix(labels))
print(comparing.Partitions(vec_pred1,vec_lab1))


D2 = dist(tsne2$Y, method="euclidean")
H2 = hclust(D2, method="ward.D2")

classes2 <- cutree(H2, k=6)
vec_pred2 = as.vector(as.matrix(classes2))
vec_lab2 = as.vector(as.matrix(labels))
print(comparing.Partitions(vec_pred2,vec_lab2))

D3 = dist(tsne3$Y, method="euclidean")
H3 = hclust(D3, method="ward.D2")

classes3 <- cutree(H3, k=6)
vec_pred3 = as.vector(as.matrix(classes3))
vec_lab3 = as.vector(as.matrix(labels))
print(comparing.Partitions(vec_pred3,vec_lab3))

D4 = dist(tsne4$Y, method="euclidean")
H4 = hclust(D4, method="ward.D2")

classes4 <- cutree(H4, k=6)
vec_pred4 = as.vector(as.matrix(classes4))
vec_lab4 = as.vector(as.matrix(labels))
print(comparing.Partitions(vec_pred4,vec_lab4))

D5 = dist(tsne5$Y, method="euclidean")
H5 = hclust(D5, method="ward.D2")

classes5 <- cutree(H5, k=6)
vec_pred5 = as.vector(as.matrix(classes5))
vec_lab5 = as.vector(as.matrix(labels))
print(comparing.Partitions(vec_pred5,vec_lab5))

D6 = dist(tsne6$Y, method="euclidean")
H6 = hclust(D6, method="ward.D2")

classes6 <- cutree(H6, k=6)
vec_pred6 = as.vector(as.matrix(classes6))
vec_lab6 = as.vector(as.matrix(labels))
print(comparing.Partitions(vec_pred6,vec_lab6))

D7 = dist(tsne7$Y, method="euclidean")
H7 = hclust(D7, method="ward.D2")

classes7 <- cutree(H7, k=6)
vec_pred7 = as.vector(as.matrix(classes7))
vec_lab7 = as.vector(as.matrix(labels))
print(comparing.Partitions(vec_pred7,vec_lab7))

D8 = dist(tsne8$Y, method="euclidean")
H8 = hclust(D8, method="ward.D2")

classes8 <- cutree(H8, k=6)
vec_pred8 = as.vector(as.matrix(classes8))
vec_lab8 = as.vector(as.matrix(labels))
print(comparing.Partitions(vec_pred8,vec_lab8))


##### confusion matrix  and dendogram visualization for the best model
table(vec_pred4,my_data2$class)
myplclust(H4, lab.col=unclass(factor(my_data2$class)))

###################################################################"



#############################"CHOOSING DBSCAN EPSILON WITH DIFF CONFIGURATIONS ################

####note : there seems to be a problem with the way random seeds are handled by R which makes the results
# on dbscan change from one initialisation to the other even when the seed is initialized.

for (i in seq(7, 15, 0.1)){
  print(i)
  db1 <- dbscan(tsne1$Y, eps = i, MinPts = 300)
  #TSNE 1 9 60.2%
  print(comparing.Partitions(db1$cluster,vec_lab1))
  
}


for (i in seq(7, 15, 0.1)){
  print(i)
  db1 <- dbscan(tsne2$Y, eps = i, MinPts = 300)
  #  TSNE 2 9.8 61.90%

  print(comparing.Partitions(db1$cluster,vec_lab1))
  
}

for (i in seq(5, 15, 0.1)){
  print(i)
  db1 <- dbscan(tsne3$Y, eps = i, MinPts = 300)
  #  TSNE 3 7.1 61.8%
  print(comparing.Partitions(db1$cluster,vec_lab1))
  
}

for (i in seq(5, 15, 0.1)){
  print(i)
  db1 <- dbscan(tsne4$Y, eps = i, MinPts = 300)
  #  TSNE 4 7.7 61.4%

  print(comparing.Partitions(db1$cluster,vec_lab1))
  
}

for (i in seq(9, 15, 0.1)){
  print(i)
  db1 <- dbscan(tsne5$Y, eps = i, MinPts = 300)
  #  TSNE 5 11.1 61.5%
  print(comparing.Partitions(db1$cluster,vec_lab1))
  
}

for (i in seq(9, 15, 0.1)){
  print(i)
  db1 <- dbscan(tsne6$Y, eps = i, MinPts = 300)
  #  TSNE 6 13.2 60%

  print(comparing.Partitions(db1$cluster,vec_lab1))
  
}

for (i in seq(9, 15, 0.1)){
  print(i)
  db1 <- dbscan(tsne7$Y, eps = i, MinPts = 300)
  #  TSNE 7 9.3 60.7%

  print(comparing.Partitions(db1$cluster,vec_lab1))
  
}

for (i in seq(9, 15, 0.1)){
  print(i)
  db1 <- dbscan(tsne8$Y, eps = i, MinPts = 300)
  #  TSNE 8 10.5 61.94%
  print(comparing.Partitions(db1$cluster,vec_lab1))
  
}
#########################################################################





############################ TESTING DBSCAN RESULTS ##########################
vec_lab1 = as.vector(as.matrix(labels))

db1 <- dbscan(tsne1$Y, eps = 9, MinPts = 300)

print(comparing.Partitions(db1$cluster,vec_lab1))

db2 <- dbscan(tsne2$Y, eps = 9.8, MinPts = 300)

print(comparing.Partitions(db2$cluster,vec_lab1))

db3 <- dbscan(tsne3$Y, eps = 7.1, MinPts = 300)

print(comparing.Partitions(db3$cluster,vec_lab1))

db4 <- dbscan(tsne4$Y, eps = 7.7, MinPts = 300)

print(comparing.Partitions(db4$cluster,vec_lab1))

db5 <- dbscan(tsne5$Y, eps = 11.1, MinPts = 300)

print(comparing.Partitions(db5$cluster,vec_lab1))

db6 <- dbscan(tsne6$Y, eps = 13.2, MinPts = 300)

print(comparing.Partitions(db6$cluster,vec_lab1))

db7 <- dbscan(tsne7$Y, eps = 9.3, MinPts = 300)

print(comparing.Partitions(db7$cluster,vec_lab1))

db8 <- dbscan(tsne8$Y, eps = 10.5, MinPts = 300)

print(comparing.Partitions(db8$cluster,vec_lab1))



#####DB8 vizualisation ######################
s3d <- scatterplot3d(x=tsne8$Y[,1],y=tsne8$Y[,2],z=tsne8$Y[,3],
                     color = c("red","green","blue", "orange", "brown", "purple", "yellow")[as.factor(db8$cluster)])
#############################################
######################################################"




