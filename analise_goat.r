### LEITURA DOS DADOS
library(readr)
messi_data <- read_csv("messi_data.csv")
cr7 <- read_csv("cr7.csv")

messi_data <- na.omit(messi_data)
cr7 <- na.omit(cr7)

## ANÁLISE EXPLÍCITA

### Checando se há NANs nos conjuntos de dados
anyNAN <- function(x) {
  any(is.nan(x))
}

sapply(cr7, anyNAN)
sapply(messi_data, anyNAN)

is.na(cr7)
is.na(messi_data)

colSums(is.na(cr7))
colSums(is.na(messi_data))

### Olhando para o resumo estatístico dos conjuntos
summary(cr7)
summary(messi_data)

str(cr7)
str(messi_data)

### Olhando as primeiras observações do conjunto de dados
head(cr7)
head(messi_data)

## TRANSFORMAÇÃO

#### Remoção de espaços em branco na coluna de assist
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

messi_data$Playing_Position <- trim(messi_data$Playing_Position)
cr7$Playing_Position <- trim(cr7$Playing_Position)

#### Transformando data da temporada para padronizar visualizações
library(plyr)
messi_data$Season <- mapvalues(messi_data$Season, from=c("11-Dec", "Dec-13"), to=c("11/12", "12/13"))
cr7$Season <- mapvalues(cr7$Season, from=c("Dec-13"), to=c("12/13"))

## ANÁLISE EXPLORATÓRIA DE DADOS

library(ggplot2)
### RONALDO - CR7

### Gols por competição - CR7
ggplot(cr7, aes(x = Competition)) +
  geom_bar() 

### Gols por temporada - CR7
ggplot(cr7, aes(x = Season)) +
  geom_bar() 

### Gols por tipo - CR7
ggplot(cr7, aes(x = Type)) +
  geom_bar() 

### Gols por posição - CR7
ggplot(cr7, aes(x = Playing_Position)) +
  geom_bar() 

### LIONEL MESSI - LM10

### Gols por competição - M10
ggplot(messi_data, aes(x = Competition)) +
  geom_bar() 

### Gols por temporada - M10
ggplot(messi_data, aes(x = Season)) +
  geom_bar() 

### Gols por tipo - M10
ggplot(messi_data, aes(x = Type)) +
  geom_bar() 

### Gols por posição - M10
ggplot(messi_data, aes(x = Playing_Position)) +
  geom_bar() 

### ANÁLISE IMPLÍCITA - CLUSTERING

library(klaR)

set.seed(1)

## run algorithm on Messi Data:
(m10 <- kmodes(messi_data, 3))

plot(jitter(x), col = m10$cluster)
points(cl$modes, col = 1:5, pch = 8)

## run algotithm on Ronaldo Data:
(ronaldo <- kmodes(cr7, 3))

plot(jitter(x), col = ronaldo$cluster)
points(cl$modes, col = 1:5, pch = 8)

### Evaluating Clustering Models
'''
Purity is quite simple to calculate. We assign a label to each cluster based on the most frequent class in it. Then the purity becomes the number of correctly matched class and cluster labels divided by the number of total data points.

Consider a case where our clustering model groups the data points into 3 clusters as seen below:
  
  
  (image by author)
Each cluster is assigned with the most frequent class label. We sum the number of correct class labels in each cluster and divide it by the total number of data points.


(image by author)
In general, purity increases as the number of clusters increases. For instance, if we have a model that groups each observation in a separate cluster, the purity becomes one.

For this very reason, purity cannot be used as a trade off between the number of clusters and clustering quality.'''
ClusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

ClusterPurity(m10[1], 3)

ClusterPurity(ronaldo[1], 3)

### A pureza nos devolve 1 pois prevemos exatamente o mesmo conjunto de dados que utilizamos na previsão
### Como nosso conjunto deve fins analíticos e não preditivos, escolhemos essa abordagem

