library(readxl) 
library(dplyr)

seg.df <- data.frame(read_excel("CHANGE THE FILE PATH", 
                                 sheet = "Cluster"))

str(seg.df)

set.seed(1234)

wss <- (nrow(seg.df)-1)*sum(apply(seg.df,2,var))
for (i in 2:20) {
  wss[i] <- sum(kmeans(seg.df,centers=i)$withinss)
}
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

seg.k <- kmeans(seg.df[,c(2,3,4)], centers=2)
seg.k$size

aggregate(seg.df, list(seg.k$cluster), function(x) mean(as.numeric(x)))

seg.df <- seg.df%>% mutate(segment = c(seg.k$cluster))
View(seg.df)

install.packages("writexl")
library("writexl")
write_xlsx(seg.df,"/Users/komori/Downloads/MG403 Individual Take-Home Assignment Documents 2021-20210504/Cluster Q4 result.xlsx")
