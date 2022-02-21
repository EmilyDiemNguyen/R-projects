data.germany <- read.csv("E:/DAB 3A/DAB 303 MA/Class 11/sample.csv")

View(data.germany)

str(data.germany)

a = matrix(0,nrow = 12, ncol = 12)

a = as.data.frame(a)

View(a)

colnames(a) = colnames(data.germany)[-1]

rownames(a) = colnames(a)

View(a)

for (i in 1:12)
  for (j in 1:12)
    a[i,j] = cor(data.germany[,(i+1)],data.germany[,(j+1)])

View(a)

write.csv(a,"samplesimilarity.csv")

a = (a-min(a)) / (max(a) - min(a))
View(a)

r = data.germany
r = r-r

View(r)

r$user = data.germany$user
View(r)

View(r[,2:13])

for (i in 1:19)
  for (j in 1:12)
    if (data.germany [i,(j+1)]!=1){
      
      k = sort(a[,j], decreasing = TRUE) [2:6]
      l = order(-a[,j])[2:6]
      h = as.numeric(data.germany[i,(l+1)])
      
      r[i,(j+1)] = sum(h*k)/sum(k)
    }

View(r)

reco = matrix(0,nrow = 19, ncol = 5)
reco = as.data.frame(reco)

View(reco)

colnames(reco) = c("Reco1","Reco2","Reco3","Reco4","Reco5")

View(reco)

for (i in 1:19)
  reco [i,] = colnames(data.germany)[order(-r[i,c(2:12)])+1] [1:5]

View(reco)

reco$user = data.germany$user

View(reco)

library(dplyr)

reco = select(reco,"user","Reco1","Reco2","Reco3","Reco4","Reco5")

View(reco)