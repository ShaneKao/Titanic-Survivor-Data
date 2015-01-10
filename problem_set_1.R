library(RCurl)
library(XML)
library(party)
x <- getURL("https://raw.githubusercontent.com/ShaneKao/Titanic-Survivor-Data/master/titanic_data.csv",
            ssl.verifypeer = FALSE)
data=read.table(text = x,sep=",",header=TRUE)
data_v1=data[,c("Survived","Pclass","Sex","Age")]
for(i in 1:3){
        for(j in c("male","female")){
                index=is.na(data[which(data_v1$Pclass==i & data_v1$Sex==j),"Age"])
                data_v1[which(data_v1$Pclass==i & data_v1$Sex==j),"Age"][index]<-
                        mean(data_v1[which(data_v1$Pclass==i & data_v1$Sex==j),"Age"],na.rm=TRUE)                
        }
}
mytree = ctree(Survived ~ ., data = data_v1)
plot(mytree)
######################
data_v2=data[,c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")]
summary(data_v2)
for(i in 1:3){
        for(j in c("male","female")){
                for(k in c("S","C","Q")){
                        index=is.na(data[which(data_v2$Pclass==i & data_v2$Sex==j & data_v2$Embarked==k),"Age"])
                        data_v2[which(data_v2$Pclass==i & data_v2$Sex==j & data_v2$Embarked==k),"Age"][index]<-
                                mean(data_v2[which(data_v2$Pclass==i & data_v2$Sex==j & data_v2$Embarked==k),"Age"],na.rm=TRUE)
                }
                
        }
}
head(data_v2)

mytree = ctree(Survived ~ ., data = data_v2)
plot(mytree)
