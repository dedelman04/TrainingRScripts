f1<-file("Data/Assignment 5.dat",open="r")
my.data<-read.table(f1,skip=4,comment.char="%",nrows=7)
my.data2<-read.table(f1,skip=3,sep=";",dec=",",nrows=2)
my.data3<-read.table(f1,skip=5,na.strings="-9999",sep=",",nrows=2)
my.all.data<-rbind(my.data,my.data2,my.data3)
