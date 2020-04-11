df<-read.csv("activity.csv")
df$date<-as.Date(df$date , format = "%Y-%m-%d")


x<-tapply(df$steps,df$date,sum)
df2<-data.frame(date=names(x), steps=as.vector(x))

library(ggplot2) 
g<-ggplot(df2,aes(x=steps ) ) + 
   geom_histogram(binwidth=1000,fill="lightblue" , color="black") +
   geom_vline(data=df2,aes(xintercept = mean(steps , na.rm=TRUE)), color = "blue" , size= 1.2  ) +
   geom_vline(data=df2,aes(xintercept = median(steps , na.rm=TRUE) ) , color="red" , size=1.2 ) +
   labs(title = "Number of steps taken each day" , x="number of steps")
print(g)
ggsave("figures\\plot1.png") 

sprintf("Mean = %f",mean(df2$steps , na.rm=TRUE))
sprintf("Median = %f",median(df2$steps , na.rm=TRUE))

x<-tapply(df$steps , df$interval , mean , na.rm=TRUE)
df3<-data.frame(interval= as.numeric(names(x)), steps= as.vector(x) )

g<-ggplot(data=df3 , aes(x=interval , y=steps)) + 
   geom_line(color="red")+
   scale_x_continuous(breaks=seq(0,2355,200) )+
  labs(title="Average steps taken per interval", x="time interval", y="number of steps")

print(g)

ggsave("figures\\plot2.png")

## max interval
df3[ which.max(df3$steps), 1]


## Number of rows with missing values
print (nrow(df[is.na(df$steps)  , ]))
## Percentage % of the missing values 
print(nrow(df[is.na(df$steps)  , ])/ nrow(df) *100)


df4<-df
for(i in 1:nrow(df4))
{
  if(is.na(df4[i,1]) )
    df4[i,1]<-df3[df3$interval==df4[i,3] , 2]
}

x<-tapply(df4$steps , df4$date , sum)
df5<-data.frame(date=names(x), steps=as.vector(x))

g<-ggplot(df5, aes(steps)) +
   geom_histogram(binwidth=1000,color="black" , fill="lightblue")+
   geom_vline( data=df5 , aes(xintercept = mean(steps) ) , color="red", size=1.2)+
   geom_vline( data=df5 , aes(xintercept = median(steps)), color="blue", size=1.2)+
   labs(title = "Number of steps taken each day (with ampitued NAs)" , x="number of steps")
   
   print(g)
   ggsave("figures\\plot3.png")
   
   print(mean(df5$steps))
   print(median(df5$steps))
   
   ##impact , days with all NA now have 10766.19 as mean which impacted the mean

   
   df<-df4
   x<-(weekdays(df$date) %in% c("samedi","dimanche"))
   df$type[x==TRUE]<-"weekend"
   df$type[x==FALSE]<-"weekday"
   df$type<-as.factor(df$type)
   
   x<-tapply(df$steps ,list(df$type,df$interval), mean)
   df6<-data.frame(steps=numeric(), interval=numeric(), type=factor(levels = c("weekday","weekend")))
   for(i in 1:ncol(x)) 
     for(j in 1:nrow(x))
     {
       df6[nrow(df6)+1 , ]<- list(steps=x[j,i] , 
                                interval=as.numeric(colnames(x)[i]) ,
                                type=rownames(x)[j]
                                )
     }
   
   g<-ggplot(df6,aes(interval,steps)) + 
      geom_line(color="red" )+
      facet_grid(type~.) +
     scale_x_continuous(breaks=seq(0,2355,200) )+
     labs(title="Average steps taken per interval (weekdays/weekends)", x="time interval", y="number of steps")
    print(g)
  ggsave("figures\\plot4.png")
