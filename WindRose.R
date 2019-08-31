#WindRose.R
#created by Artem Pavlovskii 2018-08-31

user.date="2018-12-31" #specify date of start

library(reshape)
data <- read.csv('wind.csv') #prepared weather data from rp5.ru for Saint-Petersburg, Russia
data <- na.omit(data)
chart.title=paste("Wind rose from", user.date, sep=" ")
data <- subset(data, as.Date(DateTime) > as.Date(user.date))
molten.data <- melt(data[c('direction','digits','Ff')], id = c('direction','digits'))
occurencies <- cast(molten.data, direction+digits~variable,length)
names(occurencies)[names(occurencies)=='Ff'] <- 'frequency'
mean.value <- cast(molten.data, direction+digits~variable,mean)
occurencies['digits'] <- occurencies['digits']*(-pi)/8+pi/2
megred.data <- cbind(occurencies, mean.value['Ff'])
clean.data <- megred.data[order(-megred.data$digits),]
print(clean.data)

#graphical section
require(ggplot2)
PolarPlot <- structure(list(x = structure(1:length(clean.data$frequency), .Label = as.character(clean.data$direction), class = "factor"), 
    y = clean.data$frequency), .Names = c("x", "y"),
    class = "data.frame", row.names = c(NA, -length(clean.data$frequency)))	

p <- ggplot(PolarPlot, aes(x, y, fill=x)) +
      geom_bar(width=0.2,stat="identity") +
	  ggtitle(chart.title) +
      xlab("") + ylab("") +
      theme(legend.position = "none" , axis.text.y = element_blank() ,
            axis.ticks = element_blank()) +
      scale_y_continuous(limits = c(-100, max(clean.data$frequency)))	

	  
p + coord_polar(start = -pi/16)	
#this produce a pdf in work directory

#ggsave('ggtest.jpg')

#png(file = "test.png")
#pie(clean.data$frequency, clean.data$direction)
#barplot(clean.data$frequency, names.arg=clean.data$direction)
#dev.off()