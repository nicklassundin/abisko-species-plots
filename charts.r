list.of.packages <- c('ggplot2', 'readxl');

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://ftp.acc.umu.se/mirror/CRAN/")

library(ggplot2)
library(readxl);


data <- read_xlsx(list.files('Data', full.names = TRUE)[2]);
# years <- unique(sapply(data$Date, function(x){ return(substring(as.Date(x), 1, 4)) }))

fiveDays <- function(dates){
	result = sapply(dates, function(x){
				return(floor(x/ 5)*5)
})
	return(result)
}


data$day <- fiveDays(data$DOY)
data
insertGroup <- function(group, phase = NA){
	res <- unique(data$Species[data$`Functional Group` %in% group]);
	res <- sapply(res, function(x){
			      full = NA;
			      if(!is.na(phase)){
				      full <- data$day[(data$Species %in% x)[data$Phenophase %in% phase]]
				      if(length(full) < 1) return(Inf)
			      }else{
				      full <- data$day[data$Species %in% x]
			      }
			      return(min(full))
}) 
	return(res)
}

output <- list();
output$period <- unique(data[order(data$day),]$day);
period <- seq(min(output$period), max(output$period), by = 5);
completeGroup <- function(group){
	res <- list();
	res$first <- insertGroup(group);
	res$flow <- insertGroup(group, "Flowering")
	res$fruit <- insertGroup(group, "Fruit")
	res$senescence <- insertGroup(group, "Senescence")
	res$leaf <- insertGroup(group, "Leaf development")
	# All will be catogorized as other
	res$other <- insertGroup(group, "All")
	res$new <- sapply(period, function(x){
				  return(length(res$first[res$first %in% x]));
})
	res$total <- sapply(period, function(x){
				    return(sum(res$new[period < x]))
})
	return(res)
}
species <- list();
species$w <- completeGroup("W");
species$g <- completeGroup("G");
species$f <- completeGroup("F");
species$p <- completeGroup("P");

# species
warnings()
output$total <- species$w$total + species$g$total + species$f$total + species$p$total;
# output

theme_set(theme_minimal())



df <- data.frame(
		 groups = c(rep(c("Total"), each = length(period)), 
			    rep(c(" Woody Plants (Trees & Shrubs)"), each = length(period)),
			    rep(c(" Graminoids"), each = length(period)),
			    rep(c(" Forbs"), each = length(period)),
			    rep(c(" Primitive Plants"), each = length(period))) ,
		 period = c(period),
		 group = c(output$total,
			   species$w$new,
			   species$g$new,
			   species$f$new,
			   species$p$new),
		 new = c(rep(c("Total"), each = length(period)), 
			    rep(c("New"), each = length(period)*4)))
# df
p2 <- ggplot() + 
	geom_bar(aes(y = group, x = period, fill = new), data = df, stat="identity") +
	labs(title = "Species Discovery by Date") +
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Number of Species")
p2
p1 <- ggplot() + 
	geom_bar(aes(y = group, x = period, fill = groups), data = df, stat="identity") +
	labs(title = "Number of new species by Plant Functional Group") + 
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Number of Species")

p1
