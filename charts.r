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
yAxis <- c(0, length(unique(data$Species)));
period <- unique(data[order(data$day),]$day);
period <- seq(min(period), max(period), by = 5);
xAxis <- c(min(period), max(period));


insertGroup <- function(entry, key, keys = unlist(unique(entry[key]))){
	group <- sapply(keys, function(grp){
				table <- entry[entry[[key]] %in% grp,]
				return(sapply(period, function(y){
						      res <- unique(table$Species);
						      res <- sapply(res, function(x){
									    full <- table$day[table$Species %in% x]
									    return(min(full))
})					
						      return(length(res[res %in% y]))
}))
})
	colnames(group) <- keys;
	print(group)
	total <- c(0, apply(group, 1, function(x){ return(sum(x)) }));
	total <- head(Reduce(f = "+", total, accumulate = TRUE), -1)
	group <- cbind(total, group)
	colnames(group)[1] <- "total";
	print(group)
	return(group)
}

completeGroup <- function(){
	return(list(data.frame(
			  period = c(period),
			  groups = c(rep(c("Total"), each = length(period)), 
				     rep(c(" Woody Plants (Trees & Shrubs)"), each = length(period)),
				     rep(c(" Graminoids"), each = length(period)),
				     rep(c(" Forbs"), each = length(period)),
				     rep(c(" Primitive Plants"), each = length(period))),
			  group = c(insertGroup(data, "Functional Group")),
			  new = c(rep(c("Total"), each = length(period)), 
				  rep(c("New"), each = length(period)*4))
			  ),
	       data.frame(
			  period = c(period),
			  phase = c(insertGroup(data, "Phenophase")),
			  phases = c(rep(c("Total"), each = length(period)), 
				     rep(c(" ALL"), each = length(period)),
				     rep(c(" Leaf development"), each = length(period)),
				     rep(c(" Flowering"), each = length(period)),
				     rep(c(" Other"), each = length(period)),
				     rep(c(" Fruit"), each = length(period)),
				     rep(c(" Seed Dispersal"), each = length(period)),
				     rep(c(" Senescence"), each = length(period)),
				     rep(c(" Leaf fall"), each = length(period)))
			  )))
}
df <- completeGroup();
# df
warnings()
theme_set(theme_minimal())


p2 <- ggplot() + 
	geom_bar(aes(y = group, x = period, fill = new), data = df[[1]], stat="identity") +
	labs(title = "Species Discovery by Date") +
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Number of Species") +
	scale_y_continuous(limits = yAxis) +
	scale_x_continuous(limits = xAxis) + coord_fixed()
p2
ggsave("species.png")
p1 <- ggplot() + 
	geom_bar(aes(y = group, x = period, fill = groups), data = df[[1]], stat="identity") +
	labs(title = "Number of new species by Plant Functional Group") + 
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Number of Species") +
	scale_y_continuous(limits = yAxis) +
	scale_x_continuous(limits = xAxis) + coord_fixed()
p1
ggsave("functional_group.png")
p1 <- ggplot() + 
	geom_bar(aes(y = phase, x = period, fill = phases), data = df[[2]], stat="identity") +
	labs(title = "Species Discovery by Phenology Phase") + 
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Number of Species") + 
	scale_y_continuous(limits = yAxis) +
	scale_x_continuous(limits = xAxis) + coord_fixed()

p1

ggsave("phenology.png")
