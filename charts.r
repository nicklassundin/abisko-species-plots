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

phenophase <- unique(data$Phenophase);
funcGroup <- unique(data$`Functional Group`);

dataF <- list(); 
for( species in unique(data$Species) ){
	tmp <- data[data$Species %in% species,];
	# print(tmp)

	tmp <- tmp[tmp$day %in% min(tmp$day),];
	res <- tmp[1,];
	for( phase in phenophase ){
		res[phase] <- length(tmp$Phenophase[tmp$Phenophase %in% phase]);
	}
	total <- sum(res[phenophase]);
	res[phenophase] <- sapply(res[phenophase], function(x){
					  return(x/total);
})	
	dataF <- rbind(dataF, res)
}
data <- dataF;
# data$day
dataG <- list();
for( day in period ){
	tmp <- data[data$day %in% day,]
		res <- tmp[c("day")][1,];
		if(is.na(res$day)) res$day <- day;
		res$total <- 0;
		for( group in unique(data$`Functional Group`) ){
			res[group] <- length(tmp$`Functional Group`[tmp$`Functional Group` %in% group]);
			res$total <- unlist(res[group]) + res$total
		}
		for( phase in phenophase ){
			p <- tmp[phase]
			if(nrow(p) == 0){
				res[phase] <- 0;
			}else{
				res[phase] <- sum(tmp[phase])
			}
		}
		dataG <- rbind(dataG, res);
}
data <- dataG
data$total <- c(0, head(Reduce(f = "+", data$total, accumulate = TRUE), -1))
df <- list(data.frame(
		      period = c(period),
		      groups = c(rep(c("Total"), each = length(period)), 
				 rep(c(" Woody Plants (Trees & Shrubs)"), each = length(period)),
				 rep(c(" Graminoids"), each = length(period)),
				 rep(c(" Forbs"), each = length(period)),
				 rep(c(" Primitive Plants"), each = length(period))),
		      group = c(data$total, data$W, data$G, data$F, data$P),
		      new = c(rep(c("Total"), each = length(period)), 
			      rep(c("New"), each = length(period)*4))
		      ),
	   data.frame(
		      period = c(period),
		      phase = c(data$total, data$All, data$`Leaf development`, data$Flowering, data$Other, data$Fruit, data$`Seed Dispersal`, data$Senescence, data$`Leaf fall`),
		      phases = c(rep(c("Total"), each = length(period)), 
				 rep(c(" ALL"), each = length(period)),
				 rep(c(" Leaf development"), each = length(period)),
				 rep(c(" Flowering"), each = length(period)),
				 rep(c(" Other"), each = length(period)),
				 rep(c(" Fruit"), each = length(period)),
				 rep(c(" Seed Dispersal"), each = length(period)),
				 rep(c(" Senescence"), each = length(period)),
				 rep(c(" Leaf fall"), each = length(period)))
		      ))

df 
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
