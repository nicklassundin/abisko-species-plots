list.of.packages <- c('ggplot2', 'readxl', 'dplyr');

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://ftp.acc.umu.se/mirror/CRAN/")

library(ggplot2)
library(readxl);
library(dplyr);

f <- list.files('Data', full.names = TRUE)[1]
file <- read_xlsx(f);
f <- list.files('Data', full.names = FALSE)[1]
charts <- function(data){

	fiveDays <- function(dates){
		result = sapply(dates, function(x){
					return(floor(x/ 5)*5)
})
		return(result)
	}
	print(data)
	data$day <- fiveDays(data$DOY)
	data$year <- sapply(data$Date, function(x){
				    return(as.POSIXlt(x)$year)
});
	years <- unique(data$year);	
	yAxis <- c(0, length(unique(data$Species)));
	period <- unique(data[order(data$day),]$day);
	period <- seq(min(period), max(period), by = 5);
	xAxis <- c(min(period) - 5 , max(period) + 5);

	data$Phenophase[data$Phenophase %in% "All"] <- "Leaf development"
	phenophase <- c("Leaf development", 
			"Flowering",
			"Other",
			"Fruit",
			"Seed Dispersal",
			"Senescence",
			"Leaf fall")
	funcGroup <- unique(data$`Functional Group`);

	# Total 
	dataT <- list();
	dataT$phenophase <- sapply(period, function(day){
					   tmp <- data[data$day %in% day,]
					   res <- sapply(phenophase, function(phase){
								 return(nrow(tmp[tmp$Phenophase %in% phase,]))
})
					   return(res)
			})
	dataT$group <- sapply(period, function(day){
				      tmp <- data[data$day %in% day,]
				      res <- sapply(funcGroup, function(group){
							    return(nrow(tmp[tmp$`Functional Group` %in% group,]))
})
				      return(res)
			})

	df_all <- list(data.frame(
				  period = period,
				  group = c(dataT$group),
				  groups = c(rep(c(" Woody Plants (Trees & Shrubs)"), each = length(period)),
					     rep(c("Graminoids"), each = length(period)),
					     rep(c("Forbs"), each = length(period)),
					     rep(c("Primitive Plants"), each = length(period)))),
		       data.frame(
				  period = period,
				  phase = c(dataT$phenophase),
				  phases = c(
					     rep(c("Leaf development"), each = length(period)),
					     rep(c("Flowering"), each = length(period)),
					     rep(c("Other"), each = length(period)),
					     rep(c("Fruit"), each = length(period)),
					     rep(c("Seed Dispersal"), each = length(period)),
					     rep(c("Senescence"), each = length(period)),
					     rep(c("Leaf fall"), each = length(period))
					     )))

	# allDataFrame
	#####
	#####

	dataF <- list(); 

	for( species in unique(data$Species) ){
		for(year in years){
			tmp <- data[data$year %in% year,];
			tmp <- data[tmp$Species %in% species,];
			tmp <- tmp[tmp$day %in% min(tmp$day),];
			res <- tmp[1,];
			for( phase in phenophase ){
				res[phase] <- length(tmp$Phenophase[tmp$Phenophase %in% phase]);
			}
			total <- sum(res[phenophase]);
			res[phenophase] <- sapply(res[phenophase], function(x){
							  return(x/total);
		       })	
			if(!is.na(res$Species)){
				# res$Species <- species;
				dataF <- rbind(dataF, res)
			}
		}
	}
	data <- dataF;
	dataG <- list();
	for( day in period ){
		tmp <- data[data$day %in% day,]
		res <- tmp[c("day")][1,];
		if(is.na(res$day)) res$day <- day;
		res$total <- 0;
		for( group in funcGroup ){
			res[group] <- length(tmp$`Functional Group`[tmp$`Functional Group` %in% group])/length(years);
			res$total <- unlist(res[group]) + res$total
		}
		for( phase in phenophase ){
			p <- tmp[phase]
			if(nrow(p) == 0){
				res[phase] <- 0;
			}else{
				res[phase] <- sum(tmp[phase])/length(years)
			}
		}
		dataG <- rbind(dataG, res);
	}
	data <- dataG
	data$total <- c(0, head(Reduce(f = "+", data$total, accumulate = TRUE), -1))
	data$number <- data$total + data$W + data$G + data$F + data$P;

	percent <- list();
	percent$total <- 100 * data$total / data$number;

	for(phase in phenophase){
		percent[phase] <- 100 * data[phase] / data$number;
	}

	for(group in funcGroup){
		percent[group] <- 100 * data[group] / data$number;
	}
	# print(length(data$total))
	# print(length(data$W))
	# print(data$W)
	# print(length(data$G))
	# print(data$G)
	# print(length(data$F))
	# print(data$F)
	# print(length(data$P))
	# print(data$P)
	df <- list(data.frame(
			      period = c(period),
			      groups = c(rep(c("Total"), each = length(period)), 
					 rep(c(" Woody Plants (Trees & Shrubs)"), each = length(period)),
					 rep(c("Graminoids"), each = length(period)),
					 rep(c("Forbs"), each = length(period)),
					 rep(c("Primitive Plants"), each = length(period))),
			      group = c(data$total, data$W, data$G, data$F, data$P),
			      new = c(rep(c("Total"), each = length(period)), 
				      rep(c("New"), each = length(period)*4)),
			      percent = c(percent$total, percent$W, percent$G, percent$F, percent$P)
			      
			      ),
		   data.frame(
			      period = c(period),
			      group = c(data$total, c(c(data$W + data$G + data$F + data$P))),
			      new = c(rep(c("Total"), each = length(period)), 
				      rep(c("New"), each = length(period)))
			      ),
		   data.frame(
			      period = c(period),
			      phase = c(data$total, data$`Leaf development`, data$Flowering, data$Other, data$Fruit, data$`Seed Dispersal`, data$Senescence, data$`Leaf fall`),
			      phases = c(rep(c("Total"), each = length(period)), 
					 rep(c("Leaf development"), each = length(period)),
					 rep(c("Flowering"), each = length(period)),
					 rep(c("Other"), each = length(period)),
					 rep(c("Fruit"), each = length(period)),
					 rep(c("Seed Dispersal"), each = length(period)),
					 rep(c("Senescence"), each = length(period)),
					 rep(c("Leaf fall"), each = length(period))),
			      percent = c(percent$total, percent$`Leaf development`, percent$Flowering, percent$Other, percent$Fruit, percent$`Seed Dispersal`, percent$Senescence, percent$`Leaf fall`)
			      ),
		   df_all[[1]],
		   df_all[[2]]
	)
	df$xAxis <- xAxis;
	df$yAxis <- yAxis;
	return(df)
}
df <- charts(file);
xAxis <- df$xAxis;
yAxis <- df$yAxis;


colours <- list();
colours$groups <- c("Total" = "#eeeeee", " Woody Plants (Trees & Shrubs)" ="#996600", "Graminoids" = "#990073", "Forbs" = "#006699", "Primitive Plants" = "#b30000");
colours$all <- c("Total" = "#336600", "New" = "#e6b800");
colours$phase <- c("Total" = "#eeeeee", "Leaf development" = "#e6b800", "Flowering" = "#b85500", "Other" = "#00b8e6", "Fruit" = "#00e633", "Seed Dispersal" = "#b30000", "Senescence" = "#00b300", "Leaf fall" = "#0000b3")
colours$phaseAll <- tail(colours$phase, -1)
pdf(paste(f, ".pdf", sep="_"));
## PLot I
##############
group_plot <- ggplot(data = df[[1]],
		 aes(y = group, x = period, fill = groups)) +
	geom_bar(stat="identity", width=4, colour = "black") +
	labs(title = "Number of new species by Plant Functional Group") + 
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Number of Species") +
	scale_y_continuous(limits = yAxis) +
	scale_x_continuous(limits = xAxis) + coord_fixed() +
	scale_fill_manual("legend", values = colours$groups) + 
	guides(col = guide_legend(nrow = 2))
group_plot
gG = ggplotGrob(group_plot)
ggsave(path = "figs", filename = paste(f, "functional_group.png", sep="_"))

## PLot II
##############
species_plot <- ggplot() + 
	geom_bar(aes(y = group, x = period, fill = new), colour = "black", data = df[[2]], stat="identity", width=4) +
	labs(title = "Species Discovery by Date") +
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Number of Species") +
	scale_y_continuous(limits = yAxis) +
	scale_x_continuous(limits = xAxis) + coord_fixed() +
	scale_fill_manual("legend", values = colours$all)


species_plot
sG = ggplotGrob(species_plot)
sG$width <- gG$width;
ggsave(path = "figs", filename = paste(f, "species.png", sep="_"));

## PLot III
##############
phase_plot <- ggplot() + 
	geom_bar(aes(y = phase, x = period, fill = phases), colour = "black", data = df[[3]], stat="identity", width=4) +
	labs(title = "Species Discovery by Phenology Phase") + 
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Number of Species") + 
	scale_y_continuous(limits = yAxis) +
	scale_x_continuous(limits = xAxis) + coord_fixed() +
	scale_fill_manual("legend", values = colours$phase) 

phase_plot
pG = ggplotGrob(phase_plot)
pG$width <- gG$width;
ggsave(path = "figs", filename = paste(f, "phenology.png", sep="_"));

## PLot IV
##############
group_all_plot <- ggplot() + 
	geom_bar(aes(y = group, x = period, fill = groups), colour = "black", data = df[[4]], stat="identity", width=4) +
	labs(title = "Number of species by Plant Functional Group") + 
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Number of Species") +
	scale_y_continuous(limits = yAxis) +
	scale_x_continuous(limits = xAxis) + coord_fixed() +
	scale_fill_manual("legend", values = colours$groups) +
	guides(col = guide_legend(nrow = 2))
group_all_plot
gG = ggplotGrob(group_all_plot)
ggsave(path = "figs", filename = paste(f, "functional_group_all.png", sep="_"));


## Plot V
##############
phase_all_plot <- ggplot() + 
	geom_bar(aes(y = phase, x = period, fill = phases), colour = "black", data = df[[5]], stat="identity", width=4) +
	labs(title = "Species Recorded by Phenology Phase") + 
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Number of Species") + 
	scale_y_continuous(limits = yAxis) +
	scale_x_continuous(limits = xAxis) + coord_fixed() +
	scale_fill_manual("legend", values = colours$phaseAll)
phase_all_plot
gG = ggplotGrob(phase_all_plot)
ggsave(path = "figs", filename = paste(f, "phenology_all.png", sep="_"))

## Plot VI
##############
group_percent_plot <- ggplot() + 
	geom_bar(aes(y = percent, x = period, fill = groups), colour = "black", data = df[[1]], stat="identity", width=4) +
	labs(title = "Percentage of Species Discovery by Plant Functional Group") + 
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Percent of Species") +
	scale_y_continuous(limits = c(0,100)) +
	scale_x_continuous(limits = xAxis) + coord_fixed() +
	scale_fill_manual("legend", values = colours$groups) 
group_percent_plot
gG = ggplotGrob(group_percent_plot)
ggsave(path = "figs", filename = paste(f, "functional_group_percent.png", sep="_"));

## Plot VII
##############
phase_percent_plot <- ggplot() + 
	geom_bar(aes(y = percent, x = period, fill = phases), colour = "black", data = df[[3]], stat="identity", width=4) +
	labs(title = "Percentage of Species Discovery by Phenology Phase") + 
	xlab("Day of year (based on 5-day sampling)") +
	ylab("Percent of Species") + 
	scale_y_continuous(limits = c(0,100)) +
	scale_x_continuous(limits = xAxis) + coord_fixed() +
	scale_fill_manual("legend", values = colours$phase)

phase_percent_plot
pG = ggplotGrob(phase_percent_plot)
pG$width <- gG$width;
ggsave(path = "figs", filename = paste(f, "phenology_percent.png", sep="_"))

warnings();
