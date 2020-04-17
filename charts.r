list.of.packages <- c('ggplot2', 'readxl', 'gridExtra', 'dplyr', 'grid', 'scales', 'lubridate');
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://ftp.acc.umu.se/mirror/CRAN/")

library(scales)
library(ggplot2);
library(readxl);
library(grid);
library(gridExtra);
library(dplyr);
library(lubridate);

options(warn=2)

fiveDays <- function(dates){
	result = sapply(dates, function(x){
				return(floor(x/ 5)*5)
})
	return(result)
}

phenophase <- c("Leaf development", 
		"Flowering",
		"Fruit",
		"Seed Dispersal",
		"Senescence",
		"Leaf fall")
funcGroup <- c("W", "P", "F", "G")
f <- list.files('Data', full.names = TRUE)
files <- lapply(f, function(x){
			data <- read_xlsx(x);
			if("Leaf Type" %in% colnames(data)){
				data <- data[,-8];
			}
			data <- data[,1:10]
			return(data)
		})

combData <- Reduce(function(x,y){rbind(x,y)}, files);
combData <- mutate(combData, year = as.POSIXlt(Date)$year + 1900)
combData <- mutate(combData, day = fiveDays(DOY))
combData <- mutate(combData, historical = year < 2000)
combData <- arrange(combData, year);

## Fix Axis ########
period <- unlist(unique(combData$day));
period <- c(min(period), max(period));
period <- seq(min(unlist(period)), max(unlist(period)), by = 5);
xBreaks <- seq(min(period), max(period), 15);
xLabel <- lapply(xBreaks, function(x){
			 res <- unique(combData$Date[combData$day %in% x]);
			 if(length(res) > 1) res <- res[1]
			 return(res)
			  });
xLabel <- lapply(xLabel, function(x){
			 d <- as.POSIXlt(x);
			 return(paste(format(d, '%d'), month(x, label = TRUE)))
			  })
names(xLabel) <- xBreaks;
yAxis <- c(0, length(unique(combData$Species)));
xAxis <- c(min(period) - 5, max(period) + 5);
unqSpecies <- unique(combData$Species)

###############
###############

build <- function(values, label, xfacet, yfacet){
	return(data.frame(
			  period = c(period),
			  values = values,
			  labels = c(sapply(label, function(x){
						    rep(c(x), each = length(period))
})),
			  xfacet = xfacet, 
			  yfacet = yfacet
			  ))
}

charts <- function(rawData){
	data <- rawData;
	dates <- unique(data$Date);
	print(data)
	years <- unique(data$year);

	samPeriod <- period %in% rawData$day;
	allSpecies <- data %>% 
		group_by(year) %>%
		summarise(nr = length(unique(Species)));
	nrSpecies <- mean(allSpecies$nr)

	tableYearLabel <- NA;
	if(length(years) > 1){
		tableYearLabel <- paste(c(min(years), max(years)), collapse = '-')
	}else{
		tableYearLabel <- as.character(years)
	}

	data$Phenophase[data$Phenophase %in% "All"] <- "Leaf development"
	data$Phenophase[data$Phenophase %in% "Other"] <- "Leaf development"
	# funcGroup <- unique(data$`Functional Group`);

	# Total 
	dataT <- list();

	acum <- function(tgroup, tmp){
		accu <- sapply(unique(tmp$Species), function(species){
				       return(nrow(tgroup[tgroup$Species %in% species,])/
					      nrow(tmp[tmp$Species %in% species,]))
})
		if(length(accu) == 0) accu <- c(0)
		return(sum(c(accu))/length(years))
	}


	###
	## TODO
	# exp <- data %>% 
	# 	group_by(day, Species, `Functional Group`, Phenophase) %>%
	# 	summarise(`Phenology Phase Avg` = length(unique(`Functional Group`)) / length(Species),
	# 	number = length(Species))
	# print(exp)
	# exp <- exp %>% 
	# 	group_by(day, Species, `Functional Group`) %>%
	# 	summarise(
	# 		`Leaf development`, 
	# 		Flowering,
	# 		Fruit,
	# 		`Seed Dispersal`,
	# 		Senescence,
	# 		`Leaf fall`
	# 	)
	# fdsfds
	dataT$phenophase <- sapply(period, function(day){
					   tmp <- data[data$day %in% day,]
					   res <- sapply(phenophase, 
							 function(phase){
								 tgroup <- tmp[tmp$Phenophase %in% phase,];
								 return(acum(tgroup, tmp))
							 })
					   return(res)
})

	result <- build(c(t(dataT$phenophase)), phenophase, "Total Number", "Phenology Phase")	

	dataT$group <- sapply(period, function(day){
				      tmp <- data[data$day %in% day,]
				      res <- sapply(funcGroup, function(group){
							    tgroup <- tmp[tmp$`Functional Group` %in% group,];
							    return(acum(tgroup, tmp))
							 })
				      return(res)
})

	result <- rbind(result, build(c(t(dataT$group)), funcGroup, "Total Number", "Functional Group"));	
	#####
	#####
	dataF <- list(); 

	for( species in unqSpecies ){
		for(year in years){
			# print(species)	
			tmp <- data[data$year %in% year,];
			tmp <- tmp[tmp$Species %in% species,];
			# print(species)
			# print(tmp$day)
			if(length(tmp$day) != 0){

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
	percent <- list()
	percent$total <- data$total / data$number
	percent$total[is.nan(percent$total)] <- 0;
	percentOfTotal <- list();
	NaNtoZero <- function(y){
		return(sapply(y, function(x){
				      x[sapply(x, is.nan)] <- 0;
				      return(x)
							 }))
	}
	for(g in c(phenophase, funcGroup)){
		percent[g] <- data[g] / data$number;
		percent[g][[1]] <- NaNtoZero(percent[g][[1]])
		percentOfTotal[g] <- data[g] / data$number[nrow(data)];
		percentOfTotal[g][[1]] <- NaNtoZero(percentOfTotal[g][[1]])
	}
	if(max(c(percent$W + percent$G + percent$F + percent$P)) > 1){
		print(percent)
		fsdfds
	}
	dailyPercent <- data$number[samPeriod] / nrSpecies;
	df <- list(
		   yearlyTable = data.frame(
					    `Day of Year` = c(period),
					    Woody = data$W,
					    Primitive = data$P,
					    Graminiods = data$G,
					    Forbs = data$F,
					    `Leaf Development` = data$`Leaf development`,
					    Flowering = data$Flowering,
					    Fruit = data$Fruit,
					    `Seed Dispersal` = data$`Seed Dispersal`,
					    Senescence = data$Senescence,
					    `Leaf fall` = data$`Leaf fall`,
					    Species = data$number
					    ),
		   table = list(
				data = data.frame(
						  Years = tableYearLabel, 
						  W = signif(sum(percentOfTotal$W), 3),
						  P = signif(sum(percentOfTotal$P), 3),
						  F = signif(sum(percentOfTotal$F), 3),
						  G = signif(sum(percentOfTotal$G), 3),
						  `Leaf development` = signif(sum(percentOfTotal$`Leaf development`), 3),
						  Flowering = signif(sum(percentOfTotal$Flowering), 3),
						  Fruit = signif(sum(percentOfTotal$Fruit), 3),
						  `Seed Dispersal` = signif(sum(percentOfTotal$`Seed Dispersal`), 3),
						  Senescence = signif(sum(percentOfTotal$Senescence), 3),
						  `Leaf fall` = signif(sum(percentOfTotal$`Leaf fall`), 3),
						  Species = max(data$number),
						  `Sampling Efforts (days)` = length(dailyPercent)
						  ),
				percent = data.frame(
						     years = tableYearLabel,
						     "0.05" = length(dailyPercent[
								     dailyPercent < 0.05]) + 1,
						     "0.25" = length(dailyPercent[
								     dailyPercent < 0.25]) + 1,
						     "0.50" = length(dailyPercent[
								     dailyPercent < 0.50]) + 1,
						     "0.75" = length(dailyPercent[
								     dailyPercent < 0.75]) + 1,
						     "0.95" = length(dailyPercent[
								     dailyPercent < 0.95]) + 1,
						     Species = max(data$number),
						     `Sampling Efforts (days)` = length(dailyPercent)
						     ),
				speciesByYear = data.frame(
							   Year = c(tableYearLabel)
							   )))
	for(sp in unqSpecies){
		df$table$speciesByYear[sp] <- length(rawData$Species[rawData$Species %in% sp]);
	}

	df$data <- data.frame(
			      sampled = c(samPeriod),
			      species = c(nrSpecies),
			      period = c(period),
			      yearLabel = c(tableYearLabel),
			      labels = c(
					 rep(c("Total"), each = length(period)), 
					 rep(c("New"), each = length(period)),

					 rep(c("Total"), each = length(period)), 
					 rep(c(" Woody Plants"), each = length(period)),
					 rep(c("Graminoids"), each = length(period)),
					 rep(c("Forbs"), each = length(period)),
					 rep(c("Primitive Plants"), each = length(period)),

					 rep(c("Total"), each = length(period)),
					 rep(c("Leaf development"), each = length(period)),
					 rep(c("Flowering"), each = length(period)),
					 rep(c("Fruit"), each = length(period)),
					 rep(c("Seed Dispersal"), each = length(period)),
					 rep(c("Senescence"), each = length(period)),
					 rep(c("Leaf fall"), each = length(period)),

					 rep(c(" Woody Plants"), each = length(period)),
					 rep(c("Graminoids"), each = length(period)),
					 rep(c("Forbs"), each = length(period)),
					 rep(c("Primitive Plants"), each = length(period)),

					 rep(c("Leaf development"), each = length(period)),
					 rep(c("Flowering"), each = length(period)),
					 rep(c("Fruit"), each = length(period)),
					 rep(c("Seed Dispersal"), each = length(period)),
					 rep(c("Senescence"), each = length(period)),
					 rep(c("Leaf fall"), each = length(period)),

					 rep(c(" Woody Plants"), each = length(period)),
					 rep(c("Graminoids"), each = length(period)),
					 rep(c("Forbs"), each = length(period)),
					 rep(c("Primitive Plants"), each = length(period)),

					 rep(c("Leaf development"), each = length(period)),
					 rep(c("Flowering"), each = length(period)),
					 rep(c("Fruit"), each = length(period)),
					 rep(c("Seed Dispersal"), each = length(period)),
					 rep(c("Senescence"), each = length(period)),
					 rep(c("Leaf fall"), each = length(period)),

					 rep(c(" Woody Plants"), each = length(period)),
					 rep(c("Graminoids"), each = length(period)),
					 rep(c("Forbs"), each = length(period)),
					 rep(c("Primitive Plants"), each = length(period)),

					 rep(c("Leaf development"), each = length(period)),
					 rep(c("Flowering"), each = length(period)),
					 rep(c("Fruit"), each = length(period)),
					 rep(c("Seed Dispersal"), each = length(period)),
					 rep(c("Senescence"), each = length(period)),
					 rep(c("Leaf fall"), each = length(period))
					 ),
			      values = c(
					 data$total,
					 data$W + data$G +data$F + data$P,

					 data$total,
					 data$W,
					 data$G,
					 data$F,
					 data$P,

					 data$total,
					 data$`Leaf development`,
					 data$Flowering,
					 data$Fruit,
					 data$`Seed Dispersal`, 
					 data$Senescence,
					 data$`Leaf fall`,

					 percent$W,
					 percent$G,
					 percent$F,
					 percent$P,

					 percent$`Leaf development`, 
					 percent$Flowering,
					 percent$Fruit,
					 percent$`Seed Dispersal`,
					 percent$Senescence,
					 percent$`Leaf fall`,

					 percentOfTotal$W,
					 percentOfTotal$G,
					 percentOfTotal$F,
					 percentOfTotal$P,

					 percentOfTotal$`Leaf development`,
					 percentOfTotal$Flowering,
					 percentOfTotal$Fruit,
					 percentOfTotal$`Seed Dispersal`,
					 percentOfTotal$Senescence,
					 percentOfTotal$`Leaf fall`,

					 c(t(dataT$group)),
					 c(t(dataT$phenophase))
					 ),
			      periodfacet = c(
					      rep(c("New"), 
						  each = 2*length(period)),
					      rep(c("Functional Group Number"),
						  each = 5*length(period)),
					      rep(c("Phenology Phase Number"),
						  each = 7*length(period)),
					      rep(c("Functional Group Percent"),
						  each = 4*length(period)),
					      rep(c("Phenology Phase Percent"),
						  each = 6*length(period)),
					      rep(c("Functional Group Percent Cumulative"),
						  each = 4*length(period)),
					      rep(c("Phenology Phase Percent Cumulative"),
						  each = 6*length(period)),
					      rep(c("Functional Group Total Number"),
						  each = 4*length(period)),
					      rep(c("Phenology Phase Total Number"),
						  each = 6*length(period))
					      ),
			      xfacet = c(
					 rep(c("New"), each = 2*length(period)),
					 rep(c("Functional Group"), each = 5*length(period)),
					 rep(c("Phenology Phase"), each = 7*length(period)),
					 rep(c("Functional Group"), each = 4*length(period)),
					 rep(c("Phenology Phase"), each = 6*length(period)),
					 rep(c("Functional Group"), each = 4*length(period)),
					 rep(c("Phenology Phase"), each = 6*length(period)),
					 rep(c("Functional Group"), each = 4*length(period)),
					 rep(c("Phenology Phase"), each = 6*length(period))
					 ),
			      yAxisType = c(
					    rep(c("Number of New Species"), each = 2*length(period)),
					    rep(c("Number"), each = 5*length(period)),
					    rep(c("Number"), each = 7*length(period)),
					    rep(c("Percent"), each = 4*length(period)),
					    rep(c("Percent"), each = 6*length(period)),
					    rep(c("Percent"), each = 4*length(period)),
					    rep(c("Percent"), each = 6*length(period)),
					    rep(c("Number"), each = 4*length(period)),
					    rep(c("Number"), each = 6*length(period))
					    ),
			      yfacet = c(
					 rep(c("Number of New Species"), each = 2*length(period)),
					 rep(c("Number"), each = 5*length(period)),
					 rep(c("Number"), each = 7*length(period)),
					 rep(c("Percent"), each = 4*length(period)),
					 rep(c("Percent"), each = 6*length(period)),
					 rep(c("Percent Cumulative"), each = 4*length(period)),
					 rep(c("Percent Cumulative"), each = 6*length(period)),
					 rep(c("Total Number"), each = 4*length(period)),
					 rep(c("Total Number"), each = 6*length(period))
			      )
	)
	df$data <- filter(df$data, sampled)
	return(df)
}
colours <- list();
colours$groups <- c("Total" = "#eeeeee", 
		    " Woody Plants" = "#E69F00",
		    "Graminoids" = "#56B4E9",
		    "Forbs" = "#009E73",
		    "Primitive Plants" = "#F0E442");
colours$all <- c("Total" = "#009E73",
		 "New" = "#E69F00");
colours$phase <- c("Total" = "#eeeeee",
		   "Leaf development" = "#0072B2",
		   "Flowering" = "#D55E00",
		   # "Other" = "#CC79A7",
		   "Fruit" = "#E69F00",
		   "Seed Dispersal" = "#56B4E9",
		   "Senescence" = "#009E73",
		   "Leaf fall" = "#F0E442")
colours$phaseFirst <- c("Total" = "#eeeeee",
			"Leaf development" = "#0072B2",
			"Flowering" = "#D55E00",
			# "Other" = "#CC79A7",
			"Fruit" = "#E69F00",
			"Seed Dispersal" = "#56B4E9",
			"Senescence" = "#009E73",
			"Leaf fall" = "#F0E442")
colours$phaseAll <- tail(colours$phase, -1)

colours$facet <- list("Total" = "#eeeeee", 
		      " Woody Plants" = "#E69F00",
		      "Graminoids" = "#56B4E9",
		      "Forbs" = "#009E73",
		      "Primitive Plants" = "#F0E442",
		      "New" = "#E69F00",
		      "Leaf development" = "#0072B2",
		      "Flowering" = "#D55E00",
		      "Fruit" = "#E69F00",
		      "Seed Dispersal" = "#56B4E9",
		      "Senescence" = "#009E73",
		      "Leaf fall" = "#F0E442")

saveTable <- function(table, name){
	path_out = paste(getwd(), "/tables/csv/", sep = '');
	fileName = paste(path_out, paste(name, '.csv', sep = ''), sep = '')
	write.csv(table, fileName);
	g <- tableGrob(table, rows=NULL);
	ggsave(g, path = "tables/png", filename = paste(name, '.png', sep = ''));
}
drawPlot <- function(p, name, fct = FALSE){
	p <- p +
		scale_x_continuous(limits = xAxis, breaks = xBreaks, labels=xLabel) + 
		theme_classic() + 
		labs(caption = f) + 
		theme(axis.text.x = element_text(angle = 45, hjust = 1), aspect.ratio = 1);
	if(fct) p <- p + facet_wrap("yearLabel");
	ggsave(p, path = "figs", filename = paste(f, name, sep="_"));
	return(p)
}

plot <- function(df, fct = FALSE){
	if(!fct){
		print("0")
		## Plot Faced
		###############
		facet <- ggplot(data = df$data[df$data$xfacet != "New",], aes(y = values, x = period, fill=labels)) + 
			geom_bar(stat="identity", width=4) +
			facet_grid(yfacet ~ xfacet, scales = "free_y") +
			scale_fill_manual("legend", values = colours$facet);

		p <- drawPlot(facet, "facet.png", fct)
		grid.draw(p);
	}



	print("1")
	## PLot I
	##############
	dataNumber = df$data[df$data$yfacet == "Number",]

	hline <- dataNumber %>%
		group_by(yearLabel) %>%
		summarise(nr = unique(species))
	p <- drawPlot(ggplot(data = dataNumber[dataNumber$xfacet == "Functional Group",]) +
		      geom_bar(aes(y = values, x = period, fill = labels), stat="identity", width=4) +
		      geom_hline(data = hline, aes(yintercept = nr), linetype = "dotted") +
		      labs(title = "Number of new species by Plant Functional Group") + 
		      xlab("Day of year (based on 5-day sampling)") +
		      ylab("Number of Species") +
		      scale_y_continuous(breaks=seq(yAxis[1],yAxis[2], 10), limits = yAxis) +
		      scale_fill_manual("legend", values = colours$groups) + 
		      guides(col = guide_legend(nrow = 2)),
	      "functional_group.png", fct);
	grid.draw(p)
	
	print("2")
	### PLot II
	###############
	dataNumber = df$data[df$data$yfacet == "Number of New Species",]
	p <- drawPlot(ggplot(data = dataNumber[dataNumber$xfacet == "New",]) + 
		      geom_bar(aes(y = values, x = period, fill = labels), stat="identity", width=4) +
		      geom_hline(data = hline, aes(yintercept=nr), linetype = "dotted") +
		      labs(title = "Species Discovery by Date") +
		      xlab("Day of year (based on 5-day sampling)") +
		      ylab("Number of Species") +
		      scale_y_continuous(breaks=seq(yAxis[1],yAxis[2],10),limits = yAxis) +
		      scale_fill_manual("legend", values = colours$all) + labs(caption = f),
	      "species.png", fct);
	grid.draw(p)

	print("3")
	## PLot III
	##############
	dataNumber = df$data[df$data$yfacet == "Number",];
	p <- drawPlot(ggplot(data = dataNumber[dataNumber$xfacet == "Phenology Phase",]) + 
		      geom_bar(aes(y = values, x = period, fill = labels), stat="identity", width=4) +
		      geom_hline(data = hline, aes(yintercept=nr), linetype = "dotted") +
		      labs(title = "Species Discovery by Phenology Phase") + 
		      xlab("Day of year (based on 5-day sampling)") +
		      ylab("Number of Species") + 
		      scale_y_continuous(breaks=seq(yAxis[1],yAxis[2],10),limits = yAxis) +
		      scale_fill_manual("legend", values = colours$phase),
	      "phenology.png", fct);
	grid.draw(p)

	print("4")
	# PLot IV
	##############
	dataNumber = df$data[df$data$yfacet == "Total Number",]
	p <- drawPlot(ggplot(data = dataNumber[dataNumber$xfacet == "Functional Group",]) + 
		      geom_bar(aes(y = values, x = period, fill = labels), stat="identity", width=4) +
		      labs(title = "Total Species by Plant Functional Group") + 
		      xlab("Day of year (based on 5-day sampling)") +
		      ylab("Number of Species") +
		      scale_y_continuous(breaks=seq(yAxis[1],yAxis[2],10)) +
		      scale_fill_manual("legend", values = colours$groups) +
		      guides(col = guide_legend(nrow = 2)),
	      "functional_group_all.png", fct);
	grid.draw(p)

	print("5")
	## Plot V
	##############
	p <- drawPlot(ggplot(data = dataNumber[dataNumber$xfacet == "Phenology Phase",]) + 
		      geom_bar(aes(y = values, x = period, fill = labels), stat="identity", width=4) +
		      labs(title = "Total Species by Phenology Phase") + 
		      xlab("Day of year (based on 5-day sampling)") +
		      ylab("Number of Species") + 
		      # scale_y_continuous(limits = yAxis) +
		      scale_y_continuous(breaks=seq(yAxis[1],yAxis[2],10)) +
		      scale_fill_manual("legend", values = colours$phaseAll),
	      "phenology_all.png", fct);
	grid.draw(p)

	print("6")
	### Plot VI
	###############
	dataNumber = df$data[df$data$yfacet == "Percent Cumulative",];
	p <- drawPlot(ggplot(data = dataNumber[dataNumber$xfacet == "Functional Group",]) + 
		      geom_bar(aes(y = values, x = period, fill = labels), stat="identity", width=4) +
		      labs(title = "Cummulative Percentage of Species Discovery by Plant Functional Group") + 
		      xlab("Day of year (based on 5-day sampling)") +
		      ylab("Cumulative Percent of Species") +
		      scale_y_continuous(breaks=seq(0,1,0.1),labels = percent) +
		      scale_fill_manual("legend", values = colours$groups),
	      "functional_group_percent.png", fct);
	grid.draw(p)

	print("7")
	### Plot VII
	###############
	p <- drawPlot(ggplot(data = dataNumber[dataNumber$xfacet == "Phenology Phase",]) + 
		      geom_bar(aes(y = values, x = period, fill = labels), stat="identity", width=4) +
		      labs(title = "Cumulative Percentage of Species Discovery by Phenology Phase") + 
		      xlab("Day of year (based on 5-day sampling)") +
		      ylab("Cumulative Percent of Species") + 
		      scale_y_continuous(breaks=seq(0,1,0.1),labels = percent) +
		      scale_fill_manual("legend", values = colours$phase),
	      "phenology_percent.png", fct);
	grid.draw(p)

	print("8")
	## Plot VIII
	##############
	dataNumber = df$data[df$data$yfacet == "Percent",]
	p <- drawPlot(ggplot(data = dataNumber[dataNumber$xfacet == "Functional Group",]) + 
		      geom_bar(aes(y = values, x = period, fill = labels), stat="identity", width=4) +
		      labs(title = "Percentage of Species Discovery by Plant Functional Group") + 
		      xlab("Day of year (based on 5-day sampling)") +
		      ylab("Percent of Species") +
		      scale_y_continuous(breaks=seq(0,1,0.1),labels = percent) +
		      scale_fill_manual("legend", values = colours$groups),
	      "functional_group_percent_of_total.png", fct);
	grid.draw(p)

	print("9")
	### Plot IX
	###############
	p <- drawPlot(ggplot(dataNumber[dataNumber$xfacet == "Phenology Phase",]) + 
		      geom_bar(aes(y = values, x = period, fill = labels), stat="identity", width=4) +
		      labs(title = "Percentage of Species Discovery by Phenology Phase") + 
		      xlab("Day of year (based on 5-day sampling)") +
		      ylab("Percent of Species") + 
		      scale_y_continuous(breaks=seq(0,1,0.1),labels = percent) +
		      scale_fill_manual("legend", values = colours$phase),
	      "phenology_percent_of_total.png", fct);
	grid.draw(p)
}


files <- lapply(unique(combData$year), function(x){
			charts(combData[combData$year == x, ])
	      })


names(files) <- sapply(files, function(x){
			       x$years
	      })




pasteTables <- function(tables){
	return(Reduce(function(x, y){
			      tmp <- x;
			      tmp$data <- rbind(x$data, y$data)
			      tmp$percent <- rbind(x$percent, y$percent)
			      tmp$speciesByYear <- rbind(x$speciesByYear, y$speciesByYear)
			      return(tmp)
			     }, tables))
}
tables <- lapply(files, function(x){x$table})

pdf("plot_type_by_years.pdf", width = 15, height = 10);
f <- "facet_years"
combi <- list();
combi$data <- lapply(files, function(x){x$data});
combi$data <- Reduce(function(x, y){rbind(x, y)}, combi$data);
plot(combi, TRUE);

pdf("collected_plots_years.pdf", width = 5, height = 5);
for(df in files){
	years <- unlist(unique(df$data$yearLabel))
	f <- Reduce(function(x, y){paste(x, y, sep="-")}, years)
	saveTable(df$yearlyTable, paste(f, "yearly_period_table", sep = "_"));
	plot(df)
}
f <- "";
threeYear <- lapply(unique(combData$historical), function(x){
			    charts(combData[combData$historical == x,])
	      })

#####
combined <- rbind(threeYear[[1]]$data, threeYear[[2]]$data);
pdf("compare_periods.pdf", width = 30, height = 14);
hline <- combined %>%
	group_by(yearLabel) %>%
	summarise(nr = unique(species))
facet <- ggplot(data = combined[combined$yAxisType == "Number",], aes(y = values, x = period, fill=labels)) + 
	geom_bar(stat="identity", width=4) +
	facet_grid(yearLabel ~ periodfacet, scales = "free_y") +
	geom_hline(data = hline, aes(yintercept = nr), linetype = "dotted") +
	scale_y_continuous(breaks=seq(yAxis[1],yAxis[2],10),limits = yAxis) +
	scale_fill_manual("legend", values = colours$facet);

p1 <- drawPlot(facet, "facet_period_number.png")
facet <- ggplot(data = combined[combined$yAxisType == "Percent",], aes(y = values, x = period, fill=labels)) + 
	geom_bar(stat="identity", width=4) +
	facet_grid(yearLabel ~ periodfacet, scales = "free_y") +
	scale_y_continuous(breaks=seq(0,1,0.1),labels = percent) +
	scale_fill_manual("legend", values = colours$facet);

p2 <- drawPlot(facet, "facet_period_percent.png")

grid.draw(p1);
grid.draw(p2);
#####


threeTable <- lapply(threeYear, function(x){x$table})
tables <- c(tables, threeTable);
tables <- pasteTables(tables);

pdf("plot_type_by_period.pdf", width = 10, height = 10);
f <- "facet_period"
combi <- list();
combi$data <- lapply(threeYear, function(x){x$data});
combi$data <- Reduce(function(x, y){rbind(x, y)}, combi$data);
plot(combi, TRUE);

pdf("collected_plots_periods.pdf", width = 10, height = 7);
for(df in threeYear){
	years <- unlist(unique(df$data$yearLabel))
	f <- Reduce(function(x, y){paste(x, y, sep="-")}, years)
	plot(df)
}

tmp <- t(tables$speciesByYear);
colnames(tmp) <- tmp[1,];
saveTable(tmp[-1,], "species_by_year");

tableData <- tables$data[c(1, 2, 3, 7, 4, 5, 6, 8),];
saveTable(tableData, "table");

tablePercent <- tables$percent[c(1, 2, 3, 7, 4, 5, 6, 8),]
saveTable(tablePercent, "table_percent")

warnings()

