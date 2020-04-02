list.of.packages <- c('ggplot2', 'readxl', 'gridExtra', 'dplyr', 'grid', 'scales');

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://ftp.acc.umu.se/mirror/CRAN/")

library(scales)
library(ggplot2);
library(readxl);
library(grid);
library(gridExtra);
library(dplyr);

options(warn=2)

phenophase <- c("Leaf development", 
		"Flowering",
		"Other",
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
			data$year <- sapply(data$Date, function(x){
						    return(as.POSIXlt(x)$year)
});
			return(data[])
		})

threeYear = list()
threeYear$first <- files[[1]];
threeYear$second <- rbind(files[[2]], files[[3]]);


files <- list(
	      # files[[1]][files[[1]]$year %in% 17,],
	      # files[[1]][files[[1]]$year %in% 18,],
	      # files[[1]][files[[1]]$year %in% 19,],
	      # files[[2]],
	      files[[3]]
)

charts <- function(data){

	fiveDays <- function(dates){
		result = sapply(dates, function(x){
					return(floor(x/ 5)*5)
})
		return(result)
	}
	print(data)
	data$day <- fiveDays(data$DOY)
	years <- unique(data$year);	
	yAxis <- c(0, length(unique(data$Species)));
	period <- unique(data[order(data$day),]$day);
	period <- seq(min(period), max(period), by = 5);
	xAxis <- c(min(period) - 5 , max(period) + 5);
	
	lseq <- seq(min(period), max(period), 15);
	xLabel <- lapply(lseq, function(x){
				     res <- unique(data$Date[data$day %in% x]);
				     if(length(res) > 1) res <- res[1]
				     return(res)
});
	names(xLabel) <- lseq;
	data$Phenophase[data$Phenophase %in% "All"] <- "Leaf development"
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
	dataT$phenophase <- sapply(period, function(day){
					   tmp <- data[data$day %in% day,]
					   res <- sapply(phenophase, 
							 function(phase){
								 tgroup <- tmp[tmp$Phenophase %in% phase,];
								 return(acum(tgroup, tmp))
							 })
					   return(res)
})
	dataT$group <- sapply(period, function(day){
				      tmp <- data[data$day %in% day,]
				      res <- sapply(funcGroup, function(group){
							    tgroup <- tmp[tmp$`Functional Group` %in% group,];
							    return(acum(tgroup, tmp))
							 })
				      return(res)
})

	df_all <- list(data.frame(
				  period = period,
				  group = c(t(dataT$group)),
				  groups = c(rep(c(" Woody Plants"), each = length(period)),
					     rep(c("Graminoids"), each = length(period)),
					     rep(c("Forbs"), each = length(period)),
					     rep(c("Primitive Plants"), each = length(period)))),
		       data.frame(
				  period = period,
				  phase = c(t(dataT$phenophase)),
				  phases = c(
					     rep(c("Leaf development"), each = length(period)),
					     rep(c("Flowering"), each = length(period)),
					     rep(c("Other"), each = length(period)),
					     rep(c("Fruit"), each = length(period)),
					     rep(c("Seed Dispersal"), each = length(period)),
					     rep(c("Senescence"), each = length(period)),
					     rep(c("Leaf fall"), each = length(period))
					     )),
		       number=sapply(period, function(day){
					     return(length(unique(data[data$day %in% day, ]$Species)))
				  }))
	#####
	#####
	dataF <- list(); 

	for( species in unique(data$Species) ){
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

	percent <- list();
	percent$total <- data$total / data$number;
	# print(percent$total)
	percentOfTotal <- list()
	for(phase in phenophase){
		percent[phase] <- data[phase] / data$number;
		percentOfTotal[phase] <- data[phase] / data$number[nrow(data)];
	}
	for(group in funcGroup){
		percent[group] <- data[group] / data$number;
		percentOfTotal[group] <- data[group] / data$number[nrow(data)];
	}
	percentOfTotal$total <- (1 - c(data$W + data$G + data$F + data$P)/data$number[nrow(data)])
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
					 rep(c(" Woody Plants"), each = length(period)),
					 rep(c("Graminoids"), each = length(period)),
					 rep(c("Forbs"), each = length(period)),
					 rep(c("Primitive Plants"), each = length(period))),
			      group = c(data$total, data$W, data$G, data$F, data$P),
			      new = c(rep(c("Total"), each = length(period)), 
				      rep(c("New"), each = length(period)*4))
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
					 rep(c("Leaf fall"), each = length(period)))
			      ),
		   df_all[[1]],
		   df_all[[2]],
		   xlabel = xLabel,
		   tableData = data.frame(
					  years = paste(years + 1900, sep = "-"),
					  W = sum(percentOfTotal$W),
					  P = sum(percentOfTotal$P),
					  F = sum(percentOfTotal$F),
					  G = sum(percentOfTotal$G),
					  `Leaf development` = sum(percentOfTotal$Flowering),
					  Other = sum(percentOfTotal$Other),
					  Fruit = sum(percentOfTotal$Fruit),
					  `Seed Dispersal` = sum(percentOfTotal$`Seed Dispersal`),
					  Senescence = sum(percentOfTotal$Senescence),
					  `Leaf fall` = sum(percentOfTotal$`Leaf fall`),
					  total = max(data$number)
		   )
	)
	df$phasePercent <- data.frame(
				      period = c(period),
				      phases = c(rep(c("Leaf development"), each = length(period)),
						 rep(c("Flowering"), each = length(period)),
						 rep(c("Other"), each = length(period)),
						 rep(c("Fruit"), each = length(period)),
						 rep(c("Seed Dispersal"), each = length(period)),
						 rep(c("Senescence"), each = length(period)),
						 rep(c("Leaf fall"), each = length(period))),
				      percent = c(percent$`Leaf development`, percent$Flowering, percent$Other, percent$Fruit, percent$`Seed Dispersal`, percent$Senescence, percent$`Leaf fall`),
				      percentOfTotal = c(percentOfTotal$`Leaf development`,
							 percentOfTotal$Flowering,
							 percentOfTotal$Other,
							 percentOfTotal$Fruit,
							 percentOfTotal$`Seed Dispersal`,
							 percentOfTotal$Senescence,
							 percentOfTotal$`Leaf fall`))
	df$groupPercent <- data.frame(
				      period = c(period),
				      groups = c(rep(c(" Woody Plants"), each = length(period)),
						 rep(c("Graminoids"), each = length(period)),
						 rep(c("Forbs"), each = length(period)),
						 rep(c("Primitive Plants"), each = length(period))),
				      percent = c(percent$W,
						  percent$G,
						  percent$F,
						  percent$P),
				      percentOfTotal = c(
							 percentOfTotal$W,
							 percentOfTotal$G,
							 percentOfTotal$F,
							 percentOfTotal$P))
	df$xAxis <- xAxis;
	df$yAxis <- yAxis;
	df$years <- years;
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
		   "Other" = "#CC79A7",
		   "Fruit" = "#E69F00",
		   "Seed Dispersal" = "#56B4E9",
		   "Senescence" = "#009E73",
		   "Leaf fall" = "#F0E442")
colours$phaseAll <- tail(colours$phase, -1)
pdf("collected_plots.pdf", width = 10, height = 7);
plot <- function(df){
	## PLot I
	##############
	group_plot <- ggplot(data = df[[1]],
			     aes(y = group, x = period, fill = groups)) +
	      geom_bar(stat="identity", width=4) +
	      labs(title = "Number of new species by Plant Functional Group") + 
	      xlab("Day of year (based on 5-day sampling)") +
	      ylab("Number of Species") +
	      scale_y_continuous(breaks=seq(yAxis[1],yAxis[2],10),limits = yAxis) +
	      scale_x_continuous(limits = xAxis) + 
	      scale_fill_manual("legend", values = colours$groups) + 
	      guides(col = guide_legend(nrow = 2)) + labs(caption = f)
      print(group_plot + theme_classic() + coord_fixed())
      ggsave(path = "figs", filename = paste(f, "functional_group.png", sep="_"));

      ## PLot II
      ##############
      species_plot <- ggplot() + 
	      geom_bar(aes(y = group, x = period, fill = new), data = df[[2]], stat="identity", width=4) +
	      labs(title = "Species Discovery by Date") +
	      xlab("Day of year (based on 5-day sampling)") +
	      ylab("Number of Species") +
	      scale_y_continuous(breaks=seq(yAxis[1],yAxis[2],10),limits = yAxis) +
	      scale_x_continuous(limits = xAxis) + 
	      scale_fill_manual("legend", values = colours$all) + labs(caption = f)

      print(species_plot + theme_classic() + coord_fixed()  + guides(col = guide_legend(nrow = 2)) + theme(aspect.ratio = 1))
      ggsave(path = "figs", filename = paste(f, "species.png", sep="_"));

      ## PLot III
      ##############
      phase_plot <- ggplot() + 
	      geom_bar(aes(y = phase, x = period, fill = phases), data = df[[3]], stat="identity", width=4) +
	      labs(title = "Species Discovery by Phenology Phase") + 
	      xlab("Day of year (based on 5-day sampling)") +
	      ylab("Number of Species") + 
	      scale_y_continuous(breaks=seq(yAxis[1],yAxis[2],10),limits = yAxis) +
	      scale_x_continuous(limits = xAxis) + 
	      scale_fill_manual("legend", values = colours$phase) + labs(caption = f)

      print(phase_plot + theme_classic() + coord_fixed() + theme(aspect.ratio = 1))
      ggsave(path = "figs", filename = paste(f, "phenology.png", sep="_"));

      ## PLot IV
      ##############
      group_all_plot <- ggplot() + 
	      geom_bar(aes(y = group, x = period, fill = groups), data = df[[4]], stat="identity", width=4) +
	      labs(title = "Total Species by Plant Functional Group") + 
	      xlab("Day of year (based on 5-day sampling)") +
	      ylab("Number of Species") +
	      # scale_y_continuous(limits = yAxis) +
	      scale_y_continuous(breaks=seq(yAxis[1],yAxis[2],10)) +
	      scale_x_continuous(limits = xAxis) + 
	      scale_fill_manual("legend", values = colours$groups) +
	      guides(col = guide_legend(nrow = 2)) + labs(caption = f)
      print(group_all_plot + theme_classic() + coord_fixed() + theme(aspect.ratio = 1))
      ggsave(path = "figs", filename = paste(f, "functional_group_all.png", sep="_"));


      ## Plot V
      ##############
      phase_all_plot <- ggplot() + 
	      geom_bar(aes(y = phase, x = period, fill = phases), data = df[[5]], stat="identity", width=4) +
	      labs(title = "Total Species by Phenology Phase") + 
	      xlab("Day of year (based on 5-day sampling)") +
	      ylab("Number of Species") + 
	      # scale_y_continuous(limits = yAxis) +
	      scale_y_continuous(breaks=seq(yAxis[1],yAxis[2],10)) +
	      scale_x_continuous(limits = xAxis) + 
	      scale_fill_manual("legend", values = colours$phaseAll) + labs(caption = f)
      print(phase_all_plot + theme_classic() + coord_fixed() + theme(aspect.ratio = 1))
      ggsave(path = "figs", filename = paste(f, "phenology_all.png", sep="_"));

      ## Plot VI
      ##############
      group_percent_plot <- ggplot() + 
	      geom_bar(aes(y = percent, x = period, fill = groups), data = df$groupPercent, stat="identity", width=4) +
	      labs(title = "Accummulative Percentage of Species Discovery by Plant Functional Group") + 
	      xlab("Day of year (based on 5-day sampling)") +
	      ylab("Accumulative Percent of Species") +
	      scale_y_continuous(breaks=seq(0,1,0.1),labels = percent) +
	      scale_x_continuous(limits = xAxis) + 
	      scale_fill_manual("legend", values = colours$groups) + labs(caption = f)
      print(group_percent_plot + theme_classic() + coord_fixed() + theme(aspect.ratio = 1))
      ggsave(path = "figs", filename = paste(f, "functional_group_percent.png", sep="_"));

      ### Plot VII
      ###############
      phase_percent_plot <- ggplot() + 
	      geom_bar(aes(y = percent, x = period, fill = phases), data = df$phasePercent, stat="identity", width=4) +
	      labs(title = "Accumulative Percentage of Species Discovery by Phenology Phase") + 
	      xlab("Day of year (based on 5-day sampling)") +
	      ylab("Accumulative Percent of Species") + 
	      scale_y_continuous(breaks=seq(0,1,0.1),labels = percent) +
	      scale_x_continuous(limits = xAxis) + 
	      scale_fill_manual("legend", values = colours$phase) + labs(caption = f)

      print(phase_percent_plot + theme_classic() + coord_fixed() + theme(aspect.ratio = 1))
      ggsave(path = "figs", filename = paste(f, "phenology_percent.png", sep="_"));

      ## Plot VIII
      ##############
      group_percentOfTotal_plot <- ggplot() + 
	      geom_bar(aes(y = percentOfTotal, x = period, fill = groups), data = df$groupPercent, stat="identity", width=4) +
	      labs(title = "Percentage of Species Discovery by Plant Functional Group") + 
	      xlab("Day of year (based on 5-day sampling)") +
	      ylab("Percent of Species") +
	      scale_y_continuous(breaks=seq(0,1,0.1),labels = percent) +
	      scale_x_continuous(limits = xAxis) + 
	      scale_fill_manual("legend", values = colours$groups) + labs(caption = f)
      print(group_percentOfTotal_plot + theme_classic() + coord_fixed() + theme(aspect.ratio = 1))
      ggsave(path = "figs", filename = paste(f, "functional_group_percent_of_total.png", sep="_"));

      ### Plot IX
      ###############
      phase_percent_of_total_plot <- ggplot() + 
	      geom_bar(aes(y = percentOfTotal, x = period, fill = phases), data = df$phasePercent, stat="identity", width=4) +
	      labs(title = "Percentage of Species Discovery by Phenology Phase") + 
	      xlab("Day of year (based on 5-day sampling)") +
	      ylab("Percent of Species") + 
	      scale_y_continuous(breaks=seq(0,1,0.1),labels = percent) +
	      scale_x_continuous(limits = xAxis, ) + 
	      scale_fill_manual("legend", values = colours$phase) + labs(caption = f)

      print(phase_percent_of_total_plot + theme_classic() + coord_fixed() + theme(aspect.ratio = 1))
      ggsave(path = "figs", filename = paste(f, "phenology_percent_of_total.png", sep="_"));


      warnings();
}

files <- lapply(files, function(file){
			charts(file)
			     })

names(files) <- sapply(files, function(x){
			       x$years + 1900
			     })

tableData <- list();

for(df in files){
	years <- unlist(df$years + 1900)
	f <- Reduce(function(x, y){paste(x, y, sep="-")}, years)
	xAxis <- df$xAxis;
	yAxis <- df$yAxis;

	# Table entries
	if(length(tableData) == 0){
		tableData <- df$tableData;
	}else{
		tableData <- rbind(tableData, df$tableData)
	}
	##
	plot(df)
}


#threeYear <- lapply(threeYear, function(file){charts(file)})
#for(df in threeYear){
#	years <- unlist(df$years + 1900)
#	years <- c(min(years), max(years))
#	f <- Reduce(function(x, y){paste(x, y, sep="-")}, years)
#	xAxis <- df$xAxis;
#	yAxis <- df$yAxis;
#	# Table entries
#	if(length(tableData) == 0){
#		tableData <- df$tableData;
#	}else{
#		tableData <- rbind(tableData, df$tableData)
#	}
#	####
#	plot(df)
#}
#tableData




#g <- tableGrob(tableData)
#grid.newpage();
#grid.draw(g)

#warnings()

