
#### apportion(data,  column with state names, data with state pop, number of  seats to be allocated, number of minimum seats per state, minimum number of votes to be awarded any seat)

apportion <- function(data, state, pop, seats, autoseats, threshold=0) {

	num <- seats

	data <- data[complete.cases(data[, pop]), ]
	data <- data[data[, pop]> threshold, ]
	priority <- seq(1, num + 5, by = 1)

	dta <- data.frame(state = rep(data[, state], length(priority)), row.names = NULL)

	pri <- length(data[, state]) * length(priority)

	dta2 <- data.frame(state = rep(data[, state], autoseats), row.names = NULL)
	dta2[, 2] <- 99999999


	n <- 1
	l <- 0
	for (j in 1:length(priority)) {
		k <- l + 1
		l <- k + length(data[, state]) - 1
		multiplier <- 1/(sqrt(n * (n + 1)))

		dta[k:l, 2] <- data[, pop] * multiplier

		n <- n + 1
	}

	#dta <- dta[(is.finite(dta[,2])),]
	dta <- rbind(dta, dta2)
	dta <- dta[order(-dta[, 2]), ]
	final <- dta
	final <- final[1:num, ]
	final[, 3] <- seq(1, num, by = 1)
	apportionment <- as.data.frame(table(final[, 1]))
	colnames(apportionment) <- c("state", "seats")
	return(apportionment)
}



