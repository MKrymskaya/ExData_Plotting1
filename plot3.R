## construct plot3.png
plot3 <- function(){
	## load and prepare data
	data <- load_data()
	time_series <- create_time_series( data )

	## save to png
	png( filename = "plot3.png", width = 480, height = 480, units = "px" )

	## create plot
	plot( time_series[,1], data$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering" )
	points( time_series[,1], data$Sub_metering_2, type = "l", col = "red" )
	points( time_series[,1], data$Sub_metering_3, type = "l", col = "blue" )
	legend( "topright", col = c( "black", "red", "blue" ), legend = c( "Sub_metering_1", "Sub_metering_2", "Sub_metering_3" ), lty = c( 1, 1, 1 ) )

	dev.off()
}

## collect time/date points in single data series
create_time_series <- function( data ){
	time_series <- data.frame( with( data, as.POSIXct( paste( Date, Time ), format = "%d/%m/%Y %H:%M:%S" ) ) )
}

## set of functions to load data
load_data <- function(){
	# load list of dates in the data
	dates_list <- load_dates()

	# select only dates that are 2007-02-01 or 2007-02-02
	dates_selected <- grepl( "2007-02-01", dates_list, fixed = TRUE ) | grepl( "2007-02-02", dates_list, fixed = TRUE )

	# prepare to load only data corresponding to the selected dates
	row_idx <- which( dates_selected == TRUE )
	row_idx_min <- min( row_idx )
	row_idx_max <- max( row_idx )

	# load column headers
	col_headers <- read.table( "household_power_consumption.txt", sep = ";", header = TRUE, nrows = 1 )
	col_headers <- names( col_headers )

      # load data corresponding to the selected dates only
	data <- read.table( "household_power_consumption.txt", sep = ";", header = TRUE, skip = row_idx_min-1, nrows = row_idx_max-row_idx_min+1, col.names = col_headers )
}

## load dates in the data
load_dates <- function(){
	dates <- read.table( "household_power_consumption.txt", sep = ";", colClasses = c("character", rep("NULL", 8)), header = TRUE )
	dates <- as.Date( dates$Date, format = "%d/%m/%Y" )
}