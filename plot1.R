## construct plot1.png
plot1 <- function(){
	## load and prepare data
	data <- load_data()

	## save to png
	png( filename = "plot1.png", width = 480, height = 480, units = "px" )

	## create plot
	hist( data$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)" )

	dev.off()
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