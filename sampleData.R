###############
# IMPORT DATA #
###############

# The following data was retrieved from the Bureau of Transportation Statistics through their web
# portal, TranStats. It was manually downloaded through a web form, and we compiled it into a zip
# file to load into R.

download.file(url = "http://point.fungservices.com/git/flight/data.zip", destfile = "~/Data/data.zip")
unzip(zipfile = "~/Data/data.zip", exdir = "~/Data/")

#################
# SAMPLING DATA #
#################

# WARNING: The following code is memory intensive. We used Amazon Web Services Elastic Compute Cloud.
# It took 20 minutes for the program to finish processing on r4.xlarge.
# With 10 years of data from BTS totalling almost 14GiB, we wanted to reduce the data to a manageable
# sample so that we can do some data exploration on our personal laptops. We also had to import all 120
# files, as TranStats only allowed downloading of their data by month. The code below reads all the .csv
# files in the directory, find out how many observations are in each file, and samples the same
# percentage of all the observations through simple sampling without replacement.

setwd("~/Data") # Set our working directory to be the "Data" folder.
datafiles <- list.files(".", pattern = ".csv") # List all the files from BTS and put it into variable "datafiles".
set.seed(160) # To ensure reproducible results, we're setting the seed to 160.
subset.the.data = function(x){ # We are going to sample 0.1% of each file with a function.
  y = read.csv(x) # First, read in the file.
  z = y[sample(nrow(y), round(nrow(y)*0.001), replace = FALSE),] # Sample the row numbers randomly without replacement.
}
sampling = do.call(rbind, lapply(datafiles[1:120], subset.the.data)) # Now go through all the files and execute the function.
write.csv(sampling, file = "~/Subset/pointone.csv") # Write out the samples into "pointone.csv".
subset.the.data = function(x){ # We are repeating this function but for 0.5% of each file.
  y = read.csv(x)
  z = y[sample(nrow(y), round(nrow(y)*0.005), replace = FALSE),]
}
sampling = do.call(rbind, lapply(datafiles[1:120], subset.the.data))
write.csv(sampling, file = "~/Subset/pointfive.csv") # Write out the samples into "pointfive.csv".
