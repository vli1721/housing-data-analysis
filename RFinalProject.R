# R Final Project
# Authors: Vincent Li, Karen Chen, Anna Lou

#Last modified: May 5, 2018

install.packages("ggplot2"); install.packages("e1071"); install.packages("stats4"); install.packages("MASS") # comment this out after installing packages
library(ggplot2); library(e1071); library(stats4); library(MASS)

# Finished additional points #1-7, 10-11, 13-14, 17-19
# Dataset: Brooklyn Housing Sales from 2003 to 2017 (Required dataset standards #1-4, Additional points #1-2)
# May take a minute to load, since it is a big dataset!
Brook <- read.csv("brooklyn_sales_map.csv"); head(Brook)

# Part 1 - Barplot of Housing Sale Years (using ggplot) (Required graphical displays #1, Additional points #11)
HousingSaleYears <- ggplot(Brook,
                           aes(x=year_of_sale))+
  geom_bar(fill="light green")
print(HousingSaleYears + labs(title="Housing Sale Years in Brooklyn, NY from 2003-2017", x="Year", y="Number of Houses Sold"))

which(Brook$year_of_sale < 2003); which(Brook$year_of_sale > 2017)
# No sale years before 2003 and after 2017

# The number of houses sold was at a high point in 2004, prior to the Housing Bubble bursting to precipitate the Great Recession of 2007-2008.
# However, the number of sales started to decline in 2005 and declined more quickly as the bubble burst. The number of housing sales has begun to recover
# in recent years (around 2011-2012), but the number has yet to reach its peak in 2004. It appears that the number of houses sold is
# leveling off, which may suggest that less people are looking for houses to buy or that more people are settled in to Brooklyn.



# Part 2 - Cleaning Sale Price Data using Trimmed mean and skewness (Additional points #13)
hist(Brook$sale_price); skewness(Brook$sale_price) # heavily skewed to the right
mean(Brook$sale_price); mean(Brook$sale_price, trim=0.01) # also very big outliers
prices <- which(Brook$sale_price > 0 & Brook$sale_price < 5e6) # only keep prices > $0 and < $5M
hist(Brook$sale_price[prices]); skewness(Brook$sale_price[prices]) # much better
Brooklyn <- Brook[with(Brook, prices),]



# Part 3 - Gross Area of a House Sold in Brooklyn - Plotting Probability Density Over a Histogram
# We consider houses with gross area between 500 square feet and 10000 square feet, inclusive.
GrossSqft <- Brooklyn$gross_sqft[which(Brooklyn$gross_sqft >= 500 & Brooklyn$gross_sqft <= 10000)]
length(GrossSqft); length(Brooklyn$gross_sqft)
# We are considering 243551 of 390883 houses.
hist(GrossSqft, probability=TRUE, col=rainbow(30),
     main="Gross Area Distribution of Houses in Brooklyn from 1000 to 10000 Sq Ft",
     xlab="Gross Sq Ft",
     ylab="Probability Density")
mu <- mean(GrossSqft)
sigma <- sd(GrossSqft)
curve(dnorm(x,mu,sigma), col = "black", add = TRUE)
# The distribution of gross areas is not normal.

# Now we try to overlay a gamma distribution over the histogram, since the distribution looks similar to a gamma distribution.
# We divide GrossSqft by 1000 to make the data on the scale of a gamma distribution.
# Gamma probability distribution overlaid on a histogram (Required graphical displays #3, additional points #6)
hist(GrossSqft/1000, probability=TRUE, col=rainbow(30),
     main="Gross Area Distribution of Houses in Brooklyn from 1000 to 10000 Sq Ft",
     xlab="Gross Sq Ft Divided by 1000",
     ylab="Probability Density")
fit <- fitdistr(GrossSqft/1000,"gamma")
shape <- fit[1]$estimate[1]; shape
rate <- fit[1]$estimate[2]; rate
curve(dgamma(x,shape=shape,rate=rate), col = "black", from=0, to=10, add=TRUE)
# We find that the distribution turns out to be approximately gamma (with shape = 4.663036 and rate = 1.652942)!
# Most houses are around 1500 sq ft to 2500 sq ft. This is not too surprising, as houses sold in Brooklyn are probably
# mostly apartments, due to Brooklyn being a borough of New York City. What is surpising is that the gamma distribution
# is a pretty good fit for the gross area distribution. Gamma distributions have been used to model insurance claims and
# rainfall because these events are aggregated. Thus, this analysis may suggest that there is some accumulation or
# aggregation occurring with houses being sold.

# Theoretical Analysis of Gamma Distribution (Additional points #17)
# Check to see if the expectation of this distribution matches well (additional points #7,10)
f <- function(x) x * dgamma(x, shape=shape, rate=rate)
ex <- integrate(f, -Inf, Inf)$value; ex # 2.821053
mean(GrossSqft/1000) # 2.821062 - very close!

# Check to see if the variance of this distribution matches well
f <- function(x) x^2 * dgamma(x, shape=shape, rate=rate)
ex2 <- integrate(f, -Inf, Inf)$value
var <- ex2 - ex^2; var # 1.706686
var(GrossSqft/1000) # 2.068708 - still not bad

# Check to see if the skewness of this distribution matches well (Additional points #13)
skewness(GrossSqft/1000) # 1.721424
2/sqrt(shape) # 0.9261805 - bit off



# Part 4 - Price Per Sq Ft (Using Gross Area) - Central Limit Theorem Analysis (Required analysis #4)
# Sample 50 prices per gross area from the population and repeat 10000 times
# Plot the means of each sample, which should give a normal distribution
PriceCols <- which(Brooklyn$sale_price >= 10000 & Brooklyn$sale_price <= 4000000 & Brooklyn$gross_sqft >= 1000 & Brooklyn$gross_sqft <= 20000)
Price <- Brooklyn$sale_price[PriceCols]
GrossArea <- Brooklyn$gross_sqft[PriceCols]
PricePerSqFt <- Price/GrossArea
# Mean of prices per gross area
mu <- mean(PricePerSqFt); mu
# Standard deviation of prices per gross area
sigma <- sd(PricePerSqFt); sigma
# Set sample size to 50
n <- 50
# Repeat sampling 10000 times and store sample means (Additional points #2 - using large data set as a population to draw samples)
N <- 10000; xx <- numeric(N)
for(i in 1:N){
  x <- sample(PricePerSqFt, n)
  xx[i] <- mean(x)
}
# Plot histogram of sample means
hist(xx, probability = TRUE, col=rainbow(50), main="Sample Means of Price Per Sq Ft", xlab="Price Per Sq Ft", ylab="Probability")
# Overlay a normal distribution probability density function
curve(dnorm(x, mu, sigma/sqrt(n)),add = TRUE, col = "darkblue")
# The sample means approximate a normal distribution by the Central Limit Theorem!
mean(xx); mu
# The mean of the sample means is close to the actual mean.

# Now, let's sample from the population using Monte Carlo simulation.
N <- 10000; xy <- numeric(N)
for(i in 1:N){
  xy[i] <- sample(PricePerSqFt, 1)
}
# Plot histogram of samples
hist(xy, probability = TRUE, col=rainbow(10), main="Sample Means of Price Per Sq Ft", xlab="Price Per Sq Ft", ylab="Probability")
# Unlike the Central Limit Theorem Analysis, taking samples using a Monte Carlo simulation approach does not yield a normal distribution.
# Instead, the distribution looks similar to that of the whole population (almost gamma-like distribution).
# Thus, taking samples randomly may be a good way to approximate the population.



# Part 5 - Scatter Plots and Linear Regression of Brooklyn Houses Sold Before and After the Recession's Lowest Point (June 2009)
# (Additional points #14)
dates <- as.numeric(as.POSIXct(Brooklyn$sale_date)) # convert datetime to numeric

count_houses <- function(time, min, max, color) {
  N <- (max-min)/month
  houses <- numeric(N)
  for (i in 1:N) { 
    houses[i] <- length(which(dates >= min+(i-2)*time & dates < min+(i-1)*time)) 
  }
  times <- numeric(N)
  for (i in 1:N) { times[i] <- min+(i-1)*time }
  points(times, houses, pch=".", cex=3, col=color)
  clip(min, max, 1000, 4000)
  b <- cov(times,houses)/var(times)
  a <- mean(houses) - b*mean(times)   
  abline(a, b, col=color)
}

min <- min(dates); max <- max(dates)
month <- 2.628e+6 # seconds per month
d2009 <- as.numeric(ISOdatetime(2009, 6, 1, 0, 0, 0)) # lowest point
# Set up plot
plot(0, pch=".", cex=3, xlim=c(min,max), ylim=c(1000,4000), col="red", xaxt="n",
     main="Houses Sold Per Month Before and After June 2009", xlab="Month", ylab="Houses Sold")
axis(1, at=c(1:5)*1e8+1e9, labels=c("Jan 2005","Feb 2008","Apr 2011","June 2014","Aug 2017"))

# Plot houses sold per month + regression line before lowest point (June 2009)
count_houses(month, min, d2009, "red"); clip(min, max, 1000, 4000)
# Plot houses sold per month + regression line after lowest point (June 2009)
count_houses(month, d2009, max, "blue"); clip(min, max, 1000, 4000)

# Applying linear regression to the number of houses sold before and after June 2009, which was during
# the acute phase of the Great Recession, reveals that there was a steady decline in number of houses
# sold before June 2009 and there was a steady increase in houses sold afterward. This demonstrates
# that the number of houses sold was correlated with the phases of the Great Recession and thus confirms
# that the housing bubble bursting corresponds closely to the Great Recession. Moreover, the data shows
# that the number of houses sold had already been in decline for several years prior to the Great Recession,
# which suggests that the rising housing prices of the bubble may have begun to turn away potential buyers
# years before the actual burst of the bubble. Thus, the bubble did not burst all of a sudden, but rather it
# was slowly losing air and volume as housing prices increased over the years before 2008.



# Part 6 - Contingency Tables (Required graphical displays #4, Required analysis #2-3 - p-values and contingency tables)
# Comparing year sold with zip code
zip <- Brooklyn$zip_code[which(Brooklyn$zip_code > 0)] # some addresses have zip code 0, which we are not including in our analysis
saleyr <- Brooklyn$year_of_sale[which(Brooklyn$zip_code > 0)]
tbl1 <- table(saleyr, zip); tbl1 # contingency table
chisq.test(saleyr, zip) # chisquare test
# X-squared = 22418, number of degrees of freedom = 644, p-value < 2.2e-16
# p-value is < 0.05, so the data is statistically significant

# Comparing year sold with neighborhood
tbl2 <- table(Brooklyn$year_of_sale, Brooklyn$neighborhood); tbl2
chisq.test(Brooklyn$year_of_sale, Brooklyn$neighborhood)
# X-squared = 21619, number of degrees of freedom = 882, p-value < 2.2e-16
# p-value is < 0.05, so the data is statistically significant

# Comparing year sold with building class category
tbl3 <- table(Brooklyn$year_of_sale, Brooklyn$building_class_category); tbl3
chisq.test(Brooklyn$year_of_sale, Brooklyn$building_class_category)
# X-squared = 231530, number of degrees of freedom = 1232, p-value < 2.2e-16
# p-value is < 0.05, so the data is statistically significant

# Comparing neighborhood with building class category
tbl4 <- table(Brooklyn$neighborhood, Brooklyn$building_class_category); tbl4
chisq.test(Brooklyn$neighborhood, Brooklyn$building_class_category)
# X-squared = 424310, number of degrees of freedom = 5544, p-value < 2.2e-16
# p-value is < 0.05, so the data is statistically significant
# The X-squared, or chi-square, value is significantly larger than the X-squared values found by the chi-square tests done on tbl1, tbl2, and tbl3,
# which shows that in tbl4, the observed values differ from the expected values if the two (categorical) variables were independent much more drastically than in tbl1, tbl2, tbl3



# Part 7 - Average Price Per Sq Ft - Cubic and Quintic Regressions (Additional points #5)
# Calculate average price per sq ft for each year from 2003-2017
avgPricePerSqFt <- numeric(15)
for (i in 1:15) {
  currDataCols <- which(
    Brooklyn$year_of_sale == (2002+i) &
      Brooklyn$sale_price >= 10000 &
      Brooklyn$sale_price <= 4000000 &
      Brooklyn$gross_sqft >= 1000 &
      Brooklyn$gross_sqft <= 20000)
  avgPricePerSqFt[i] <- mean(Brooklyn$sale_price[currDataCols]/Brooklyn$gross_sqft[currDataCols])
}
# Plot the average price per Sq Ft data using ggplot2
Year <- c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)-2003     # Year since 2003
df <- data.frame(Year,avgPricePerSqFt); df
AvgPriceSqFtPlot <- ggplot(df, aes(x=Year,y=avgPricePerSqFt)) + geom_point()

print(AvgPriceSqFtPlot + labs(
  title="Average Price Per Square Foot for Brooklyn House Sales From 2003-2017",
  x="Years since 2003",
  y="Average Price Per Square Foot ($)"))
# In this scatter plot, we see that on average, housing prices (measured as average prife per sq ft based on gross area)
# in Brooklyn were rising from 2003 until 2007 and then they began declining after the bubble burst around 2008. However,
# the decline is not very sharp. In fact, after a few years of approximately stagnant prices, prices began growing rapidly
# in 2013. It appears that if prices are continuing their rapid, increasing rate of growth, we may be heading toward another bubble.

# These points look as if they form a cubic or quintic polynomial!

# Let's try to find a polynomial of best fit.
# Cubic regression
cubic.model <- lm(avgPricePerSqFt ~ poly(Year,3),data=df)
summary(cubic.model)
# Adjusted R-squared is 0.9636 for cubic regression
AvgPriceSqFtPlot <- ggplot(df, aes(x=Year,y=avgPricePerSqFt)) + geom_point() + stat_smooth(method="lm",
                                                                                           se=TRUE,
                                                                                           fill=NA,
                                                                                           formula=y ~ poly(x, 3, raw=TRUE),
                                                                                           colour="red")
print(AvgPriceSqFtPlot + labs(
  title="Average Price Per Square Foot for Brooklyn House Sales From 2003-2017",
  x="Year",
  y="Average Price Per Square Foot ($)"))

#Let's try a quintic regression
quintic.model <- lm(avgPricePerSqFt ~ poly(Year,5),data=df)
summary(quintic.model)
# Adjusted R-squared is 0.9899, which is better than the cubic regression!
df <- data.frame(Year,avgPricePerSqFt); df
AvgPriceSqFtPlot <- ggplot(df, aes(x=Year,y=avgPricePerSqFt)) + geom_point() + stat_smooth(method="lm",
                                                                                           se=TRUE,
                                                                                           fill=NA,
                                                                                           formula=y ~ poly(x, 5, raw=TRUE),
                                                                                           colour="red")
print(AvgPriceSqFtPlot + labs(
  title="Average Price Per Square Foot for Brooklyn House Sales From 2003-2017",
  x="Year",
  y="Average Price Per Square Foot ($)"))
# The quintic regression is a pretty good fit!
# However, we would need more data and models to make more accurate predictions about the trajectory of housing prices in coming years.




# Part 8 - Permutation Test and Monte Carlo Simulation (Required analysis #1)
ZipCodes <- unique(Brooklyn$zip_code); ZipCodes
# Remove outliers (0 and 33803) from Zip Codes vector
ZipCodes <- sort(ZipCodes)[-1]; ZipCodes
ZipCodes <- sort(ZipCodes, decreasing=TRUE)[-1]; ZipCodes
# Now we have removed the two outliers!

# Randomly choose two zip codes for the permutation test
salePriceChecker <- Brooklyn$sale_price >= 10000 & Brooklyn$sale_price <= 4000000
sampledZipCodes <- sample(ZipCodes, 2); sampledZipCodes
zip1 <- sampledZipCodes[1]
zip2 <- sampledZipCodes[2]
# Sampling Distribution Theoretical Analysis - Let us explore this sample function (additional points #18)
samples <- numeric(10000)
for (i in 1:10000) {
  samples[i] <- sample(ZipCodes, 1)
}
barplot(table(samples), col=rainbow(30),
        main="Sample Distribution",
        xlab="Zip Code",
        ylab="Frequency")
# Samples seem to be pretty uniformly drawn, which makes sense because the R function "sample()" samples from the uniform distribution!

# Take subset of dataset with the either of the two randomly selected zip codes and a sale price between $10000 and $4000000 inclusive
Brooklyn.subset <- Brooklyn[with(Brooklyn, (zip_code==zip1 | zip_code==zip2) & salePriceChecker),]
zip1Avg <- sum(Brooklyn.subset$sale_price*(Brooklyn.subset$zip_code==zip1))/sum(Brooklyn.subset$zip_code==zip1); zip1Avg
zip2Avg <- sum(Brooklyn.subset$sale_price*(Brooklyn.subset$zip_code==zip2))/sum(Brooklyn.subset$zip_code==zip2); zip2Avg
observed <- zip1Avg - zip2Avg; observed   # Observed diference between two average prices based on zip code
Zip <- sample(Brooklyn.subset$zip_code)

# Use a Monte Carlo Simulation of 10000 random permutations of the Zip Code column to take differences for the Permutation Test
N <- 10000
diffs <- numeric(N)   # Store differences between two average prices based on zip code
for (i in 1:N){
  Zip <- sample(Brooklyn.subset$zip_code)   # permuted zip code column
  zip1Avg <- sum(Brooklyn.subset$sale_price*(Zip==zip1))/sum(Zip==zip1); zip1Avg
  zip2Avg <- sum(Brooklyn.subset$sale_price*(Zip==zip2))/sum(Zip==zip2); zip2Avg
  diffs[i] <- zip1Avg - zip2Avg
}
# Find average of differences
mean(diffs)
hist(diffs, breaks = "FD", col=rainbow(30),
     main="Permutation Test on the Average Sale Prices of Houses in Two Randomly Selected Zip Codes in Brooklyn",
     xlab="Average Difference in Sale Price ($)",
     ylab="Frequency")
# Add a line on the histogram to represent the observed value
abline(v = observed, col = "black")

# Calculate P-value (probability that a difference this large could have arisen with a random subset)
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue

# After running this permutation test several times, we find that some randomly selected zip codes may give distributions that appear normal,
# while others may give distributions that are clearly not normal.



# Part 9 - Plot to visualize range of prices depending on year sold and school district in Brooklyn (Additional Points #19)
SchoolDistricts <- sort(unique(Brooklyn$SchoolDist)); SchoolDistricts # get rid of NA school district number (by using sort function)
# Filter dataset to houses that cost $10000 to $4000000 and with gross area of 1000 sq ft to 20000 sq ft and with valid school district (not NA)
checker <- Brooklyn$sale_price >= 10000 & Brooklyn$sale_price <= 4000000 & Brooklyn$gross_sqft >= 1000 & Brooklyn$gross_sqft <= 20000 & !is.na(Brooklyn$SchoolDist)
# Create filtered subset of dataset
Brooklyn.subset2 <- Brooklyn[with(Brooklyn, checker),]
# Store average price, year, and school district results in vectors that will later be combined in a data frame
Average <- numeric(length(SchoolDistricts)*15)
Year <- numeric(length(SchoolDistricts)*15)
SchoolDistrict <- numeric(length(SchoolDistricts)*15)
# Calculate average prices per school district per year
for (i in 1:length(SchoolDistricts)) {
  SchoolDistrictCompare <- rep(SchoolDistricts[i],length(Brooklyn.subset2$SchoolDist))
  
  for (j in 1:15) {
    YearCompare <- rep(2002+j, length(Brooklyn.subset2$year_of_sale))
    Average[(i-1)*15+j] <- sum(Brooklyn.subset2$sale_price*(Brooklyn.subset2$SchoolDist==SchoolDistrictCompare)*(Brooklyn.subset2$year_of_sale==YearCompare))/sum((Brooklyn.subset2$SchoolDist==SchoolDistrictCompare)*(Brooklyn.subset2$year_of_sale==YearCompare))
    Year[(i-1)*15+j] <- 2002+j
    SchoolDistrict[(i-1)*15+j] <- SchoolDistricts[i]
  }
}
# Combine results into a data frame and plot with ggplot
DF <- data.frame(Average,Year,SchoolDistrict); DF
DFPlot <- ggplot(DF,
       aes(x=Year,
           y=Average,
           color=SchoolDistrict))+
  geom_point()
print(DFPlot + labs(title="Avg Sale Prices of Houses Based on Year and School District", x="Year", y="Average Sale Price ($)"))
# The data appears to show that housing prices are becoming more widely distributed in recent years.
# There was an overall drop in housing prices after the housing bubble burst in 2007-2008, but housing
# prices have all begun to rise recently. Analyzing prices by school district reveals that the rising prices
# are not only driven by a single area but rather that prices are rising across the board, across all school districts.
# Interestingly, the school districts with the lowest prices seem to have prices increasing the least, whereas the most
# expensive school districts have housing prices growing at increasing rates. This widening housing price gap may suggest
# increasing economic disparity across school districts, as affluent neighborhoods attract the wealthy of New York City and
# less affluent areas are paid less attention. This raises several ethical questions, including whether school districts with
# lower housing prices are being neglected due to poorer education or lack of resources that more affluent school districts have.
# Policymakers may use this data to determine which school districts may need the most aid, as housing prices can potentially
# reflect the state of education and the state of wellbeing in a certain neighborhood. Ultimately, the data reveals interesting
# trends that may or may not be rationally justified. Exploring effects of policy on these trends may reveal more about how they
# arise and how we can use them to improve our society.
