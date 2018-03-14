# Enterprise Database Technologies CA1
# X00119321 Jason Domican
# X00123156 Robert Fitzgerald

# Read in data from the original csv dataset (Hosted on Google Drive)
health_original <- read.table(file = "https://drive.google.com/uc?export=download&id=17bHVJflRAzaPcAIeYNaPRmbcvQGuafhB", header=TRUE, sep =",", stringsAsFactors = TRUE)
columns <- c("age","sex","cp","trestbps","cholesterol","Fasting blood sugar","restecg","diastbpexerc","thalach","exang","oldpeak","slope","ca","thal","class")

# Include libraries
library(nortest)
library(ggplot2)
library(stats)
library(e1071)
library(corrplot)
library(classInt)
library(mice)

# Get the summary of the original dataset, noting NA values and other inconsistencies
summary(health_original)

# Fix issues in the Sex, Chest Pain and Coloured Artery fields
health <- data.frame(health_original)
health[health=="f"]<-"Female"
health[health=="m"]<-"Male"
health[health==" Asymptomatic"]<-"Asymptomatic"
# Remove unnecessary factor levels
health$sex <- factor(health$sex)
health$cp <- factor(health$cp)
# Discretise variable for number of coloured vessels (originally numeric)
health$ca <- factor(health$ca)

# Get the summary of the entire health dataset (Min, Max, Mean and Median provided here)
summary(health)

# Mode function to calculate the mode of attributes
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Call mode function on each attribute
Mode(health$age)
Mode(health$sex)
Mode(health$cp)
Mode(health$trestbps)
Mode(health$cholesterol)
Mode(health$Fasting.blood.sugar...120)
Mode(health$restecg)
Mode(health$diastbpexerc)
Mode(health$thalach)
Mode(health$exang)
Mode(health$oldpeak)
Mode(health$slope)
Mode(health$ca)
Mode(health$thal)
Mode(health$class)

# Calculate the (median) standard deviations for each relevant field
sd(health$age)
sd(health$trestbps)
sd(health$cholesterol, na.rm = TRUE)
sd(health$diastbpexerc)
sd(health$thalach)
sd(health$oldpeak)

# Uses nortest package to determine Normality
# Format function to stop the print of exponent('e') values and see raw value
format(ad.test(health$age), scientific = F)
format(ad.test(health$trestbps), scientific = F)
format(ad.test(health$cholesterol), scientific = F)
format(ad.test(health$diastbpexerc), scientific = F)
format(ad.test(health$thalach), scientific = F)
format(ad.test(health$oldpeak), scientific = F)

# Uses the Shapiro-Wilks test to determine normality. If the p-value is > 0.05, passes 
# normality test and allows you to state no significant departure from normality was found
format(shapiro.test(health$age), scientific = F)
format(shapiro.test(health$trestbps), scientific = F)
format(shapiro.test(health$cholesterol), scientific = F)
format(shapiro.test(health$diastbpexerc), scientific = F)
format(shapiro.test(health$thalach), scientific = F)
format(shapiro.test(health$oldpeak), scientific = F)

# PlotQ function to test for normality
plotQ <- function(mydata, mainIn) {
  qqnorm(mydata, main = mainIn)
  qqline(mydata, col='red')
}

# Call the function with all numeric attributes
plotQ(health$age, "Normal Q-Q Plot for age")
plotQ(health$trestbps, "Normal Q-Q Plot for trestbps")
plotQ(health$cholesterol, "Normal Q-Q Plot for cholesterol")
plotQ(health$diastbpexerc, "Normal Q-Q Plot for diastbpexerc")
plotQ(health$thalach, "Normal Q-Q Plot for thalach")
plotQ(health$oldpeak, "Normal Q-Q Plot for oldpeak")

# Call skewness function on each numeric attribute
skewness(health$age)
skewness(health$trestbps)
skewness(health$cholesterol, na.rm = TRUE)
skewness(health$diastbpexerc)
skewness(health$thalach)
skewness(health$oldpeak)

# Correlation between predictor variables
health_correlation <- data.frame(
  health$age,
  as.integer(health$sex),
  as.integer(health$cp),
  health$trestbps,
  health$cholesterol,
  as.integer(health$Fasting.blood.sugar...120),
  as.integer(health$restecg),
  health$diastbpexerc,
  health$thalach,
  as.integer(health$exang),
  health$oldpeak,
  as.integer(health$slope),
  as.integer(health$ca),
  as.integer(health$thal)
)

# Assign more readable column names to the data frame, excluding the target variable
colnames(health_correlation) <- columns[1:14]

# Plot correlation data frame in corrplot to see the level of correlation among the variables
M <- cor(health_correlation)
corrplot(M,method = 'shade', type = "lower")


# Histogram Function, Plots histogram with target variable overlay
PlotHistogram <- function(myData, labelIn, binwidthIn) {
  ggplot(health, aes(x = myData)) + 
    geom_histogram(colour = "black", aes(fill = health$class), position = "dodge", binwidth = binwidthIn) + xlab(labelIn) + ylab("Count")
}

# Plot Histograms for each numeric attribute
PlotHistogram(health$age, "Age", 4)
PlotHistogram(health$trestbps, "Resting Blood Pressure", 4)
PlotHistogram(health$cholesterol, "Cholesterol", 50)
PlotHistogram(health$diastbpexerc, "Diastolic exercising blood pressure", 4)
PlotHistogram(health$thalach, "Maximum heart rate achieved", 4)
PlotHistogram(health$oldpeak, "ST depression induced by exercise relative to rest", 4)

# Plotbar Function, Plots Bar chart with target variable overlay
PlotBar <- function(myData, labelIn) {
  ggplot(health, aes(x = myData, fill = health$class)) +
    geom_bar(colour = "black", aes(fill = health$class), position = "dodge") + xlab(labelIn)
}

# Plot bar chart for each categorical attribute
PlotBar(health$sex, "Sex")
PlotBar(health$cp, "Cp")
PlotBar(health$Fasting.blood.sugar...120, "Blood Sugar")
PlotBar(health$restecg, "Restecg")
PlotBar(health$exang, "Exang")
PlotBar(health$slope, "Slope")
PlotBar(health$thal, "Thal")
PlotBar(health$ca, "Number of miscoloured blood vessels")

# Find outliers statistically
quantile(health$cholesterol, na.rm = TRUE)

boxplot.stats(health$age)$out
boxplot.stats(health$trestbps)$out
boxplot.stats(health$cholesterol)$out
boxplot.stats(health$diastbpexerc)$out
boxplot.stats(health$thalach)$out
boxplot.stats(health$oldpeak)$out


# PlotScatterPair Function
PlotScatterPair <- function(xIn, yIn, xlabIn, ylabIn) {
  ggplot(health, aes(xIn, yIn )) + geom_point(size =2) + xlab(xlabIn) + ylab(ylabIn)
}

# Scatter plot for each numeric pair
PlotScatterPair(health$age, health$trestbps, "Age", "Resting Blood Pressure")
PlotScatterPair(health$age, health$cholesterol, "Age", "Cholesterol")
PlotScatterPair(health$age, health$diastbpexerc, "Age", "Diastolic exercising blood pressure")
PlotScatterPair(health$age, health$thalach, "Age", "Maximum heart rate achieved")
PlotScatterPair(health$age, health$oldpeak, "Age", "ST depression induced by exercise relative to rest")
PlotScatterPair(health$trestbps, health$cholesterol, "Resting Blood Pressure", "Cholesterol")
PlotScatterPair(health$trestbps, health$diastbpexerc, "Resting Blood Pressure", "Diastolic exercising blood pressure")
PlotScatterPair(health$trestbps, health$thalach, "Resting Blood Pressure", "Maximum heart rate achieved")
PlotScatterPair(health$trestbps, health$oldpeak, "Resting Blood Pressure", "ST depression induced by exercise relative to rest")
PlotScatterPair(health$cholesterol, health$diastbpexerc, "Cholesterol", "Diastolic exercising blood pressure")
PlotScatterPair(health$cholesterol, health$thalach, "Cholesterol", "Maximum heart rate achieved")
PlotScatterPair(health$cholesterol, health$oldpeak, "Cholesterol", "Maximum heart rate achieved")
PlotScatterPair(health$diastbpexerc, health$thalach, "Diastolic exercising blood pressure", "Maximum heart rate achieved")
PlotScatterPair(health$diastbpexerc, health$oldpeak, "Diastolic exercising blood pressure", "ST depression induced by exercise relative to rest")
PlotScatterPair(health$thalach, health$oldpeak, "Maximum heart rate achieved", "ST depression induced by exercise relative to rest")

# Statistically verify correlation
cor(health$trestbps, health$diastbpexerc, method = c("pearson"))
cor(health$trestbps, health$diastbpexerc, method = c("kendall"))
cor(health$trestbps, health$diastbpexerc, method = c("spearman"))


# Equal with binning, library("classInt")
nbins <- 5
classIntervals(health$age, nbins, style = 'equal')

# K-means clustering, package 'stats' used
k <- kmeans(health$age, centers = nbins)
whichbin_kmeans <- k$cluster
table(whichbin_kmeans)

# Cluster centers
k$center

# k$withinss - the sum of the square of the distance from each data point to the cluster center.  Lower is better.
# High means outliersor you need more clusters.
k$withinss

# k$betweenss tells you the sum of the squared distance between cluster centers.
# Ideally you want cluster centers far apart from each other.
k$betweenss
# Count of data points in each cluster
table(k$cluster)

# Choose a skewed numeric variable - Oldpeak is most skewed variable
# Z-score Standardisation
oldpeak_z <- scale(health$oldpeak, center = TRUE, scale = TRUE)

# Natural Log transformation
natlog.oldpeak <- log(health$oldpeak)
natlog.oldpeak
# Replace Inf values with 0
natlog.oldpeak[!is.finite(natlog.oldpeak)] <- 0

# Square Root transformation
sqrt.oldpeak <- sqrt(health$oldpeak)
sqrt.oldpeak

# Inverse Square Root transformation
invsqrt.oldpeak <- 1/sqrt(health$oldpeak)
invsqrt.oldpeak
invsqrt.oldpeak[!is.finite(invsqrt.oldpeak)] <- 0

# Original Skewness
skewness(health$oldpeak)

skewness(oldpeak_z)
skewness(sqrt.oldpeak)
skewness(natlog.oldpeak)
skewness(invsqrt.oldpeak)

# Health dataset containing the normalised oldpeak values
health_normalised <- data.frame(health)
health_normalised$oldpeak <- sqrt.oldpeak
# Rename column to reflect Square Root transformation
names(health_normalised)[names(health_normalised) == 'oldpeak'] <- 'sqrt.oldpeak'

# Impute missing data for a categorical variable
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(health,2,pMiss)

# Replace NA values with the median/ mode
health_normalised[c("cholesterol")][is.na(health_normalised[c("cholesterol")])]<- 
  median(health_normalised$cholesterol, na.rm = TRUE)

health_normalised[c("class")][is.na(health_normalised[c("class")])]<- 
  Mode(health_normalised$class)

# Parameter m is the number of imputed datasets
imputedData <- mice(health_normalised,m=50,maxit=25,seed=506)

# The imputed data for restecg, displays all imputed datasets (5 passthroughs in this case)
imputedData$imp$restecg
Mode(imputedData$imp$restecg)
summary(imputedData)

# Replace missing restecg values with the most commonly generated imputed values
imputedData$imp$restecg <- Mode(imputedData$imp$restecg)
health_normalised <- complete(imputedData,1)
health_normalised$restecg