# Enterprise Database Technologies
# X00119321 Jason Domican
# X00123156 Robert Fitzgerald


# Read in data from the csv dataset
health <- read.table(file = "https://drive.google.com/uc?export=download&id=12ndSWZTA-2YsxWEqZMHgBV6sitAh1ktc", header=TRUE, sep =",", stringsAsFactors = FALSE)

#For each predictor variable, where appropriate,find the following

# Get the summary of the entire health dataset
summary(health)

#Discretise variable for number of coloured vessels (originally numeric)
ca_factor <- factor(health$ca)

#https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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
Mode(ca_factor)
Mode(health$thal)
Mode(health$class)

#Calculate the (median) standard deviations for each relevant field
sd(health$age)
sd(health$trestbps)
sd(health$cholesterol, na.rm = TRUE)
sd(health$diastbpexerc)
sd(health$thalach)
sd(health$oldpeak)

#Define a function to calculate skewness
Skewness <- function(x) {
  return (3 * ((mean(x, na.rm = TRUE) - median(x, na.rm = TRUE))) / sd(x, na.rm = TRUE))
}

Skewness(health$age)
Skewness(health$trestbps)
Skewness(health$cholesterol)
Skewness(health$diastbpexerc)
Skewness(health$thalach)
Skewness(health$oldpeak)

# Plots histogram with target variable overlay
PlotHistogram <- function(myData, labelIn) {
  ggplot(health, aes(x = myData, fill = health$class)) + 
  geom_histogram(colour = "black", position = "fill") + xlab(labelIn)
}

PlotBar <- function(myData, labelIn) {
  ggplot(health, aes(x = myData, fill = health$class)) +
  geom_bar(colour = "black", position = "fill") + xlab(labelIn)
}

PlotScatterPair <- function(xIn, yIn, xlabIn, ylabIn) {
  ggplot(health, aes(xIn, yIn )) + geom_point(size =2) + xlab(xlabIn) + ylab(ylabIn)
}

PlotHistogram(health$age, "Age")
PlotHistogram(health$trestbps, "Resting Blood Pressure")
PlotHistogram(health$cholesterol, "Cholesterol")
PlotHistogram(health$diastbpexerc, "Diastolic exercising blood pressure")
PlotHistogram(health$thalach, "Maximum heart rate achieved")
PlotHistogram(health$oldpeak, "ST depression induced by exercise relative to rest")

PlotBar(health$sex, "Sex")
PlotBar(health$cp, "Cp")
PlotBar(health$Fasting.blood.sugar...120, "Blood Sugar")
PlotBar(health$restecg, "Restecg")
PlotBar(health$exang, "Exang")
PlotBar(health$slope, "Slope")
PlotBar(health$thal, "Thal")
PlotBar(ca_factor, "Number of miscoloured blood vessels")

#Scatter plot for each numeric pair
PlotScatterPair(health$age, health$trestbps, "Age", "Resting Blood Pressure")
PlotScatterPair(health$age, health$cholesterol, "Age", "Cholesterol")
PlotScatterPair(health$age, health$diastbpexerc, "Age", "Diastolic exercising blood pressure")
PlotScatterPair(health$age, health$thalach, "Age", "Maximum heart rate achieved")
PlotScatterPair(health$age, health$oldpeak, "Age", "ST depression induced by exercise relative to rest")

#Uses nortest package to determine Normality
ad.test(health$age)
ad.test(health$age)
ad.test(health$trestbps)
ad.test(health$cholesterol)
ad.test(health$diastbpexerc)
ad.test(health$thalach)
ad.test(health$oldpeak)

age <- health$age
sex <- health$sex
cp <- health$cp
trestbps<- health$trestbps
cholesterol <- health$cholesterol
Fasting.blood.sugar...120 <- health$Fasting.blood.sugar...120
restecg <- health$restecg
diastbpexerc <- health$diastbpexerc
thalach <- health$thalach
exang <- health$exang
oldpeak <- health$oldpeak
slope<- health$slope
ca <- health$ca
thal <- health$thal
class <- health$class


#Correlation
d <- data.frame(age=rnorm(308),
                sex=rnorm(308),
                cp=rnorm(308),
                trestbps=rnorm(308),
                cholesterol=rnorm(308),
                Fasting.blood.sugar...120=rnorm(308),
                restecg=rnorm(308),
                diastbpexerc=rnorm(308),
                thalach=rnorm(308),
                exang=rnorm(308),
                oldpeak=rnorm(308),
                slope=rnorm(308),
                ca=rnorm(308),
                thal=rnorm(308),
                class=rnorm(308))
M <- cor(d)
corrplot(M,method = 'ellipse')


#Equal with binning, library("classInt")
classIntervals(health$age, 5)
x <- classIntervals(health$age, 5, style = 'equal')

#http://www.learnbymarketing.com/tutorials/k-means-clustering-in-r-example/
#K-means clustering, package 'stats' used
k <- kmeans(health$age, centers = 5)
#Cluster centers
k$center

#k$withinss - the sum of the square of the distance from each data point to the cluster center.  Lower is better.
#High means outliersor you need more clusters.
k$withinss

#k$betweenss tells you the sum of the squared distance between cluster centers.
#Ideally you want cluster centers far apart from each other.
k$betweenss
#Count of data points in each cluster
table(k$cluster)


#Choose a skewed numeric variable - Oldpeak is most skewed variable


