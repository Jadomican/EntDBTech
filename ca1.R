# Enterprise Database Technologies
# X00119321 Jason Domican
# X00123156 Robert Fitzgerald


# Read in data from the csv dataset
health <- read.table(file = "https://drive.google.com/uc?export=download&id=12ndSWZTA-2YsxWEqZMHgBV6sitAh1ktc", header=TRUE, sep =",")

#For each predictor variable, where appropriate,find the following

# Get the summary of the entire health dataset
summary(health)

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
Mode(health$ca)
Mode(health$thal)
Mode(health$class)

#Calculate the (median) standard deviations for each relevant field
sd(health$age)
sd(health$trestbps)
sd(health$cholesterol, na.rm = TRUE)
sd(health$diastbpexerc)
sd(health$thalach)
sd(health$oldpeak)
sd(health$ca)

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
Skewness(health$ca)

# Plots histogram with target variable overlay
PlotHistogram <- function(myData, labelIn) {
  ggplot(health, aes(x = myData, fill = health$class)) + 
  geom_histogram(colour = "black", position = "fill") + xlab(labelIn)
}

PlotBar <- function(myData, labelIn) {
  ggplot(health, aes(x = myData, fill = health$class)) +
  geom_bar(position = "fill") + xlab(labelIn)
}

PlotHistogram(health$age, "Age")
PlotHistogram(health$trestbps, "Resting BP")
PlotHistogram(health$cholesterol, "Cholesterol")
PlotHistogram(health$diastbpexerc, "Diastolic exercising blood pressure")
PlotHistogram(health$thalach, "Maximum heart rate achieved")
PlotHistogram(health$oldpeak, "ST depression induced by exercise relative to rest")
PlotHistogram(health$ca, "Number of coloured vessels")

PlotBar(health$sex, "Sex")
PlotBar(health$cp, "Cp")
PlotBar(health$Fasting.blood.sugar...120, "Blood Sugar")
PlotBar(health$restecg, "Restecg")
PlotBar(health$exang, "Exang")
PlotBar(health$slope, "Slope")
PlotBar(health$thal, "Thal")