# Enterprise Database Technologies
# X00119321 Jason Domican


# Read in data from the csv dataset
health <- read.table(file = "C:/Users/x00123156/Downloads/CardiologyRel.csv", header=TRUE, sep =",")


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

ggplot(health, aes(x = health$age, fill = health$class)) + geom_histogram(position = "fill")
