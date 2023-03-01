# # Installation of packages
# install.packages("caTools")
library(caTools)

# Data set source:
# https://www.kaggle.com/datasets/dipam7/student-grade-prediction
student=read.csv(file.choose(),header=T)

# Identifying Dependent Variable
# As there are 3 grades variable on the data set, we will aggregate them and create a new variable "meangrade"
student$meangrade=(student$G1+student$G2+student$G3)/3
# Remove G1,G2,G3 as they are used to calculate mean grade
student[,c("G1","G2","G3")] = NULL
head(student)

# Plot a Histogram distribution of mean grades
hist(student$meangrade, xlab = "Students' Mean Grade", ylab = "Number of Students", main = "Distribution of Students' Mean Grades")

# Check summary and types of variables
summary(student)
categorical = sapply(student,is.character)
head(student[categorical])


# Convert categorical variables to numeric
student[ ,categorical] = lapply(student[ ,categorical],function (x) as.numeric(factor(x)))
head(student)

# Calculate the correlation of independent variables against mean grade
corr = round(cor(student,student$meangrade),2)
# Order by magnitude of correlation
ordered_corr = corr[order(abs(corr[,1]),decreasing = TRUE),]

# Select top 10 features by correlation (excluding meangrade)
top_corr = ordered_corr[2:11] 
top_corr
names(top_corr)

# Remove all other columns, leaving only the top 10 correlated variables
student = student[c(names(top_corr),"meangrade")]
head(student)

# Splitting the dataset into 80% train, 20% test
student_split = sample.split(student$meangrade,SplitRatio = 0.8)
train = subset(student, student_split == TRUE)
test = subset(student, student_split == FALSE)

# Fit the linear regression model with the test set
linreg = lm(meangrade~failures+Medu+higher+Fedu+goout+schoolsup+age+traveltime+studytime+reason, data = train)
summary(linreg)

# Predict results of the test set using our trained model
predicted_grade = predict(linreg,test)
# Adding predicted grades to a new column in test set
test$predicted = predicted_grade
View(test)

# Plot Actual and Predicted Grades 
plot(test$meangrade,test$predicted,xlab = "Actual Grade", ylab = "Predicted Grade", main = "Predicted Grade vs Actual Grade (Linear Regression)")
# Plot a best fit line
abline(lm(test$predicted~test$meangrade))
