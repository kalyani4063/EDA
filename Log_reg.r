install.packages('ggplot2')
library(ggplot2)
admissionData <- read.csv('admissions.csv')
str(admissionData)
table(admissionData)
dim(admissionData)
ggplot(admissionData, aes(x=gpa, y=admitted))+
Name: Kalyani S Roll no: 22CSEB11 Page no:
 labs(x = "GPA", y = "Admission Status (0 = Not Admitted, 1 = Admitted)") +
 geom_jitter(color=6)#where n is the notion for purple color
# 2.
lrModel <- glm(admitted ~ gpa , data=admissionData, family = binomial)#for family =
gausian, quasi,poison,gamma etc
#logit= log(myu/1-myu) converts probability to -infinity to +infinity "this is for
modeling purpose "
summary(lrModel)
# 3.
admissionData$predictedProbablity <- predict(lrModel, type="response")
ggplot(admissionData, aes(x=gpa, y=admitted))+
 labs(x = "GPA", y = "Admission Status (0 = Not Admitted, 1 = Admitted)") +
 geom_jitter(color=3)+
 geom_line(aes(y = predictedProbablity), color = "blue", linewidth = 1)
# 4.
gpa_summary <- aggregate(admitted ~ gpa, data = admissionData,
 FUN = function(x) c(num_admitted = sum(x),
 prop_admitted = mean(x),
 num_students = length(x)))
print(gpa_summary)
gpa_summary <- do.call(data.frame, gpa_summary)
ggplot(gpa_summary, aes(x = gpa, y = admitted.prop_admitted)) +
 geom_point(color = "blue", size = 3) + # Scatter plot points
 labs(x = "GPA", y = "Proportion of Admitted Students") +
 theme_minimal() +
 ggtitle("Relationship Between GPA and Proportion of Admitted Students")
Name: Kalyani S Roll no: 22CSEB11 Page no:
# 6.
weighted_lr_model <- glm(admitted.prop_admitted ~ gpa,
 family = binomial,
 weights = admitted.num_students,
 data = gpa_summary)
summary(weighted_lr_model)
# 7.
ggplot(gpa_summary, aes(x = gpa, y = admitted.prop_admitted)) +
 geom_point(aes(size = admitted.num_students), color = "darkblue", alpha = 0.6) +
 scale_size_continuous(range = c(2, 10)) +
 labs(x = "GPA", y = "Proportion of Admitted Students",
 size = "Number of Students") +
 theme_minimal() +
 geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE,
color = "red", linewidth = 0.8) +
 ggtitle("Weighted Scatter Plot with Logistic Regression Curve")
