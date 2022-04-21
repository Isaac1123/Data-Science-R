#######Chapter 6.7#######

#import the essential data set that we will use in this exercise
library(rpart)
print(iris)
iris <- as.data.frame(iris)
#Exercise 3
#a)
iris$Group <- gsub("setosa","0", iris$Species)
iris$Group <- gsub("versicolor", "0", iris$Group)
iris$Group <- gsub("virginica", "1", iris$Group)
#print(iris$Group)
#Output:
#[1] "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"
#[30] "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"
#[59] "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"
#[88] "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1"
#[117] "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1" "1"
#[146] "1" "1" "1" "1" "1"

#b)
#using sepal.length, sepal.width, petal.length, petal.width as an independent variable and 
#predict the iris data that which is virginica(1) 

iris$Group <- as.factor(iris$Group)
iris_logit <- glm(Group ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
                  data= iris, family= "binomial")
summary(iris_logit)

#Output:
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-2.01105  -0.00065   0.00000   0.00048   1.78065  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)   -42.638     25.708  -1.659   0.0972 .
#Sepal.Length   -2.465      2.394  -1.030   0.3032  
#Sepal.Width    -6.681      4.480  -1.491   0.1359  
#Petal.Length    9.429      4.737   1.990   0.0465 *
#Petal.Width    18.286      9.743   1.877   0.0605 .
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
#(Dispersion parameter for binomial family taken to be 1)
  
#Null deviance: 190.954  on 149  degrees of freedom
#Residual deviance:  11.899  on 145  degrees of freedom
#AIC: 21.899
  
#Number of Fisher Scoring iterations: 12

iris_prob <- round(predict(iris_logit, iris, type= "response"),2)  
iris_prob
#Output
#1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24 
#0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 
#25   26   27   28   29   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48 
#0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 
#49   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64   65   66   67   68   69   70   71   72 
#0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.06 0.00 0.40 0.00 
#73   74   75   76   77   78   79   80   81   82   83   84   85   86   87   88   89   90   91   92   93   94   95   96 
#0.22 0.00 0.00 0.00 0.00 0.28 0.00 0.00 0.00 0.00 0.00 0.87 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 
#97   98   99  100  101  102  103  104  105  106  107  108  109  110  111  112  113  114  115  116  117  118  119  120 
#0.00 0.00 0.00 0.00 1.00 1.00 1.00 1.00 1.00 1.00 0.89 1.00 1.00 1.00 0.99 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.92 
#121  122  123  124  125  126  127  128  129  130  131  132  133  134  135  136  137  138  139  140  141  142  143  144 
#1.00 1.00 1.00 0.95 1.00 1.00 0.82 0.80 1.00 0.97 1.00 1.00 1.00 0.20 0.97 1.00 1.00 1.00 0.67 1.00 1.00 1.00 1.00 1.00 
#145  146  147  148  149  150 
#1.00 1.00 1.00 1.00 1.00 0.98 
 

#c)
#Sepal.Length=9, Sepal.Width=5, Petal.Length=10, Petal.Width=7

#P(1)= 1/(1+exp-(-42.638-2.465*9-6.681*5+9.429*10+18.286*7))= 1
#Because the probability is closer to 1 which is classify Virginica in our dataset

#Exercise 4
#import the kyphosis data set which already in environment to complete the exercise
#a)
#convert "absent" and "present" to numeric 0,1 respectively, which in kyphosis column 
kyphosis <- as.data.frame(kyphosis)
kyphosis$Kyphosis <- as.numeric(gsub("absent","0",
                                gsub("present","1",kyphosis$Kyphosis)))
#print(kyphosis$Kyphosis)
#Output:
#[1] 0 0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0 0 0 1 0 0 1 0 0 0 1 0 0 0 0 1
#[59] 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0

#b)
#using Age, number,start as an independent variable and 
#predict the kyphosis that determine which is present(1) 

kyphosis_logit <- glm(Kyphosis ~ Age+Number+Start,
                      data= kyphosis, family= "binomial")
summary(kyphosis_logit)
#Output:
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.3124  -0.5484  -0.3632  -0.1659   2.1613  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept) -2.036934   1.449575  -1.405  0.15996   
#Age          0.010930   0.006446   1.696  0.08996 . 
#Number       0.410601   0.224861   1.826  0.06785 . 
#Start       -0.206510   0.067699  -3.050  0.00229 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 83.234  on 80  degrees of freedom
#Residual deviance: 61.380  on 77  degrees of freedom
#AIC: 69.38

#Number of Fisher Scoring iterations: 5

kyphosis_prob <- round(predict(kyphosis_logit, kyphosis, type= "response"),2)  
kyphosis_prob
#Output:
#1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24 
#0.26 0.12 0.49 0.46 0.03 0.01 0.02 0.02 0.04 0.20 0.12 0.08 0.45 0.05 0.06 0.02 0.14 0.32 0.08 0.12 0.01 0.63 0.10 0.40 
#25   26   27   28   29   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48 
#0.64 0.07 0.12 0.07 0.02 0.05 0.02 0.11 0.22 0.05 0.33 0.04 0.07 0.34 0.23 0.19 0.65 0.04 0.93 0.52 0.05 0.21 0.12 0.23 
#49   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64   65   66   67   68   69   70   71   72 
#0.47 0.10 0.21 0.01 0.91 0.01 0.12 0.09 0.03 0.42 0.39 0.09 0.69 0.61 0.57 0.06 0.08 0.09 0.07 0.21 0.08 0.04 0.24 0.18 
#73   74   75   76   77   78   79   80   81 
#0.06 0.45 0.02 0.18 0.15 0.17 0.07 0.51 0.06 

#In this case only variable-"Age" is significant around 0.01, other predictors like number and start are both
#over 0.05 that we should remove from our model to avoid overfitting

#c)
#Age=50, Number=5, start=10

#P(1)= 1/(1+exp-(-2.04+0.01*50+0.41*5-0.21*10))= 0.26
#Because the probability is <0.5 we can assume that the given observation is more likely to be negative
#since we know "positive"(1) and "negative"(0)

#Exercise 5
#independent variable -- Sepal.Width
iris_reg1 <- lm(Sepal.Length ~ Sepal.Width, data=iris)
summary(iris_reg1)
#Output:
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.5561 -0.6333 -0.1120  0.5579  2.2226 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   6.5262     0.4789   13.63   <2e-16 ***
#  Sepal.Width  -0.2234     0.1551   -1.44    0.152    
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.8251 on 148 degrees of freedom
#Multiple R-squared:  0.01382,	Adjusted R-squared:  0.007159 
#F-statistic: 2.074 on 1 and 148 DF,  p-value: 0.1519

sw_plot <- plot(x=iris$Sepal.Length,	y=iris$Sepal.Width, 
     type="p")

#independent variable -- Petal.Length
iris_reg2 <- lm(Sepal.Length ~ Petal.Length, data=iris)
summary(iris_reg2)
#Output:
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.24675 -0.29657 -0.01515  0.27676  1.00269 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.30660    0.07839   54.94   <2e-16 ***
#  Petal.Length  0.40892    0.01889   21.65   <2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4071 on 148 degrees of freedom
#Multiple R-squared:   0.76,	Adjusted R-squared:  0.7583 
#F-statistic: 468.6 on 1 and 148 DF,  p-value: < 2.2e-16

pl_plot <- plot(x=iris$Sepal.Length,	y=iris$Petal.Length,
     type="p")

#independent variable -- Petal.Width
iris_reg3 <- lm(Sepal.Length ~ Petal.Width, data=iris)
summary(iris_reg3)
#Output:
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.38822 -0.29358 -0.04393  0.26429  1.34521 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4.77763    0.07293   65.51   <2e-16 ***
#  Petal.Width  0.88858    0.05137   17.30   <2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.478 on 148 degrees of freedom
#Multiple R-squared:  0.669,	Adjusted R-squared:  0.6668 
#F-statistic: 299.2 on 1 and 148 DF,  p-value: < 2.2e-16

pw_plot <- plot(x=iris$Sepal.Length,	y=iris$Petal.Width, 
     type="p")

#Through the plot function we can visualize the regression on the x_axis and y_axis
#Although it's hard to say that neither of them has the perfect linear (homoscedastic)
#we can still rank the level of correlation between each model: we fix the x-axis with Sepal.Length
#therefore "pl_plot" > pw_plot > sw_plot

