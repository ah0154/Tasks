library (swirl)
swirl ()
Austin Holtzapfel
1
1
1
1
5+7
x<-5+7
x
y<-x-3
y
z<-c(1.1, 9, 3.14)
?c
z
c(z, 555, z)
z*2+100
my_sqrt<-sqrt(z-1)
2
my_sqrt
my_div<-z/my_sqrt
1
my_div
c(1, 2, 3, 4)+c(0, 10)
c(1, 2, 3, 4)+c(0, 10, 100)
z*2+1000
my_div
1
2
library (swirl)
swirl ()
Austin Holtzapfel
1
2
getwd()
ls()
x <- 9
ls()
dir()
?list.files
args(list.files)
old.dir<- getwd()
dir.create("testdir")
setwd("testdir")
ls()
list.files()
file.exists("mytest.R")
file.info("mytest.R")
file.rename("mytest.R", "mytest2.R")
file.copy("mytest2.R", "mytest3.R")
file.path("mytest3.R")
?dir.create
dir.create(file.path('testdir2', 'testdir3'), recursive = TRUE)
setwd(old.dir)
2
1
3
1:20
pi:10
15:1
?':'
seq(1, 20)
seq(0, 10, by=0.5)
length(my_seq)
1:length(my_seq)
seq_along(my_seq)
rep(0, times = 40)
rep(c(0, 1, 2), times = 10)
rep(c(0, 1, 2), each = 10)
1
5
x<- c(44, NA, 5, NA)
x*3
y <- rnorm(1000)
z <- rep(NA, 1000)
my_data <- sample(c(y, z), 100)
my_na <- is.na(my_data)
my_na
my_data == NA
sum(my_na)
my_data
0/0
Inf-Inf
1
6
x
x[1:10]
x[is.na(x)]
y <- x[!is.na(x)]
y
x[x>0]
x[!is.na(x) & x> 0]
x[c(3, 5, 7)]
x[0]
x[3000]
x[c(-2, -10)]
x[-c(2, 10)]
vect <- c(foo =11, bar = 2, norf = NA)
vect
names(vect)
vect2 <- c(11, 2, NA)
names(vect2) <- c("foo", "bar", "norf")
identical(vect, vect2)
vect[c("foo", "bar")]
2
1
7
my_vector <- 1:20
my_vector
dim(my_vector)
length(my_vector)
dim(my_vector) <- c(4, 5)
dim(my_vector)
attributes(my_vector)
my_vector
class(my_vector)
my_matrix<-my_vector
?matrix
my_matrix2 <- matrix(1:20, nrow = 4, ncol = 5, byrow = FALSE)
identical(my_matrix, my_matrix2)
patients <- c("Bill", "Gina", "Kelly", "Sean")
cbind(patients, my_matrix)
my_data <- data.frame(patients, my_matrix)
my_data
class(my_data)
cnames <- c("patient", "age", "weight", "bp", "rating","test")
colnames(my_data) <- cnames
my_data
1
8
TRUE == TRUE
6 == 7
6<7
10<=10
3
4
5!=7
!5==7
2
1
FALSE & FALSE
TRUE & c(TRUE, FALSE, FALSE)
TRUE && c(TRUE, FALSE, FALSE)
TRUE | c(TRUE, FALSE, FALSE)
TRUE || c(TRUE, FALSE, FALSE)
5>8||6!=8&&4>3.9
1
1
isTRUE(6>4)
1
identical('twins', 'twins')
3
xor(5==6, !FALSE)
ints <- sample(10)
ints
ints>5
which(ints>7)
2
any(ints<0)
all(ints>0)
3
1
9
Sys.Date()
mean(c(2, 4, 5))
submit()
boring_function('My first function!')
boring_function
submit()
my_mean(c(4, 5, 10))
submit()
remainder(5)
remainder(11,5)
remainder(divisor=11, num=5)
remainder(4, div=2)
args(remainder)
submit()
evaluate(sd, c(1.4, 3.6, 7.9, 8.8))
evaluate(function(x){x+1}, 6)
evaluate(function(x){x[1]}, c(8, 4, 0))
evaluate(function(x){x[length(x)]}, c(8, 4, 0))
?paste
paste("Programming", "is", "fun!")
submit()
telegram("Good", "morning")
submit()
mad_libs(place = "London", adjustive = "beautiful", noun ="dog")
submit()
"I" %p% "love" %p% "R!"
1
15
data(cars)
?cars
head(cars)
plot(cars)
?plot
plot(x = cars$speed, y = cars$dist)
plot(x = cars$dist, y = cars$speed)
plot(x = cars$speed, y = cars$dist, xlab = "Speed")
plot(x = cars$speed, y = cars$dist, ylab = "Stopping Distance")
plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance")
plot(cars, main = "My Plot")
plot(cars, sub = "My Plot Subtitle")
plot(cars, col = 2)
plot(cars, xlim = c(10, 15))
plot(cars, pch = 2)
data(mtcars)
?boxplot
boxplot(mpg ~ cyl, data = mtcars)
hist(mtcars$mpg)
1
2