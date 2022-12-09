
# Karolis Koncevičius
# A Collection of Simple R Shortcuts
# https://tinyurl.com/36u9uvu2

For displaying the result after an assignment, instead of printing the variable explicitly on a new line:

x <- 1:10
x
## [1]  1  2  3  4  5  6  7  8  9 10

The assignment can be surrounded with parenthesis:

(x <- 1:10)
## [1]  1  2  3  4  5  6  7  8  9 10

multiple assignments

For initiating empty variables, instead of assigning them one by one:

x <- numeric(10)
y <- numeric(10)
z <- numeric(10)

x
## [1] 0 0 0 0 0 0 0 0 0 0

They all can be assigned in a single line:

x <- y <- z <- numeric(10)

x
## [1] 0 0 0 0 0 0 0 0 0 0

assignment from an if statement

For assigning a value based on logical condition, instead of making an assignment within an if statement:

if(rnorm(1) > 0) {
  result <- "positive"
} else {
  result <- "negative"
}

The statement itself can be assigned to a variable directly:

result <- if(rnorm(1) > 0) "positive" else "negative"

reversing a vector

For reversing the direction of elements within a vector, instead of selecting the elements in decreasing order:

x <- 1:10

x[length(x):1]
## [1] 10  9  8  7  6  5  4  3  2  1

A short rev() function can be used:

x <- 1:10

rev(x)
## [1] 10  9  8  7  6  5  4  3  2  1

selecting the last element

To select the last element of a vector, instead of specifying the index of the last element:

rivers[length(rivers)]
## [1] 1770

A tail() function can be used:

tail(rivers, 1)
## [1] 1770

elements in a nested list

For selecting elements from a nested list, instead of stacking brackets:

a <- list(3, list(8, list(0, 5)))

a[[2]][[2]][[1]]
## 0

A single vector of indices can be used:

a <- list(3, list(8, list(0, 5)))

a[[c(2,2,1)]]
## 0

getting length of each element in a list

For obtaining lengths of each element inside a list, instead of calling length() on each element:

xs <- list(1:10, 2, 3:4, letters, rivers)

sapply(xs, length)
## [1]  10   1   2  26 141

The lengths can be obtained by using a special function:

xs <- list(1:10, 2, 3:4, letters, rivers)

lengths(xs)
## [1]  10   1   2  26 141

checking if string starts with a specified prefix

For checking which strings in a vector start with a specified prefix, instead of using grepl():

x <- c("something", "wholesome", "somebody")

grepl("^some", x)
## [1]  TRUE FALSE  TRUE

A simpler startsWith() function can be used instead:

x <- c("something", "wholesome", "somebody")

startsWith(x, "some")
## [1]  TRUE FALSE  TRUE

And equivalent function exists for looking at suffixes:

x <- c("something", "wholesome", "somebody")

endsWith(x, "some")
## [1] FALSE  TRUE FALSE

repeating a string multiple times

For repeating extending a string by repeating it a specified number of times, instead of rep() and paste():

x <- c("ABABAB")

paste(rep(x, 3), collapse="")
## [1] "ABABABABABABABABAB"

A short dedicated function can be used:

strrep(x, 3)
## [1] "ABABABABABABABABAB"

changing value to NA via assignment

For turning vector elements to NA, instead of regular replacement:

x <- c(2, 5, 9, 0)

x[c(2,4)] <- NA
x
## [1]  2 NA  9 NA

A special assignment function can be used:

x <- c(2, 5, 9, 0)

is.na(x) <- c(2, 4)
x
## [1]  2 NA  9 NA

Or, alternatively, using names:

x <- c(a=2, b=5, c=9, d=0)

is.na(x) <- c("b", "d")
x
##  a  b  c  d
##  2 NA  9 NA

repeating random computations multiple times

For repeating expressions multiple times, instead of running a for loop or using sapply():

means <- numeric(10)
for(i in 1:10) {
  means[i] <- mean(rnorm(20))
}

means
## [1] -0.10172177  0.54172359  0.25915355 -0.07303793 -0.17569846
## [6]  0.37802472 -0.39325875  0.07182746  0.04113407 -0.06892520


sapply(1:10, function(x) mean(rnorm(20)))
## [1] -0.10172177  0.54172359  0.25915355 -0.07303793 -0.17569846
## [6]  0.37802472 -0.39325875  0.07182746  0.04113407 -0.06892520

A dedicated replicate() function can be used:

replicate(10, mean(rnorm(20)))
## [1] -0.10172177  0.54172359  0.25915355 -0.07303793 -0.17569846
## [6]  0.37802472 -0.39325875  0.07182746  0.04113407 -0.06892520

generating factor levels

For creating factors with equal number of levels, instead of repeating strings:

as.factor(rep(c("Child", "Man", "Woman"), each=3))
## [1] Child Child Child Man   Man   Man   Woman Woman Woman
## Levels: Child Man Woman

A special gl() function can be used:

gl(3, 3, labels = c("Child", "Man", "Woman"))
## [1] Child Child Child Man   Man   Man   Woman Woman Woman
## Levels: Child Man Woman

replacing values

For replacing values in a vector, instead of doing multiple assignments:

x <- c("a", "b", "a", "c", "a", "b", "b")

x[x=="a"] <- "apple"
x[x=="b"] <- "bananna"
x[x=="c"] <- "cherry"

x
## [1] "apple"   "bananna" "apple"   "cherry"  "apple"   "bananna" "bananna"

The match() function can be used:

x <- c("a", "b", "a", "c", "a", "b", "b")
n <- c(apple="a", bananna="b", cherry="c")

names(n)[match(x, n)]
## [1] "apple"   "bananna" "apple"   "cherry"  "apple"   "bananna" "bananna"

And a similar strategy can be used for replacing interval regions using findInterval():

x <- c(2, 16, 88, 15, 33, 1, 21)
n <- c(baby=0, toddler=1, child=3, teen=12, adult=18, senior=65)

names(n)[findInterval(x, n)]
## [1] "toddler" "teen"    "senior"  "teen"    "adult"   "toddler" "adult"

data sums and averages by group subtotal

For obtaining group-wise sums from a matrix or a data frame, instead of using aggregate():

aggregate(iris[,-5], list(iris[,5]), FUN=sum)
##      Group.1 Sepal.Length Sepal.Width Petal.Length Petal.Width
## 1     setosa        250.3       171.4         73.1        12.3
## 2 versicolor        296.8       138.5        213.0        66.3
## 3  virginica        329.4       148.7        277.6       101.3

A special rowsum() function can be used:

rowsum(iris[,-5], iris[,5])
##            Sepal.Length Sepal.Width Petal.Length Petal.Width
## setosa            250.3       171.4         73.1        12.3
## versicolor        296.8       138.5        213.0        66.3
## virginica         329.4       148.7        277.6       101.3

And for averages:

rowsum(iris[,-5], iris[,5]) / table(iris[,5])
##            Sepal.Length Sepal.Width Petal.Length Petal.Width
## setosa            5.006       3.428        1.462       0.246
## versicolor        5.936       2.770        4.260       1.326
## virginica         6.588       2.974        5.552       2.026

maximum value for each row in a matrix

For finding a column index holding the maximum value for each row, instead of using which.max():

apply(VADeaths, 1, which.max)
## 50-54 55-59 60-64 65-69 70-74
##     3     3     3     3     3

A special max.col() function can be used:

max.col(VADeaths)
## [1] 3 3 3 3 3

multi-line titles in base plots

For constructing multi-line titles in plots, instead of combining strings with paste():

plot(1:10, main=paste("Title", "sub", sep="\n"), xlab=paste("index", "x", sep="\n"))

Titles can be specified by passing a vector of strings:

plot(1:10, main=c("Title", "sub"), xlab=c("Index", "x"))

extending plotting region area

For increasing the range of the axes within a plot, instead of computing limits manually:

x <- 1:10
y <- rnorm(10)

plot(x, y, xlim=range(x) + diff(range(x)) * c(-0.2, 0.2))

A dedicated function can be used:

x <- 1:10
y <- rnorm(10)

plot(x, y, xlim=extendrange(x, f=0.2))

Thanks to /u/morphicphicus from /r/rstats reddit thread for a reminder about (x <- y) print shortcut.
2020-12-17 (last update 2020-12-30)
