setwd("/home/alice/Desktop/")

test<-read.table("rtest_table.csv", sep=",")
#edit(test)

test <- replace(test, 2, "x")
test

test[2:3,4:5][test[2:3,4:5]=="6"]<-"b"
test

test[1:4,1:5][test[1:4,1:5]==(is.character==TRUE)]<-"char"




#replace(test, 2, "swap")




R version 3.1.1 (2014-07-10) -- "Sock it to Me"
Copyright (C) 2014 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> a <- c(1,2,"A")
> a
[1] "1" "2" "A"
> grep("[[:digit:]]", a)
[1] 1 2
> grepl("[[:digit:]]", a)
[1]  TRUE  TRUE FALSE
> a[grepl("[[:digit:]]", a)]
[1] "1" "2"
> a[grepl("[[:digit:]]", a)] <- "x"
> a
[1] "x" "x" "A"
> a <- c(1,2,"A")
> b <- c("B", 3, "C")
> t <- data.frame(A = a, B = b)
> t
A B
1 1 B
2 2 3
3 A C
> t[grepl("[[:digit:]]", a)]
A B
1 1 B
2 2 3
3 A C
> t[grepl("[[:digit:]]", t)]
A B
1 1 B
2 2 3
3 A C
> grepl("[[:digit:]]", t)
[1] TRUE TRUE
> t
A B
1 1 B
2 2 3
3 A C
> str(t)
'data.frame':  3 obs. of  2 variables:
  $ A: Factor w/ 3 levels "1","2","A": 1 2 3
$ B: Factor w/ 3 levels "3","B","C": 2 1 3
> t$A <- as.character(t$A)
> t$B <- as.character(t$B)
> str(t)
'data.frame':	3 obs. of  2 variables:
  $ A: chr  "1" "2" "A"
$ B: chr  "B" "3" "C"
> grepl("[[:digit:]]", t)
[1] TRUE TRUE
> grepl("[[:digit:]]", t[2:3,1:2])
[1] TRUE TRUE
> t[2:3,1:2][grepl("[[:digit:]]", t[2:3,1:2])]
A B
2 2 3
3 A C
> t[2:3,1:2]
A B
2 2 3
3 A C
> grepl("[[:digit:]]", t)
[1] TRUE TRUE
> grep("[[:digit:]]", t)
[1] 1 2
> regexpr("[[:digit:]]", t)
[1] 4 9
attr(,"match.length")
[1] 1 1
attr(,"useBytes")
[1] TRUE
> t[regexpr("[[:digit:]]", t)]
Error in `[.data.frame`(t, regexpr("[[:digit:]]", t)) : 
  undefined columns selected
> regexpr("[[:digit:]]", t)
[1] 4 9
attr(,"match.length")
[1] 1 1
attr(,"useBytes")
[1] TRUE
> t
A B
1 1 B
2 2 3
3 A C
> ?regexpr
> ?regexpr
> regexpr("[[:digit:]]", t)
[1] 4 9
attr(,"match.length")
[1] 1 1
attr(,"useBytes")
[1] TRUE
> regexpr("[[:digit:]]", t$A)
[1]  1  1 -1
attr(,"match.length")
[1]  1  1 -1
attr(,"useBytes")
[1] TRUE
> grepl("[[:digit:]]", t$A)
[1]  TRUE  TRUE FALSE
> t$A[grepl("[[:digit:]]", t$A)]
[1] "1" "2"
> grepl("[[:digit:]]", t[,2:3])
Error in `[.data.frame`(t, , 2:3) : undefined columns selected
> grepl("[[:digit:]]", t[,1:2])
[1] TRUE TRUE
> grepl("[[:digit:]]", t[1,1:2])
[1]  TRUE FALSE
> grepl("[[:digit:]]", t[2,1:2])
[1] TRUE TRUE
> grepl("[[:digit:]]", t[,1:2])
[1] TRUE TRUE
> grepl("[[:digit:]]", t[,1])
[1]  TRUE  TRUE FALSE
> (grepl("[[:digit:]]", t[,1]) & t[,1] != 0)
[1]  TRUE  TRUE FALSE
> (grepl("[[:digit:]]", t[,1]) & t[,1] != 2)
[1]  TRUE FALSE FALSE
> t[,1](grepl("[[:digit:]]", t[,1]) & t[,1] != 2)
Error: attempt to apply non-function
> t[,1][(grepl("[[:digit:]]", t[,1]) & t[,1] != 2)]
[1] "1"
> t[,1][(grepl("[[:digit:]]", t[,1]) & t[,1] != 1)]
[1] "2"
> t[,1][(grepl("[[:digit:]]", t[,1]) & t[,1] != 0)]
[1] "1" "2"
> t[1:3,1][(grepl("[[:digit:]]", t[1:3,1]) & t[1:3,1] != 0)]
[1] "1" "2"
> t[1:3,1][(grepl("[[:digit:]]", t[1:3,1]) & t[1:3,1] != 0)]
[1] "1" "2"
> t[1:3,1][(grepl("[[:digit:]]", t[1:3,1]) & t[1:3,1] != 0)]
[1] "1" "2"
> t[1:3,][(grepl("[[:digit:]]", t[1:3,]) & t[1:3,] != 0)]
[1] "1" "2" "A" "B" "3" "C"
> t[1:3,1][(grepl("[[:digit:]]", t[1:3,1]) & t[1:3,1] != 0)]
[1] "1" "2"
> grepl("[[:digit:]]", t[1:3,1]) & t[1:3,1] != 0
[1]  TRUE  TRUE FALSE
> (regexpr("[[:digit:]]", t[1:3,1]) & t[1:3,1] != 0)
[1] TRUE TRUE TRUE
> (regexpr("[[:digit:]]", t[1:3,1]) & t[1:3,1] != 0)
[1] TRUE TRUE TRUE
> (regexpr("[[:digit:]]", t[1:3,]) & t[1:3,] != 0)
A    B
1 TRUE TRUE
2 TRUE TRUE
3 TRUE TRUE
> t[(regexpr("[[:digit:]]", t[1:3,]) & t[1:3,] != 0)]
[1] "1" "2" "A" "B" "3" "C"
> regexpr("[[:digit:]]", t[1:3,]) & t[1:3,] != 0)
Error: unexpected ')' in "regexpr("[[:digit:]]", t[1:3,]) & t[1:3,] != 0)"
> (regexpr("[[:digit:]]", t[1:3,]) & t[1:3,] != 0)
A    B
1 TRUE TRUE
2 TRUE TRUE
3 TRUE TRUE
> (regexec("[[:digit:]]", t[1:3,]) & t[1:3,] != 0)
Error in regexec("[[:digit:]]", t[1:3, ]) : invalid 'text' argument
> for(i in seq(22, 125)){
  +   t[1:3,i][(grepl("[[:digit:]]", t[1:3,i]) & t[1:3,i] != 0)]  
  + }
> for(i in seq(22, 125)){
  +   t[1:3,i][(grepl("[[:digit:]]", t[1:3,i]) & t[1:3,i] != 2)] <- "x"
  + }
Show Traceback

Rerun with Debug
Error in `[<-.data.frame`(`*tmp*`, 1:3, i, value = character(0)) : 
  new columns would leave holes after existing columns > t
A B
1 1 B
2 2 3
3 A C
> for(i in seq(1, 2)){
  +   t[1:3,i][(grepl("[[:digit:]]", t[1:3,i]) & t[1:3,i] != 2)] <- "x"
  + }
> t
A B
1 x B
2 2 x
3 A C