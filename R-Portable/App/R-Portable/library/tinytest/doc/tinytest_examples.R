### R code from vignette source 'tinytest_examples.Rnw'

###################################################
### code chunk number 1: tinytest_examples.Rnw:76-81
###################################################
options(prompt="  ",
        continue = "  ",
        width=75,
        tt.pr.color=FALSE)
library(tinytest)


###################################################
### code chunk number 2: tinytest_examples.Rnw:96-97 (eval = FALSE)
###################################################
## ## [this is an extra comment, only for this vignette] 


###################################################
### code chunk number 3: tinytest_examples.Rnw:119-122
###################################################
options(prompt="R>  ",
        continue = "    ",
        width=75)


###################################################
### code chunk number 4: tinytest_examples.Rnw:124-126
###################################################
expect_equal(1,1)
expect_equal(1, c(x=1))


###################################################
### code chunk number 5: tinytest_examples.Rnw:129-132
###################################################
0.9-0.7-0.2
expect_equal(0.9-0.7-0.2,0) 
expect_equal(0.9-0.7-0.2,0, tolerance=0)


###################################################
### code chunk number 6: tinytest_examples.Rnw:134-137
###################################################
options(prompt="  ",
        continue = "  ",
        width=75)


###################################################
### code chunk number 7: tinytest_examples.Rnw:146-147 (eval = FALSE)
###################################################
## expect_equal(stringdist("ab", "ba", method="osa"), 1)


###################################################
### code chunk number 8: tinytest_examples.Rnw:152-162 (eval = FALSE)
###################################################
## b <- benchr::benchmark(1 + 1, 2 + 2)
## m <- mean(b)
## 
## expect_equal(class(m), c("summaryBenchmark", "data.frame"))
## expect_equal(dim(m), c(2L, 7L))
## expect_equal(names(m), c("expr", "n.eval", "mean", "trimmed", "lw.ci", "up.ci", "relative"))
## 
## expect_equal(class(m$expr), "factor")
## expect_equal(levels(m$expr), c("1 + 1", "2 + 2"))
## expect_true(all(sapply(m[-1], is.numeric)))


###################################################
### code chunk number 9: tinytest_examples.Rnw:171-174
###################################################
options(prompt="R>  ",
        continue = "    ",
        width=75)


###################################################
### code chunk number 10: tinytest_examples.Rnw:176-178
###################################################
expect_equivalent(1,1)
expect_equivalent(1, c(x=1))


###################################################
### code chunk number 11: tinytest_examples.Rnw:180-183
###################################################
options(prompt="  ",
        continue = "  ",
        width=75)


###################################################
### code chunk number 12: tinytest_examples.Rnw:192-195 (eval = FALSE)
###################################################
## v <- validator(x > 0)
## d <- data.frame(x=c(1,-1,NA))
## expect_equivalent(values(confront(d,v)), matrix(c(TRUE,FALSE,NA)) )


###################################################
### code chunk number 13: tinytest_examples.Rnw:202-205 (eval = FALSE)
###################################################
## refD <- as.Date("2016-01-01")+0:2
## 
## expect_equivalent(refD, anydate(20160101L + 0:2))


###################################################
### code chunk number 14: tinytest_examples.Rnw:216-219
###################################################
options(prompt="R>  ",
        continue = "    ",
        width=75)


###################################################
### code chunk number 15: tinytest_examples.Rnw:221-229
###################################################
La <- list(x=1); 
Lb <- list(x=1)
expect_identical(La, Lb)
a <- new.env()
a$x <- 1
b <- new.env()
b$x <- 1
expect_identical(a,b)


###################################################
### code chunk number 16: tinytest_examples.Rnw:235-237
###################################################
print(a)
print(b)


###################################################
### code chunk number 17: tinytest_examples.Rnw:242-245
###################################################
options(prompt="  ",
        continue = "  ",
        width=75)


###################################################
### code chunk number 18: tinytest_examples.Rnw:253-256 (eval = FALSE)
###################################################
##   a <- c(k1 = "aap",k2="noot")
##   expect_identical(stringdistmatrix(a,useNames="none")
##                  , stringdistmatrix(a,useNames=FALSE))


###################################################
### code chunk number 19: tinytest_examples.Rnw:263-271 (eval = FALSE)
###################################################
## list(
## ## [long list of results removed for brevity]
## ) -> results
## fils <- list.files(system.file("extdat", package="wand"), full.names=TRUE)
## tst <- lapply(fils, get_content_type)
## names(tst) <- basename(fils)
## 
## for(n in names(tst)) expect_identical(results[[n]], tst[[n]])


###################################################
### code chunk number 20: tinytest_examples.Rnw:280-283
###################################################
options(prompt="R>  ",
        continue = "    ",
        width=75)


###################################################
### code chunk number 21: tinytest_examples.Rnw:285-287
###################################################
expect_null(iris$hihi)
expect_null(iris$Species)


###################################################
### code chunk number 22: tinytest_examples.Rnw:289-292
###################################################
options(prompt="  ",
        continue = "  ",
        width=75)


###################################################
### code chunk number 23: tinytest_examples.Rnw:302-305
###################################################
options(prompt="R>  ",
        continue = "    ",
        width=75)


###################################################
### code chunk number 24: tinytest_examples.Rnw:307-309
###################################################
expect_true(1 == 1)
expect_false(1 == 2)


###################################################
### code chunk number 25: tinytest_examples.Rnw:311-314
###################################################
options(prompt="  ",
        continue = "  ",
        width=75)


###################################################
### code chunk number 26: tinytest_examples.Rnw:320-326 (eval = FALSE)
###################################################
## ## Datetime: factor and ordered (#44)
## refD <- as.Date("2016-09-01")
## expect_true(refD == anydate(as.factor("2016-09-01")))
## expect_true(refD == anydate(as.ordered("2016-09-01")))
## expect_true(refD == utcdate(as.factor("2016-09-01")))
## expect_true(refD == utcdate(as.ordered("2016-09-01")))


###################################################
### code chunk number 27: tinytest_examples.Rnw:332-333 (eval = FALSE)
###################################################
## expect_equal(anydate(as.factor("2016-09-01")), refD)


###################################################
### code chunk number 28: tinytest_examples.Rnw:340-342 (eval = FALSE)
###################################################
## x <- ULIDgenerate(20)
## expect_true(is.character(x))


###################################################
### code chunk number 29: tinytest_examples.Rnw:352-355
###################################################
options(prompt="R>  ",
        continue = "    ",
        width=75)


###################################################
### code chunk number 30: tinytest_examples.Rnw:357-361
###################################################
expect_message(message("hihi"))
expect_message(message("hihi"), pattern = "hi")
expect_message(message("hihi"), pattern= "ha")
expect_message(print("hihi"))


###################################################
### code chunk number 31: tinytest_examples.Rnw:363-366
###################################################
options(prompt="  ",
        continue = "  ",
        width=75)


###################################################
### code chunk number 32: tinytest_examples.Rnw:375-378
###################################################
options(prompt="R>  ",
        continue = "    ",
        width=75)


###################################################
### code chunk number 33: tinytest_examples.Rnw:380-384
###################################################
expect_warning(warning("hihi"))
expect_warning(warning("hihi"), pattern = "hi")
expect_warning(warning("hihi"), pattern= "ha")
expect_warning(1+1)


###################################################
### code chunk number 34: tinytest_examples.Rnw:386-389
###################################################
options(prompt="  ",
        continue = "  ",
        width=75)


###################################################
### code chunk number 35: tinytest_examples.Rnw:399-402
###################################################
options(prompt="R>  ",
        continue = "    ",
        width=75)


###################################################
### code chunk number 36: tinytest_examples.Rnw:404-408
###################################################
expect_error(stop("hihi"))
expect_error(stop("hihi"), pattern = "hi")
expect_error(stop("hihi"), pattern= "ha")
expect_error(print("hoho"))


###################################################
### code chunk number 37: tinytest_examples.Rnw:410-413
###################################################
options(prompt="  ",
        continue = "  ",
        width=75)


###################################################
### code chunk number 38: tinytest_examples.Rnw:421-425 (eval = FALSE)
###################################################
## # Check that log and centering cannot be combined
## expect_error(
##   centscaleSpectra2D(tiny, center = TRUE, scale = "log"),
##   "Cannot take log of centered data")


###################################################
### code chunk number 39: tinytest_examples.Rnw:436-439
###################################################
options(prompt="R>  ",
        continue = "    ",
        width=75)


###################################################
### code chunk number 40: tinytest_examples.Rnw:441-443
###################################################
expect_silent(print(10))
expect_silent(stop("haha"))


###################################################
### code chunk number 41: tinytest_examples.Rnw:445-448
###################################################
options(prompt="  ",
        continue = "  ",
        width=75)


###################################################
### code chunk number 42: tinytest_examples.Rnw:456-461 (eval = FALSE)
###################################################
## data <- data.frame(A = 1)
## rule <- validator(A > 0)
## cf <- confront(data, rule)
## expect_silent(plot(rule))
## expect_silent(plot(cf))


###################################################
### code chunk number 43: tinytest_examples.Rnw:469-473 (eval = FALSE)
###################################################
## run("runs/multiple_loggers.R")
## simple_ok <- expect_true(file.exists("runs/simple_log.csv"))
## expect_silent(read.csv("runs/simple_log.csv"))
## if (simple_ok) unlink("runs/simple_log.csv")


###################################################
### code chunk number 44: tinytest_examples.Rnw:488-489 (eval = FALSE)
###################################################
## ignore(expect_equal)(1+1, 2)


###################################################
### code chunk number 45: tinytest_examples.Rnw:495-509 (eval = FALSE)
###################################################
## mantissa <- gsub(" [0-9]*$", "", x.hex)
## ignore(expect_true)(all(
##     sapply(
##         head(seq_along(mantissa), -1),
##         function(i){
##             all(
##                 grepl(
##                     paste0("^", mantissa[i], ".*"),
##                     tail(mantissa, -i)
##                 )
##             )
##         }
##     )
## ))


