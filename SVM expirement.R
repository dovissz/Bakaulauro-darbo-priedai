start_svm <- function(){

	setwd("C:/Users/Dovydas/Desktop/data")
	duom <- read.table("rez.txt", header=TRUE, sep = " ", quote = "")
	library(e1071)
	klas <- duom[,8061]
	klas <- as.factor(klas)
	duom <- duom[,1:8060]

cost <- 0.1
gamma <- 0.1
epsilon <- 0.1
kernel <- "radial"

for (i in 1:50 ) {

	trainstart <- 1
	trainend <- 240
	teststart <- 241
	testend <- 360
	trainstart2 <- 0
	trainend2 <- 0
	teststart2 <- 0
	testend2 <- 0

	for (crosval in 1:10) {

		train <- duom[c(trainstart:trainend,trainstart2:trainend2),]
		test <- duom[c(teststart:testend,teststart2:testend2),]
		klas.train <- klas[c(trainstart:trainend,trainstart2:trainend2)]
		klas.test <- klas[c(teststart:testend,teststart2:testend2)]
	model <- svm(as.factor(klas[1:240]) ~ ., data = train, cost = cost, gamma = gamma, epsilon = epsilon, Kernel = kernel)
	prediction <- predict(model, test)
	print(table(prediction,klas.test))

	params <- c(cost,gamma,epsilon,kernel)
		write(params, file = "data.txt", append = TRUE, ncolumns = if(is.character(model)) 1 else 40)
		tbl <- table(prediction,klas.test)
		write(nrow(tbl), file = "data.txt", append = TRUE, ncolumns = if(is.character(model)) 1 else 40)
		write(ncol(tbl), file = "data.txt", append = TRUE, ncolumns = if(is.character(model)) 1 else 40)
		write.table(tbl, file = "data.txt", append = TRUE)

		trainstart <- trainstart + 20
		testend2 <- testend2 + 20

		if(trainend != 360) trainend <- trainend + 20
		else {
			if(trainstart2 == 0) trainstart2 <- 1
			trainend2 <- trainend2 + 20
		}

		if(teststart != 0) teststart <- teststart + 20
    		if(teststart == 361) {
			teststart <- 0
			testend <- 0
		}
    		if(teststart2 == 0) teststart2 <- 1
    		if(testend2 > 120) teststart2 <- teststart2 + 20

indexes <- c(trainstart,trainend,trainstart2,trainend2,teststart,testend,teststart2,testend2)
write(indexes, file = "data.txt", append = TRUE, ncolumns = if(is.character(mode)) 1 else 40)


}
if(i < 16)
	cost <- cost + 0.1
if(i < 32 && i > 15)
	gamma <- gamma + 0.1
if(i > 31)
	epsilon <- elipson + 0.1
if(i < 13)
	kernel <- "radial"
if(i < 26 && i > 12)
	kernel <- "linear"
if(i < 39 && i > 25)
	kernel <- "polynomial"
if(i > 38)
	kernel <- "sigmoid"
}
}

start_svm()
