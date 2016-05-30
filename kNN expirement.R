start_knn <- function(){
	setwd("C:/Users/Dovydas/Desktop/data")
	duom <- read.table("rez.txt", header=TRUE, sep = " ", quote = "")
	library(FNN)
	klas <- duom[,8061]
	klas <- as.factor(klas)

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
		knn_rez <- knn(train = train[,1:8060], test = test[,1:8060], cl = klas.train, k=i)
		print(table(knn_rez,klas.test))

		write(i, file = "data.txt", append = TRUE, ncolumns = if(is.character(knn_rez)) 1 else 40)
		tbl <- table(knn_rez,klas.test)
		write(nrow(tbl), file = "data.txt", append = TRUE, ncolumns = if(is.character(knn_rez)) 1 else 40)
		write(ncol(tbl), file = "data.txt", append = TRUE, ncolumns = if(is.character(knn_rez)) 1 else 40)
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
write(indexes, file = "data.txt", append = TRUE, ncolumns = if(is.character(knn_rez)) 1 else 40)

}
	 }
}
start_knn()
