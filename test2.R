#####################################################################
# TEST	(Structure matrix with time by column and pixels by row)    #
#####################################################################


nrows = 161
# nrows = 100


region = "n0"
param = "vx"
proj = "l"
tstamp = "000"

name<-paste(region,param,proj,tstamp,".ts", sep="")


# Function defined by the user (flexible)

myfunction<-function(name,AVG=1,CV=1,MK=1,MK.PVAL=1,TREND=1){

# Calling packages and/or define our empty vectors

require(Kendall)

avg.tot<-c()
cv.tot<-c()
mk.tot<-c()
mk.pval.tot<-c()
trend.tot<-c()

# Reading procedure

for(j in 1:nrows){

rowfile <-"tsline.txt"

command<-as.vector(paste('getrtsinput', ' -t -0 -a 0.004 -b -0.1 ', j, ' ', name, ' > ', rowfile, sep=""))
print(command)

# system(command)
shell(command)
	
test<-read.table(rowfile,header=T,stringsAsFactors=F)
test<-test[,-c(1,2)]

time<-1:dim(test)[[2]]  # dim(test) return 2 elems, nrows/ncols, so [[2]] gets the ncols; note "1:ncols" fills with sequence starting from 1
npixel<-dim(test)[[1]]

avg <- apply(test,1,FUN=mean)	# second arg ("1") means by row, function is applied to the row values
w <- avg==-1			# w is a logical T/F resulting from equality of a values to -1
avg[w] <- -99			# sets values of avg for which w=T to -99

sd <- apply(test,1,FUN=sd)
cv <- sd/avg
w <- cv==0
s <- cv>100
cv[w] <- -99
cv[s] <- -1

avg.tot<-c(avg.tot,avg)		# appends new results to existing results
cv.tot<-c(cv.tot,cv)

mk<-c()
mk.pval<-c()
trend<-c()

#begin of second FOR cycle 

for (i in 1:npixel){

	val <- as.numeric(test[i,])	# i runs down row by row, empty retrieves full row, numeric stores as number, 

	if(val==-1){		# this works iif all values in vector are -1
		app1 <- -99
		app2 <- -99
		app3 <- -99
		mk <- c(mk,app1)
		mk.pval <- c(mk.pval,app2)
		trend <- c(trend,app3) 
	}

	if(val>-1){
	
		app2 <- MannKendall(val)$sl	# $ at end means output is list, "sl" extracts the prob level
	
		if(app2<=0.05){
			app1<-1
		}
		if(app2>0.05){
			app1<-0
		}

		mk <- c(mk,app1)		# Appending output sig flag to array mk
		mk.pval <- c(mk.pval,app2)
	
	
		mod <- lm(val~time)		# linear model, time[] defined beforehand 
		app3 <- mod$coefficients[2]	# mod is a list, this extracts element coefficient[2]
		trend <- c(trend,app3)		# Appends slope to trend output vector (this builds one line of output at the end of the loop

	}

#end of FOR cycle
}


mk.tot<-c(mk.tot,mk)			# Appending of the result of a for loop (line) to the final output vector
mk.pval.tot<-c(mk.pval.tot,mk.pval)
trend.tot<-c(trend.tot,trend)

#end of reading cycle			# moves on to the next file...
}


#final writing procedure

	if(AVG==1){
	avg.matrix<-matrix(round(avg.tot,3),nrow=nrows,ncol=npixel,byrow=T)	# length(name) contains the N of names, i.e. number of raster lines
	write.table(avg.matrix,col.names=F,row.names=F,file="Avg.txt",quote=F)
	}
	if(CV==1){
	cv.matrix<-matrix(round(cv.tot,2),nrow=nrows,ncol=npixel,byrow=T)
	write.table(cv.matrix,col.names=F,row.names=F,file="CV.txt",quote=F)
	}
	if(MK==1){
	mk.matrix<-matrix(mk.tot,nrow=nrows,ncol=npixel,byrow=T)
	write.table(mk.matrix,col.names=F,row.names=F,file="MK.txt",quote=F)
	}
	if(MK.PVAL==1){
	mk.pval.matrix<-matrix(round(mk.pval.tot,2),nrow=nrows,ncol=npixel,byrow=T)
	write.table(mk.pval.matrix,col.names=F,row.names=F,file="MKPVAL.txt",quote=F)
	}
	if(TREND==1){
	trend.matrix<-matrix(round(trend.tot,5),nrow=nrows,ncol=npixel,byrow=T)
	write.table(trend.matrix,col.names=F,row.names=F,file="Trend.txt",quote=F)
	}

#end of our function
}



