
# Author: tim
###############################################################################

Dat           <- local(get(load("/home/tim/git/MorbCon/MorbCon/Data/resultsP.Rdata")))

library(reshape2)
library(data.table)

wmean <- function(x,w){
	sum(x*w,na.rm=TRUE)/sum(w,na.rm=TRUE)
}

varnames      <- unlist(lapply(Dat, "[[", "var"))
names(Dat)    <- varnames


Res           <- melt(Dat)
colnames(Res) <- c("TTD", "Age", "Cohort", "Prev", "Surf", "Sex", "Varname")
Res$Surf      <- NULL
Res$Prev      <- as.numeric(Res$Prev)
Res           <- Res[!is.na(Res$Prev), ]



# mean prev as of eq 4:
chunk   <- Res[Res$Cohort == 1925 & Res$Varname == "adl3" & Res$Sex == "Male", ]
Prev    <- chunk$Prev
TTD     <- chunk$TTD
Age     <- chunk$Age
get_D_4(TTD, Prev)

get_D_4 <- function(TTD,Prev, Age, Agemin = 70, TTDmin = 12){
	ind  <- Age >= Agemin
	if (max(TTD[ind]) < TTDmin){
		return(as.double(NA))
	}
	prev <- tapply(Prev[ind], TTD[ind], mean, na.rm = TRUE)
	ttd  <- as.integer(names(prev))
	wmean(x = ttd, w = prev)
}

#get_D_5 <- function(.SD){
#	.SD$L <- .SD$Age + .SD$TTD
#	ATL  <- acast(.SD, TTD~L,value.var="Prev")
#	ATL  <- ATL[, colSums(!is.na(ATL)) > 10]
#	prev <- tapply(Prev, TTD, mean, na.rm = TRUE)
#	ttd  <- as.integer(names(prev))
#	wmean(x = ttd, w = prev)
#}

# mean TTD
Res <- data.table(Res)
D4  <- Res[,list(D4 = get_D_4(TTD,Prev,Age)), by = list(Cohort, Sex, Varname)]
D4  <- data.frame(D4)
D4  <- D4[!is.na(D4$D4), ]

MalesD4   <- acast(D4[D4$Sex == "Male", ], Cohort~Varname, value.var = "D4")
FemalesD4 <- acast(D4[D4$Sex == "Female", ], Cohort~Varname, value.var = "D4")
par(mfrow = c(1,2))
matplot(1905:1929,
		FemalesD4,
		type = 'l',
		xlim = c(1905,1925),
		ylim = c(2,7),
		lty = 1,
		col = gray(.7),
		ylab = "Dispersion",
		xlab = "Birth Cohort",
		las = 1)
lines(1905:1929, FemalesD4[, "adl5_3"], col = "red", lwd = 2)

matplot(1905:1929,
		MalesD4,
		type = 'l',
		xlim = c(1905,1925),
		ylim = c(2,7),
		lty = 1,
		col = gray(.7),
		ylab = "Dispersion",
		xlab = "Birth Cohort",
		las = 1)

lines(1905:1929, MalesD4[, "adl5_3"], col = "red", lwd = 2)

makeTrend <- function(MAT,relative = FALSE,ylab = "Dispersion",
		xlab = "Birth Cohort",...){
	if (relative){
		MAT <- t(t(MAT) / MAT[1,])
	}
	matplot(1905:1929,
			MAT,
			type = 'l',
			xlim = c(1905,1925),
			lty = 1,
			col = gray(.7),
			ylab = ylab,
			xlab = xlab,
			las = 1,
			...)
}
highlightTrend <- function(MAT,
		varname = "adl3", 
		relative = FALSE,
		 ...){
	if (relative){
		MAT <- t(t(MAT) / MAT[1,])
	}
	matplot(1905:1929,
			MAT[, varname],
			type = 'l',
			lty = 1,
			add = TRUE,
			...)
}
cols5 <- c("#5e904e", "#9a23b1", "#d36411", "#657bec", "#927231")

library(RColorBrewer)
ADL3cols <- brewer.pal(5,"Greens")[3:5]
IADL3cols <- brewer.pal(5,"Reds")[3:5]

makeTrend(MalesD4, ylim=c(2,7))
highlightTrend(MalesD4, varname = c("adl5_1"), col = cols5[1],lwd=2)
highlightTrend(MalesD4, varname = c("adl5_1","adl5_2"), col = cols5[1:2],lwd=2)
highlightTrend(MalesD4, varname = c("adl5_1","adl5_2","adl5_3"), col = ADL3cols,lwd=3)
highlightTrend(MalesD4, varname = c("iadl5_1","iadl5_2","iadl5_3"), col = IADL3cols,lwd=3)


highlightTrend(MalesD4, varname = c("adl5_3","srhpoor","psych","iadl5_3"), col = cols5[1:4],lwd=2)

MalesD4[,c("adl5_3","adl5_2","adl5_3")]
#
#
#D4  <- Res[,list(D4 = get_D_4(TTD,Prev,Age, Agemin=65)), by = list(Cohort, Sex, Varname)]
#D4  <- data.frame(D4)
#D4  <- D4[!is.na(D4$D4), ]
#
#MalesD4   <- acast(D4[D4$Sex == "Male", ], Cohort~Varname, value.var = "D4")
#FemalesD4 <- acast(D4[D4$Sex == "Female", ], Cohort~Varname, value.var = "D4")
#par(mfrow=c(1,2))
#matplot(1905:1929,
#		FemalesD4,
#		type = 'l',
#		xlim = c(1905,1925),
#		ylim = c(2,7),
#		lty = 1,
#		col = gray(.7),
#		ylab = "Dispersion",
#		xlab = "Birth Cohort",
#		las = 1)
#lines(1905:1929, FemalesD4[,"adl5_3"], col = "red", lwd = 2)
#
#matplot(1905:1929,
#		MalesD4,
#		type = 'l',
#		xlim = c(1905,1925),
#		ylim = c(2,7),
#		lty = 1,
#		col = gray(.7),
#		ylab = "Dispersion",
#		xlab = "Birth Cohort",
#		las = 1)
#
#lines(1905:1929, MalesD4[,"adl5_3"], col = "red", lwd = 2)