
# recreate Frie-like diagrams.

drawLL <- function(to=7.5,at=9.5,init=6.5,prop=1,...){
	segments(.5, at, to, at,lwd = 2)
	polygon(x = c(init, to, to), y = c(at, at + prop, at),...)
}
ats <- c(9.5,8,6.5,5,3.5,2,.5)
par(xpd=TRUE)
plot(0:10, 0:10, type = 'n', asp = 1, axes = FALSE, xlab="",ylab="")
drawLL(init=5.5,prop=.5,at=ats[1],col=gray(.7))
drawLL(at=ats[2],col=gray(.7))
drawLL(init=5.5,prop=.25,at=ats[3],col=gray(.7))
drawLL(init=5.5,prop=.75,at=ats[4],col=gray(.7))
drawLL(to=8.5,init=6.5,prop=.5,at=ats[5],col=gray(.7))
drawLL(to=8.5,init=5.5,prop=.75,at=ats[6],col=gray(.7))
drawLL(to=8.5,init=7.5,at=ats[7],col=gray(.7))

drawLL2 <- function(to=7.5,init=6.5,prop=1,...){
	at <- 0
	par(xpd = TRUE, xaxs='i',yaxs='i',mai=c(.1,.1,.1,.1))
	plot(NULL, type = 'n', xlim=c(0,9),ylim=c(-.1,1.1),axes = FALSE, xlab = "", ylab = "")
	segments(.5, at, to, at,lwd = 2)
	polygon(x = c(init, to, to), y = c(at, at + prop, at),...)
}
save.drawLL2 <- function(path="/home/tim/git/MorbCon/MorbCon/Figures/scenarios",nr=1,to=7.5,init=6.5,prop=1,...){
	path <- file.path(path,paste0("scenario",nr,".pdf"))
	pdf(path, width=5,height=1)
	drawLL2(to=to,init=init,prop=prop,...)
	dev.off()
}

save.drawLL2(nr=1,init=5.5,prop=.5,col=gray(.7))
save.drawLL2(nr=2,col=gray(.7))
save.drawLL2(nr=3,init=5.5,prop=.25,col=gray(.7))
save.drawLL2(nr=4,init=5.5,prop=.75,col=gray(.7))
save.drawLL2(nr=5,to=8.5,init=6.5,prop=.5,col=gray(.7))
save.drawLL2(nr=6,to=8.5,init=5.5,prop=.75,col=gray(.7))
save.drawLL2(nr=7,to=8.5,init=7.5,col=gray(.7))
save.drawLL2(nr=8,to=7.5,init=4.5,prop=1/3,col=gray(.7))
save.drawLL2(nr=9,init=6.5,prop=.5,col=gray(.7))


pdf("/home/tim/git/MorbCon/MorbCon/DGDtalk/Figures/scenarios/scenario0.pdf", width=5,height=1)
par(mai=c(0,0,0,0))
plot(NULL, type = "n",xlim=c(0,1),ylim=c(0,1),xlab = "",ylab="",axes=FALSE)
dev.off()