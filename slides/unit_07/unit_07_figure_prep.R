require(here)
source(here("data", "environment_vars.R"))

{
  classData <- read.csv(here(chris_dat_dir, "classData16.csv"), h=T)
  mander <- read.csv(here(chris_dat_dir, "mander.csv"), h=T)
  bp <- read.table(here(chris_dat_dir, "bearpoop.txt"), h=T)
  
  x1 <- rnorm(100000,50,10)
  x2 <- rnorm(100000,150,10)
  x3 <- rnorm(100000,52,10)
  x4 <- rnorm(100000,60,10)
}

{
  pdf(here(slide_img_dir, "meansA.pdf"), height=4,width=5)
  par(mfrow=c(2,1),oma=c(0,0,0,0),mar=c(2,2,1,1))
  plot(density(x1),col=1,lwd=1,xlim=c(0,200),ylim=c(0,0.05),
       main="",axes=F,xlab="",ylab="")
  abline(v=mean(x1),lwd=2,col=2)
  abline(v=mean(x2),lwd=2,col=4)
  lines(density(x1),col=1, lwd=1)
  lines(density(x2),col=1, lwd=1)
  axis(1)
  box(bty="l")
  boxplot(cbind(x1,x2),ylim=c(0,200),horizontal=TRUE,pch="",
          col=adjustcolor(c(2,4),0.3),axes=F)
  axis(1)
  abline(v=mean(x1),lwd=2,col=2)
  abline(v=mean(x2),lwd=2,col=4)
  boxplot(cbind(x1,x2),ylim=c(0,200),horizontal=TRUE,pch="",
          col=adjustcolor(c(2,4),0.3),axes=F,add=T)
  box(bty="l")
  dev.off()
}

{
  pdf(here(slide_img_dir, "means2.pdf"),height=4,width=5)
  par(mfrow=c(2,1),oma=c(0,0,0,0),mar=c(2,2,1,1))
  plot(density(x1),col=1,lwd=1,xlim=c(0,200),ylim=c(0,0.05),
       main="",axes=F,xlab="",ylab="")
  abline(v=mean(x1),lwd=2,col=2)
  abline(v=mean(x3),lwd=2,col=4)
  lines(density(x1),col=1, lwd=1)
  lines(density(x3),col=1, lwd=1)
  axis(1)
  box(bty="l")
  boxplot(cbind(x1,x3),ylim=c(0,200),horizontal=TRUE,pch="",
          col=adjustcolor(c(2,4),0.3),axes=F)
  axis(1)
  abline(v=mean(x1),lwd=2,col=2)
  abline(v=mean(x3),lwd=2,col=4)
  boxplot(cbind(x1,x3),ylim=c(0,200),horizontal=TRUE,pch="",
          col=adjustcolor(c(2,4),0.3),axes=F,add=T)
  box(bty="l")
  dev.off()
}

{
  pdf(here(slide_img_dir, "means3.pdf"),height=4,width=5)
  par(mfrow=c(2,1),oma=c(0,0,0,0),mar=c(2,2,1,1))
  plot(density(x1),col=1,lwd=1,xlim=c(0,200),ylim=c(0,0.05),
       main="",axes=F,xlab="",ylab="")
  abline(v=mean(x1),lwd=2,col=2)
  abline(v=mean(x4),lwd=2,col=4)
  lines(density(x1),col=1, lwd=1)
  lines(density(x4),col=1, lwd=1)
  axis(1)
  box(bty="l")
  boxplot(cbind(x1,x4),ylim=c(0,200),horizontal=TRUE,pch="",
          col=adjustcolor(c(2,4),0.3),axes=F)
  axis(1)
  abline(v=mean(x1),lwd=2,col=2)
  abline(v=mean(x4),lwd=2,col=4)
  boxplot(cbind(x1,x4),ylim=c(0,200),horizontal=TRUE,pch="",
          col=adjustcolor(c(2,4),0.3),axes=F,add=T)
  box(bty="l")
  dev.off()
  i = 1;
  set.seed(12345)
  nrow = 10
  dat = data.frame(length = runif(n = nrow), width = rexp(nrow), mass = rpois(nrow, 34))
}

{
  pdf(here(slide_img_dir, "randhist.pdf"), height=3, width=7)
  par(mfrow=c(1,2), oma=c(0,0,0,0), mar=c(4,4,1,1))
  set.seed(123)
  hist(rnorm(300,50,10), col="orange", xlab="Values", main="", las=1, ylim=c(0,70),xlim=c(10,90))
  box(bty="l")
  plot(0,0,type="n",axes=F,xlab="",ylab="")
  dev.off()
}

{
  pdf(here(slide_img_dir, "randhist2.pdf"), height=3, width=7)
  par(mfrow=c(1,2), oma=c(0,0,0,0), mar=c(4,4,1,1))
  set.seed(123)
  hist(rnorm(300,50,10), col="orange", xlab="Values", main="", las=1, ylim=c(0,70),xlim=c(10,90))
  box(bty="l")
  set.seed(123)
  hist(rnorm(300,50,10), col="orange", xlab="Values", main="", las=1, ylim=c(0,70),xlim=c(10,90),breaks=seq(10,90,2))
  box(bty="l")
  dev.off()
}

{
  pdf(here(slide_img_dir, "randhist3.pdf"), height=3, width=5)
  par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(4,4,1,1))
  set.seed(123)
  rnd <- rnorm(300,50,10)
  hist(rnd, col="orange", xlab="Values", main="", las=1, ylim=c(0,0.05), xlim=c(10,90), probability = TRUE)
  box(bty="l")
  lines(density(rnd),lwd=2,col=4)
  dev.off()
}

{
  pdf(here(slide_img_dir, "bw.pdf"),height=3.5, width=3)
  par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(4,4,2,2))
  boxplot(rnd,las=1, pch="")
  dev.off()
}

{
  pdf(here(slide_img_dir, "four.pdf"),height=3.5, width=3)
  par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(4,4,2,2))
  boxplot(rnd,las=1, axes=FALSE, pch="")
  dev.off()
}


{
  n_samples_wyola = 32
  n_samples_quabbin = 120
  mean_wyola = 50
  mean_quabbin = 42
  sd_wyola = 5
  sd_quabbin = 5.7
  
  sim_fish = function(mean_wyola, mean_quabbin, sd_wyola, sd_quabbin, n_samples_wyola, n_samples_quabbin)
  {
    data.table(
      lake = c(rep("Wyola", n_samples_wyola), rep("Quabbin", n_samples_quabbin)),
      mass = c(rnorm(n_samples_wyola, mean = mean_wyola, sd = sd_wyola), quabbin = rnorm(n_samples_quabbin, mean = mean_quabbin, sd = sd_quabbin))
    )
  }
  
  dat = sim_fish(mean_wyola, mean_quabbin, sd_wyola, sd_quabbin, n_samples_wyola, n_samples_quabbin)
  
  
  
}



# Differences: wyola and quabbin ----
{
  plot_dif = function(dat, main = "Lake Trout Mass")
  {
    col_wy = 2
    col_qu = 4
    
    par(mfrow=c(2,1),oma=c(0,0,2.2,0),mar=c(3,2,1,1))
    
    d1 = dat[lake == "Wyola"]
    d2 = dat[lake == "Quabbin"]
    
    x1 = d1$mass
    x2 = d2$mass
    
    dens1 = density(x1)
    dens2 = density(x2)
    
    xlim = c(min(c(x1, x2)), max(c(x1, x2)))
    ylim = c(min(c(dens1$y, dens2$y)), 1.35 * max(c(dens1$y, dens2$y)))
    
    plot(density(x1),
         col = col_wy,
         lwd = 1,
         xlim = xlim,
         main = "",
         ylim = ylim,
         axes = F,
         ann = T,
         xlab = "",
         ylab = "", 
         type = "n")
    
    mtext(main, cex = 1.8, line = 1)
    mtext(side = 1, text = "grams", line = 2)
    mtext(side = 2, text = "density", line = 0.7)
     legend(x = "topleft", legend = c(paste0("Wyola (n = ", n_samples_wyola, ")"), paste0("Quabbin (n = ", n_samples_quabbin, ")")), 
           lwd = 3.4,
           col = c(col_wy, col_qu), lty = 1, bty = "n")
    abline(v=mean(x1),lwd = 2,col = col_wy)
    abline(v=mean(x2),lwd = 2,col = col_qu)
   
     lines(dens1, col = col_wy, lwd=1)
    lines(dens2, col = col_qu, lwd=1)
   
     axis(1)
    box(bty="l")

    par(oma=c(0,0,2.2,0),mar=c(2,2,1,1), new = T)
    boxplot(
      # cbind(x1,x2),
        x1,x2,
      ylim = xlim,
      horizontal=TRUE,
      pch="",
      col = adjustcolor(c(col_wy, col_qu),0.3),
      axes=F)
    axis(1)
    abline(v=mean(x1),lwd=2,col=2)
    abline(v=mean(x2),lwd=2,col=4)
    # boxplot(x1, x2,
    #   # cbind(x1,x2),
    #         ylim=c(0,200),horizontal=TRUE,pch="",
    #         col = adjustcolor(c(col_wy, col_qu),0.3),
    #         axes=F,add=T)
    box(bty="l")
    }
  plot_dif(dat)
  
  
  plot_dif(sim_fish(mean_wyola, mean_quabbin, sd_wyola, sd_quabbin, n_samples_wyola, n_samples_quabbin))

  pdf(here(slide_img_dir, "quab_wyola_1.pdf"), height = 4.8, width = 8)  
    plot_dif(sim_fish(34, 34.2, 3, 3.1, 45, 61))
  dev.off()
  pdf(here(slide_img_dir, "quab_wyola_2.pdf"), height = 4.8, width = 8)  
    plot_dif(sim_fish(34, 37.2, 3.7, 3.1, 45, 61))
  dev.off()
  pdf(here(slide_img_dir, "quab_wyola_3.pdf"), height = 4.8, width = 8)  
  plot_dif(sim_fish(34, 37.2, 0.7, 0.5, 45, 61))
  dev.off()
  
      }


# {
#   pdf(here(slide_img_dir, "runavg.pdf"), height=4, width=5)
#   plot(bp$Poop, lwd=2,col=4,bty="l",las=1,ylab="Running Average", xlab="no. of samples", ylim=c(0,2), type="l")
#   lines(bp$Bear, lwd=2,col=2,bty="l",las=1,ylab="Running Average", xlab="no. of samples")
#   abline(h=mean(bp$Bear), lty=2, col=2)
#   abline(h=mean(bp$Poop), lty=2, col=4)
#   legend("bottomright", lwd=2,col=c(4,2),legend=c("Poop","bear"), bty="n")
#   dev.off()
# }
# 
# {
#   pdf(here(slide_img_dir, "salbox.pdf"), height=4, width=5)
#   boxplot(mander$SVL~mander$Sex, col=adjustcolor(c(2,3,4),0.2),ylab="SV length", las=1)
#   dev.off()
# }
# 
# {
#   tmp <- c(0.7,1.9,3.1)
#   pdf(here(slide_img_dir, "salbarse.pdf"), height=4, width=5)
#   bars <- tapply(mander$SVL,mander$Sex,mean)
#   ss <- c(tapply(mander$SVL,mander$Sex,sd))/c(sqrt(table(mander$Sex)))
#   barplot(bars, col=adjustcolor(c(2,3,4),0.2),ylab="SV length", las=1, ylim=c(0,50))
#   arrows(c(tmp),bars-4.96*ss,c(tmp),bars+4.96*ss,lwd=2,length=0)
#   dev.off()
# }
# 
# {
#   pdf(here(slide_img_dir, "salbar.pdf"), height=4, width=5)
#   bars <- tapply(mander$SVL,mander$Sex,mean)
#   barplot(bars, col=adjustcolor(c(2,3,4),0.2),ylab="SV length", las=1, ylim=c(0,50))
#   dev.off()
# }
# 
# {
#   pdf(here(slide_img_dir, "salscatter.pdf"), height=4, width=7)
#   par(mfrow=c(1,2))
#   plot(mander$SVL ~ mander$Total_length, bg=adjustcolor(3,0.3), ylab="SV length (mm)", xlab="Total length (mm)", las=1, ylim=c(10,55), bty="l",pch=21, cex=1.2)
#   plot(mander$SVL ~ jitter(as.numeric(mander$Date)+5,3), bg=adjustcolor(4,0.3), ylab="SV length (mm)", xlab="Temperature (C)", las=1,ylim=c(10,55), bty="l", pch=21,cex=1.2)
#   dev.off()
# }
# 
# {
#   pdf(here(slide_img_dir, "classpie.pdf"), height=4, width=4)
#   par(mar=c(1,1,1,1),oma=c(0,0,0,0))
#   pietab <- table(classData$Eyes)
#   pie(pietab, labels = paste(names(pietab),"\n (",pietab,")",sep=""), col = adjustcolor(c(4,"turquoise","brown",3),0.6), main="Eye color")
#   dev.off()
# }
# 
# {
#   pdf(here(slide_img_dir, "classbar.pdf"), height=4, width=7)
#   bartab <- table(classData$Gender,classData$Eyes)
#   barplot(bartab, col = adjustcolor(c(2,4),0.4), main="Eye color by Gender", las=1, ylab="Count", beside=TRUE, ylim=c(0,10),legend=rownames(bartab))
#   dev.off()
# }