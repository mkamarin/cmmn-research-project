#
## A set of helper functions
#
std <- function(x) sd(x)/sqrt(length(x)) # Standard error of the mean

# data frame for the weight descriptive statistics
w.data.descr <- data.frame(Measure = character(0), N = integer(0), Mean = numeric(0), SD = numeric(0), SEM = numeric(0))

#Arguments
# w is complexity weight vector
# v is the mean value
# t is text with the name of the weight element
descriptive.stats.weights <- function(w,v,t,df) 
{
  ww <- na.omit(w)
  nm <- sub("data$","",deparse(substitute(w)),fixed=TRUE)
  
  return(rbind(df, data.frame(Measure = as.character(nm),
                              N = as.integer(length(ww)),
                              Mean = as.numeric(round(mean(ww), digits=3)),
                              SD = as.numeric(round(sd(ww), digits=3)),
                              SEM = as.numeric(round(std(ww), digits=3)))))
  #print(paste(name,"(",t,")",v[1]," (",val,")",length(ww),round(mean(val), digits=3),
  #             round(sd(val), digits=3),round(std(val), digits=3)))
  #print(paste(name,length(ww),round(mean(ww), digits=3),
  #             round(sd(ww), digits=3),round(std(ww), digits=3)))
}

str.p <- function(p)
{
  #Note. *p<.05, **p<.01, ***p<.001.
  if(p<0.001) "***" else if(p<0.01) "**" else if(p<0.05) "*" else "" #paste0("[",as.character(round(p,3)),"]")
}

ro.range <- function(p)
{
  #Note. *p<.05, **p<.01, ***p<.001.
  if(p<0.001) "$p<.001$" else if(p<0.01) "$p<.01$" else if(p<0.05) "$p<.05$" else as.character(round(p,3))
}


w.data.t.stats <- data.frame(Measure = character(0), N = integer(0), mu = numeric(0), t = numeric(0), 
                             Mean = numeric(0), Lower = numeric(0), Upper = numeric(0), p = character(0))

t.stats.weights <- function(w,v,df) 
{
  ww <- na.omit(w)
  nm <- sub("data$","",deparse(substitute(w)),fixed=TRUE)
  
  # Calculate weight value
  #val <- round(((v[1] * 4) + 3) / 2, digits=2)
  
  t <- t.test(ww, mu=v[1], alternative="two.sided", var.equal=TRUE, na.action = na.omit)
  
  return(rbind(df, data.frame(Measure = as.character(nm),
                              N = as.integer(length(ww)), 
                              mu = as.numeric(v[1]),
                              t = as.numeric(round(t$statistic, digits=3)),
                              Mean = as.numeric(round(t$estimate, digits=3)),
                              Lower = as.numeric(round(t$conf.int[1], digits=3)),
                              Upper = as.numeric(round(t$conf.int[2], digits=3)),
                              p = as.character(ro.range(t$p.value)))))
  #print(paste0(name,"   ",round(t$statistic, digits=3),str.p(t$p.value),"(",t$parameter,")   ",
  #             round(t$estimate -1, digits=3),"   ",
  #             round(t$conf.int[1] -1, digits=3),"   ",round(t$conf.int[2] -1, digits=3)))
  #str(t)
  #print(paste(name,t))
  #t
}
plot.weights <- function(in.df,names,mus)
{
  for (r in 1:length(names))
  { 
    nm <- names[r]
    u <- mus[r]
    
    val <- na.omit(in.df[[nm]])
    total.vals <-  c(length(which(val==1)),
                     length(which(val==2)),
                     length(which(val==3)),
                     length(which(val==4)),
                     length(which(val==5)),
                     length(which(val==6)),
                     length(which(val==7)),
                     length(which(val==8)))
    total.names <-c("1", "2","3","4","5","6","7","8")
    
    xx <- barplot(height = total.vals,
                  names.arg = total.names,
                  ylim = c(0, 7+max(total.vals)),
                  #las = 2, # rotate labels
                  main = nm, #space = 1,
                  ylab = "Frequency",
                  font.main = 1, # plain text for title
                  cex.main = 1 # normal size for title
    )
    lb <-as.character(total.vals)
    a <- lb[u]
    lb[u] <- paste0("(",a,")")
    text(x = xx, y=total.vals, pos = 3, cex = 0.8, labels=lb)#, xpd=TRUE)
  }
}
plot.perceived <- function(in.df,name)
{
    val <- na.omit(in.df[[name]])
    total.vals <-  c(length(which(val==1)),
                     length(which(val==2)),
                     length(which(val==3)),
                     length(which(val==4)),
                     length(which(val==5)),
                     length(which(val==6)),
                     length(which(val==7)))
    total.names <-c("1", "2","3","4","5","6","7")
    
    xx <- barplot(height = total.vals,
                  names.arg = total.names,
                  ylim = c(0, 7+max(total.vals)),
                  #las = 2, # rotate labels
                  main = name, #space = 1,
                  ylab = "Frequency",
                  font.main = 1, # plain text for title
                  cex.main = 1 # normal size for title
    )
    text(x = xx, y=total.vals, pos = 3, cex = 0.8, labels=as.character(total.vals))#, xpd=TRUE)
}

plot.compare <- function(in.df,name)
{
  val <- na.omit(in.df[[name]])
  total.vals <-  c(length(which(val==1)),
                   length(which(val==2)),
                   length(which(val==3)),
                   length(which(val==4)),
                   length(which(val==5)),
                   length(which(val==6)),
                   length(which(val==7)),
                   length(which(val==8)),
                   length(which(val==9)))
  total.names <-c("1", "2","3","4","5","6","7","8","9")
  
  xx <- barplot(height = total.vals,
                names.arg = total.names,
                ylim = c(0, 7+max(total.vals)),
                #las = 2, # rotate labels
                main = name, #space = 1,
                ylab = "Frequency",
                font.main = 1, # plain text for title
                cex.main = 1 # normal size for title
  )
  text(x = xx, y=total.vals, pos = 3, cex = 0.8, labels=as.character(total.vals))#, xpd=TRUE)
}

mk.wilcoxon.test <- function(in.df,names,mus)
{
  out <- data.frame(Name = character(0), N = integer(0), Mean = numeric(0), SD = numeric(0), mu = numeric(0), V = numeric(0), p = character(0))
  
  for (r in 1:length(names))
  { 
    nm <- names[r]
    u <- mus[r]
    
    val <- na.omit(in.df[[nm]])
    
    tst <- wilcox.test(val, mu=u, alternative = "two.sided", exact=FALSE, conf.level=95 )
    #str(tst)
    #print(tst)
    out <- rbind(out, data.frame(Name = as.character(nm), 
                                 N = as.integer(length(val)), 
                                 Mean = as.numeric(mean(val)), 
                                 SD = as.numeric(sd(val)),
                                 mu = as.numeric(u), 
                                 V = as.numeric(round(tst$statistic,3)), 
                                 p = as.character(ro.range(tst$p.value))))
    
  }
  return(out)
}

mk.paired.wilcoxon.test <- function(in.df,names)
{
  out <- data.frame(Name = character(0), N = integer(0), V = numeric(0), p = character(0))
  
  for (r in 1:length(names))
  { 
    nm <- names[r]
    
    df <- data.frame(a = in.df[[paste0("A.",nm)]], b = in.df[[paste0("B.",nm)]])
    df <- df[complete.cases(df),]
    val.a <- df$a
    val.b <- df$b
    
    tst <- wilcox.test(val.a, val.b, paired = TRUE, alternative = "two.sided", exact=FALSE, conf.level=95 )
    #str(tst)
    #print(tst)
    out <- rbind(out, data.frame(Name = as.character(paste0("A.",nm," vs. B.",nm)), 
                                 N = as.integer(length(val.a)),  
                                 V = as.numeric(round(tst$statistic,3)), 
                                 p = as.character(ro.range(tst$p.value))))
    
  }
  return(out)
}

mk.two.sample.wilcoxon.tutorial <- function(in.df,names)
{
  out <- data.frame(Name = character(0), N = integer(0), V = numeric(0), p = character(0))
  
  for (r in 1:length(names))
  { 
    nm <- names[r]
   
    val.one <- in.df[[nm]][in.df$Tutorial.Time <= 240]
    val.two <- in.df[[nm]][in.df$Tutorial.Time > 240]
    
    tst <- wilcox.test(val.one, val.two, paired = FALSE, alternative ="two.sided")
    #tst <- t.test(val ~ factor.vect, alternative ="two.sided")
    #str(tst)
    #print(tst)
    out <- rbind(out, data.frame(Name = as.character(paste(nm,"[short] vs",nm,"[long]")), 
                                 N = as.integer(min(c(length(val.one),length(val.two)))), 
                                 V = as.numeric(round(tst$statistic,3)), 
                                 p = as.character(ro.range(tst$p.value))))
    
  }
  return(out)
}

descriptive.stats.ratio <- function(in.df,cols)
{
  out <- data.frame(Name = character(0), N = integer(0), min = numeric(0), max = numeric(0), 
                    mean = numeric(0), sd = numeric(0), sem = numeric(0))
  for (c in cols)
  {
    v <- na.omit(in.df[[c]])
    out <- rbind(out, data.frame(Name = as.character(c), 
                                 N = as.integer(length(v)), 
                                 min = as.numeric(round(min(v),3)), 
                                 max = as.numeric(round(max(v),3)), 
                                 mean = as.numeric(round(mean(v),3)), 
                                 sd = as.numeric(round(sd(v),3)), 
                                 sem = as.numeric(round(std(v),3))))
    
  }
  return(out)
}
mk.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

descriptive.stats.ordinal <- function(in.df,cols)
{
  out <- data.frame(Name = character(0), N = integer(0), min = numeric(0), max = numeric(0), 
                    median = numeric(0), mode = numeric(0))
  for (c in cols)
  {
    v <- na.omit(in.df[[c]])
    out <- rbind(out, data.frame(Name = as.character(c), 
                                 N = as.integer(length(v)), 
                                 min = as.numeric(round(min(v),3)), 
                                 max = as.numeric(round(max(v),3)), 
                                 median = as.numeric(round(median(v),3)), 
                                 mode = as.numeric(round(mk.mode(v),3))))
    
  }
  return(out)
}

normality.stats <- function(in.df,cols)
{
  out <- data.frame(Name = character(0), N = integer(0), 
                    skewness = numeric(0), kurtosis = numeric(0), 
                    W = numeric(0), p = character(0))
  for (c in cols)
  {
    v <- na.omit(in.df[[c]])
    st <- shapiro.test(v)
    out <- rbind(out, data.frame(Name = as.character(c), 
                                 N = as.integer(length(v)), 
                                 skewness = as.numeric(skewness(v,type=2)),
                                 kurtosis = as.numeric(kurtosis(v,type=2)),
                                 W = as.numeric(round(st$statistic,3)),
                                 p = as.character(ro.range(st$p.value))))
    
  }
  return(out)
}

normality.charts <- function(in.df,cols)
{
  for (c in cols)
  {  
    v <- na.omit(in.df[[c]])
    txt <- as.character(c)
    
    if(length(v) > 0)
    {
      #plot.new()
      #par(mfrow=c(1,3),mar=c(5,5,5,2))
      #plot.qqnorm.spss(qqnorm_spss(b),1)
      
      hist(v, main=paste("Histogram of\n",txt), xlab=txt, prob=TRUE, col="grey")
      lines(density(v), col="blue", lwd=2)
      
      qqnorm(v, main=paste("Normal Q-Q Plot of",txt),
             ylab="Observed value",xlab="Expected Normal", datax=TRUE)
      qqline(v, datax=TRUE)
      
      #plot(density(v), main=paste("Density (x=\n",txt,")"))
      
      #plot.qqnorm.spss(qqnorm_spss(b),2)
      
      #boxplot(v, xlab = txt)
    }
    else
      print(paste0(txt," is empty"))
  }
}

normality.chart.var <- function(v,txt)
{
  v <- na.omit(v)  
  if(length(v) > 0)
    {
      hist(v, main=paste("Histogram of\n",txt), xlab=txt, prob=TRUE, col="grey")
      lines(density(v), col="blue", lwd=2)
      
      qqnorm(v, main=paste("Normal Q-Q Plot of",txt),
             ylab="Observed value",xlab="Expected Normal", datax=TRUE)
      qqline(v, datax=TRUE)
    }
    else
      print(paste0(txt," is empty"))
}


mk.correlation <- function(in.df,rows,cols)
{
  count <- 0
  out <- data.frame(stringsAsFactors=FALSE)
  all.cols <- c("Measure",cols)
  for (n in all.cols)
  {
    out[[n]] <- as.character()
  }
  #out <- data.frame(lapply(out, as.character), stringsAsFactors=FALSE)
  for (r in rows)
  {
    v <- c(r)
    for (c in cols)
    {
      df <- data.frame(x = in.df[[r]], y = in.df[[c]])
      df <- df[complete.cases(df),]
      p <- cor.test(df$x,df$y,alternative="two.sided",method="spearman", exact = FALSE, 
                    continuity=TRUE)
      #print(p)
      v <- c(v,as.character(paste0(round(p$estimate,3),str.p(p$p.value),"(",length(df$x)-2,")")))
    }
    count <- count + 1
    out[count,] <- v
  }
  return(out)
}

mk.correlationEx <- function(in.df,rows,cols)
{
  count <- 0
  out <- data.frame(stringsAsFactors=FALSE)
  all.cols <- c("Measure","DF",paste(cols,"[p]"))
  for (n in all.cols)
  {
    out[[n]] <- as.character()
  }
  #print("out=")
  #print(out)
  #out <- data.frame(lapply(out, as.character), stringsAsFactors=FALSE)
  for (r in rows)
  {
    v <- c(r)
    for (c in cols)
    {
      df <- data.frame(x = in.df[[r]], y = in.df[[c]])
      df <- df[complete.cases(df),]
      p <- cor.test(df$x,df$y,alternative="two.sided",method="spearman", exact = FALSE, continuity=TRUE)
      
      if(length(v)==1) v <- c(v,as.character(length(df$x)-2))
      
      #print(p)
      v <- c(v,as.character(paste0(round(p$estimate,3)," [",ro.range(p$p.value),"]")))
    }
    count <- count + 1
    #print("row=")
    #print(v)
    out[count,] <- v
  }
  return(out)
}

test.normal.sample.data <- function()
{
  n.sample <- rnorm(n = 5000, mean = 55, sd = 4.5)
  st <- shapiro.test(n.sample)
  print(paste("n:", length(n.sample),"Skewness:",round(as.numeric(skewness(n.sample,type=2)),5),
              "Kurtosis:",round(as.numeric(kurtosis(n.sample,type=2)),5),"\rW:", 
              as.numeric(round(st$statistic,3)),"ro",as.numeric(round(st$p.value,3)),
              "p:",as.character(ro.range(st$p.value))))
  hist(n.sample, main=paste("Histogram of normal sample"))
  qqnorm(n.sample, main=paste("Normal Q-Q Plot of normal sample"),
         ylab="Observed value",xlab="Expected Normal", datax=TRUE)
  qqline(n.sample, datax=TRUE)
  plot(density(n.sample), main=paste("Density (x=normal sample)"))
  boxplot(n.sample, xlab = "boxplot of normal sample")
  rm(n.sample)
}

compare2.boxplot <- function(in.df,var.val, cat.list)
{
  for(c in cat.list)
  {
    df <- data.frame(x = in.df[[var.val]], y= in.df[[c]])
    df <- df[complete.cases(df),]
  
    boxplot(x ~ y, data=df, main = gsub("\\."," ",sub("iv\\.C\\.order","Order ",c)), 
            font.main = 1, cex.main = 1)
    #cat(paste0(var.val," vs ",
    #           gsub("\\."," ",sub("iv\\.C\\.order","Order ",c)),
    #           " (",paste(unique(df$y), sep = ",", collapse = ","),")"))
    rm(df)
  }
}

compare3.boxplot <- function(in.df,var.val, var.factor, cat.list)
{
  
  for(c in cat.list)
  {
    df <- data.frame(x = in.df[[var.val]], y = in.df[[var.factor]], iv = in.df[[c]])
    df <- df[complete.cases(df),]
    
    means<- round(tapply(df$iv, df$y, mean), digits=0)
    
    boxplot(x ~ y, data=df, main = gsub("\\."," ",sub("iv\\.C\\.order","Order ",c)), 
            font.main = 1, cex.main = 1, las=2)
    points(means, col="red",pch=5)
    #cat(paste0(var.val," vs ",
    #           gsub("\\."," ",sub("iv\\.C\\.order","Order ",c)),
    #           " (",paste(unique(df$y), sep = ",", collapse = ","),")"))
    rm(df)
  }
}

boxplot.tutorial <- function(in.df, factor.val, var.list)
{
  for(c in var.list)
  {
    df <- data.frame(x = in.df[[c]], y= in.df[[factor.val]])
    df <- df[complete.cases(df),]
    
    boxplot(x ~ y, data=df, main = c, 
            font.main = 1, cex.main = 1)
    rm(df)
  }
}


oneway.ANOVA <- function(in.df,var.val, factors.list)
{
  out <- data.frame(Name = character(0), N = integer(0), 
                    F = numeric(0), num.df = numeric(0), 
                    denom.df = numeric(0), p = character(0))
  p.vector <- c()
  for (c in factors.list)
  {
    df <- data.frame(x = in.df[[var.val]], y= in.df[[c]])
    df <- df[complete.cases(df),]
 
    #print(paste(var.val,"vs",paste(unique(df$y), sep = ",", collapse = ",")))
    #str(df)
    #print(summary(df))
    #print(tapply(df$x, df$y, mean))
    #print(tapply(df$x, df$y, var))
    #print(tapply(df$x, df$y, length))
    #boxplot(x ~ y, data=df)
    #print(bartlett.test(x ~ y, data=df))
    tst <- oneway.test(x ~ y, data=df, var.equal=TRUE)
    #str(tst)
    #print(tst)
    p.vector <- c(p.vector,tst$p.value)
    out <- rbind(out, data.frame(Name = as.character(gsub("\\."," ",sub("iv\\.C\\.order","Order ",c))), 
                                 N = as.integer(nrow(df)), 
                                 F = as.numeric(round(tst$statistic,4)),
                                 num.df = as.numeric(tst$parameter["num df"]),
                                 denom.df = as.numeric(tst$parameter["denom df"]),
                                 p = as.character(ro.range(tst$p.value))))
    rm(df)
  }
  # Now let adjust p using Bonferroni
  out$Bonferroni <- p.adjust(p.vector, method="bonferroni")
  return(out)
}


plot.pairwise <- function(nm,x,cc,cl,cs,cas)
{
    val <- na.omit(x)
 
    total.vals <-  c(length(which(val==1)),
                     length(which(val==2)),
                     length(which(val==3)),
                     length(which(val==4)),
                     length(which(val==5)),
                     length(which(val==6)),
                     length(which(val==7)),
                     length(which(val==8)),
                     length(which(val==9)))
    total.names <-c("1", "2","3","4","5","6","7","8","9")
    
    xx <- barplot(height = total.vals,
                  names.arg = total.names,
                  ylim = c(0, 7+max(total.vals)),
                  #las = 2, # rotate labels
                  main = paste("C.Compare group",nm), #space = 1,
                  ylab = "Frequency",
                  font.main = 1, # plain text for title
                  cex.main = 1 # normal size for title
    )
    lb <-as.character(total.vals)
    lb[round(cc,0)] <- ""
    lb[round(cl,0)] <- ""
    lb[round(cs,0)] <- ""
    lb[round(cas,0)] <- ""
    
    lb[round(cc,0)] <- paste(lb[round(cc,0)],"C",sep="")
    lb[round(cl,0)] <- paste(lb[round(cl,0)],"L",sep="")
    lb[round(cs,0)] <- paste(lb[round(cs,0)],"S",sep="")
    lb[round(cas,0)] <- paste(lb[round(cas,0)],"A",sep="")
    text(x = xx, y=total.vals, pos = 3, cex = 0.8, labels=lb)#, xpd=TRUE)
}

plot.pairwise.X <- function(x)
{
  val <- na.omit(x)
  
  total.vals <-  c(length(which(val==1)),
                   length(which(val==2)),
                   length(which(val==3)),
                   length(which(val==4)),
                   length(which(val==5)),
                   length(which(val==6)),
                   length(which(val==7)),
                   length(which(val==8)),
                   length(which(val==9)))
  total.names <-c("1", "2","3","4","5","6","7","8","9")
  
  xx <- barplot(height = total.vals,
                names.arg = total.names,
                ylim = c(0, 7+max(total.vals)),
                #las = 2, # rotate labels
                main = "C.Compare", #space = 1,
                ylab = "Frequency",
                font.main = 1, # plain text for title
                cex.main = 1 # normal size for title
  )
  lb <-as.character(total.vals)
  text(x = xx, y=total.vals, pos = 3, cex = 0.8, labels=lb)#, xpd=TRUE)
}


mk.scatter.plots <- function(df,lst1,lst2, Mean = FALSE)
{
  for(v1 in lst1)
  {
    for(v2 in lst2)
    {
      tmp <- data.frame(a = df[[v1]], b = df[[v2]])
      tmp <- tmp[complete.cases(tmp),]
      a <- tmp$a
      b <- tmp$b
      
      # scatter plot
      plot(a, b, main=paste(v1,"vs",v2),	xlab=v1, ylab=v2, #frame.plot=FALSE,
           pch=20, font.main = 1, cex.main = 1) 
      
      # Regression line
      abline(lm(b~a), col = "blue") #, col="red") # regression line (y~x) 
      
      # mean line for x
      #abline(v=mean(a), col="orange")
      
      if(isTRUE(Mean))
      {
        # calculating mean
        out1 <- data.frame(tapply( b, factor(a), mean))
        names(out1) <- c("b.mean")
        out1$grp <- rownames (out1)
      
        # ploting mean connected with lines
        points (out1$grp, out1$b.mean, type = "b", col = "red", pch = 4)
      }
    }
  }
}

