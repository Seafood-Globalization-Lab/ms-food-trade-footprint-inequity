# Two-lines test for an inverted U (Kuznets curve)
#
# Based on the two-lines test by Uri Simonsohn (urisohn@gmail.com), version 0.52, 2018 11 23
# Modified by Jessica, 2026 11 06: web-app-only code removed; Kuznets summary added
# Modified 2026 11 11: base-R plot replaced by a ggplot object returned by twolines()
####################################################################################
library(mgcv)         #Additive model with smoothing function
library(sandwich)     #Robust standard errors
library(lmtest)       #Linear tests
library(ggplot2)      #Plotting
library(ggrepel)      #Point labels
library(colorspace)   #lighten()

#OUTLINE
#Function 1 - eval2()           Evaluates a string as an R expression
#Function 2 - reg2()            Interrupted regression with entered formula plus cutoff point (no plotting)
#Function 3 - kuznets.summary() Summarize evidence for an inverted U (Kuznets curve)
#Function 4 - twolines()        Run two-lines test and build the ggplot

################################################################################
# Fixed color palette (remove this block if jv_palette is already defined elsewhere)
dist_1 <- lighten("#D5BD75", amount = -0.05)
dist_2 <- lighten("#FFEAB1", amount = -0.05)
ghg_1  <- lighten("#DA6098", amount = -0.05)
ghg_2  <- lighten("#FFCEE0", amount = -0.05)
nut_1  <- "#795B48"
nut_2  <- "#E7C7B5"
wat_1  <- "#3E638E"
wat_2  <- "#C5F0FF"

jv_palette <- c(dist_1, ghg_1, nut_1, wat_1,
                dist_2, ghg_2, nut_2, wat_2)

################################################################################
#Function 1 - eval2()
eval2=function(string) eval(parse(text=string))

#Function 2 - two-line regression with glm() (heteroskedasticity-robust standard errors)
reg2=function(f,xc,family="gaussian")
{
  #f: formula as in y~x1+x2+x3; the first predictor is the one the u-shape is tested on
  #xc: where to set the breakpoint, a number
  
  #(1) Extract variable names from formula
  y.f=all.vars(f)[1]
  x.f=all.vars(f)[2]
  var.count=length(all.vars(f))
  if (var.count>2) nox.f=drop.terms(terms(f),dropx=1,keep.response = T)
  
  #1.2 Key variables
  xu=eval2(x.f)
  yu=eval2(y.f)
  
  #1.4 xc is included in the first line
  xlow1  =ifelse(xu<=xc,xu-xc,0)
  xhigh1=ifelse(xu>xc,xu-xc,0)
  high1 =ifelse(xu>xc,1,0)
  
  #1.5 xc is included in the second line
  xlow2=ifelse(xu<xc,xu-xc,0)
  xhigh2=ifelse(xu>=xc,xu-xc,0)
  high2=ifelse(xu>=xc,1,0)
  
  #(2) Interrupted regressions
  if (var.count>2)
  {
    glm1.f=update(nox.f,~ xlow1+xhigh1+high1+.)
    glm2.f=update(nox.f,~ xlow2+xhigh2+high2+.)
  }
  if (var.count==2)
  {
    glm1.f=as.formula("yu~ xlow1+xhigh1+high1")
    glm2.f=as.formula("yu~ xlow2+xhigh2+high2")
  }
  glm1=glm(as.formula(format(glm1.f)),family=family)
  glm2=glm(as.formula(format(glm2.f)),family=family)
  
  #2.3 Robust standard errors
  rob1=coeftest(glm1, vcov=vcovHC(glm1,"HC3"))
  rob2=coeftest(glm2, vcov=vcovHC(glm2,"HC3"))
  
  msg=""
  if (is.na(rob1[2,4]))
  {
    rob1=coeftest(glm1, vcov=vcovHC(glm1,"HC1"))
    msg=paste0(msg,"\nFor line 1 the heteroskedastic standard errors HC3 resulted in an error thus we used HC1 instead.")
  }
  if (is.na(rob2[2,4]))
  {
    rob2=coeftest(glm2, vcov=vcovHC(glm2,"HC1"))
    msg=paste0(msg,"\nFor line 2 the heteroskedastic standard errors HC3 resulted in an error thus we used HC1 instead.")
  }
  
  #2.4 Slopes, z-values, p-values
  b1=as.numeric(rob1[2,1]);  b2=as.numeric(rob2[3,1])
  z1=as.numeric(rob1[2,3]);  z2=as.numeric(rob2[3,3])
  p1=as.numeric(rob1[2,4]);  p2=as.numeric(rob2[3,4])
  
  #3) Is the u-shape significant? (either direction; see kuznets.summary() for the inverted U)
  u.sig =ifelse(b1*b2<0 & p1<.05 & p2<.05,1,0)
  
  list(b1=b1,p1=p1,b2=b2,p2=p2,u.sig=u.sig,xc=xc,z1=z1,z2=z2,
       glm1=glm1,glm2=glm2,rob1=rob1,rob2=rob2,msg=msg)
}  #End of reg2() function


################################################################################
#Function 3 - kuznets.summary()
kuznets.summary=function(res, alpha=.05)
{
  pos1      = res$b1>0
  neg2      = res$b2<0
  sig1      = res$p1<alpha
  sig2      = res$p2<alpha
  inv.signs = pos1 & neg2
  both.sig  = sig1 & sig2
  shape.ok  = identical(res$shape,"inv-ushape")
  
  u.shape.sig = (res$b1<0 & res$b2>0 & both.sig)
  
  evidence = if (inv.signs & both.sig & shape.ok)  "Strong: significant inverted U"
  else if (inv.signs & both.sig)             "Moderate: significant inverted U, but quadratic suggests U-shape"
  else if (inv.signs & (sig1 | sig2))        "Weak: right signs, only one slope significant"
  else if (u.shape.sig)                      "Reversed: significant U-shape"
  else                                       "None"
  
  data.frame(
    kuznets_b1          = res$b1,
    kuznets_p1          = res$p1,
    kuznets_b2          = res$b2,
    kuznets_p2          = res$p2,
    kuznets_breakpoint  = as.numeric(res$xc),
    kuznets_inv_signs   = inv.signs,
    kuznets_both_sig    = both.sig,
    kuznets_shape_ok    = shape.ok,
    kuznets_strong      = inv.signs & both.sig & shape.ok,
    kuznets_evidence    = evidence,
    stringsAsFactors    = FALSE
  )
}


################################################################################
#Function 4 - twolines()
#Arguments:
#  f            formula, e.g. nutrient_percap_d ~ pc_gdp_d (plain variable names only)
#  data         data frame; must contain the columns region_d and iso3_d
#  alpha        significance level used for the Kuznets summary AND for solid vs dashed lines
#  x.scale      divide the x variable by this number before testing AND plotting (e.g. 1000)
#  n.labels     number of iso3_d labels to show (the points furthest from the fitted lines); Inf = all
#  line.cols    colors of line 1 (left of breakpoint) and line 2 (right of breakpoint)
#  xlab, ylab, color.lab  axis / legend titles (defaults: variable names, "Region")
#Each line is solid if its slope is significant (p < alpha) and dashed otherwise.
#Points are always colored by region_d (using jv_palette) and labeled with iso3_d.
#Returns a list: $results (one-row data frame), $plot (ggplot object), $fit (full reg2 output)
twolines=function(f,data=NULL,alpha=.05,
                  x.scale=1,
                  n.labels=10,
                  line.cols=c("grey20","grey20"),
                  xlab=NULL,ylab=NULL,color.lab="Region")  {
  data.name=deparse(substitute(data))
  
  # transformations/interactions inside the formula would be silently ignored; create the variable first
  if (!identical(attr(terms(f),"term.labels"), all.vars(f)[-1]) || deparse(f[[2]])!=all.vars(f)[1])
    stop("The formula must contain plain variable names only (no log(), I(), interactions); create transformed variables in the data first")
  if (!all(c("region_d","iso3_d") %in% names(data)))
    stop("data must contain the columns region_d and iso3_d")
  
  #Rescale x before testing, so the breakpoint is in the same units as the plot
  data[[all.vars(f)[2]]] = data[[all.vars(f)[2]]] / x.scale
  
  # clean-up before attach(), so a failed call does not leave 'data' attached
  n.attached=sum(search()=="data")
  on.exit(while (sum(search()=="data")>n.attached) detach("data"), add=TRUE)
  attach(data)
  
  #(1) Extract variable names
  y.f=all.vars(f)[1]
  x.f=all.vars(f)[2]
  var.count=length(all.vars(f))
  if (var.count>2) nox.f=drop.terms(terms(f),dropx=1,keep.response = T)
  
  #1.1 Drop rows with missing values on any of the variables being used
  vars=all.vars(f)
  cols=c()
  for (var in vars) cols=c(cols, which(names(data)==var))
  full.rows=complete.cases(data[,cols])
  data=data[full.rows,]
  detach(data)
  attach(data)
  
  #1.2 Key variables
  xu=eval2(x.f)
  yu=eval2(y.f)
  
  #1.3 Smoother formula
  unique.x=length(unique(xu))
  sx.f=paste0("s(",x.f,",bs='cr', k=min(10,",unique.x,"))" )
  
  #2 Run smoother
  if (var.count>2)  gam.f=paste0(format(nox.f),"+",sx.f)
  if (var.count==2) gam.f=paste0("yu~",sx.f)
  gams=gam(as.formula(gam.f))
  
  #(3) Generate yobs (dots)
  if (var.count==2) yobs=yu
  
  if (var.count>2) {
    data.obs=as.data.frame(matrix(nrow=length(xu),ncol=var.count))
    colnames(data.obs)=all.vars(f)
    for (i in 1:(var.count)) data.obs[,i]=eval(as.name(all.vars(f)[i]))
    data.obs=na.omit(data.obs)
    
    data.xufixed    =data.obs
    data.xufixed[,2]=mean(data.obs[,2])
    
    yhat.xufixed=predict.gam(gams,newdata = data.xufixed)
    yobs = yu-yhat.xufixed
    
    data.otherfixed              = data.obs
    for (i in 3:var.count) {
      if (is.factor(data.obs[,i]) || is.character(data.obs[,i])) {
        xt=sort(data.obs[,i]); n.xt=length(xt)
        data.otherfixed[,i]=xt[round(n.xt/2,0)]
      } else data.otherfixed[,i]=mean(data.obs[,i])
    }
  }
  
  #4) Fitted values at sample means for covariates
  if (var.count>2)   g.fit=predict.gam(gams,newdata = data.otherfixed,se.fit=TRUE)
  if (var.count==2)  g.fit=predict.gam(gams,se.fit=TRUE)
  y.hat=g.fit$fit
  y.se =g.fit$se.fit
  
  #5) Most extreme fitted value
  xu2=xu^2
  if (var.count>2)  lmq.f=update(nox.f,~xu+xu2+.)
  if (var.count==2) lmq.f=yu~xu+xu2
  lmq=lm(as.formula(format(lmq.f)))
  bqs=lmq$coefficients
  bx1= bqs[2]
  bx2=bqs[3]
  x0=min(xu)
  s0=bx1+2*bx2*x0
  if (s0>0)  shape='inv-ushape'
  if (s0<=0) shape='ushape'
  
  #5.2 Middle 80% of data
  x10=quantile(xu,.1)
  x90=quantile(xu,.9)
  middle=(xu>x10 & xu<x90)
  x.middle=xu[middle]
  
  y.hat=y.hat[middle]
  y.se=y.se[middle]
  
  y.ub=y.hat+y.se
  y.lb=y.hat-y.se
  
  if (shape=='inv-ushape') y.most=max(y.hat)
  if (shape=='ushape')     y.most=min(y.hat)
  
  x.most=x.middle[match(y.most, y.hat)]
  
  if (shape=='inv-ushape') flat=(y.ub>y.most)
  if (shape=='ushape')     flat=(y.lb<y.most)
  xflat=x.middle[flat]
  
  #6 RUN TWO LINE REGRESSIONS
  rmid=reg2(f,xc=median(xflat))
  z1=abs(rmid$z1)
  z2=abs(rmid$z2)
  xc=quantile(xflat,z2/(z1+z2))
  res=reg2(as.formula(format(f)),xc=xc)
  
  #7 Add other results to the output
  res$yobs       = yobs
  res$y.hat      = y.hat
  res$y.ub       = y.ub
  res$y.lb       = y.lb
  res$y.most     = y.most
  res$x.most     = x.most
  res$midflat    = median(xflat)
  res$shape      = shape
  res$xflat      = xflat
  res$kuznets    = kuznets.summary(res, alpha=alpha)
  
  #8 BUILD THE GGPLOT
  xc=as.numeric(xc)
  plot.df=data.frame(x=xu, y=as.numeric(yobs),
                     region=data[["region_d"]],
                     lab=as.character(data[["iso3_d"]]))
  
  #Each line has the estimated slope (b1, b2) and passes through the centroid of the points on its side.
  left =plot.df$x<=xc
  right=plot.df$x>=xc
  line1=data.frame(x=c(min(plot.df$x), xc))
  line1$y=mean(plot.df$y[left]) + res$b1*(line1$x-mean(plot.df$x[left]))
  line2=data.frame(x=c(xc, max(plot.df$x)))
  line2$y=mean(plot.df$y[right]) + res$b2*(line2$x-mean(plot.df$x[right]))
  
  #Solid line if the slope is significant, dashed otherwise
  lt1=if (res$p1<alpha) "solid" else "dashed"
  lt2=if (res$p2<alpha) "solid" else "dashed"
  
  #Label only the n.labels points furthest from the fitted lines
  fit.y=ifelse(plot.df$x<=xc,
               mean(plot.df$y[left])  + res$b1*(plot.df$x-mean(plot.df$x[left])),
               mean(plot.df$y[right]) + res$b2*(plot.df$x-mean(plot.df$x[right])))
  plot.df$resid=abs(plot.df$y-fit.y)
  lab.df=plot.df[order(-plot.df$resid)[seq_len(min(n.labels,nrow(plot.df)))],]
  
  p=ggplot(plot.df, aes(x=x, y=y))+
    geom_point(aes(color=region))+
    geom_line(data=line1, aes(x=x, y=y), inherit.aes=FALSE,
              color=line.cols[1], linetype=lt1, linewidth=1.1)+
    geom_line(data=line2, aes(x=x, y=y), inherit.aes=FALSE,
              color=line.cols[2], linetype=lt2, linewidth=1.1)+
    geom_vline(xintercept=xc, linetype="dotted", color="grey40")+
    geom_text_repel(data=lab.df, aes(label=lab), size=3, max.overlaps=Inf)+
    scale_color_manual(values=jv_palette)+
    labs(x=if (is.null(xlab)) x.f else xlab,
         y=if (is.null(ylab)) y.f else ylab,
         color=color.lab)+
    theme_minimal()
  
  #9 Return
  list(results = data.frame(datasource = data.name, footprint = all.vars(f)[1], res$kuznets),
       plot    = p,
       fit     = res)
} #End function


# ---- Example: nutrients ----
tl_nutrients <- twolines(f = nutrient_percap_d ~ pc_gdp_d,
                         data = total_stressor_percap,
                         x.scale = 1000,
                         xlab = "GDP per capita (1000)",
                         ylab = "Nutrient per capita")

kuznets_total_nutrients <- tl_nutrients$plot   # ggplot object
kuznets_total_nutrients
tl_nutrients$results                           # one-row summary table