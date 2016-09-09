## ----include=FALSE, cache=FALSE------------------------------------------
# Some customization.  You can alter or delete as desired (if you know what you are doing).
# knitr settings to control how R chunks work.

## To make the html file do
## render("exploration1.Rmd",output_format=html_document(fig_retina=FALSE))
## To make the pdf file do
## render("exploration1.Rmd",output_format=pdf_document())

require(knitr)
opts_chunk$set(
  tidy=FALSE,     # display code as typed
  size="small",    # slightly smaller font for code
  echo=TRUE,
  results='markup',
  strip.white=TRUE,
  cache=FALSE,
  highlight=TRUE,
  width.cutoff=132,
  size='footnotesize',
  fig.retina=FALSE,
  message=FALSE,
  comment=NA)

## ----initialize-b,echo=FALSE---------------------------------------------
##First, just setup the R environment for today:
if(!file.exists('figs')) dir.create('figs')

options(SweaveHooks=list(fig=function(){
			   par(mar=c(3.5, 3, 1.1, 0),
			       pty="s",
			       mgp=c(1.5,0.5,0),
			       oma=c(0,0,0,0))},
			 echo=function(){options(continue=" ") ##Don't show "+" prompts,
			 options(prompt=" ")
			 }),
	digits=4,
	scipen=8,
	width=132
	)
options(error=function(){options(prompt="> ",continue="+ ");NULL})

## ----tidy=FALSE----------------------------------------------------------
load(url("http://jakebowers.org/Data/ho05.rda"))
wrkdat<-ho05[!is.na(ho05$hlphrs)&ho05$Rage!=0,] ## removing bad obs
effect1<-coef(lm(hlphrs~postbomb,data=wrkdat))[["postbomb"]]
effect1

covariatesLabels <- c( "GOR" = "Government Office Region",
		      "Rsex" = "Gender",
		      "Rdmstat" = "Respondent de facto marital status",
		      "Rage" = "Age",
		      "Ethnic11" = "Ethnicity",
		      "RILO4A" = "Economic Status",
		      "hhinc" = "Household Income",
		      "ZQuals1" = "Highest qualification: includes 70+ (???)",
		      "DVHSize" = "Number of people in household",
		      "immigrant" = "Immigrant",
		      "workstatus" = "Employment status",
		      "SLive" = "Years lived in neighborhood",
		      "relig.and.act" = "Interaction of religion and practicing questions",
		      "Rnssec17" = "NSSec grouped into 17 categories (???)",
		      "HTen1" = "Owns or rents",
		      "Rage5cat" = "5 level categorical variable for age",
		      "hhinc5cat" = "5 level cateogrical variable for household income",
		      "DVHSizeCat" = "Categorical coding of household size: 1, 2, 3, or 4+" ,
		      "SLive5cat" = "5 level cateogrical variable for years lived in neighborhood")

covariates <- names(covariatesLabels)

## ------------------------------------------------------------------------
effect2lm<-lm(hlphrs~postbomb+SLive,data=wrkdat)
effect3lm<-lm(hlphrs~postbomb+hhinc,data=wrkdat)

## ----echo=FALSE,out.width=".95\\textwidth",fig.width=12,fig.height=6-----
par(mfrow=c(1,2),pty="s",mgp=c(1.5,.5,0),oma=rep(0,4))

with(wrkdat,{
       plot(SLive,hlphrs,
	    pch=c(1,19)[postbomb+1],
	    cex=c(1.3,.8)[postbomb+1],
	    col=c("black","gray")[postbomb+1])
       abline(coef(effect2lm)[1]+coef(effect2lm)[2]*1,coef(effect2lm)[3],col="gray",lwd=2)
       abline(coef(effect2lm)[1]+coef(effect2lm)[2]*0,coef(effect2lm)[3],lwd=2)
       plot(hhinc,hlphrs,
	    pch=c(1,19)[postbomb+1],
	    cex=c(1.3,.8)[postbomb+1],
	    col=c("black","gray")[postbomb+1])
       abline(coef(effect3lm)[1]+coef(effect3lm)[2]*1,coef(effect3lm)[3],col="gray",lwd=2)
       abline(coef(effect3lm)[1]+coef(effect3lm)[2]*0,coef(effect3lm)[3],lwd=2)
}
)
legend(x="topright",pch=c(1,19),
       col=c("black","gray"),
       legend=c("PreBomb","PostBomb"))

## ------------------------------------------------------------------------
effect4lm<-lm(hlphrs~postbomb+hhinc+SLive,data=wrkdat)

## ----out.width=".5\\textwidth", fig.align="center"-----------------------
with(wrkdat,{ plot(hhinc,SLive,
		   pch=c(1,19)[postbomb+1],
		   cex=c(1.3,.8)[postbomb+1],
		   col=c("black","gray")[postbomb+1]) })


## ----results="hide"------------------------------------------------------

lm1<-lm(hlphrs~GOR + Rsex + Rdmstat + Rage + Ethnic11 + RILO4A + hhinc +
	ZQuals1 + DVHSize + immigrant + workstatus + SLive + relig.and.act +
	Rnssec17 + HTen1, data=wrkdat)

hlphrsWithNoCovs<-residuals(lm1)

lm2<-lm(postbomb~GOR + Rsex + Rdmstat + Rage + Ethnic11 + RILO4A + hhinc +
	ZQuals1 + DVHSize + immigrant + workstatus + SLive + relig.and.act +
	Rnssec17 + HTen1, data=wrkdat)

postbombWithNoCovs<-residuals(lm2)

effect5lm<-lm(hlphrsWithNoCovs~postbombWithNoCovs)
coef(effect5lm)[[2]]

lmbig<-lm(hlphrs~postbomb+GOR + Rsex + Rdmstat + Rage + Ethnic11 + RILO4A + hhinc +
	ZQuals1 + DVHSize + immigrant + workstatus + SLive + relig.and.act +
	Rnssec17 + HTen1, data=wrkdat)


## ----results='hide'------------------------------------------------------
library(RItools)

balfmla<-reformulate(covariates[1:15],response="postbomb")

xb1<-xBalance(balfmla,data=wrkdat,
	      report=c("std.diffs","z.scores","adj.means",
                    "adj.mean.diffs", "chisquare.test","p.values"))

print(xb1,horizontal=TRUE,show.pvals=TRUE)
## xb1$overall
## xb1$results


## ----results="hide"------------------------------------------------------
summary(abs(xb1$results[,"std.diff","unstrat"]))
xb1$results[order(abs(xb1$results[,"std.diff","unstrat"])),"std.diff","unstrat"]
xb1$results["Ethnic11White",,]
xb1$results[c("hhinc","hhinc.NATRUE"),,]
xb1$overall

## ------------------------------------------------------------------------
library(optmatch)

newdata<-fill.NAs(balfmla,data=wrkdat) ## We want to match on missingness too
names(newdata)<-make.names(names(newdata))

propensityModel<-glm(postbomb~.,data=newdata,family=binomial(link="logit"))
psDistMat<-match_on(propensityModel,data=newdata)

mhDistMat<-match_on(postbomb~.,data=newdata,method="rank_mahalanobis")
summary(as.vector(psDistMat))

##tmp <- wrkdat$Rage
##names(tmp) <- rownames(wrkdat)
##absdist <- match_on(tmp, z = wrkdat$postbomb,
##                  within = exactMatch(postbomb ~ immigrant, wrkdat))


## Following Rosenbaum Chapter 8: mahalanobis distance with propensity caliper
## fm1<-fullmatch(mhDistMat+caliper(psDistMat,1)+caliper(absdist,10),data=newdata)
fm1<-fullmatch(mhDistMat+caliper(psDistMat,1),data=newdata)

summary(fm1,max.controls=Inf,propensity.model=propensityModel)
table(matched(fm1))

xb2<-xBalance(postbomb~.,strata=list(unstrat=NULL,fm1=~fm1),
	      data=newdata,
	       report=c("std.diffs","z.scores","adj.means",
                    "adj.mean.diffs", "chisquare.test","p.values"))


wrkdat$fm1<-as.factor(fm1)
effect6lm<-lm(hlphrs~postbomb+fm1,data=wrkdat)
coef(effect6lm)[[2]]

xBalance(postbomb~hlphrs,strata=list(unstrt=NULL,fm1=~fm1),
	 data=wrkdat,report=c("std.diffs","z.scores","adj.means",
                    "adj.mean.diffs", "chisquare.test","p.values"))


## ------------------------------------------------------------------------
save(fm1,file="fm1.rda")

