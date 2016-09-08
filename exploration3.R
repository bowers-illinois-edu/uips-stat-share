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
  fig.path='figs/fig',
  cache=FALSE,
  highlight=TRUE,
  width.cutoff=132,
  size='footnotesize',
  out.width='.9\\textwidth',
  fig.retina=FALSE,
  message=FALSE,
  comment=NA)

## ----initialize,echo=FALSE-----------------------------------------------
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

## ------------------------------------------------------------------------
download.file("http://jakebowers.org/Data/ANES/anes_pilot_2016_csv.zip",destfile="anespilot2016.csv.zip")
unzip("anespilot2016.csv.zip")
anespi16<-read.csv("anes_pilot_2016.csv",as.is=TRUE,strip.white=TRUE)

