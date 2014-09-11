library(devtools)
install_github('rentrez','ropensci')
install_github('rmetadata','ropensci')
install_github('rcrossref','ropensci')
install_github('rplos','ropensci')
install_github('plotly','ropensci')



options(stringsAsFactors = F)
library(rcrossref)
library(rentrez)
library(rmetadata)
library(rplos)
library(plotly)
library(XML)

### Data for web of science
years <- 1995:2013
wosDat <- c(0,0,1,1,0,1,2,5,2,3,11,10,12,15,26,42,38,70,74)
wosTot <- c(1988832,2212870,2487627,3147376,3179260,3336860,3384475,3642685, 3965098,4045333,
            4288723,4567485,4824696,5507441,5733786,5719911,6028404,6578979, 7128743)


osPubDF <- data.frame(cbind(years,(wosDat/wosTot)*1000))
osPubDF$Source <- rep("Web of Science",dim(osPubDF)[1])

colnames(osPubDF) <- c("Year","Percent","Source")

papers_by_year <- function(years, search_term) {
  return(sapply(years, function(y) entrez_search(db = "pubmed", term = search_term, 
                                                 mindate = y, maxdate = y, retmax = 0)$count))
}

total_papers <- papers_by_year(years, "")
opensci <- c("'open data'")
trend_data <- sapply(opensci, function(t) papers_by_year(years, t))
trend_props <- data.frame((trend_data/total_papers)*1000)
trend_props$years <- years
#trend_data <- data.frame(trend_data)
#trend_data$years <- years
  
trend_df <- melt(as.data.frame(trend_props), id.vars = "years")
trend_df$Source <- rep("PubMed",dim(trend_df)[1])
trend_df <- trend_df[,c(1,3,4)]
colnames(trend_df) <-  c("Year","Percent","Source")

osPubDF <- rbind(osPubDF,trend_df)

### We can also get data for plos one papers
# Get raw plos counts.
plosKey <- "K7AUTjsEK1C149dS_Aqn"
plosXML <- xmlInternalTreeParse("http://api.plos.org/search?q=*:*&rows=0&facet=true&facet.range=publication_date&facet.range.start=NOW/YEAR-10YEAR&facet.range.end=NOW/YEAR%2B1YEAR&facet.range.gap=%2B1YEAR&api_key=KEY")
plosCounts <- as.numeric(unlist(xpathApply(plosXML, "//int",xmlValue)))
plosYears <- unlist(lapply(xpathApply(plosXML, "//int",xmlGetAttr,"name"),function(x){return(as.numeric(strsplit(x,"-")[[1]][1]))}))
out <- searchplos(q='everything:"open science"~0', fl=c('title','publication_date','id'), fq='doc_type:full',limit=1000)
plYears <- unlist(lapply(strsplit(out$data$publication_date,"-"),function(x){return(x[1])}))
plDF <- data.frame(cbind(table(plYears),as.numeric(names(table(plYears)))))
colnames(plDF) <- c("count","year")
plDF$prop<- (plDF$count/plosCounts[plosYears%in%plDF$year])*1000
plDF$Source <- rep("PLoS",dim(plDF)[1])
#strip down to fit with trend_df
plDF <- plDF[plDF$year<2014,2:4]
colnames(plDF) <-  c("Year","Percent","Source")
osPubDF <- rbind(osPubDF,plDF)


p <- ggplot(osPubDF, aes(Year, Percent, colour = Source))
p <- p +geom_line()+geom_point(size=3)+scale_y_log10("tmp") + theme_bw()+ylab("Thousandth of a percent of all papers published in a given source") + xlab("Year")



ggsave("publication_plot.png",p,path = "~/scratch/open_sci_manuscript_figure",height=7,width=8)





### Get raw data from cross ref and plos

out <- searchplos(q='everything:"open science"~0', fl=c('title','publication_date','id'), fq='doc_type:full',limit=1000)
plYears <- unlist(lapply(strsplit(out$data$publication_date,"-"),function(x){return(x[1])}))
plDF <- data.frame(cbind(table(plYears),as.numeric(names(table(plYears)))))
colnames(plDF) <- c("count","year")


crOS <- cr_fundref_works(query='"open science"',limit = 1000)
## Grab all the years
crYears <- unlist(lapply(crOS$items,function(x){return(x$deposited$`date-parts`[[1]][1])} ))
crDF <- data.frame(cbind(table(crYears),as.numeric(names(table(crYears)))))
colnames(crDF) <- c("count","year")

rawDF <- rbind(plDF,crDF)
rawDF <- rawDF[rawDF$year<2014,]
rawDF$Source <- sort(rep(c("PLoS","CrossRef"),7),decreasing=T)
rawct <- ggplot(rawDF,aes(x=year,y=count,colour=Source,group=Source))+geom_line()+geom_point()+theme_bw()+ylab("Count of papers mentioning 'open science'")
ggsave("raw_os_count.png",rawct,path = "~/scratch/open_sci_manuscript_figure",height=7,width=8)


