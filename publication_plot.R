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

p <- ggplot(osPubDF, aes(Year, Percent, colour = Source))
p <- p + geom_point(size = 3) +geom_line() + theme_bw()+ylab("Thousandth of a percent of all papers published in a given source") + xlab("Year")
ggsave("publication_plot.png",p,path = "~/scratch/open_sci_manuscript_figure",height=7,width=8)



  
