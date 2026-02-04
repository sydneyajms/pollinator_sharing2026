install.packages("bipartite")
library(bipartite)

webnames <- c("barrett1987","bezerra2009","elberling1999",
                 "inouye1988","junker2013","kato1990","kevan1970"
                 ,"memmott1999","mosquin1967","motten1982",
                 "olesen2002aigrettes","olesen2002flores",
                 "olito2015","ollerton2003","schemske1978",
                 "small1976")
weblist <- list(barrett1987,bezerra2009,elberling1999,
                inouye1988,junker2013,kato1990,kevan1970
                ,memmott1999,mosquin1967,motten1982,
                olesen2002aigrettes,olesen2002flores,
                olito2015,ollerton2003,schemske1978,
                small1976)
names(weblist) <- webnames

par(srt=0,xpd=NA)
for (w in seq_along(webnames)){
  plotweb(weblist[[w]], text_size = .4,srt=30)
  rect(-.06,1.06,1.06,1.1,col=adjustcolor("white",0.6))
  text(-.05,1.08,webnames[w],col="navy",adj=0)
  no <- grouplevel(weblist[[w]],"niche overlap")
  no <- round(no,3)
  text(1.05,1.08,paste(names(no),no,collapse=" "),adj=1,col="navy")
}

#higher = pollinators
#lower = plants
#high niche overlap.HL = many pollinators visit the same plants
  #barrett1987, schemske1978, ollerton2003, 
#high niche overlap.LL = many plants share the same pollinators
  #bezerra2009, oslen2002aigrettes, 
#high-high = everyone visits everyone
#low-low = recipricol specialization
#ideal for pollen transfer but not for attraction

#visitation (pollinator quantity, richness) to focal plant 
#increases when 
  #plants have high overlap?

#seed set (pollinator quality) of focal plant
#increases when 
  #pollinator overlap is low?

#best case scenario is when high-low?




plotweb(barrett1987)

plotweb(bezerra2009)
grouplevel(bezerra2009,"niche overlap","higher")

plotweb(elberling1999)
plotweb(inouye1988)
plotweb(junker2013)
plotweb(kato1990)
plotweb(kevan1970)
#rect(-.09,1.06,0.017*(nchar(webnames[w])-4),1.1,col=adjustcolor("white",0.6))
#rect(0.44,1.06,1.06,1.1,col=adjustcolor("white",0.6))