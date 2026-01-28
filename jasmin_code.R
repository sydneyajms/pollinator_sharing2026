BeeNos <- read.csv("./BEE IDs - RF 24 - Sheet1.csv") 

BeePlants <- read.csv("./Specimens_Riverfront_JA - Sheet1.csv")

#fpid <- factor(BeePlants$)
gdDate <- BeePlants[BeePlants$FieldPlantID=='GD',]$Date
BeePlants[BeePlants$Date==gdDate,]
gerdis <- BeePlants[BeePlants$FieldPlantID=='GERDIS',]$Unique.ID

BeeNos[BeeNos$sp.no%in%gerdis,]


BeePlants[BeePlants$FieldPlantID=='GC',]



# read in cleaned data
pollinators <- read.csv("./Pollinator_data_cleaned.csv")
seeds <- read.csv("./Cleaned_seeds.csv")

head(seeds)


trts <- unique(seeds$genSpec)
clist <- vector(mode='list', length=length(trts)) #list of communities (treatments)
names(clist) <- trts
for (tt in trts){
  tmp <- seeds[seeds$genSpec==tt,]
  plot <- unique(tmp$Plot)
  block <- unique(tmp$Block)
  #pl_sp <- unique(tmp$sp)
  
  polls <- pollinators[pollinators$Block %in% block & pollinators$Plot %in% plot,]
  ppmat <- as.matrix(table(polls$plant, polls$Genus))
  clist[[tt]] <- ppmat
}

H_values <- lapply(clist, networklevel, index="H2")
overlap <- lapply(clist, grouplevel, index='niche overlap')
lapply(clist, plotweb)
par(mfrow=c(2,3))
lapply(clist, visweb, text='interaction', labsize=.8)

cols <- hcl.colors(51, "PuBuGn", rev=T)
zmax <- max(unlist(clist))
par(mfrow=c(2,3), mar=c(1,1,1,1), bty='n')
for (i in trts){
  Opolls <- order(colSums(clist[[i]]), decreasing=T)
  Oplants <- order(rowSums(clist[[i]]), decreasing=F)
  tmp <- clist[[i]][Oplants, Opolls]
  image(t(tmp), zlim=c(0, zmax), col=cols, asp=1)
}



library(RColorBrewer)
cols <- hcl.colors(10, "YlOrRd")
cols <- brewer.pal(9, "YlOrRd")
par(mfrow =c(2,3))
lapply(clist, visweb, box.col = "black", def.col = cols, 
 labsize = 0.8, text = "interaction", textcol = "white")

# join bee data to plant data 
network <- pollinators %>%
  inner_join(seeds, by = "Stand", relationship = "many-to-many") %>%
  subset(select = -c(X, Year1, Site, Year, DoyStart)) %>%
  rename("Pollinator" = GenusSpecies, "Plant" = PlantGenusSpecies.y) 

# split into each treatment
treatment_df <- split(network, network$genSpec, drop = FALSE)

# make every treatment into adjaceny matrix list
WHOLE_LIST <- vector("list", length = length(treatment_df))
for(i in seq_along(treatment_df)){
  ThisMix <- treatment_df[[i]] %>%
    ungroup() %>%
    # complete(Plant = all_plants, Pollinator = all_pollinators, fill = list(interactions = 0)) %>%
    group_by(sp) %>%
    summarize(interactions = n(), .groups = "drop") %>%
    # pivot_wider(names_from = Pollinator, values_from = interactions, values_fill = 0) %>%
    # column_to_rownames( var = "Plant")
    # ThisMix[is.na(ThisMix)] <- 0
    # ThisMix[ThisMix == 1] <- 0
    WHOLE_LIST[[i]] <- ThisMix
    names(WHOLE_LIST) <- names(Stand_df)
}







#whole experiment network:
pp_all <- as.matrix(table(pollinators$plant, pollinators$Genus))



library(bipartite)


head(seeds)





