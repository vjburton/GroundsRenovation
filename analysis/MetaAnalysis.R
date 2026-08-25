###### 1. Change working directory to project folder
#setwd("~/GroundsRenovation")
#setwd("C:/Users/victb/Documents/naturepark/GroundsRenovation")
## In column headings:
## "Current" always refers to estimates of the grounds as they are now
## "Proposed" refer to grounds estimates after renovation
## "Corrected" densities estimates are when the species density estimate have been scaled for area of the habitat

#################
## Functions
#################

source("R/GoogleSpreadsheets.R")
source("R/DataFormat.R")
source("R/ModelFunctions.R")

# For plotting model residuals
model_plot <- function(mod.for.plot){
	par(mfrow = c(1,3))
	qqnorm(resid(mod.for.plot))
	qqline(resid(mod.for.plot), col = 2)
	plot(fitted(mod.for.plot), resid(mod.for.plot),xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs fitted")
	abline(h=0, lty=2)
	lines(smooth.spline(fitted(mod.for.plot), resid(mod.for.plot)), col = "red")
	hist(resid(mod.for.plot))
  }

# Capitalising first word for plots
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}


#################
## Libraries
#################

#install.packages(c("MuMin", "Hmisc"))

library(lme4)
#library(maps)
#library(mapdata)
#library(maptools)
library(MuMIn)
library(dplyr)
library(car)
library(Hmisc)
library(xlsx)
library(GISTools)

#################
## Locations
#################

## Incase figures are to be saved

 if(!dir.exists("Figures")){
	dir.create("Figures")
 }
 figure_out <- "Figures"


## Incase models are to be saved

 if(!dir.exists("Models")){
	dir.create("Models")
 }
 models_out <- "Models"


#################
## Data
#################

garden <- read.csv("data/Wildlife Garden - data collection - Sheet1.csv")
garden <- prepareGS(garden) ## Makes sure all factors are factors, and that all coordinates are in decimal degrees. NAs due to some sites not have coordinates

#################
## Study info 
#################

length(unique(garden$Study.ID)) ## 34
sources <- gsub("\\ [0-9]$", "", garden$Study.ID)
length(unique(sources)) ## 25

#################
## Data Management
#################

## Uses convertArea function to convert habitat area and sampled area to consistent units
garden$Sampled.Area_metres <- convertArea(garden$Sampled.Area, garden$Sampled.Area.Units, "sample")
garden$Habitat.Area_metres <- convertArea(garden$Habitat.Area, garden$Habitat.Area.Units, "habitat")


## Need to convert species richness estimates to species density (scaled with area)
# "Flag" column so we know what has been converted to a species denisty
garden$SpeciesDensity <- FALSE

# When we know sampling area, coverting to species density per 10m2, and changing the "Flag"
## If the sampled area is NA, then just replace with the richness estimate (and change flag to false to indicate that is is not a density)
## If sampled area is not NA, convert richness to density estimate, and put flag as true
garden$Corrected_Taxon.Richness <- ifelse(is.na(garden$Sampled.Area_metres), garden$Taxon.Richness, calculate_density_cSAR(S=garden$Taxon.Richness, A=garden$Sampled.Area_metres, z=0.1, newA = 10)) ## 
garden$SpeciesDensity <- ifelse(is.na(garden$Sampled.Area_metres), FALSE, TRUE)

# When we know habitat area, coverting to species density per 10m2, and changing the "Flag"
## If the sampled area is NA and the habitat area is not NA, convert richness to density estimate, and put flag as true
## If sampled area and habtiat area is NA, the flag stays as false
garden$Corrected_Taxon.Richness <- ifelse(is.na(garden$Sampled.Area_metres) & !(is.na(garden$Habitat.Area_metres)), calculate_density_cSAR(S=garden$Taxon.Richness, A=garden$Habitat.Area_metres, z=0.1, newA=10), garden$Corrected_Taxon.Richness)
garden$SpeciesDensity <- ifelse(is.na(garden$Sampled.Area_metres) & !(is.na(garden$Habitat.Area_metres)), TRUE, garden$SpeciesDensity)

## Dealing with the sampling area units which are not area
levels(garden$Sampled.Area.Units)
not_areas <- c("Minutes (pond net)", "Minutes (sweep netting)")
## As they are not areas, they will not be species density (despite not being NA)
## If they are not areas, change the Corrected_Taxon.Richness back to the unscaled estimates, and change flag to false
garden$Corrected_Taxon.Richness <- ifelse(garden$Sampled.Area.Units %in% not_areas, garden$Taxon.Richness, garden$Corrected_Taxon.Richness)
garden$SpeciesDensity <- ifelse(garden$Sampled.Area.Units %in% not_areas, FALSE, garden$SpeciesDensity) 

hist(garden$Corrected_Taxon.Richness)

###########################################################
## SAMPLING AREA COMPARISONS
## Species Density models

#########
## Data
#########
sampled_area <- garden[garden$SpeciesDensity == TRUE,] ## Only species density estimates
sampled_area <- sampled_area[sampled_area$Taxonimic.Level == "Species",] ## Only measures of species
sampled_area <- sampled_area[complete.cases(sampled_area$Taxon.Richness),]## 
sampled_area <- droplevels(sampled_area)


table(sampled_area$Habitat, sampled_area$Study.ID)
table(sampled_area$Habitat)

## Some habitats have too few data. These are removed
not_enough <- c("Not present in NHM", "species-poor hedgerow", "Unsure/Not Clear", "Unspecified grass/meadows", "orchard", "ferns and cycad plantings", "hard standing")
sampled_area <- sampled_area[!(sampled_area$Habitat %in% not_enough),] ## 325

## Need studies that have sampled more than one habitat
sample_studies <- as.data.frame(aggregate(sampled_area$Habitat, list(sampled_area$Study.ID), function(x){N = length(unique(x, na.rm=TRUE))}))
sample_studies <- sample_studies[sample_studies$x > 1,]
## The studies with only one habitat in are amenity grasslands (pretty much)
sampled_area <- sampled_area[sampled_area$Study.ID %in% sample_studies$Group.1,] ## 180
sampled_area <- droplevels(sampled_area)

## Checking data
table(sampled_area$Habitat)
tapply(sampled_area$Corrected_Taxon.Richness, sampled_area$Habitat, summary)
table(sampled_area$Study.ID, sampled_area$Habitat)
table(sampled_area$Study.ID, sampled_area$Taxa)

## This study is removed
exclude_these_studies <- c("2003_Thompson 1")
sampled_area <- sampled_area[!(sampled_area$Study.ID %in% exclude_these_studies),] # 176
sampled_area <- droplevels(sampled_area) 

sampled_area$Habitat <- relevel(sampled_area$Habitat, ref = "broadleaved woodland")

############
## Models
############
taxa <- sampled_area$Taxa
levels(taxa)[levels(taxa) != "Plants"] <- "Inverts" ## Coarse classification
table(sampled_area$Habitat, taxa)
density <- round(sampled_area$Corrected_Taxon.Richness)

## Not enough data for this model 
# density1 <- glmer(density ~ Habitat * taxa + (1|Study.ID), data = sampled_area, family = poisson) 

density2 <- glmer(density ~ Habitat + taxa + (1|Study.ID), data = sampled_area, family = poisson)
density3 <- glmer(density ~ Habitat + (1|Study.ID), data = sampled_area, family = poisson)
anova(density2, density3) # Not significant
density4 <- glmer(density ~ taxa + (1|Study.ID), data = sampled_area, family = poisson)
anova(density2, density4) ## Significant. Remove taxa
density5 <- glmer(density ~ 1 + (1|Study.ID), data = sampled_area, family = poisson)
anova(density3, density5) ## Significant. Keep habitat
summary(density3)
Anova(density3)
model_plot(density3)

# save(density3, file = file.path(models_out, "FinalDensityModel.rda"))
# load(file.path(models_out, "FinalDensityModel.rda"))

density3_means <- model_Means(density3)


#############################################
### Other habitats
#############################################

shrubs <- read.csv("data/AdditionalData.csv") ## From Strong et al. 1979
trees <- shrubs$Species.Richness[shrubs$Host == "Trees" & shrubs$Status == "Middle"]
introduced_shrubs <- shrubs$Species.Richness[shrubs$Host == "Shrubs" & shrubs$Status == "Middle"]
introduced_shrubs_coef <- introduced_shrubs/trees
angiosperm_shrubs <- shrubs$Species.Richness[shrubs$Host == "Shrubs" & shrubs$Status == "Middle"]
angiosperm_shrubs_coef <- angiosperm_shrubs/trees

scriven <- garden[garden$Study.ID == "2013_Scriven 1",]
species_poor_hedge <- scriven$Taxon.Richness[scriven$Habitat == "species-poor hedgerow"]
species_rich_hedge <- scriven$Taxon.Richness[scriven$Habitat == "species-rich hedgerow"]
species_poor_hedge_coef <- species_poor_hedge/species_rich_hedge


#############################################
### BIODIVERSITY CHANGE FOR SPECIES DENSITY MODEL
#############################################

habitat_areas <- data.frame(
Habitat = c("Broadleaved woodland", "Acid grassland (heath)", "Chalk grassland", "Neutral grassland", "Fen (incl. reedbed)", "Marginal vegetation (pond edge)", "Ponds", "Green roof", "Species-poor hedgerow", "Species-rich hedgerow", "Short/perennial vegetation", "Amenity grass/turf", "Introduced shrubs", "Hard standing", "Fern and cycad planting", "Agricultural plants", "Paleogene Asteraceae", "Neogene grass", "Cretaceous Angiosperm shrubs", "Total"),
Current_area_m2 = c(1978.36, 111.71, 344.58, 2103.15, 64.6, 163.6, 341.28, 9.98, 109, 121.87, 423.65, 3303.63, 2218.62, 10415, 0, 0, 0, 0, 0, NA),
Proposed_area_m2=c(3477.67, 82.1, 526, 2133.45, 133.86, 99.15, 459.37, 0, 0, 607.5, 0, 1573.91, 1346.69, 9525.16, 739.82, 583.97, 176.57, 156.27, 244.97,NA)
)


totalCurrent <- sum(habitat_areas$Current_area_m2[1:19], na.rm = TRUE)
totalProposed <- sum(habitat_areas$Proposed_area_m2[1:19], na.rm = TRUE)


habitat_areas$Habitat <- tolower(habitat_areas$Habitat)
density3_means$Habitat <- tolower(density3_means$Habitat)
density3_means$Habitat %in% habitat_areas$Habitat
# Check this to make sure habitat names match up

habitat_areas <- merge(habitat_areas, density3_means, by.x = "Habitat", by.y = "Habitat", all.x = TRUE)[,1:4]

names(habitat_areas)[4] <- "SpeciesDensity_10m2"

habitat_areas$SpeciesDensity_10m2 <- exp(habitat_areas$SpeciesDensity_10m2)


## Adding in other coefficients 
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "fern and cycad planting"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "introduced shrubs"] 
					
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "cretaceous angiosperm shrubs"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "introduced shrubs"]


habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "species-poor hedgerow"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "species-rich hedgerow"] * species_poor_hedge_coef
		
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "neogene grass"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "amenity grass/turf"]
		
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "paleogene asteraceae"] <- 
		habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "short/perennial vegetation"]
		
habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat == "hard standing"] <- 0




## Calculating the biodiversity change between the two when scaled for habitat area (Assumption 1)
habitat_areas$Corrected_SpeciesDensity_Current <- calculate_density_cSAR(S=habitat_areas$SpeciesDensity_10m2, A=10, z=0.1, newA=habitat_areas$Current_area_m2)
habitat_areas$Corrected_SpeciesDensity_Proposed <- calculate_density_cSAR(S=habitat_areas$SpeciesDensity_10m2, A=10, z=0.1, newA=habitat_areas$Proposed_area_m2)


### Adding weighting for each habitat
habitat_areas$Current_weight <- habitat_areas$Current_area_m2/totalCurrent
habitat_areas$Current_Weighted_density <- habitat_areas$SpeciesDensity_10m2 * habitat_areas$Current_weight
habitat_areas$Current_Weighted_Correcteddensity <- habitat_areas$Corrected_SpeciesDensity_Current * habitat_areas$Current_weight

habitat_areas$Proposed_weight <- habitat_areas$Proposed_area_m2/totalProposed
habitat_areas$Proposed_Weighted_density <- habitat_areas$SpeciesDensity_10m2 * habitat_areas$Proposed_weight
habitat_areas$Proposed_Weighted_Correcteddensity <- habitat_areas$Corrected_SpeciesDensity_Proposed * habitat_areas$Proposed_weight


## Totals
habitat_areas[nrow(habitat_areas), c(2: 12)] <- colSums(habitat_areas[1:nrow(habitat_areas)-1,c(2:12)], na.rm = TRUE)
habitat_areas$Current_area_m2[nrow(habitat_areas)] <- totalCurrent
habitat_areas$Proposed_area_m2[nrow(habitat_areas)] <- totalProposed

write.csv(habitat_areas, file = "Habitat_totals.csv", row.names = FALSE)

## Percent change
# Species density scaled
curr <- habitat_areas$Current_Weighted_Correcteddensity[nrow(habitat_areas)]
prop <- habitat_areas$Proposed_Weighted_Correcteddensity[nrow(habitat_areas)]

((prop - curr)/curr) *100

# Species density
curr2 <- habitat_areas$Current_Weighted_density[nrow(habitat_areas)]
prop2 <- habitat_areas$Proposed_Weighted_density[nrow(habitat_areas)]

((prop2 - curr2)/curr2) *100



##########################
## SAMPLED RICHNESS MODELS
##########################

# Uses the same dataset
head(sampled_area)
richness_data <- sampled_area
taxa <- relevel(taxa, ref = "Plants")

# Round all the richness estimates
any(richness_data$Taxon.Richness != round(richness_data$Taxon.Richness))
richness <- round(richness_data$Taxon.Richness)
any(richness != round(richness)) ## just to check


hist(richness)
table(richness_data$Habitat, taxa)
table(taxa)

## Again, can't do the model with an interaction
richness1 <- glmer(richness ~ Habitat + taxa + (1|Study.ID), data = richness_data, family = poisson)
richness2 <- glmer(richness ~ Habitat + (1|Study.ID), data = richness_data, family = poisson)
anova(richness1, richness2) ## Not significant
richness3 <- glmer(richness ~ taxa + (1|Study.ID), data = richness_data, family = poisson)
anova(richness1, richness3) # Significant. Remove taxa, not habitat
richness4 <- glmer(richness ~ 1 + (1|Study.ID), data = richness_data, family = poisson)
anova(richness2, richness4) # Significant. Need habitat

summary(richness2)
Anova(richness2)
## save(richness2, file = file.path(models_out, "FinalRichnessModel.rda"))
## load(file.path(models_out, "FinalRichnessModel.rda"))

richness2_means <- model_Means(richness2)

#############################################
### BIODIVERSITY CHANGE FOR SPECIES RICHNESS MODEL
#############################################

richness_areas <- data.frame(
Habitat = c("Broadleaved woodland", "Acid grassland (heath)", "Chalk grassland", "Neutral grassland", "Fen (incl. reedbed)", "Marginal vegetation (pond edge)", "Ponds", "Green roof", "Species-poor hedgerow", "Species-rich hedgerow", "Short/perennial vegetation", "Amenity grass/turf", "Introduced shrubs", "Hard standing", "Fern and cycad planting", "Agricultural plants", "Paleogene Asteraceae", "Neogene grass", "Cretaceous Angiosperm shrubs", "Total"),
Current_area_m2 = c(1978.36, 111.71, 344.58, 2103.15, 64.6, 163.6, 341.28, 9.98, 109, 121.87, 423.65, 3303.63, 2218.62, 10415, 0, 0, 0, 0, 0, NA),
Proposed_area_m2=c(3477.67, 82.1, 526, 2133.45, 133.86, 99.15, 459.37, 0, 0, 607.5, 0, 1573.91, 1346.69, 9525.16, 739.82, 583.97, 176.57, 156.27, 244.97,NA)
)


totalCurrent <- sum(richness_areas$Current_area_m2[1:19], na.rm = TRUE)
totalProposed <- sum(richness_areas$Proposed_area_m2[1:19], na.rm = TRUE)

richness_means <- richness2_means[1:13,]

richness_areas$Habitat <- tolower(richness_areas$Habitat)
richness_means$Habitat <- tolower(richness_means$Habitat)
richness_means$Habitat %in% richness_areas$Habitat
# Check this everytime

richness_areas <- merge(richness_areas, richness_means, by.x = "Habitat", by.y = "Habitat", all.x = TRUE)[,c(1:3,4)]


richness_areas$richness <- exp(richness_areas$richness)


## Adding in other coefficients 
richness_areas$richness[richness_areas$Habitat == "fern and cycad planting"] <- 
		richness_areas$richness[richness_areas$Habitat == "introduced shrubs"] 
				
richness_areas$richness[richness_areas$Habitat == "cretaceous angiosperm shrubs"] <- 
		richness_areas$richness[richness_areas$Habitat == "introduced shrubs"]


richness_areas$richness[richness_areas$Habitat == "species-poor hedgerow"] <- 
		richness_areas$richness[richness_areas$Habitat == "species-rich hedgerow"] * species_poor_hedge_coef
		
richness_areas$richness[richness_areas$Habitat == "neogene grass"] <- 
		richness_areas$richness[richness_areas$Habitat == "amenity grass/turf"]
		
richness_areas$richness[richness_areas$Habitat =="paleogene asteraceae"] <- 
		richness_areas$richness[richness_areas$Habitat == "short/perennial vegetation"]
		
richness_areas$richness[richness_areas$Habitat == "hard standing"] <- 0


### Area weights
richness_areas$Current_weight <- richness_areas$Current_area_m2/totalCurrent
richness_areas$Current_Weighted_richness <- richness_areas$richness * richness_areas$Current_weight

richness_areas$Proposed_weight <- richness_areas$Proposed_area_m2/totalProposed
richness_areas$Proposed_Weighted_richness <- richness_areas$richness * richness_areas$Proposed_weight


## Totals
richness_areas[nrow(richness_areas), c(2:8)] <- colSums(richness_areas[1:nrow(richness_areas)-1,c(2:8)], na.rm = TRUE)
richness_areas$Current_area_m2[nrow(richness_areas)] <- totalCurrent
richness_areas$Proposed_area_m2[nrow(richness_areas)] <- totalProposed

## Percent change
# Species richness
curr <- richness_areas$Current_Weighted_richness[nrow(richness_areas)]
prop <- richness_areas$Proposed_Weighted_richness[nrow(richness_areas)]

((prop - curr)/curr) *100

write.csv(richness_areas, file = "richness.csv", row.names = FALSE)


#################
## Density and Richness plot
#################


 pdf(file.path(figure_out, "Habitat_density&Richness_plusmissing.pdf"), pointsize=11)
	
  cex.txt <- 1.2
  missing_coefs <- c("cretaceous angiosperm shrubs", "fern and cycad planting", "hard standing", 
		 "neogene grass", "paleogene asteraceae", "species-poor hedgerow")
	labs <- levels(sampled_area$Habitat)
	labs2 <- c(labs, missing_coefs)
	labs2 <- sapply(labs2, simpleCap)
	labs2[9] <- "Marginal Vegetation (pond edge)"
	labs2[15] <- "Fern and Cycad Planting"
	par(mar=c(15.5, 5, 1, 5))
	
	# Density estimates
	errbar(1:nrow(density3_means), exp(density3_means[,2]), exp(density3_means[,3]), exp(density3_means[,4]), 
		col = "white", main = "", sub ="", xlab ="", bty = "n", pch = 19, xaxt = "n", ylim=c(0,60), las = 1, cex= cex.txt, ylab = "", xlim=c(0, length(labs2)))
	points(1:nrow(density3_means),exp(density3_means[,2]),col="black",bg="white",pch=19,cex=cex.txt)
	missing_densities <- habitat_areas$SpeciesDensity_10m2[habitat_areas$Habitat %in% missing_coefs]
	points((nrow(density3_means)+1):(length(labs2)), missing_densities, col="red", pch=19)
	# Species richness estimates
	errbar(1:nrow(richness_means)+0.2, exp(richness_means[,2]), exp(richness_means[,4]), exp(richness_means[,3]), add=T, pch=1, cap=0.01, col ="white", errbar.col = "darkgrey")
	
	points(1:nrow(richness_means) + 0.2, exp(richness_means[,2]),col="darkgrey",bg="white",pch=19,cex=1)
	
	missing_richness <- richness_areas$richness[richness_areas$Habitat %in% missing_coefs]
	points((nrow(richness_means)+1):(length(labs2)) + 0.2, missing_richness, col="pink", pch=19)


	axis(1, at=1:length(labs2), labels = labs2, las = 2, cex.axis = cex.txt)
	mtext(expression(Within-sample ~ Species ~ Density ~ (per ~ 10~m^{2})), side = 2, line = 2, cex = cex.txt)
	axis(4, at=seq(0, 60, by = 10), las = 2)
	mtext("Within-sample Species Richness", side = 4, line = 2, cex = cex.txt)
	
 dev.off()

#################
## MAP
#################
# studies_used <- unique(density3@frame$Study.ID)
# canada <- c("2011_MacIvor 1", "2011_MacIvor 2")
# studies_used <- studies_used[!(studies_used %in% canada)]
# studies_modelled <- garden[garden$Study.ID %in% studies_used,]

# coord<-aggregate(cbind(studies_modelled$long, studies_modelled$lat), list(studies_modelled$Study.ID), max)
# coord$X<-coord$Group.1
# coord<-coord[2:4]
# names(coord)<-c("Long", "Lat", "X")
# dsSPDF<-SpatialPointsDataFrame(coord[,1:2], data.frame(coord[,1:3]))
# proj4string(dsSPDF)<-CRS("+proj=longlat")

#  pdf(file.path(figure_out, "Map_ModelledStudes.pdf"), pointsize=11, width = 2.4, height = 5)
# 	 par(mar=c(0, 0, 0, 0))
# 	 map('worldHires', c("UK"),border=0,fill=TRUE, col="lightgrey",  xlim=c(-8.1,2), ylim=c(49,60.9), mar = c(0, 0, 0, 0))
# 	 points(dsSPDF, col="black", bg="black", cex= 1.5, pch=19)
# 	 maps:::map.scale(y=57, x=-1, ratio=FALSE, cex = 0.7, srt = 310)  
# 	 north.arrow(xb=-0.2, yb=58, len=0.1, lab="N")
#  dev.off()
	 
