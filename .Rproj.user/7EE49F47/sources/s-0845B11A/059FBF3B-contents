library(dplyr)
library(ggplot2)
library(scales)
#Get data from csv
penguins <- read.csv("penguins.csv", header=TRUE, stringsAsFactors=FALSE)

#View(penguins)

getSexPieChart <- function(){
  sex_count <- count(penguins, sex)
  sex_count$sex[is.na(sex_count$sex)] <- "unidentified"
  colors <- c("#967D7C", "#90A1B5", "#C5C3BA")
  pie(sex_count$n, sex_count$n, col = colors, main="Penguin Sex Distribution")
  legend("topright", sex_count$sex, cex = 0.8,
         fill = colors)
}

getBillDimensionsScatterPlot <- function(){
  

  ggplot(penguins, aes(x=bill_length_mm, y=bill_depth_mm, color=species, shape=species)) +
    geom_point(size=2) +
    geom_smooth(method=lm, se=FALSE) +
    labs(x="Bill length [mm]", y="Bill depth [mm]", title="Bill Dimensions Scatter Plot") 
  #print(bill_dimensions)
  #penguin_bill_length$bill_length_mm[is.na(sex_count$sex)] <- "unidentified"
}

getBillDimensionsScatterPlotBySex <- function(){

  penguins_clean <- na.omit(penguins)
  ggplot(penguins_clean, aes(x=bill_length_mm, y=bill_depth_mm, color=interaction(sex, species), shape=interaction(sex, species))) +
    geom_point(size=2) +
    geom_smooth(method=lm, se=FALSE) +
    labs(x="Bill length [mm]", y="Bill depth [mm]", title="Detailed Bill Dimensions Scatter Plot")

}

getBillDimensionsScatterPlotBySexWithUnidentified <- function(){
  penguins$sex[is.na(penguins$sex)] <- "unidentified"
  ggplot(penguins, aes(x=bill_length_mm, y=bill_depth_mm, color=interaction(sex, species), shape=interaction(sex, species))) +
    geom_point(size=2) +
    geom_smooth(method=lm, se=FALSE) +
    labs(x="Bill length [mm]", shape="Sex.Species", colour="Sex.Species", y="Bill depth [mm]", title="Assuming Gender", subtitle="Trying to guess gender by distribution") +
    scale_shape_manual(values = 0:8)
  
  #print(bill_dimensions)
  #penguin_bill_length$bill_length_mm[is.na(sex_count$sex)] <- "unidentified"
}

getSpeciesCount <- function(){
  ggplot(penguins, aes(fill=species)) + 
    geom_histogram (aes(x=species), stat="count") +
    labs(title="Species Count", caption=paste("Total Penguin count:", count(penguins)))
}

getIslandCount <- function(){
  ggplot(penguins, aes(fill=island)) + 
    geom_histogram (aes(x=island), stat="count") +
    labs(title="Penguin Count by Island", caption=paste("Total Penguin count:", count(penguins)))
}

getSpeciesCountBiscoe <- function(){
  penguins_clean <- penguins[(penguins$island=="Biscoe"),]
  species_count <- count(penguins_clean, species)
  ggplot(species_count, aes(x="", y=n, fill=species)) + 
    geom_col(color="black") +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste0(n," (" , percent(n/sum(n)),")" ),),
              position = position_stack(vjust = 0.5)) +
    labs(title="Count by species: Biscoe", fill="Species", y="", x="", caption=paste("Total Penguin count:", sum(species_count$n)))
}

getSpeciesCountDream <- function(){
  penguins_clean <- penguins[(penguins$island=="Dream"),]
  species_count <- count(penguins_clean, species)
  ggplot(species_count, aes(x="", y=n, fill=species)) + 
    geom_col(color="black") +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste0(n," (" , percent(n/sum(n)),")" ),),
              position = position_stack(vjust = 0.5)) +
    labs(title="Count by species: Dream", fill="Species", y="", x="", caption=paste("Total Penguin count:", sum(species_count$n)))
}

getSpeciesCountTorgersen <- function(){
  penguins_clean <- penguins[(penguins$island=="Torgersen"),]
  species_count <- count(penguins_clean, species)
  ggplot(species_count, aes(x="", y=n, fill=species)) + 
    geom_col() +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste0(n," (" , percent(n/sum(n)),")" ),),
              position = position_stack(vjust = 0.5)) +
    labs(title="Count by species: Torgersen", fill="Species", y="", x="", caption=paste("Total Penguin count:", sum(species_count$n)))
}

getBillAndFlippersScatterPlot <- function(){
  
  
  ggplot(penguins, aes(x=bill_length_mm, y=flipper_length_mm, color=species, shape=species)) +
    geom_point(size=2) +
    geom_smooth(method=lm, se=FALSE) +
    labs(x="Bill length [mm]", y="Flipper length [mm]", title="Bill & Flipper Length Scatter Plot") 
  #print(bill_dimensions)
  #penguin_bill_length$bill_length_mm[is.na(sex_count$sex)] <- "unidentified"
}

getFlippersAndBodyMassScatterPlot <- function(){
  
  ggplot(penguins, aes(x=flipper_length_mm, y=body_mass_g, color=species, shape=species)) +
    geom_point(size=2) +
    geom_smooth(method=lm, se=FALSE) +
    labs(x="Flipper length [mm]", y="Body Mass [g]", title="Bill & Flipper Length Scatter Plot") 
  #print(bill_dimensions)
  #penguin_bill_length$bill_length_mm[is.na(sex_count$sex)] <- "unidentified"
}

getBodyMassBySexAndSpecies <- function(){
    penguins_clean <- na.omit(penguins)
    ggplot(data = penguins_clean, aes(x = interaction(species, sex), y = body_mass_g)) +
    geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
    geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE, position = position_jitter(width = 0.2, seed = 0)) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    labs(x = "Species.Sex",
         y = "Body Mass (g)",
         title="Body Mass by Sex and Species")
 # penguins_clean <- na.omit(penguins)
#  ggplot(penguins_clean, aes(x=sex, y=body_mass_g, color=interaction(sex, species), shape=interaction(sex, species))) +
 #   geom_point(size=2) +
  #  labs(x="Bill length [mm]", y="Bill depth [mm]", title="Detailed Bill Dimensions Scatter Plot")
  
}

getBodyMassCurve = function() {
  body_mass_sd <- sd(penguins$body_mass_g, na.rm=TRUE)
  body_mass_mean <- mean(penguins$body_mass_g, na.rm=TRUE)
  y <- dnorm(penguins$body_mass_g, mean=b, sd=a)
  #plot(penguins$body_mass_g, y, col="red")
  
  
  ggplot(penguins, aes(body_mass_g)) +
    stat_function(fun = dnorm, n = count(penguins), args = list(mean = body_mass_mean, sd = body_mass_sd)) + ylab("") +
    scale_y_continuous(breaks = NULL) +
    labs(title="Body Mass Curve", subtitle = paste0("Standard Deviation: ", body_mass_sd, "\n", "Mean: ", body_mass_mean))
}

#getSpeciesCount()

#getSexPieChart()

#getBillDimensionsScatterPlot()

#getBillDimensionsScatterPlotBySex()

#getBillDimensionsScatterPlotBySexWithUnidentified()
 
#getIslandCount()

#getSpeciesCount()

#getSpeciesCountBiscoe()

#getSpeciesCountDream()

#getSpeciesCountTorgersen()

#getBillAndFlippersScatterPlot()

#getFlippersAndBodyMassScatterPlot()

#getBodyMassBySexAndSpecies()

getBodyMassCurve()