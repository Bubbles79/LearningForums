library(ggplot2)
library(likert)

#Import Beliefs data
PBfs = read.csv("/Users/paulwagner/Desktop/Learning/Revise and Resubmit 3/Data/BeliefsIre.csv", header = TRUE, stringsAsFactors=FALSE)
PBfs <- PBfs[-c(10,14,23,37,41),-c(1,2)]
PBfs [PBfs  == 97] <- 3 #Code blank answers as neutral

PBfs [PBfs  == 1] <- "No, totally reject"
PBfs [PBfs  == 2] <- "Disagree"
PBfs [PBfs  == 3] <- "Neutral"
PBfs [PBfs  == 4] <- "Agree"
PBfs [PBfs  == 5] <- "Strongly Agree"

for(i in 1:ncol(PBfs)){
  PBfs[,i] <- as.factor(PBfs[,i])
}

levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree")

names(PBfs) <- c(
  G0101="Voluntary action by businesses",
  G0102="Emission trading within EU",
  G0103="Self-commitment of individual states to reduce GHG-emissions",
  G0104="Expansion of nuclear energy",
  G0105="Subvention of renewable energy",
  G0106="Increased use of  bio fuels",
  G0107="Use of carbon capture storage technology",
  G0108="Reforestation and avoided deforestation strategies",
  G0109="Reduction of fuel consumption in transportation",
  G0110="Private action to minimize the individual ecological footprint",
  G0111="Carbon offsetting",
  G0112="Lower speed limits",
  G0113="Tax on CO2",
  G0114="Sector specific goals to reduce GHG-emission",
  G0115="Sector specific  legal limits to GHG-emission",
  G0117="Developing cleantech business solutions",
  G0401="My country should take a leading international role in GHG reduction",
  G0403="GHG-emission have negative impacts on the economy",
  G0404="GHG reduction creates jobs and opportunities for economic growth",
  G0405="Securing the national energy supply is more important than the reducing emission",
  G0406="The current GHG reduction target of my government is too ambitious",
  G0407="The  transition to renewable energy supply is too costly",
  G0408="In the long term, energy supply can be secured exclusively by renewable energies",
  G0409="Nuclear energy is the most realistic alternative to fossil fuels",
  G0410="In the long term, the economy profits from the transition to renewable energies",
  G0411="The target of my country's government for renewable energy is too ambitious")

'names(PBfs) <- c(
  G0101="Voluntary action by businesses (94%)",
  G0102="Emission trading within EU (90%)",
  G0103="Self-commitment of individual states to reduce GHG-emissions (92%)",
  G0104="Expansion of nuclear energy (85%)",
  G0105="Subvention of renewable energy(89%)",
  G0106="Increased use of  bio fuels (87%)",
  G0107="Use of carbon capture storage technology (87%)",
  G0108="Reforestation and avoided deforestation strategies (89%)",
  G0109="Reduction of fuel consumption in transportation ((89%)",
  G0110="Private action to minimize the individual ecological footprint (87%)",
  G0111="Carbon offsetting (87%)",
  G0112="Lower speed limits (85%)",
  G0113="Tax on CO2 (85%)",
  G0114="Sector specific goals to reduce GHG-emission(89%)",
  G0115="Sector specific  legal limits to GHG-emission (87%)",
  G0117="Developing cleantech business solutions (89%)",
  G0401="My country should take a leading international role in GHG reduction (89%)",
  G0403="GHG-emission have negative impacts on the economy (89%)",
  G0404="GHG reduction creates jobs and opportunities for economic growth (89%)",
  G0405="Securing the national energy supply is more important than the reducing emission (87%)",
  G0406="The current GHG reduction target of my government is too ambitious (89%)",
  G0407="The  transition to renewable energy supply is too costly (89%)",
  G0408="In the long term, energy supply can be secured exclusively by renewable energies (89%)",
  G0409="Nuclear energy is the most realistic alternative to fossil fuels (87%)",
  G0410="In the long term, the economy profits from the transition to renewable energies (89%)",
  G0411="The target of my countrys government for renewable energy is too ambitious (89%)")'

PBfs[,1] <- factor(PBfs[,1], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,2] <- factor(PBfs[,2], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,3] <- factor(PBfs[,3], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,4] <- factor(PBfs[,4], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,5] <- factor(PBfs[,5], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,6] <- factor(PBfs[,6], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,7] <- factor(PBfs[,7], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,8] <- factor(PBfs[,8], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,9] <- factor(PBfs[,9], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,10] <- factor(PBfs[,10], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,11] <- factor(PBfs[,11], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,12] <- factor(PBfs[,12], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,13] <- factor(PBfs[,13], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,14] <- factor(PBfs[,14], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,15] <- factor(PBfs[,15], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,16] <- factor(PBfs[,16], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,17] <- factor(PBfs[,17], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,18] <- factor(PBfs[,18], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,19] <- factor(PBfs[,19], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,20] <- factor(PBfs[,20], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,21] <- factor(PBfs[,21], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,22] <- factor(PBfs[,22], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,23] <- factor(PBfs[,23], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,24] <- factor(PBfs[,24], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,25] <- factor(PBfs[,25], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)
PBfs[,26] <- factor(PBfs[,26], levels = c("No, totally reject", "Disagree", "Neutral", "Agree", "Strongly Agree"), ordered = TRUE)

str(PBfs)

Result <- likert(PBfs)
plot(Result,type="bar", text.size = 3, wrap =100, plot.percent.high = TRUE, plot.percent.low = TRUE, low.color = "black", high.color = "Black",neutral.color = "grey90", neutral.color.ramp = "white", colors = NULL)


