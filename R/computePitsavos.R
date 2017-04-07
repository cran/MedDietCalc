computePitsavos <- function(data, WholeCereals, Fruit, Vegetables, Potatoes, Legumes, OliveOil, OOmeasure = "gr",
                            Fish, Meat, Poultry, WholeDairy, Wine,
                            output = "percent", frequency = "daily", rm.na = FALSE) {

  arguments <- as.list( match.call() )
  WholeCereals <- eval(arguments$WholeCereals, data)
  Fruit <- eval(arguments$Fruit, data)
  Vegetables <- eval(arguments$Vegetables, data)
  Potatoes <- eval(arguments$Potatoes, data)
  Legumes <- eval(arguments$Legumes, data)
  OliveOil <- eval(arguments$OliveOil, data)
  Fish <- eval(arguments$Fish, data)
  Meat <- eval(arguments$Meat, data)
  Poultry <- eval(arguments$Poultry, data)
  WholeDairy <- eval(arguments$WholeDairy, data)
  Wine <- eval(arguments$Wine, data)


    # this code chunk tests if data has not been introduced in a monthly fashion, and if so, transform data to montly consumption
  if(is.null(frequency)){stop("please, provide the frequency of consumption in which the data is tabulated with the 'frequency' argument. Accepted values are 'daily', 'weekly' and 'monthly'")}

  if(frequency == "daily" || frequency == "weekly"){
    Vars <- list(WholeCereals = WholeCereals, Fruit = Fruit, Vegetables = Vegetables, Potatoes = Potatoes, Legumes = Legumes,
                 OliveOil= OliveOil, Fish = Fish, Meat = Meat, Poultry = Poultry, WholeDairy = WholeDairy, Wine = Wine)
    Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "monthly")

    WholeCereals <- Vars$WholeCereals
    Fruit <- Vars$Fruit
    Vegetables <- Vars$Vegetables
    Potatoes <- Vars$Potatoes
    Legumes <- Vars$Legumes
    OliveOil <- Vars$OliveOil
    Fish <- Vars$Fish
    Meat <- Vars$Meat
    Poultry <- Vars$Poultry
    WholeDairy <- Vars$WholeDairy
    Wine <- Vars$Wine
  } else {
    if(frequency != "monthly"){stop("accepted values for 'frequency' argument are 'daily', 'weekly' and 'monthly'")}
  }


  Cscore <- numeric()
  Cscore[WholeCereals < 1] <- 0
  Cscore[WholeCereals >= 1 & WholeCereals < 5] <- 1
  Cscore[WholeCereals >= 5 & WholeCereals < 9] <- 2
  Cscore[WholeCereals >= 9 & WholeCereals < 13] <- 3
  Cscore[WholeCereals >= 13 & WholeCereals <= 18] <- 4
  Cscore[WholeCereals > 18] <- 5


  Frscore <- numeric()
  Frscore[Fruit < 1] <- 0
  Frscore[Fruit >= 1 & Fruit < 5] <- 1
  Frscore[Fruit >= 5 & Fruit < 9] <- 2
  Frscore[Fruit >= 9 & Fruit < 13] <- 3
  Frscore[Fruit >= 13 & Fruit <= 18] <- 4
  Frscore[Fruit > 18] <- 5


  Vscore <- numeric()
  Vscore[Vegetables < 1] <- 0
  Vscore[Vegetables >= 1 & Vegetables < 5] <- 1
  Vscore[Vegetables >= 5 & Vegetables < 9] <- 2
  Vscore[Vegetables >= 9 & Vegetables < 13] <- 3
  Vscore[Vegetables >= 13 & Vegetables <= 18] <- 4
  Vscore[Vegetables > 18] <- 5


  Ptscore <- numeric()
  Ptscore[Potatoes < 1] <- 0
  Ptscore[Potatoes >= 1 & Potatoes < 5] <- 1
  Ptscore[Potatoes >= 5 & Potatoes < 9] <- 2
  Ptscore[Potatoes >= 9 & Potatoes < 13] <- 3
  Ptscore[Potatoes >= 13 & Potatoes <= 18] <- 4
  Ptscore[Potatoes > 18] <- 5

  Lscore <- numeric()
  Lscore[Legumes < 1] <- 0
  Lscore[Legumes >= 1 & Legumes < 5] <- 1
  Lscore[Legumes >= 5 & Legumes < 9] <- 2
  Lscore[Legumes >= 9 & Legumes < 13] <- 3
  Lscore[Legumes >= 13 & Legumes <= 18] <- 4
  Lscore[Legumes > 18] <- 5



  if(OOmeasure == "gr") {OliveOil <- OliveOil/0.918/15}
  else {
    if(OOmeasure == "ml") {OliveOil <- OliveOil/15}
    else {
      if(OOmeasure == "serving") {"no conversion needed"}
      else {stop("check units of Olive Oil")
      }
    }
  }

  OOscore <- numeric()
  OOscore[OliveOil < 1] <- 0
  OOscore[OliveOil >= 1 & OliveOil < 5] <- 1
  OOscore[OliveOil >= 5 & OliveOil < 9] <- 2
  OOscore[OliveOil >= 9 & OliveOil < 13] <- 3
  OOscore[OliveOil >= 13 & OliveOil <= 18] <- 4
  OOscore[OliveOil > 18] <- 5

  Fiscore <- numeric()
  Fiscore[Fish < 1] <- 0
  Fiscore[Fish >= 1 & Fish < 5] <- 1
  Fiscore[Fish >= 5 & Fish < 9] <- 2
  Fiscore[Fish >= 9 & Fish < 13] <- 3
  Fiscore[Fish >= 13 & Fish <= 18] <- 4
  Fiscore[Fish > 18] <- 5


  Mscore <- numeric()
  Mscore[Meat < 1] <- 5
  Mscore[Meat >= 1 & Meat < 5] <- 4
  Mscore[Meat >= 5 & Meat < 9] <- 3
  Mscore[Meat >= 9 & Meat < 13] <- 2
  Mscore[Meat >= 13 & Meat <= 18] <- 1
  Mscore[Meat > 18] <- 0

  Plscore <- numeric()
  Plscore[Poultry < 1] <- 5
  Plscore[Poultry >= 1 & Poultry < 5] <- 4
  Plscore[Poultry >= 5 & Poultry < 9] <- 3
  Plscore[Poultry >= 9 & Poultry < 13] <- 2
  Plscore[Poultry >= 13 & Poultry <= 18] <- 1
  Plscore[Poultry > 18] <- 0

  Dscore <- numeric()
  Dscore[WholeDairy < 1] <- 5
  Dscore[WholeDairy >= 1 & WholeDairy < 5] <- 4
  Dscore[WholeDairy >= 5 & WholeDairy < 9] <- 3
  Dscore[WholeDairy >= 9 & WholeDairy < 13] <- 2
  Dscore[WholeDairy >= 13 & WholeDairy <= 18] <- 1
  Dscore[WholeDairy > 18] <- 0


  # note that wine consumption should be scored as daily consumption, but periodicity function as changed everything to be monthly
  Wine <- Wine / 30

  Wscore <- numeric()
  Wscore[Wine < 3] <- 5
  Wscore[Wine >= 3 & Wine < 4] <- 4
  Wscore[Wine >= 4 & Wine < 6] <- 3
  Wscore[Wine >= 6 & Wine < 6] <- 2
  Wscore[Wine >= 7 & Wine <= 6] <- 1
  Wscore[Wine > 7] <- 0



  score <- data.frame(Cscore, Frscore, Vscore, Ptscore, Lscore, OOscore, Fiscore, Mscore, Plscore, Dscore, Wscore)

  score$absolute <- apply(score, 1, function(x) sum(x, na.rm = rm.na))
  score$percent <- round(100 * score$absolute / 55, 1)


  if(missing(output) || output == "percent") {return(score$percent)
  } else {
    if(output == "absolute") {return(score$absolute)
    } else {
      if(output == "data.frame") {return(score)
      } else {
        stop("please, select a valid output argument, admited values are 'percent' -default-, 'absolute' and 'data.frame' " )
      }
    }
  }

}
