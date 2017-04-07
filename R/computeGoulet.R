computeGoulet <- function(data, WholeCereals, Vegetables, Fruit, LegumesAndNuts, OliveOil,
                          OOmeasure = "gr", Olives, Dairy, Fish, Poultry, Eggs, Sweets, Meat,
                          output = "percent", frequency = "daily", rm.na = FALSE){

  arguments <- as.list( match.call() )
  WholeCereals <- eval(arguments$WholeCereals, data)
  Vegetables <- eval(arguments$Vegetables, data)
  Fruit <- eval(arguments$Fruit, data)


  LAN <- eval(arguments$LegumesAndNuts, data)
  OliveOil <- eval(arguments$OliveOil, data)
  Olives <- eval(arguments$Olives, data)
  Dairy <- eval(arguments$Dairy, data)
  Fish <- eval(arguments$Fish, data)
  Poultry <- eval(arguments$Poultry, data)
  Eggs <- eval(arguments$Eggs, data)
  Sweets <- eval(arguments$Sweets, data)
  Meat <- eval(arguments$Meat, data)


  # this code chunk tests if data has not been introduced in a monthly fashion, and if so, transform data to montly consumption
  if(is.null(frequency)){stop("please, provide the frequency of consumption in which the data is tabulated with the 'frequency' argument. Accepted values are 'daily', 'weekly' and 'monthly'")}

  if(frequency == "monthly" || frequency == "weekly"){
    Vars <- list(WholeCereals = WholeCereals, Vegetables = Vegetables, Fruit = Fruit,  LAN = LAN,
                 OliveOil = OliveOil, Olives = Olives, Dairy = Dairy, Fish = Fish, Poultry = Poultry, Eggs = Eggs, Sweets = Sweets, Meat = Meat)
    Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "daily")

    WholeCereals <- Vars$WholeCereals
    Vegetables <- Vars$Vegetables
    Fruit <- Vars$Fruit
    LAN <- Vars$LAN
    OliveOil <- Vars$OliveOil
    Olives <- Vars$Olives
    Dairy <- Vars$Dairy
    Fish <- Vars$Fish
    Poultry <- Vars$Poultry
    Eggs <- Vars$Eggs
    Sweets <- Vars$Sweets
    Meat <- Vars$Meat

  } else {
    if(frequency != "daily"){stop("accepted values for 'frequency' argument are 'daily', 'weekly' and 'monthly'")}
  }

  Cscore <- numeric()
  Cscore[WholeCereals < 1] <- 0
  Cscore[WholeCereals >= 1 & WholeCereals < 3] <- 1
  Cscore[WholeCereals >= 3 & WholeCereals < 5] <- 2
  Cscore[WholeCereals >= 5 & WholeCereals < 7] <- 3
  Cscore[WholeCereals >= 7] <- 4


  Vscore <- numeric()
  Vscore[Vegetables < 1] <- 0
  Vscore[Vegetables >= 1 & Vegetables < 2] <- 1
  Vscore[Vegetables >= 2 & Vegetables < 3] <- 2
  Vscore[Vegetables >= 3 & Vegetables < 4] <- 3
  Vscore[Vegetables >= 4] <- 4

  Frscore <- numeric()
  Frscore[Fruit < 1] <- 0
  Frscore[Fruit >= 1 & Fruit < 2] <- 1
  Frscore[Fruit >= 2 & Fruit < 3] <- 2
  Frscore[Fruit >= 3 & Fruit < 4] <- 3
  Frscore[Fruit >= 4] <- 4

  LANscore <- numeric()
  LANscore[LAN < 0.5] <- 0
  LANscore[LAN >= 0.5 & LAN < 1] <- 1
  LANscore[LAN >= 1 & LAN < 2] <- 2
  LANscore[LAN >= 2 & LAN < 3] <- 3
  LANscore[LAN >= 3] <- 4


  if(OOmeasure == "gr") {OliveOil <- OliveOil/0.918/15}
  else {
    if(OOmeasure == "ml") {OliveOil <- OliveOil/15}
    else {
      if(OOmeasure == "serving") {"no conversion needed"}
      else {stop("check units of Olive Oil")
      }
    }
  }
  OOO <- OliveOil + Olives

  OOscore <- numeric()
  OOscore[OOO < 1] <- 0
  OOscore[OOO >= 1 & OOO < 2] <- 1
  OOscore[OOO >= 2 & OOO < 3] <- 2
  OOscore[OOO >= 3 & OOO < 4] <- 3
  OOscore[OOO >= 4] <- 4

  Dscore <- numeric()
  Dscore[Dairy < 1] <- 0
  Dscore[Dairy >= 4] <- 1
  # there is no consumption that receives 2 points
  Dscore[Dairy >= 1 & Dairy < 2] <- 3
  Dscore[Dairy >= 2 & Dairy < 4] <- 4

  Fiscore <- numeric()
  Fish <- 7 * Fish # Fish consumption is scored weekly
  Fiscore[Fish == 0] <- 0
  Fiscore[Fish >= 0 & Fish < 1] <- 1
  Fiscore[Fish >= 1 & Fish < 2] <- 2
  Fiscore[Fish >= 2 & Fish < 3] <- 3
  Fiscore[Fish >= 3] <- 4


  Plscore <- numeric()
  Poultry <- 7 * Poultry # scored weekly
  Plscore[Poultry == 0] <- 0
  Plscore[Poultry >= 0 & Poultry < 1] <- 1
  Plscore[(Poultry >= 1 & Poultry < 2) | (Poultry >= 4)] <- 2
  Plscore[Poultry >= 2 & Poultry < 3] <- 3
  Plscore[Poultry >= 3 & Poultry < 4] <- 4

  Eggscore <- numeric()
  Eggs <- 7 * Eggs # scored weekly
  Eggscore[Eggs >= 7] <- 0
  # no consumption deserves 1 point
  Eggscore[Eggs >= 5 & Eggs < 7] <- 2
  # no consumpion deserves 3 points
  Eggscore[Eggs <5] <- 4

  Sweetscore <- numeric()
  Sweets <- 7 * Sweets # scored weekly
  Sweetscore[Sweets >= 7] <- 0
  Sweetscore[Sweets >= 5 & Sweets < 7] <- 1
  Sweetscore[Sweets >= 3 & Sweets < 5] <- 2
  Sweetscore[Sweets >= 1 & Sweets < 3] <- 3
  Sweetscore[Sweets < 1] <- 4

  Mscore <- numeric()
  Meat <- 7 * Meat # scored weekly
  Mscore[Meat >= 7] <- 0
  Mscore[Meat >= 5 & Meat < 7] <- 1
  Mscore[Meat >= 3 & Meat < 5] <- 2
  Mscore[Meat >= 1 & Meat < 3] <- 3
  Mscore[Meat < 1] <- 4


  score <- data.frame(Cscore, Vscore, Frscore, LANscore, OOscore, Dscore, Fiscore, Plscore, Eggscore, Sweetscore, Mscore)

  score$absolute <- apply(score, 1, function(x) sum(x, na.rm = rm.na))
  score$percent <- round(100 * score$absolute / 44, 1)


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
