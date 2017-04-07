computeCardio <- function(data = NULL, Vegetables, Fruit, OliveOil, OOmeasure = "gr",
                          Legumes, Fish, Meat, RefinedRice, RefinedBread, WholeBread, Wine,
                          frequency = "percent", output = "percent", rm.na = FALSE) {

  arguments <- as.list( match.call() )
  Fruit <- eval(arguments$Fruit, data)
  Vegetables <- eval(arguments$Vegetables, data)
  OliveOil <- eval(arguments$OliveOil, data)
  Legumes <- eval(arguments$Legumes, data)
  Fish <- eval(arguments$Fish, data)
  Wine <- eval(arguments$Wine, data)
  Meat <- eval(arguments$Meat, data)
  RefinedRice <- eval(arguments$RefinedRice, data)
  RefinedBread <- eval(arguments$RefinedBread, data)
  WholeBread <- eval(arguments$WholeBread, data)



  # this code chunk tests if data has not been introduced in a daily fashion, and if so, transform data to daily consumption
  if(is.null(frequency)){stop("please, provide the frequency of consumption in which the data is tabulated with the 'frequency' argument. Accepted values are 'daily', 'weekly' and 'monthly'")}

  if(frequency == "weekly" || frequency == "monthly"){
    Vars <- list(Vegetables = Vegetables, Fuit = Fruit, OliveOil = OliveOil,
                 Legumes = Legumes, Fish = Fish, Meat = Meat,
                 RefinedRice = RefinedRice, RefinedBread = RefinedBread, WholeBread = WholeBread,
                 Wine = Wine)

    Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "daily")

    Vegetables <- Vars$Vegetables
    Fruit <- Vars$Fruit
    OliveOil <- Vars$OliveOil
    Legumes <- Vars$Legumes
    Fish <- Vars$Fish
    Meat <- Vars$Meat
    RefinedRice <- Vars$RefinedRice
    RefinedBread <- Vars$RefinedBread
    WholeBread <- Vars$WholeBread
    Wine <- Vars$Wine

  } else {
    if(frequency != "daily"){stop("accepted values for 'frequency' argument are 'daily', 'weekly' and 'monthly'")}
  }


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
  OOscore[OliveOil >= 1] <- 1

  Frscore <- numeric()
  Frscore[Fruit < 1] <- 0
  Frscore[Fruit >= 1] <- 1

  Vscore <- numeric()
  Vscore[Vegetables < 1] <- 0
  Vscore[Vegetables >= 1] <- 1

  # this score marks 1 point if both vegetables and fruit consumption have received 1 point each one
  FVscore <- numeric()
  FVscore[Frscore == 1 & Vscore == 1] <- 1
  FVscore[Frscore == 0 | Vscore == 0] <- 0

  LegumesWeek <- 7 * Legumes
  Lscore <- numeric()
  Lscore[LegumesWeek < 2] <- 0
  Lscore[LegumesWeek >= 2] <- 1

  FishWeek <- 7 * Fish
  Fiscore <- numeric()
  Fiscore[FishWeek < 3] <- 0
  Fiscore[FishWeek >= 3] <- 1

  Wscore <- numeric()
  Wscore[Wine < 1] <- 0
  Wscore[Wine >= 1] <- 1


  # this item scores either a small amount of refined cereals, or enough amount of whole bread
  RefinedRiceW <- 7 * RefinedRice
  WholeBreadW <- 7 * WholeBread

  Rscore <- numeric()
  Rscore <- ifelse((RefinedBread < 1 & RefinedRiceW < 7) | (WholeBreadW > 5), 1, 0)

  score <- data.frame(OOscore, Frscore, Vscore, FVscore, Lscore, Fiscore, Wscore, Rscore)

  score$absolute <- apply(score, 1, function(x) sum(x, na.rm = rm.na))
  score$percent <- round(100 * score$absolute / 9, 1)


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
