computeMDQI <- function(data, FruitAndVegetables, OliveOil, OOmeasure = "gr", Fish, Cereals,
                        Meat, SatFats, Cholesterol,
                        Kcal = NULL, invert = TRUE,
                        frequency = NULL, output = "percent", rm.na = FALSE) {

  arguments <- as.list( match.call() )
    FruitAndVegetables <- eval(arguments$FruitAndVegetables, data)
    OliveOil <- eval(arguments$OliveOil, data)
    Fish <- eval(arguments$Fish, data)
    Cereals <- eval(arguments$Cereals, data)
    Meat <- eval(arguments$Meat, data)
    SatFats <- eval(arguments$SatFats, data)
    Cholesterol <- eval(arguments$Cholesterol, data)
    Kcal <- eval(arguments$Kcal, data)

    # this code chunk tests if data has not been introduced in a daily fashion, and if so, transform data to daily consumption
    if(is.null(frequency)){stop("please, provide the frequency of consumption in which the data is tabulated with the 'frequency' argument. Accepted values are 'daily', 'weekly' and 'monthly'")}

    if(frequency == "weekly" || frequency == "monthly"){
      Vars <- list(FruitAndVegetables = FruitAndVegetables, OliveOil = OliveOil, Fish = Fish, Cereals = Cereals,
                   Meat = Meat, SatFats = SatFats, Cholesterol = Cholesterol, Kcal = Kcal)

      Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "daily")

      FruitAndVegetables <- Vars$FruitAndVegetables
      OliveOil <- Vars$OliveOil
      Fish <- Vars$Fish
      Cereals <- Vars$Cereals
      Meat <- Vars$Meat
      SatFats <- Vars$SatFats
      Cholesterol <- Vars$Cholesterol
      Kcal <- Vars$Kcal

    } else {
      if(frequency != "daily"){stop("accepted values for 'frequency' argument are 'daily', 'weekly' and 'monthly'")}
    }

  FVscore <- numeric()
  FVscore[FruitAndVegetables < 400] <- 2
  FVscore[FruitAndVegetables >= 400 & FruitAndVegetables < 700] <- 1
  FVscore[FruitAndVegetables >= 700] <- 0

  if(OOmeasure == "gr") {OliveOil <- OliveOil/0.918} else {
    if(OOmeasure == "ml") {"no conversion needed"} else {stop("check units of Olive Oil")}
  }

  OOscore <- numeric()
  OOscore[OliveOil < 5] <- 2
  OOscore[OliveOil >= 5 & OliveOil < 15] <- 1
  OOscore[OliveOil >= 15] <- 0

  Fscore <- numeric()
  Fscore[Fish < 30] <- 2
  Fscore[Fish >= 30 & Fish < 60] <- 1
  Fscore[Fish >= 60] <- 0

  Cscore <- numeric()
  Cscore[Cereals < 100] <- 2
  Cscore[Cereals >= 100 & Cereals < 300] <- 1
  Cscore[Cereals >= 300] <- 0

  # Please note that Meats, Saturated Fats and Cholesterol are scored as detrimental to health
  Mscore <- numeric()
  Mscore[Meat < 25] <- 0
  Mscore[Meat >= 25 & Meat < 125] <- 1
  Mscore[Meat >= 125] <- 2

  if (!is.null(Kcal)) {SatFats <- 100*SatFats/Kcal}
  SFAscore <- numeric()
  SFAscore[SatFats < 10] <- 0
  SFAscore[SatFats >= 10 & SatFats < 13] <- 1
  SFAscore[SatFats >= 13] <- 2

  if (mean(Cholesterol, na.rm = TRUE) < 1) {warning("Please, check units of cholesterol, it should be provided as miligrams")}
  Chscore <- numeric()
  Chscore[Cholesterol < 300] <- 0
  Chscore[Cholesterol >= 300 & Cholesterol < 400] <- 1
  Chscore[Cholesterol >= 400] <- 2


  score <- data.frame(FVscore, OOscore, Fscore, Cscore, Mscore, SFAscore, Chscore)

  if(invert == TRUE) {score <- 2-score}

  score$absolute <- apply(score, 1, function(x) sum(x, na.rm = rm.na))
  score$percent <- round(100 * score$absolute / 14, 1)


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
