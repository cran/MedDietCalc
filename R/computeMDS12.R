computeMDS12 <- function (data, Vegetables, Legumes, FruitAndNuts, Cereals, Potatoes = NULL, Fish, Dairy, Meat, Alcohol,
                          OOprincipal, Sex, men = "male", women = "female",
                          frequency = NULL, output = "percent", rm.na = FALSE) {

  arguments <- as.list( match.call() )
  Vegetables <- eval(arguments$Vegetables, data)
  Legumes <- eval(arguments$Legumes, data)
  FruitAndNuts <- eval(arguments$FruitAndNuts, data)
  Cereals <- eval(arguments$Cereals, data)
  Potatoes <- eval(arguments$Potatoes, data)
  Fish <- eval(arguments$Fish, data)
  Dairy <- eval(arguments$Dairy, data)
  Meat <- eval(arguments$Meat, data)
  Alcohol <- eval(arguments$Alcohol, data)
  OOprincipal <- eval(arguments$OOprincipal, data)
  Sex <- eval(arguments$Sex, data)



  # this code chunk tests if data has not been introduced in a daily fashion, and if so, transform data to daily consumption
  if(is.null(frequency)){stop("please, provide the frequency of consumption in which the data is tabulated with the 'frequency' argument. Accepted values are 'daily', 'weekly' and 'monthly'")}

  if(frequency == "weekly" || frequency == "monthly"){
    Vars <- list(Vegetables = Vegetables, Legumes = Legumes, FruitAndNuts = FruitAndNuts, Cereals = Cereals,
                 Potatoes = Potatoes, Fish = Fish, Dairy = Dairy, Meat = Meat, Alcohol = Alcohol)

    Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "daily")

    Vegetables <- Vars$Vegetables
    Legumes <- Vars$Legumes
    FruitAndNuts <- Vars$FruitAndNuts
    Cereals <- Vars$Cereals
    Potatoes <- Vars$Potatoes
    Fish <- Vars$Fish
    Dairy <- Vars$Dairy
    Meat <- Vars$Meat
    Alcohol <- Vars$Alcohol

  } else {
    if(frequency != "daily"){stop("accepted values for 'frequency' argument are 'daily', 'weekly' and 'monthly'")}
  }

  Vscore <- rep(NA, length = nrow(data))
  Vscore[Sex == men & Vegetables <= 550] <- 0
  Vscore[Sex == men & Vegetables > 550] <- 2
  Vscore[Sex == women & Vegetables < 350] <- 0
  Vscore[Sex == women & Vegetables >= 350 & Vegetables <= 500] <- 1
  Vscore[Sex == women & Vegetables > 500] <- 2

  Lscore <- rep(NA, length = nrow(data))
  Lscore[Sex == men & Legumes <= 6] <- 0
  Lscore[Sex == men & Legumes > 6] <- 2
  Lscore[Sex == women & Legumes <= 7] <- 0
  Lscore[Sex == women & Legumes > 7] <- 2

  Frscore <- rep(NA, length = nrow(data))
  Frscore[Sex == men & FruitAndNuts < 250] <- 0
  Frscore[Sex == men & FruitAndNuts >= 250 & FruitAndNuts <= 500] <- 1
  Frscore[Sex == men & FruitAndNuts > 500] <- 2
  Frscore[Sex == women & FruitAndNuts < 300] <- 0
  Frscore[Sex == women & FruitAndNuts >= 300 & FruitAndNuts <= 350] <- 1
  Frscore[Sex == women & FruitAndNuts > 350] <- 2


  # here, it checks if potatoes are provided in order to join them with cereals
  if(!is.null(Potatoes)) {Cereals <- Cereals + Potatoes
  print("NOTE: Potatoes consumption has been included in cereals scoring, as 'Potatoes' argument has been provided. Please note that currently it is usually suggested not to include potatoes with cereal scoring")
  } else {print("NOTE: Potatoes consumption has not been included in cereals scoring -as currently suggested-, because 'Potatoes' argument has not been provided")}

  Cscore <- rep(NA, length = nrow(data))
  Cscore[Sex == men & Cereals <= 150] <- 0
  Cscore[Sex == men & Cereals > 150] <- 2
  Cscore[Sex == women & Cereals < 130] <- 0
  Cscore[Sex == women & Cereals >= 130 & Cereals <= 150] <- 1
  Cscore[Sex == women & Cereals > 150] <- 2

  Fiscore <- rep(NA, length = nrow(data))
  Fiscore[Sex == men & Fish <= 15] <- 0
  Fiscore[Sex == men & Fish > 15] <- 2
  Fiscore[Sex == women & Fish <= 10] <- 0
  Fiscore[Sex == women & Fish > 10] <- 2

  # Please note this item is scored reversely
  Dscore <- rep(NA, length = nrow(data))
  Dscore[Sex == men & Dairy <= 150] <- 2
  Dscore[Sex == men & Dairy > 150] <- 0
  Dscore[Sex == women & Dairy < 100] <- 2
  Dscore[Sex == women & Dairy >= 100 & Dairy <= 130] <- 1
  Dscore[Sex == women & Dairy > 130] <- 0

  # Please note this item is scored reversely
  Mscore <- rep(NA, length = nrow(data))
  Mscore[Sex == men & Meat < 140] <- 2
  Mscore[Sex == men & Meat >= 140 & Meat <= 150] <- 1
  Mscore[Sex == men & Meat > 150] <- 0
  Mscore[Sex == women & Meat < 130] <- 2
  Mscore[Sex == women & Meat >= 130 & Meat <= 150] <- 1
  Mscore[Sex == women & Meat > 150] <- 0

  Ascore <- rep(NA, length = nrow(data))
  Ascore[Sex == men & Alcohol < 12] <- 1
  Ascore[Sex == men & Alcohol >= 12 & Alcohol <= 24] <- 2
  Ascore[Sex == men & Alcohol > 24] <- 0
  Ascore[Sex == women & Alcohol < 6] <- 1
  Ascore[Sex == women & Alcohol >= 6 & Alcohol <= 12] <- 2
  Ascore[Sex == women & Alcohol > 12] <- 0

  OOscore <- ifelse(OOprincipal == 1, 2, 0)

  score <- data.frame(Vscore, Lscore, Frscore, Cscore, Fiscore, Dscore, Mscore, Ascore, OOscore)

  score$absolute <- apply(score, 1, function(x) sum(x, na.rm = rm.na))
  score$percent <- round(100 * score$absolute / 18, 1)


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
