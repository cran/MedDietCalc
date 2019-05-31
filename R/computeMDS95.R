computeMDS95 <- function (data, Vegetables, Legumes, FruitAndNuts, Cereals, Potatoes = NULL, Meat, Dairy, Alcohol,
                          Fats = NULL, MUFA = NULL, SFA = NULL,
                          Sex, men = "male", women = "female",
                          frequency = NULL, output = "percent", rm.na = FALSE) {

  arguments <- as.list( match.call() )
  Vegetables <- eval(arguments$Vegetables, data)
  Legumes <- eval(arguments$Legumes, data)
  FruitAndNuts <- eval(arguments$FruitAndNuts, data)
  Cereals <- eval(arguments$Cereals, data)
  Potatoes <- eval(arguments$Potatoes, data)
  Meat <- eval(arguments$Meat, data)
  Dairy <- eval(arguments$Dairy, data)
  Alcohol <- eval(arguments$Alcohol, data)
  Fats <- eval(arguments$Fats, data)
  MUFA <- eval(arguments$MUFA, data)
  SFA <- eval(arguments$SFA, data)
  Sex <- eval(arguments$Sex, data)



  # this code chunk tests if data has not been introduced in a daily fashion, and if so, transform data to daily consumption
  if(is.null(frequency)){stop("please, provide the frequency of consumption in which the data is tabulated with the 'frequency' argument. Accepted values are 'daily', 'weekly' and 'monthly'")}

  if(frequency == "weekly" || frequency == "monthly"){
    Vars <- list(Vegetables = Vegetables, Legumes = Legumes, FruitAndNuts = FruitAndNuts, Cereals = Cereals,
                 Potatoes = Potatoes, Meat = Meat, Dairy = Dairy, Alcohol = Alcohol,
                 Fats = Fats, MUFA = MUFA, SFA = SFA)

    Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "daily")

    Vegetables <- Vars$Vegetables
    Legumes <- Vars$Legumes
    FruitAndNuts <- Vars$FruitAndNuts
    Cereals <- Vars$Cereals
    Potatoes <- Vars$Potatoes
    Meat <- Vars$Meat
    Dairy <- Vars$Dairy
    Alcohol <- Vars$Alcohol
    Fats <- Vars$Fats
    MUFA <- Vars$MUFA
    SFA <- Vars$SFA

  } else {
    if(frequency != "daily"){stop("accepted values for 'frequency' argument are 'daily', 'weekly' and 'monthly'")}
  }



  Me <- function(x) {stats::median(x, na.rm = TRUE)}


  Vscore <- rep(NA, length = nrow(data))
  Vscore[Sex == men & Vegetables < Me(Vegetables[Sex == men])] <- 0
  Vscore[Sex == men & Vegetables >= Me(Vegetables[Sex == men])] <- 1
  Vscore[Sex == women & Vegetables < Me(Vegetables[Sex == women])] <- 0
  Vscore[Sex == women & Vegetables >= Me(Vegetables[Sex == women])] <- 1
  
  Lscore <- rep(NA, length = nrow(data))
  Lscore[Sex == men & Legumes < Me(Legumes[Sex == men])] <- 0
  Lscore[Sex == men & Legumes >= Me(Legumes[Sex == men])] <- 1
  Lscore[Sex == women & Legumes < Me(Legumes[Sex == women])] <- 0
  Lscore[Sex == women & Legumes >= Me(Legumes[Sex == women])] <- 1
  
  Frscore <- rep(NA, length = nrow(data))
  Frscore[Sex == men & FruitAndNuts < Me(FruitAndNuts[Sex == men])] <- 0
  Frscore[Sex == men & FruitAndNuts >= Me(FruitAndNuts[Sex == men])] <- 1
  Frscore[Sex == women & FruitAndNuts < Me(FruitAndNuts[Sex == women])] <- 0
  Frscore[Sex == women & FruitAndNuts >= Me(FruitAndNuts[Sex == women])] <- 1

  # here, it checks if potatoes are provided in order to join them with cereals
  if(!is.null(Potatoes)) {Cereals <- Cereals + Potatoes
  print("NOTE: Potatoes consumption has been included in cereals scoring, as 'Potatoes' argument has been provided. Please note that currently it is usually suggested not to include potatoes with cereal scoring")
  } else {
    print("NOTE: Potatoes consumption has not been included in cereals scoring -as currently suggested-, because 'Potatoes' argument has not been provided")
  }
  
  Cscore <- rep(NA, length = nrow(data))
  Cscore[Sex == men & Cereals < Me(Cereals[Sex == men])] <- 0
  Cscore[Sex == men & Cereals >= Me(Cereals[Sex == men])] <- 1
  Cscore[Sex == women & Cereals < Me(Cereals[Sex == women])] <- 0
  Cscore[Sex == women & Cereals >= Me(Cereals[Sex == women])] <- 1

  Mscore <- rep(NA, length = nrow(data))
  Mscore[Sex == men & Meat < Me(Meat[Sex == men])] <- 1
  Mscore[Sex == men & Meat >= Me(Meat[Sex == men])] <- 0
  Mscore[Sex == women & Meat < Me(Meat[Sex == women])] <- 1
  Mscore[Sex == women & Meat >= Me(Meat[Sex == women])] <- 0
  
  Dscore <- rep(NA, length = nrow(data))
  Dscore[Sex == men & Dairy < Me(Dairy[Sex == men])] <- 1
  Dscore[Sex == men & Dairy >= Me(Dairy[Sex == men])] <- 0
  Dscore[Sex == women & Dairy < Me(Dairy[Sex == women])] <- 1
  Dscore[Sex == women & Dairy >= Me(Dairy[Sex == women])] <- 0

  Ascore <- rep(NA, length = nrow(data))
  Ascore[Sex == men & Alcohol >= 10 & Alcohol <= 50] <- 1
  Ascore[Sex == men & (Alcohol < 10 | Alcohol > 50)] <- 0
  Ascore[Sex == women & Alcohol >= 5 & Alcohol <= 25] <- 1
  Ascore[Sex == women & (Alcohol < 5 | Alcohol > 25)] <- 0

  Fatscore <- numeric(length = nrow(data))
  FATS <- MUFA / SFA


if(!is.null(Fats) && (is.null(MUFA) || is.null(SFA))) {
  FATS <- Fats
}

if(is.null(Fats) && !is.null(MUFA) && !is.null(SFA)){
  FATS <- MUFA / SFA
}

if(!is.null(Fats) && (!is.null(MUFA) || !is.null(SFA))) {
  FATS <- Fats
  warning("To compute the score, the 'Fats' argument has been used, but redundandt arguments ('MUFA' or 'SFA' of both) have been provided, please, check if the arguments have been properly writen or if mistyping happenend. If you don't want to get this warning, provide 'Fats' argument or the diada 'MUFA' and 'SFA', but not both.")
  }

  Fatscore[Sex == men & FATS < Me(FATS[Sex == men])] <- 0
  Fatscore[Sex == men & FATS >= Me(FATS[Sex == men])] <- 1
  Fatscore[Sex == women & FATS < Me(FATS[Sex == women])] <- 0
  Fatscore[Sex == women & FATS >= Me(FATS[Sex == women])] <- 1


  score <- data.frame(Vscore, Lscore, Frscore, Cscore, Mscore, Dscore, Ascore, Fatscore)


  score$absolute <- apply(score, 1, function(x) sum(x, na.rm = rm.na))
  score$percent <- round(100 * score$absolute / 8, 1)


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
