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


  # This code chunk checks the 'Sex' argument to identify if it contents enough information to understand sex labels, or if it needs the 'men' and 'women' arguments
  if((missing(men) || missing(women)) && is.numeric(Sex)) {stop("'Sex' argument is numeric, and the function knows not how to handle it, please, provide 'men' and 'women' arguments (v.gr. men=1, women=2)")}

  if(missing(men) || missing(women) && (is.factor(Sex) || is.character(Sex))) {
    if(any(levels(Sex)) %in% c("man", "male", "MAN", "Male", "MALE")) {men <- levels(Sex)[which(levels(Sex)) %in% c("man", "male", "MAN", "Male", "MALE")]
    } else {
      if(any(levels(Sex)) %in% c("woman", "female", "WOMAN", "Female", "FEMALE")) {women <- levels(Sex)[which(levels(Sex)) %in% c("woman", "female", "WOMAN", "Female", "FEMALE")]
      } else {
        if(any(names(table(Sex)) %in% c("man", "male", "MAN", "Male", "MALE"))) {men <- names(table(Sex))[which(names(table(Sex)) %in% c("man", "male", "MAN", "Male", "MALE"))]
        } else {
          if(any(names(table(Sex)) %in% c("woman", "female", "WOMAN", "Female", "FEMALE"))) {women <- names(table(Sex))[which(names(table(Sex)) %in% c("woman", "female", "WOMAN", "Female", "FEMALE"))]
          } else {stop("function knows not to handle the 'Sex' argument, please, set values for men and women identification with 'men' and 'women' arguments")}
        }
      }
    }
  }


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

  Vscore <- numeric(length = nrow(data))
  Vscore[Vegetables[Sex == men] <= 550] <- 0
  Vscore[Vegetables[Sex == men] >= 550] <- 2
  Vscore[Vegetables[Sex == women] < 350] <- 0
  Vscore[Vegetables[Sex == women] >= 350 & Vegetables[Sex == women] <= 500] <- 1
  Vscore[Vegetables[Sex == women] > 500] <- 2

  Lscore <- numeric(length = nrow(data))
  Lscore[Legumes[Sex == men] <= 6] <- 0
  Lscore[Legumes[Sex == men] > 6] <- 2
  Lscore[Legumes[Sex == women] <= 7] <- 0
  Lscore[Legumes[Sex == women] > 7] <- 2

  Frscore <- numeric(length = nrow(data))
  Frscore[FruitAndNuts[Sex == men] < 250] <- 0
  Frscore[FruitAndNuts[Sex == men] >= 250 & FruitAndNuts[Sex == men] <= 500] <- 1
  Frscore[FruitAndNuts[Sex == men] > 500] <- 2
  Frscore[FruitAndNuts[Sex == women] < 300] <- 0
  Frscore[FruitAndNuts[Sex == women] >= 300 & FruitAndNuts[Sex == women] <= 350] <- 1
  Frscore[FruitAndNuts[Sex == women] > 350] <- 2


  # here, it checks if potatoes are provided in order to join them with cereals
  if(!is.null(Potatoes)) {Cereals <- Cereals + Potatoes
  warning("Potatoes consumption has been included in cereals scoring, as 'Potatoes' argument has been provided")
  } else {warning("Potatoes consumption has not been included in cereals scoring, as 'Potatoes' argument has not been provided")}

  Cscore <- numeric(length = nrow(data))
  Cscore[Cereals[Sex == men] <= 150] <- 0
  Cscore[Cereals[Sex == men] > 150] <- 2
  Cscore[Cereals[Sex == women] < 130] <- 0
  Cscore[Cereals[Sex == women] >= 130 & Cereals[Sex == women] <= 150] <- 1
  Cscore[Cereals[Sex == women] > 150] <- 2

  Fiscore <- numeric(length = nrow(data))
  Fiscore[Fish[Sex == men] <= 15] <- 0
  Fiscore[Fish[Sex == men] > 15] <- 2
  Fiscore[Fish[Sex == women] <= 10] <- 0
  Fiscore[Fish[Sex == women] > 10] <- 2

  Dscore <- numeric(length = nrow(data))
  Dscore[Dairy[Sex == men] <= 150] <- 0
  Dscore[Dairy[Sex == men] > 150] <- 2
  Dscore[Dairy[Sex == women] < 100] <- 1
  Dscore[Dairy[Sex == women] >= 100 & Dairy[Sex == women] <= 130] <- 1
  Dscore[Dairy[Sex == women] > 130] <- 2

  Mscore <- numeric(length = nrow(data))
  Mscore[Meat[Sex == men] < 140] <- 0
  Mscore[Meat[Sex == men] >= 140 & Meat[Sex == men] <= 150] <- 1
  Mscore[Meat[Sex == men] > 150] <- 2
  Mscore[Meat[Sex == women] < 130] <- 0
  Mscore[Meat[Sex == women] >= 130 & Meat[Sex == women] <= 150] <- 1
  Mscore[Meat[Sex == women] > 150] <- 2


  Ascore <- numeric(length = nrow(data))
  Ascore[Alcohol[Sex == men] < 12] <- 1
  Ascore[Alcohol[Sex == men] >= 12 & Alcohol[Sex == men] <= 24] <- 2
  Ascore[Alcohol[Sex == men] > 24] <- 0
  Ascore[Alcohol[Sex == women] < 6] <- 1
  Ascore[Alcohol[Sex == women] >= 6 & Alcohol[Sex == women] <= 12] <- 2
  Ascore[Alcohol[Sex == women] > 12] <- 0

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
