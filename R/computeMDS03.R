computeMDS03 <- function (data, Vegetables, Legumes, FruitAndNuts, Cereals, Potatoes = NULL, Fish, Meat, Dairy, Alcohol,
                            Fats = NULL, MUFA = NULL, SFA = NULL,
                            Sex, men = "male", women = "female",
                            frequency = "daily", output = "percent", rm.na = FALSE) {

  arguments <- as.list( match.call() )
  Vegetables <- eval(arguments$Vegetables, data)
  Legumes <- eval(arguments$Legumes, data)
  FruitAndNuts <- eval(arguments$FruitAndNuts, data)
  Cereals <- eval(arguments$Cereals, data)
  Potatoes <- eval(arguments$Potatoes, data)
  Fish <- eval(arguments$Fish, data)
  Meat <- eval(arguments$Meat, data)
  Dairy <- eval(arguments$Dairy, data)
  Alcohol <- eval(arguments$Alcohol, data)
  Fats <- eval(arguments$Fats, data)
  MUFA <- eval(arguments$MUFA, data)
  PUFA <- eval(arguments$PUFA, data)
  SFA <- eval(arguments$SFA, data)
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
                 Potatoes = Potatoes, Fish = Fish, Meat = Meat, Dairy = Dairy, Alcohol = Alcohol,
                 Fats = Fats, MUFA = MUFA, SFA = SFA)

    Vars <- periodicity(Vars, OriginalFreq = frequency, TargetFreq = "daily")

    Vegetables <- Vars$Vegetables
    Legumes <- Vars$Legumes
    FruitAndNuts <- Vars$FruitAndNuts
    Cereals <- Vars$Cereals
    Potatoes <- Vars$Potatoes
    Fish <- Vars$Fish
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

  Vscore <- numeric(length = nrow(data))
  Vscore[Vegetables[Sex == men] < Me(Vegetables[Sex == men])] <- 0
  Vscore[Vegetables[Sex == men] >= Me(Vegetables[Sex == men])] <- 1
  Vscore[Vegetables[Sex == women] < Me(Vegetables[Sex == women])] <- 0
  Vscore[Vegetables[Sex == women] >= Me(Vegetables[Sex == women])] <- 1

  Lscore <- numeric(length = nrow(data))
  Lscore[Legumes[Sex == men] < Me(Legumes[Sex == men])] <- 0
  Lscore[Legumes[Sex == men] >= Me(Legumes[Sex == men])] <- 1
  Lscore[Legumes[Sex == women] < Me(Legumes[Sex == women])] <- 0
  Lscore[Legumes[Sex == women] >= Me(Legumes[Sex == women])] <- 1

  Frscore <- numeric(length = nrow(data))
  Frscore[FruitAndNuts[Sex == men] < Me(FruitAndNuts[Sex == men])] <- 0
  Frscore[FruitAndNuts[Sex == men] >= Me(FruitAndNuts[Sex == men])] <- 1
  Frscore[FruitAndNuts[Sex == women] < Me(FruitAndNuts[Sex == women])] <- 0
  Frscore[FruitAndNuts[Sex == women] >= Me(FruitAndNuts[Sex == women])] <- 1


  # here, it checks if potatoes are provided in order to join them with cereals
  if(!missing(Potatoes)) {Cereals <- Cereals + Potatoes
  warning("Potatoes consumption has been included in cereals scoring, as 'Potatoes' argument has been provided")
  } else {warning("Potatoes consumption has not been included in cereals scoring, as 'Potatoes' argument has not been provided")}

  Cscore <- numeric(length = nrow(data))
  Cscore[Cereals[Sex == men] < Me(Cereals[Sex == men])] <- 0
  Cscore[Cereals[Sex == men] >= Me(Cereals[Sex == men])] <- 1
  Cscore[Cereals[Sex == women] < Me(Cereals[Sex == women])] <- 0
  Cscore[Cereals[Sex == women] >= Me(Cereals[Sex == women])] <- 1

  Fiscore <- numeric(length = nrow(data))
  Fiscore[Fish[Sex == men] < Me(Fish[Sex == men])] <- 0
  Fiscore[Fish[Sex == men] >= Me(Fish[Sex == men])] <- 1
  Fiscore[Fish[Sex == women] < Me(Fish[Sex == women])] <- 0
  Fiscore[Fish[Sex == women] >= Me(Fish[Sex == women])] <- 1

  Mscore <- numeric(length = nrow(data))
  Mscore[Meat[Sex == men] < Me(Meat[Sex == men])] <- 1
  Mscore[Meat[Sex == men] >= Me(Meat[Sex == men])] <- 0
  Mscore[Meat[Sex == women] < Me(Meat[Sex == women])] <- 1
  Mscore[Meat[Sex == women] >= Me(Meat[Sex == women])] <- 0

  Dscore <- numeric(length = nrow(data))
  Dscore[Dairy[Sex == men] < Me(Dairy[Sex == men])] <- 1
  Dscore[Dairy[Sex == men] >= Me(Dairy[Sex == men])] <- 0
  Dscore[Dairy[Sex == women] < Me(Dairy[Sex == women])] <- 1
  Dscore[Dairy[Sex == women] >= Me(Dairy[Sex == women])] <- 0

  Ascore <- numeric(length = nrow(data))
  Ascore[Alcohol[Sex == men] >= 10 & Alcohol[Sex == men] <= 50] <- 1
  Ascore[Alcohol[Sex == men] < 10 | Alcohol[Sex == men] > 50] <- 0
  Ascore[Alcohol[Sex == women] >= 5 & Alcohol[Sex == women] <= 25] <- 1
  Ascore[Alcohol[Sex == women] < 5 | Alcohol[Sex == women] > 25] <- 0

  Fatscore <- numeric(length = nrow(data))
  FATS <- MUFA / SFA

  ## NO HE CONSEGUDO HACERLO FUNCIONAR CON ESTA PARTE DE C?DIGO PARA EVALUAR LA FALTA DEL ARGUMENTO Fats,
  ## AL TENERLO AL INICIO  Fats <- eval(arguments$Fats, data) YA NO NO ES MISSING
  #if(!missing(Fats) && (missing(MUFA) || missing(PUFA) || missing(SFA))) {
  #  FATS <- Fats
  #}

  #if(missing(Fats) && !missing(MUFA) && !missing(PUFA) && !missing(SFA)){
  #  FATS <- (MUFA + PUFA) / SFA
  #}

  #if(!missing(Fats) && (!missing(MUFA) || !missing(PUFA) || !missing(SFA))) {
  #  FATS <- Fats
  #  warning("To compute the score, the 'Fats' argument has been used, but redundandt arguments ('MUFA', 'PUFA' or 'SFA') has been provided,
  #          please, check if the arguments have been properly writen or if mistyping happenend.
  #          If you don't want to get this warning, provide 'Fats' argument or the triada 'MUFA''PUFA''SFA', but not both.")
  #}
  ######################

  Fatscore[FATS[Sex == men] < Me(FATS[Sex == men])] <- 0
  Fatscore[FATS[Sex == men] >= Me(FATS[Sex == men])] <- 1
  Fatscore[FATS[Sex == women] < Me(FATS[Sex == women])] <- 0
  Fatscore[FATS[Sex == women] >= Me(FATS[Sex == women])] <- 1


  score <- data.frame(Vscore, Lscore, Frscore, Cscore, Fiscore, Mscore, Dscore, Ascore, Fatscore)


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
