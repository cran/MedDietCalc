computeMAI99 <- function(data, Bread, Cereals, Legumes, Potatoes, Vegetables, FruitAndNuts,
                        Fish, Wine, Oil, Milk, Cheese, Meat, Eggs, AnimalFats, SoftDrinks, Pastries, Sugar,
                        Kcal,
                        output = NULL, rm.na = FALSE){


  arguments <- as.list(match.call())

df <- data.frame(
  Br = eval(arguments$Bread, data),
  Ce = eval(arguments$Cereals, data),
  Le = eval(arguments$Legumes, data),
  Po = eval(arguments$Potatoes, data),
  Ve = eval(arguments$Vegetables, data),
  FN = eval(arguments$FruitAndNuts, data),
  Fi = eval(arguments$Fish, data),
  Wi = eval(arguments$Wine, data),
  Oil = eval(arguments$Oil, data),

  Mi = eval(arguments$Milk, data),
  Ch = eval(arguments$Cheese, data),
  Me = eval(arguments$Meat, data),
  Eg = eval(arguments$Eggs, data),
  Fa = eval(arguments$AnimalFats, data),
  SD = eval(arguments$SoftDrinks, data),
  Pa = eval(arguments$Pastries, data),
  Su = eval(arguments$Sugar, data)
)

  Kcal = eval(arguments$Kcal, data)

df <- 100 * df / Kcal

num <- apply(df[ ,c("Br", "Ce", "Le", "Po", "Ve", "FN", "Fi", "Wi", "Oil")], 1, sum)
den <- apply(df[ ,c("Mi", "Ch", "Me", "Eg", "Fa", "SD", "Pa", "Su")], 1, sum)

df$MAI <- round(num / den, 2)


if(output == "index"){return(df$MAI)
}else{
    if(output == "data.frame"){return(df)
    }else{
        (stop("please, chec 'output' argument. Allowed values are 'index' and 'data.frame'"))
      }
}

}
