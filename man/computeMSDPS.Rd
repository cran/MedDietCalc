\name{computeMSDPS}
\encoding{UTF-8}
\alias{computeMSDPS}

\title{
computeMSDPS
}

\description{
Computes Mediterranean-Style Dietary Pattern Score (MSDPS).
}

\usage{
computeMSDPS(data, WholeCereals, Fruit, Vegetables, Dairy, Wine,
             Fish, Poultry, LegumesAndMore, Potatoes, Eggs, Sweets,
             Meat, OOprincipal,
             WholeCerealsK, FruitK, VegetablesK, DairyK, WineK,
             FishK, PoultryK, LegumesAndMoreK, PotatoesK, EggsK, SweetsK,
             MeatK, OliveOilK, Kcal,
             Sex, men = "male", women = "female",
             output = "percent", frequency = "daily", rm.na = FALSE)
}

\arguments{
  \item{data}{Your data set with nutritional information about food or nutrient consumption. Each row is expected to be a person, and food or nutrient intake are in columns.}
  \item{WholeCereals}{Numeric variable with consumption of whole grain products as servings.}
  \item{Fruit}{Numeric variable with fruit consumption as servings.}
  \item{Vegetables}{Numeric variable with vegetables consumption as servings.}
  \item{Dairy}{Numeric variable with dairy consumption as servings.}
  \item{Wine}{Numeric variable with wine consumption as glasses.}
  \item{Fish}{Numeric variable with fish consumption as servings.}
  \item{Poultry}{Numeric variable with poultry consumption as servings.}
  \item{LegumesAndMore}{Numeric variable with legumes, nuts and olives consumption as servings.}
  \item{Potatoes}{Numeric variable with potatoes consumption as servings.}
  \item{Eggs}{Numeric variable with eggs consumption as servings.}
  \item{Sweets}{Numeric variable with sweets consumption as servings.}
  \item{Meat}{Numeric variable with red meat and meat products consumption as servings.}
  \item{OOprincipal}{Integer. This argument informs wether olive oil is the main dietary fat. 0 = olive oil is not usually consumed. 1 = olive oil and other vegetable oils are usually consumed. 2 = only olive oil is usually consumed.}
  \item{WholeCerealsK}{Numeric variable with energy (as Kcal) due to consumption of whole grain products.}
  \item{FruitK}{Numeric variable with energy (as Kcal) due to consumption of fruit.}
  \item{VegetablesK}{Numeric variable with energy (as Kcal) due to consumption of vegetables.}
  \item{DairyK}{Numeric variable with energy (as Kcal) due to consumption of dairy.}
  \item{WineK}{Numeric variable with energy (as Kcal) due to consumption of wine.}
  \item{FishK}{Numeric variable with energy (as Kcal) due to consumption of fish.}
  \item{PoultryK}{Numeric variable with energy (as Kcal) due to consumption of poultry.}
  \item{LegumesAndMoreK}{Numeric variable with energy (as Kcal) due to consumption of legumes, nuts and olives.}
  \item{PotatoesK}{Numeric variable with energy (as Kcal) due to consumption of potatoes.}
  \item{EggsK}{Numeric variable with energy (as Kcal) due to consumption of eggs.}
  \item{SweetsK}{Numeric variable with energy (as Kcal) due to consumption of sweets.}
  \item{MeatK}{Numeric variable with energy (as Kcal) due to consumption of red meat.}
  \item{OliveOilK}{Numeric variable with energy (as Kcal) due to consumption of olive oil.}
  \item{Kcal}{Numeric with total energy intake (as Kcal).}
  \item{Sex}{Vector with gender, it can be numeric, factor or character, as long as its values are provided by 'men' and 'women' arguments. If 'Sex' argument is character or factor, and values for male are either 'man', 'male', 'MAN' or 'MALE', and for females are 'woman', 'female', 'WOMAN' or 'FEMALE', then, the arguments 'men' and 'women' can be missing}
  \item{men}{A character string with the value of male gender, default is "male"}
  \item{women}{A character string with the value of female gender, default is "female"}
  \item{frequency}{A character string. Allowed values are 'daily', 'weekly' and 'monthly'. It informs about the frequency which food or nutrient consumption refers to (i.e. wether the rest of arguments are 'grams per day' or 'grams per week' or 'grams per month')}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame' and 'percent' (default).}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned, but percent of score adherence will be computed with basis of the whole score range (see Details)}
}


\details{
Computes Mediterranean-Style Dietary Pattern Score according to Rumawas et al. 2009.

\deqn{MSDPS = \frac{\Sigma S_{i}}{130} * 100 * P }{MSDPS = (\Sigma Si / 130) * 100 * P}

Where

\deqn{S_{i} = 10 - \frac{desviation-from-recomended[\%]}{10}}{Si = 10 - (desviation-from-recommended[\%] / 10)}

\deqn{P = \frac{Energy from med foods}{Total energy}}{P = Energy from med foods / Total energy}

As this scoring schema is not similar to others, we briefly explain it:

 Step 1: The Score "S" of an item "i" has full score (10 points) if its consumed amount is the same as the standard recommendation (for instance, for fruit, 3 servings a day). If the amount is different, both as a lack or as an excess, more or less points are taken from the maximun possible, depending on how big this difference is.

For instance, if a particular food consumption is 80\% of the recomended, the deviation from the recommendation is 20\%. This "0\% takes 2 points (1 point per each ten), so, instead of the maximun 10, this item deserves Si = 10 - 2 = 8 points.

Olive oil is not measured the same way as the other items. It is considered categorically: only olive oil (10 points), olive oil and other vegetable oils (5 points), no olive oil (0 points).

 Step 2: After all items have been computed, they are sumed, and considered a percentage of maximun possible (13 items * 10 points = 130). So, at this step range goes from 0 to 100\%.

 Step 3: The previous percentage is adjusted with a correction factor "P", ranging from 0 to 1. This correction factor is the proportion of total energy intake provided by all foods included in the mediterranean diet pyramid, i.e., each of the 13 foods included in the score, over total energy intake. This allows the use of the score in non-Mediterranean populations, where large proportion of energy intake comes from foods that wouldn't be found in a mediterranean diet pyramid (like sugar sweetened soft drinks or margarine). Al the arguments about energy intake information are used to compute this correction factor.

This way a 100\% is hard to reach.

Please note that Legumes are included with Nuts and Olives.
}

\value{
Computed Mediterranean-Style Dietary Pattern Score. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'percent'}{Instead of the full data.frame, just the numeric vector corresponding to the percent of adherence to Mediterranean Diet for each person, from 0 (min. adherence) to a theoretical maximun of 100\% (max. adhrence)}
}

\references{
Rumawas, Marcella E., Johanna T. Dwyer, Nicola M. Mckeown, James B. Meigs, Gail Rogers, and Paul F. Jacques. 2009. 'The Development of the Mediterranean-Style Dietary Pattern Score and Its Application to the American Diet in the Framingham Offspring Cohort'. The Journal of Nutrition 139 (6): 1150-56. doi:10.3945/jn.108.103424.
}

\author{
Miguel Menendez
}



\examples{
data(nutriSample)

# wether olive oil is principal or not is stored in the sample dataset
# in a different way than asked by formula.
# In the data set it is 1=olive oil, 2=seeds oil, 3=both
# so a transformation is performed:
Oil <- ifelse(nutriSample$AceiteTipo == 2, 0,
              ifelse(nutriSample$AceiteTipo == 3, 1,
                     ifelse(nutriSample$AceiteTipo == 1, 2, 0)))

MedDiet <-
computeMSDPS(data = nutriSample,
        # group of arguments about food consumption:
            WholeCereals = P56rac + ifelse(nutriSample$P63_2 == 2, nutriSample$P61rac, 0),
            Fruit = P50rac + P52rac,
            Vegetables = P41rac + P42rac,
            Dairy = P19rac + P20rac + P20rac + P22rac + P23rac +
                    P24rac + P25rac + P26rac + P27rac,
            Wine = P96rac,
            Fish = P35rac + P36rac + P37rac + P38rac,
            Poultry = P33rac,
            LegumesAndMore = P46rac + P53rac + P54rac,
            Potatoes = P43grCom +  P44grCom +  P45grCom,
            Eggs = P28rac,
            Sweets = P69rac + P70rac + P71rac + P72rac + P73rac,
            Meat = P29rac + P30rac + P31rac + P32rac,
            OOprincipal = Oil,

        # group of arguments about energy intake to compute correction factor:
            WholeCerealsK = P56Kcal + ifelse(nutriSample$P63_2 == 2, nutriSample$P61Kcal, 0),
            FruitK = P50Kcal + P52Kcal,
            VegetablesK = P41Kcal + P42Kcal,
            DairyK = P19Kcal + P20Kcal + P20Kcal + P22Kcal + P23Kcal +
                     P24Kcal + P25Kcal + P26Kcal + P27Kcal,
            WineK = P96Kcal,
            FishK = P35Kcal + P36Kcal + P37Kcal + P38Kcal,
            PoultryK = P33Kcal,
            LegumesAndMoreK = P46Kcal + P53Kcal + P54Kcal,
            PotatoesK = P43grCom +  P44grCom +  P45grCom,
            EggsK = P28Kcal,
            SweetsK = P69Kcal + P70Kcal + P71Kcal + P72Kcal + P73Kcal,
            MeatK = P29Kcal + P30Kcal + P31Kcal + P32Kcal,
            OliveOilK = AceiteKcal,
            Kcal = totalKcal,

        # final arguments:
            Sex = SEXO, men = "Hombre", women = "Mujer",
            output = "percent", frequency = "daily", rm.na = FALSE)
hist(MedDiet)
}

