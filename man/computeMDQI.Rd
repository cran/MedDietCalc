\name{computeMDQI}
\encoding{UTF-8}
\alias{computeMDQI}

\title{
computeMDQI
}
\description{
Computes Mediterranean Diet Quality Index.
}
\usage{
computeMDQI(data, FruitAndVegetables, OliveOil, OOmeasure = "gr", Fish, Cereals,
            Meat, SatFats, Cholesterol,
            Kcal = NULL, invert = TRUE,
            frequency = NULL, output = "percent", rm.na = FALSE)
}

\arguments{
  \item{data}{Your data set with nutritional information about food or nutrient consumption. Each row is expected to be a person, and food or nutrient intake are in columns}
  \item{FruitAndVegetables}{Numeric variable with consumption of fruit and vegetables as grams}
  \item{OliveOil}{Numeric variable with olive oil consumption, measure is set with the 'OOmeasure' argument}
  \item{OOmeasure}{Character string which informs about the unit of the argument 'OliveOil'. Allowed values are 'gr', 'ml' and 'serving', which means respectively grams, mililiters and servings of 1 table spoon (15 ml).}
  \item{Fish}{Numeric variable with fish consumption as grams}
  \item{Cereals}{Numeric variable with cereals consumption as grams}
  \item{Meat}{Numeric variable with Meat consumption as grams}
  \item{SatFats}{Numeric variable with energy contribution  of saturated fats to diet. The formula will score it as percent of total energy intake, but it can be provided in one of two ways (see Details)}
  \item{Cholesterol}{Numeric variable with cholesterol consumption as miligrams}
  \item{Kcal}{Optional numeric variable with total energy intake as kilocalories. If provided, it makes a modification in 'SatFats' argument (see Details)}
  \item{invert}{Logical. If set to TRUE (default), the score is inverted, if set to FALSE, the score is kept as in the original (see Details)}
  \item{frequency}{A character string. Allowed values are 'daily', 'weekly' and 'monthly'. It informs about the frequency which food or nutrient consumption refers to (i.e. wether the rest of arguments are 'grams per day' or 'grams per week' or 'grams per month')}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame', 'score' and 'percent' (default).}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned, but percent of score adherence will be computed with basis of the whole score range (see Details)}

}
\details{
This score is a mediterranean adaptation [Scali et al., 2000; Gerber 2006] from a previous Diet Quality Index (DQI) by Patterson et al. [Patterson et al., 1994], thus it was named MDQI (Mediterraean DQI).

In this score, originally, higher puntuations mean LOWER adherence. As this is not the usual in mediterranean diet scores, the argument 'invert' can make it reverse. If invert = TRUE (default), higher puntuations mean higher adherence.

Saturated fats (SFA) are scored as percent of total energy that is provided by SFA. This information can be provided in one of two ways: 1) 'SatFats' argument can be directly the percent of total energy intake provided by SFA, if so, the 'Kcal' argument must be missing or NULL. 2) 'SatFats' argument can be the amount of kilocalories provided by SFA, if so, the 'Kcal' argument must be provided, for formula to know the required percentage.

Cholesterol should be provided as miligrams. If mean consumption of cholesterol is lower than 1, a warning will be produce to ask user to check units.
}


\value{Computed MDQI score. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'score'}{Instead of the full data.frame, just the integer vector corresponding to the absolute points of adherence to Mediterranean Diet for each person, ranging from 0 to 14. Depending on 'invert' argument higher puntuations can mean higher or lower adherence (see Details)}
  \item{if output = 'percent'}{Instead of the full data.frame, just the numeric vector corresponding to the percent of adherence to Mediterranean Diet for each person. Depending on 'invert' argument higher puntuations can mean higher or lower adherence (see Details)}
}


\references{
Patterson, R. E., P. S. Haines, and B. M. Popkin. 1994. 'Diet Quality Index: Capturing a Multidimensional Behavior'. Journal of the American Dietetic Association 94 (1): 57-64.


Scali, Jacqueline, Aurelia Richard, and Mariette Gerber. 2001. 'Diet Profiles in a Population Sample from Mediterranean Southern France'. Public Health Nutrition 4 (02): 173-182. doi:10.1079/PHN200065.


Gerber, Mariette. 2006. 'Qualitative Methods to Evaluate Mediterranean Diet in Adults'. Public Health Nutrition 9 (1A): 147-51.

}
\author{
Miguel Menendez
}


\examples{
data(nutriSample)

# If Saturated Fats are provided as the energy they provide,
# and Kcal arguments informs about total energy intake:
MedDiet <- computeMDQI(data = nutriSample,
                      FruitAndVegetables = P50grCom + P52grCom + P41grCom + P42grCom,
                      OliveOil = Aceitegr,
                      OOmeasure = "gr",
                      Fish = P35grCom + P36grCom + P37grCom + P38grCom,
                      Cereals = P55grCom + P56grCom + P57grCom + P59grCom +
                                P60grCom + P61grCom + P62grCom,
                      Meat = P29grCom + P30grCom + P31grCom + P32grCom,
                      SatFats = totalGrasaSat,
                      Cholesterol = totalCol,
                      Kcal = totalKcal,
                      invert = TRUE,
                      frequency = "daily", output = "percent", rm.na = FALSE)


# If Saturated Fats are provided as the percent of energy they provide, so Kcal is not provided:
nutriSample$MySFApercent <- 100 * nutriSample$totalGrasaSat / nutriSample$totalKcal

MedDiet2 <- computeMDQI(data = nutriSample,
                      FruitAndVegetables = P50grCom + P52grCom + P41grCom + P42grCom,
                      OliveOil = Aceitegr,
                      OOmeasure = "gr",
                      Fish = P35grCom + P36grCom + P37grCom + P38grCom,
                      Cereals = P55grCom + P56grCom + P57grCom + P59grCom +
                                P60grCom + P61grCom + P62grCom,
                      Meat = P29grCom + P30grCom + P31grCom + P32grCom,
                      SatFats = MySFApercent,
                      Cholesterol = totalCol,
                      # don't provide Kcal
                      invert = TRUE,
                      frequency = "daily", output = "percent", rm.na = FALSE)

hist(MedDiet2)
}

