\name{computeMAI99}
\encoding{UTF-8}
\alias{computeMAI99}

\title{
computeMAI99
}
\description{
Computes Mediterranean Adequacy Index according to Alberti-Fidanza et al. 1999.
}
\usage{
computeMAI99(data, Bread, Cereals, Legumes, Potatoes,
           Vegetables, FruitAndNuts, Fish, Wine, Oil,
           Milk, Cheese, Meat, Eggs, AnimalFats, SoftDrinks, Pastries, Sugar,
           Kcal, output = NULL, rm.na = FALSE)
}

\arguments{
  \item{data}{Your data set with nutritional information about food or nutrient consumption. Each row is expected to be a person, and food or nutrient intake are in columns.}
  \item{Bread}{Numeric with energy (as Kilocalories) attributable to bread. The argument is the energy measured as Kcal, although the function will score it as percentage of energy respect total energy intake (see Details).}
  \item{Cereals}{Numeric with energy (as Kilocalories) attributable to cereals.}
  \item{Legumes}{Numeric with energy (as Kilocalories) attributable to legumes.}
  \item{Potatoes}{Numeric with energy (as Kilocalories) attributable to potatoes.}
  \item{Vegetables}{Numeric with energy (as Kilocalories) attributable to vegetables.}
  \item{FruitAndNuts}{Numeric with energy (as Kilocalories) attributable to FruitAndNuts.}
  \item{Fish}{Numeric with energy (as Kilocalories) attributable to fish.}
  \item{Wine}{Numeric with energy (as Kilocalories) attributable to wine.}
  \item{Oil}{Numeric with energy (as Kilocalories) attributable to vegetal oils.}
  \item{Milk}{Numeric with energy (as Kilocalories) attributable to milk.}
  \item{Cheese}{Numeric with energy (as Kilocalories) attributable to cheese.}
  \item{Meat}{Numeric with energy (as Kilocalories) attributable to meat.}
  \item{Eggs}{Numeric with energy (as Kilocalories) attributable to eggs.}
  \item{AnimalFats}{Numeric with energy (as Kilocalories) attributable to fats of animal origin.}
  \item{SoftDrinks}{Numeric with energy (as Kilocalories) attributable to soft drinks.}
  \item{Pastries}{Numeric with energy (as Kilocalories) attributable to pastries.}
  \item{Sugar}{Numeric with energy (as Kilocalories) attributable to sugar.}
  \item{Kcal}{Numeric with total energy intake measured as Kcal.}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame' and 'index'.}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned, but percent of score adherence will be computed with basis of the whole score range (see Details)}
}

\details{
The index components are scored as percent of calories. But to make it easier to the user, arguments should provide the energy each food item provides. Also the total energy intake should be provided, so formula can internally relate them.

Mediterranean Adequacy Index is a ratio of Kcal attributable to healthy foods over Kcal attributable to unhealthy foods, so values could range from 0 to more than 100 (Alberti et al. 2009). The reference italian-mediterranean diet is 7.5 (Alberti-Fidanza et al. 1999). So, value is not a percentage, and comparability with other scores is not direct.

Periodicity argument is not provided, as the equation is a ratio and it is not to vary if food is recorded daily, weekly or monthly.

}
\value{
Computed Mediterranean Adequacy Index. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'index'}{Instead of the full data.frame, just the numeric vector corresponding to the absolute points of adherence to Mediterranean Diet for each person. Range can vary widely (see Details).}
}

\references{
Alberti-Fidanza, A., F. Fidanza, M. P. Chiuchiù, G. Verducci, and D. Fruttini. 1999. "Dietary Studies on Two Rural Italian Population Groups of the Seven Countries Study. 3. Trend Of Food and Nutrient Intake from 1960 to 1991." European Journal of Clinical Nutrition 53 (11): 854–60.

Alberti, Adalberta, Daniela Fruttini, and Flaminio Fidanza. 2009. "The Mediterranean Adequacy Index: Further Confirming Results of Validity.” Nutrition, Metabolism and Cardiovascular Diseases 19 (1): 61–66. doi:10.1016/j.numecd.2007.11.008.
}

\author{
Miguel Menendez
}

\examples{
data(nutriSample)

MedDiet <- computeMAI99(data = nutriSample,
  Bread = P55Kcal + P56Kcal + P57Kcal,
  Cereals = P55Kcal + P56Kcal + P57Kcal + P59Kcal + P60Kcal + P61Kcal + P62Kcal,
  Legumes = P46Kcal,
  Potatoes = P43Kcal + P44Kcal + P45Kcal,
  Vegetables = P41Kcal + P42Kcal,
  FruitAndNuts = P50Kcal + P53Kcal,
  Fish = P35Kcal + P36Kcal + P37Kcal + P38Kcal,
  Wine = P96Kcal,
  Oil = AceiteKcal,
  Milk = P19Kcal + P20Kcal + P21Kcal,
  Cheese = P26Kcal + P27Kcal,
  Meat = P29Kcal + P30Kcal + P31Kcal + P32Kcal,
  Eggs = P28Kcal,
  AnimalFats = P29grGrasa + P30grGrasa + P31grGrasa + P32grGrasa + P33grGrasa + P34grGrasa ,
  SoftDrinks = P89Kcal + P90Kcal,
  Pastries = P69Kcal + P70Kcal + P71Kcal + P72Kcal + P73Kcal,
  Sugar = P84Kcal,
  Kcal = totalKcal,
  output = "index", rm.na = FALSE)
hist(MedDiet)
}

