\name{computeRMED}
\encoding{UTF-8}
\alias{computeRMED}

\title{
computeRMED
}

\description{
Computes the Revised Mediterranean Diet adherence score according to Buckland et al. in 2009, also known as rMED.
}

\usage{
computeRMED(data, FruitAndNuts, Vegetables, Legumes, Cereals, Fish,
            OliveOil, OOmeasure = "gr", Meat, Dairy, Alcohol,
            Kcal, Sex, men="male", women="female",
            frequency = NULL, output = "percent", rm.na = FALSE)
}

\arguments{
  \item{data}{Your data set with nutritional information about food or nutrient consumption. Each row is expected to be a person, and food or nutrient intake are in columns}
  \item{FruitAndNuts}{Numeric variable with consumption of Fruits and Nuts as grams.}
  \item{Vegetables}{Numeric variable with Vegetables consumption as grams}
  \item{Legumes}{Numeric variable with Legumes consumption as grams}
  \item{Cereals}{Numeric variable with Legumes consumption as grams}
  \item{Fish}{Numeric variable with Fish consumption as grams}
  \item{OliveOil}{Numeric variable with olive oil consumption, measure is set with the 'OOmeasure' argument}
  \item{OOmeasure}{Character string which informs about the unit of the argument 'OliveOil'. Allowed values are 'gr', 'ml' and 'serving', which means respectively grams, mililiters and servings of 1 table spoon (15 ml).}
  \item{Meat}{Numeric variable with Meat consumption as grams}
  \item{Dairy}{Numeric variable with Dairy consumption as grams}
  \item{Alcohol}{Numeric variable with Alcohol consumption as etanol grams from any beberage origin}
  \item{Kcal}{Numeric variable with energy consumption in kilocalories.}
  \item{Sex}{Vector with gender, it can be numeric, factor or character, as long as its values are provided by 'men' and 'women' arguments. If 'Sex' argument is character or factor, and values for male are either 'man', 'male', 'MAN' or 'MALE', and for females are 'woman', 'female', 'WOMAN' or 'FEMALE', then, the arguments 'men' and 'women' can be missing}
  \item{men}{A character string with the value of male gender, default is "male"}
  \item{women}{A character string with the value of female gender, default is "female"}
  \item{frequency}{A character string. Allowed values are 'daily', 'weekly' and 'monthly'. It informs about the frequency which food or nutrient consumption refers to (i.e. wether the rest of arguments are 'grams per day' or 'grams per week' or 'grams per month')}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame', 'score' and 'percent' (default).}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned, but percent of score adherence will be computed with basis of the whole score range (see Details)}
}

\details{
the rMED questionnaire scores food consumption as grams by 1000Kcal/day, but arguments are expected to be provided as grams eaten by day.
}

\value{Computed RMed score. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'score'}{Instead of the full data.frame, just the integer vector corresponding to the absolute points of adherence to Mediterranean Diet for each person, from 0 (min.) to 18 (max.)}
  \item{if output = 'percent'}{Instead of the full data.frame, just the numeric vector corresponding to the percent of adherence to Mediterranean Diet for each person, from 0 (min. adherence) to 100 percent (max. adhrence)}
}

\references{
Buckland, Genevieve, Carlos A. Gonzalez, Antonio Agudo, Mireia Vilardell, Antoni Berenguer, Pilar Amiano, Eva Ardanaz, et al. 2009. 'Adherence to the Mediterranean Diet and Risk of Coronary Heart Disease in the Spanish EPIC Cohort Study'. American Journal of Epidemiology, January, kwp282. doi:10.1093/aje/kwp282.

}

\author{
Miguel Menendez
}


\examples{
data(nutriSample)

MedDiet <- computeRMED(data = nutriSample,
                      Kcal = totalKcal,
                      FruitAndNuts = P50grCom + P52grCom,
                      Vegetables = P41grCom + P42grCom,
                      Legumes = P46grCom,
                      Cereals = P55grCom + P56grCom + P57grCom + P59grCom +
                                P60grCom + P61grCom + P62grCom,
                      Fish = P35grCom + P36grCom + P37grCom + P38grCom,
                      OliveOil = Aceitegr,
                      Meat = P29grCom + P30grCom + P31grCom + P32grCom,
                      Dairy= P19grCom + P20grCom + P20grCom + P22grCom + P23grCom +
                             P24grCom + P25grCom + P26grCom + P27grCom,
                      Alcohol = 10 * (P94rac + P96rac + P97rac + P98rac + P99rac),
                      Sex = SEXO, men = "Hombre", women = "Mujer",
                      frequency = "daily", output = "percent", rm.na = FALSE)
summary(MedDiet)
}
