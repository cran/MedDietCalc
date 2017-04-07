\name{computePredimed}
\encoding{UTF-8}
\alias{computePredimed}

\title{
computePredimed
}

\description{
Computes the Mediterranean Diet adherence score used in PreDiMed trial (Prevencion con Dieta Mediterranea, Spanish which means Prevention with Mediterranean Diet)
}

\usage{
computePredimed(data, OliveOil, OOmeasure = "gr", OOprincipal,
                Vegetables, Fruit, RedMeat, Butter, SoftDrinks,
                Wine, Legumes, Fish, Pastries, Nuts, WhiteMeat,
                Sofritos, output = "percent", rm.na = FALSE, frequency = NULL)
}

\arguments{
  \item{data}{Your data set with nutritional information about food or nutrient consumption. Each row is expected to be a person, and food or nutrient intake are in columns.}
  \item{OliveOil}{Numeric with olive oil consumption. Units are set with the argument 'OOmeasure'.}
  \item{OOmeasure}{Character string which informs about the unit of the argument 'OliveOil'. Allowed values are 'gr', 'ml' and 'serving', which means respectively grams, mililiters and servings of 1 table spoon (10 ml).}
  \item{OOprincipal}{Integer. This item scores wether olive oil is the main dietary fat as a dichotomous variable (1-yes, 0-no).}
  \item{Vegetables}{Numeric. Vegetables consumption measured as servings.}
  \item{Fruit}{Numeric. Fruit consumption measured as servings.}
  \item{RedMeat}{Numeric. RedMeat consumption measured as servings.}
  \item{Butter}{Numeric. Butter consumption measured as servings.}
  \item{SoftDrinks}{Numeric. SoftDrinks consumption measured as servings.}
  \item{Wine}{Numeric. Wine consumption measured as servings (glasses).}
  \item{Legumes}{Numeric. Legumes consumption measured as servings.}
  \item{Fish}{Numeric. Fish consumption measured as servings.}
  \item{Pastries}{Numeric. Pastries consumption measured as servings.}
  \item{Nuts}{Numeric. Nuts consumption measured as servings.}
  \item{WhiteMeat}{Integer. This item scores wether wite meats are preferred over red meats. So it is a dichotomous variable (1-yes, 0-no).}
  \item{Sofritos}{Numeric. Number of times 'sofrito' is consumed (see Details).}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame', 'score' and 'percent' (default).}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned, but percent of score adherence will be computed with basis of the whole score range (see Details)}
  \item{frequency}{A character string. Allowed values are 'daily', 'weekly' and 'monthly'. It informs about the frequency which food or nutrient consumption refers to (i.e. wether the rest of arguments are 'grams per day' or 'grams per week' or 'grams per month')}

}

\details{
This score was used in the landmark PreDiMed trial (Prevencion con Dieta Mediterranea, Spanish which means Prevention with Mediterranean Diet) (Estruch et al. 2013). It can also be found under the name MEDAS (MEditerranean Diet Adherence Screener) (Schroder et al. 2011)


Please note that olive oil is in thtree items: one measuring the amount of servings, other measuring if it is the main dietary fat, and another asking about 'sofrito' consumption.
Supplementary material of Estruch et al. 2013 informs that one tablespoon is 10ml.


'Sofrito' is a special way to cook, a sauce made with tomato and onion, leek, or garlic, simmered with olive oil.
}

\value{
 Computed Predimed score. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'score'}{Instead of the full data.frame, just the integer vector corresponding to the absolute points of adherence to Mediterranean Diet for each person, from 0 (min.) to 9 (max.)}
  \item{if output = 'percent'}{Instead of the full data.frame, just the numeric vector corresponding to the percent of adherence to Mediterranean Diet for each person, from 0 (min. adherence) to 100 percent (max. adhrence)}
}

\references{
Estruch, Ramon, Emilio Ros, Jordi Salas-Salvado, Maria-Isabel Covas, Dolores Corella, Fernando Aros, Enrique Gomez-Gracia, et al. 2013. "Primary Prevention of Cardiovascular Disease with a Mediterranean Diet." New England Journal of Medicine 368 (14): 1279-90. doi:10.1056/NEJMoa1200303.
(Supplementary material available at http://www.nejm.org/action/showSupplements?doi=10.1056%2FNEJMoa1200303&viewType=Popup&viewClass=Suppl)

Martinez-Gonzalez, Miguel Angel, Dolores Corella, Jordi Salas-Salvado, Emilio Ros, Maria Isabel Covas, Miquel Fiol, Julia Warnberg, et al. 2012. "Cohort Profile: Design and Methods of the PREDIMED Study." International Journal of Epidemiology 41 (2): 377-385. http://ije.oxfordjournals.org/content/41/2/377.short.

Schroder, Helmut, Montserrat Fito, Ramon Estruch, Miguel A. Martinez-Gonzalez, Dolores Corella, Jordi Salas-Salvado, Rosa Lamuela-Raventos, et al. 2011. 'A Short Screener Is Valid for Assessing Mediterranean Diet Adherence among Older Spanish Men and Women'. The Journal of Nutrition 141 (6): 1140-45. doi:10.3945/jn.110.135566.

}
\author{
Miguel Menendez
}

\examples{
data(nutriSample)
MedDiet <- computePredimed(data = nutriSample, OliveOil = Aceitegr, OOmeasure = "gr",
                  OOprincipal = ifelse(nutriSample$AceiteTipo == 1, 1, 0),
                  Vegetables = P41rac + P42rac,
                  Fruit = P50rac + P52rac,
                  RedMeat = P29rac + P31rac,
                  Butter = P79rac,
                  SoftDrinks = P89rac + P90rac,
                  Wine = P96rac,
                  Legumes = P46rac,
                  Fish = P35rac + P36rac + P37rac + P38rac,
                  Pastries = P69rac + P70rac + P71rac + P72rac + P73rac,
                  Nuts = P53rac,
                  WhiteMeat = ifelse(nutriSample$P30rac > nutriSample$P29rac, 1, 0),
                  Sofritos = rep(0, nrow(data)), # data lacks this variable, so we go on without it
                  output = "percent", rm.na = FALSE, frequency = "daily")
hist(MedDiet)
}

