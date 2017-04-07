\name{computeMDS05}
\encoding{UTF-8}
\alias{computeMDS05}
\title{computeMDS05}

\description{Computes the Mediterranean Diet adherence score developed by Trichopoulou et al. in 2005 (MDS05), which is an update of their previously developed version.
}

\usage{computeMDS05(data, Vegetables, Legumes, FruitAndNuts,
             Cereals, Potatoes = NULL, Fish, Meat, Dairy, Alcohol,
             Fats = NULL, MUFA = NULL, PUFA = NULL, SFA = NULL,
             Sex, men = "male", women= "female",
             frequency = NULL, output = "percent", rm.na = FALSE)
}

\arguments{
  \item{data}{Your data set with nutritional information about food or nutrient consumption. Each row is expected to be a person, and food or nutrient intake are in columns}
  \item{Vegetables}{Numeric variable with Vegetables consumption as grams}
  \item{Legumes}{Numeric variable with Legumes consumption as grams}
  \item{FruitAndNuts}{Numeric variable with consumption of Fruits and Nuts as grams}
  \item{Cereals}{Numeric variable with Cereals consumption as grams}
  \item{Potatoes}{Numeric variable with Potatoes consumption as grams}
  \item{Fish}{Numeric variable with Fish consumption as grams}
  \item{Meat}{Numeric variable with Meat consumption as grams}
  \item{Dairy}{Numeric variable with Dairy consumption as grams}
  \item{Alcohol}{Numeric variable with Alcohol consumption as etanol grams from any beberage origin}
  \item{Fats}{Optional. Numeric variable with a ratio of consumption of Mono and Poli Unsaturated Fatty Acids (MUFA + PUFA) over Saturated Fatty Acids (SFA).
 If it is not provided, then individual MUFA, PUFA and SFA should be provided}
  \item{MUFA}{Optional if Fats is provided. Numeric variable with consumption of Mono Unsaturated Fatty Acids, units should be the same as used with PUFA and SFA}
  \item{PUFA}{Optional if Fats is provided. Numeric variable with consumption of Poli Unsaturated Fatty Acids}
  \item{SFA}{Optional if Fats is provided. Numeric variable with consumption of Saturated Fatty Acids}
  \item{Sex}{Vector with gender, it can be numeric, factor or character, as long as its values are provided by 'men' and 'women' arguments. If 'Sex' argument is character or factor, and values for male are either 'man', 'male', 'MAN' or 'MALE', and for females are 'woman', 'female', 'WOMAN' or 'FEMALE', then, the arguments 'men' and 'women' can be missing}
  \item{men}{A character string with the value of male gender, default is "male"}
  \item{women}{A character string with the value of female gender, default is "female"}
  \item{frequency}{A character string. Allowed values are 'daily', 'weekly' and 'monthly'. It informs about the frequency which food or nutrient consumption refers to (i.e. wether the rest of arguments are 'grams per day' or 'grams per week' or 'grams per month')}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame', 'score' and 'percent' (default).}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned, but percent of score adherence will be computed with basis of the whole score range (see Details)}
}

\details{
 This score is an update of the landmark first Mediterranean Diet Score (MDS), published in 1995, which includes fish consumption (previously introduced) and which sums MUFA + PUFA.

 Original 1995 paper of Trichopoulou et al. included potatoes with cereals, but later research has challenged this view. If you want to compute the score as originally developed, provide potato consumption as 'Potatoes' argument, and you will get a warning informing you that both have been used together in the score.
 If you don't want to compute potatoes consumption, don't provide 'Potatoes' argument, and you will receive a warning informing you that you are diverting from the very original score.
 Some score components are a combination of foods you may have as separated variables, if so, you can just add them toghether (v.gr. miFruitVariable + miNutsVariable).

 Score values (MUFA + PUFA) / SFA. Depending in how your data has been developed, you can provide the ratio as 'Fats' argument or the triada 'MUFA', 'PUFA' and 'SFA', but if you provide this information by both of the ways, just 'Fats' argument will be computed, and you will receive a warning asking you to check the arguments.
}

\value{Computed MDS05 score. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'score'}{Instead of the fullchecking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped data.frame, just the integer vector corresponding to the absolute points of adherence to Mediterranean Diet for each person, from 0 (min.) to 9 (max.)}
  \item{if output = 'percent'}{Instead of the full data.frame, just the numeric vector corresponding to the percent of adherence to Mediterranean Diet for each person, from 0 (min. adherence) to 100 percent (max. adhrence)}
}

\references{
Trichopoulou, A., A. Kouris-Blazos, M. L. Wahlqvist, C. Gnardellis, P. Lagiou, E. Polychronopoulos, T. Vassilakou, L. Lipworth, and D. Trichopoulos. 1995. "Diet and Overall Survival in Elderly People." BMJ (Clinical Research Ed.) 311 (7018): 1457–60.

Trichopoulou, Antonia, Tina Costacou, Christina Bamia, and Dimitrios Trichopoulos. 2003. "Adherence to a Mediterranean Diet and Survival in a Greek Population." New England Journal of Medicine 348 (26): 2599–2608. doi:10.1056/NEJMoa025039.

Trichopoulou, Antonia, Philippos Orfanos, Teresa Norat, Bas Bueno-de-Mesquita, Marga C. Ocke, Petra HM Peeters, Yvonne T. van der Schouw, et al. 2005. "Modified Mediterranean Diet and Survival: EPIC-Elderly Prospective Cohort Study." BMJ 330 (7498): 991. doi:10.1136/bmj.38415.644155.8F.
}

\examples{
data(nutriSample)
MedDiet <- computeMDS05(data = nutriSample,
        Vegetables = P41grCom + P42grCom,
        Legumes =  P46grCom,
        FruitAndNuts =  P50grCom + P52grCom + P53grCom,
        Cereals = P55grCom + P56grCom + P57grCom + P59grCom +
                  P60grCom + P61grCom + P62grCom,
        Fish = P35grCom + P36grCom + P37grCom + P38grCom,
        Meat = P29grCom + P30grCom + P31grCom + P32grCom,
        Dairy = P19grCom + P20grCom + P20grCom + P22grCom + P23grCom +
                P24grCom + P25grCom + P26grCom + P27grCom,
        Alcohol =  12 * (P94rac + P96rac + P97rac + P98rac + P99rac),
        Potatoes =  P43grCom +  P44grCom +  P45grCom,
        MUFA =  totalGrasaMonoins,
        PUFA =  totalGrasaPoliins,
        SFA =  totalGrasaSat,
        Sex =  SEXO, men = "Hombre", women = "Mujer", frequency = "daily",
        output = "percent", rm.na = FALSE)
hist(MedDiet)
}
