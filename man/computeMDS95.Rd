\name{computeMDS95}
\encoding{UTF-8}
\alias{computeMDS95}

\title{
computeMDS95
}
\description{
Fist Mediterranean Adherence score index, developed by Trichopoulou et al. which has been extensively used and modified.
}

\usage{
computeMDS95(data, Vegetables, Legumes, FruitAndNuts,
             Cereals, Potatoes = NULL, Meat, Dairy, Alcohol,
             Fats = NULL, MUFA = NULL, SFA = NULL,
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
  \item{Meat}{Numeric variable with Meat consumption as grams}
  \item{Dairy}{Numeric variable with Dairy consumption as grams}
  \item{Alcohol}{Numeric variable with Alcohol consumption as etanol grams from any beberage origin}
  \item{Fats}{Optional. Numeric variable with a ratio of consumption of Mono Unsaturated Fatty Acids (MUFA) over Saturated Fatty Acids (SFA).
 If it is not provided, then individual MUFA and SFA should be provided}
  \item{MUFA}{Optional if Fats is provided. Numeric variable with consumption of Mono Unsaturated Fatty Acids, units should be the same as used with PUFA and SFA}
  \item{SFA}{Optional if Fats is provided. Numeric variable with consumption of Saturated Fatty Acids}
  \item{Sex}{Vector with gender, it can be numeric, factor or character, as long as its values are provided by 'men' and 'women' arguments. If 'Sex' argument is character or factor, and values for male are either 'man', 'male', 'MAN' or 'MALE', and for females are 'woman', 'female', 'WOMAN' or 'FEMALE', then, the arguments 'men' and 'women' can be missing}
  \item{men}{A character string with the value of male gender, default is "male"}
  \item{women}{A character string with the value of female gender, default is "female"}
  \item{frequency}{A character string. Allowed values are 'daily', 'weekly' and 'monthly'. It informs about the frequency which food or nutrient consumption refers to (i.e. wether the rest of arguments are 'grams per day' or 'grams per week' or 'grams per month')}
  \item{output}{A character string to set which output should the formula give, allowed values are 'data.frame', 'score' and 'percent' (default).}
  \item{rm.na}{Logical. If set to FALSE (default), a diet score will be computed only if a person has all score components informed. If set to TRUE, NA values in score components will be drop off and a value of available components will be returned,
 but percent of score adherence will be computed with basis of the whole score range (see Details)}
}

\details{
 This is the fist Mediterranean Diet Score, which was developed by Antonia Trichopoulou and coleagues. At present, this score is not widely used, since it was later updated by its authors. Nevertheless, as it is the first Mediterranean Diet Score developed, and is the basis of most of them, we think it deserves a places here.

 Original 1995 paper of Trichopoulou et al. included potatoes with cereals, but later research has challenged this view. If you want to compute the score as originally developed, provide potato consumption as 'Potatoes' argument, and you will get a warning informing you that both have been used together in the score.
 If you don't want to compute potatoes consumption, don't provide 'Potatoes' argument, and you will receive a warning informing you that you are diverting from the very original score.
}

\value{
 Computed MDS95 score. Depending on 'output' argument, value can be a data.frame, or a vector:
  \item{if output = 'data.frame'}{A data frame with a row corresponding to each person in data. Columns are the score of each component, as well as the global score as natural sum ('absolute' column) and as percentage ('percent' column)}
  \item{if output = 'score'}{Instead of the full data.frame, just the integer vector corresponding to the absolute points of adherence to Mediterranean Diet for each person, from 0 (min.) to 9 (max.)}
  \item{if output = 'percent'}{Instead of the full data.frame, just the numeric vector corresponding to the percent of adherence to Mediterranean Diet for each person, from 0 (min. adherence) to 100 percent (max. adhrence)}
}

\references{
Trichopoulou, A., A. Kouris-Blazos, M. L. Wahlqvist, C. Gnardellis, P. Lagiou, E. Polychronopoulos, T. Vassilakou, L. Lipworth, and D. Trichopoulos. 1995. “Diet and Overall Survival in Elderly People.” BMJ (Clinical Research Ed.) 311 (7018): 1457–60.

}
\author{
 Miguel Menendez
}


\examples{
data(nutriSample)
MedDiet <- computeMDS95(data = nutriSample,
        Vegetables = P41grCom + P42grCom,
        Legumes =  P46grCom,
        FruitAndNuts =  P50grCom + P52grCom + P53grCom,
        Cereals = P55grCom + P56grCom + P57grCom + P59grCom +
                  P60grCom + P61grCom + P62grCom,
        Meat = P29grCom + P30grCom + P31grCom + P32grCom,
        Dairy = P19grCom + P20grCom + P20grCom + P22grCom + P23grCom +
                P24grCom + P25grCom + P26grCom + P27grCom,
        Alcohol =  12 * (P94rac + P96rac + P97rac + P98rac + P99rac),
        Potatoes =  P43grCom +  P44grCom +  P45grCom,
        MUFA =  totalGrasaMonoins,
        SFA =  totalGrasaSat,
        Sex =  SEXO, men = "Hombre", women = "Mujer", frequency = "daily",
        output = "percent", rm.na = FALSE)
hist(MedDiet)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
