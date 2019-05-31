\name{nutriSample}
\encoding{UTF-8}
\alias{nutriSample}
\docType{data}
\title{
Sample from a nutriepidemiological study
}
\description{
A sample of 192 Spanish people from a nutriepidemiological study, from DRECE group (Dieta y Riesgo de Enfermedad Cardiovascular en España [Diet and Cardiovascular Risk in Spain]).
Food consumption was assessed by a Frequency Food Questionnaire. For all foods or nutrients, food consumption is stored as daily consumption.}
\usage{data("nutriSample")}
\format{
  A data frame with 192 observations on the following variables.
  \describe{
    \item{\code{IDE}}{integer, identification number}
    \item{\code{PROVINCIA}}{geographic region of procedence of the person}
    \item{\code{SEXO}}{gender information}
    \item{\code{EDAD}}{age as years}
    \item{\code{FUMADOR}}{smoking habit of the person 0 = never smoker, 1 = current smoker, 2 = former smoker}
    \item{\code{HIPERTENSO}}{if the person has prior diagnose of High Blood Pressure}
    \item{\code{MEDIC_TENS}}{if the person is under blood lowering pressure treatment}
    \item{\code{HIPER_COLES}}{if the person has prior diagnose of Dyslipidemia}
    \item{\code{MEDIC_COLES}}{if the person is under lipid lowering treatment}
    \item{\code{ANT_CARDIO}}{if the person has a history of coronary events}
    \item{\code{DIABETES}}{if the person has prior diagnose of Diabetes Mellitus}
    \item{\code{peso}}{weight in kg}
    \item{\code{altura}}{heigth in cm}
    \item{\code{TAS1}}{first systolic blood pressure measurement in mmHg}
    \item{\code{TAD1}}{first diastolic blood pressure measurement in mmHg}
    \item{\code{TAS2}}{second systolic blood pressure measurement in mmHg}
    \item{\code{TAD2}}{second diastolic blood pressure measurement in mmHg}
    \item{\code{Colesterol}}{plasmatic total Cholesterol}
    \item{\code{LDL}}{plasmatic Low Density Lipoprotein}
    \item{\code{HDL}}{plasmatic High Density Lipoprotein}
    \item{\code{TG}}{plasmatic triglycerides}
    \item{\code{APO.B}}{plasmatic Apolipoprotein B}
    \item{\code{APO.A}}{plasmatic Apolipoprotein A}
    \item{\code{P19grCom}}{Edible portion (in grams) of whole milk}
    \item{\code{P19Kcal}}{Kcal attributable to consumption of whole milk}
    \item{\code{P19rac}}{Servings of whole milk}
    \item{\code{P20grCom}}{Edible portion (in grams) of skimmed or semi-skimmed mil}
    \item{\code{P20Kcal}}{Kcal attributable to consumption of skimmed or semi-skimmed mil}
    \item{\code{P20rac}}{Servings of skimmed or semi-skimmed mil}
    \item{\code{P21Kcal}}{Kcal attributable to consumption of milk enriched with omega-3 acid}
    \item{\code{P22grCom}}{Edible portion (in grams) of whole yogurt}
    \item{\code{P22rac}}{Servings of whole yogurt}
    \item{\code{P22Kcal}}{Kcal attributable to consumption of whole yogurt}
    \item{\code{P23grCom}}{Edible portion (in grams) of skimmed or semi-skimmed yogurt}
    \item{\code{P23rac}}{Servings of skimmed or semi-skimmed yogurt}
    \item{\code{P23Kcal}}{Kcal attributable to consumption of skimmed or semi-skimmed yogurt}
    \item{\code{P24grCom}}{Edible portion (in grams) of enriched with probiotics yogurt}
    \item{\code{P24rac}}{Servings of enriched with probiotics yogurt}
    \item{\code{P24Kcal}}{Kcal attributable to consumption of enriched with probiotics yogurt}
    \item{\code{P25grCom}}{Edible portion (in grams) of dairy products, usually desserts, like custard, junket, flan or requeson}
    \item{\code{P25rac}}{Servings of dairy products, usually desserts, like custard, junket, flan or requeson}
    \item{\code{P25Kcal}}{Kcal attributable to consumption of dairy products, usually desserts, like custard, junket, flan or requeson}
    \item{\code{P26grCom}}{Edible portion (in grams) of unripened cheese}
    \item{\code{P26Kcal}}{Kcal attributable to consumption of unripened cheese}
    \item{\code{P26rac}}{Servings of unripened cheese}
    \item{\code{P27grCom}}{Edible portion (in grams) of cheese (hard, semi-hard, ball, blue...)}
    \item{\code{P27Kcal}}{Kcal attributable to consumption of cheese (hard, semi-hard, ball, blue...)}
    \item{\code{P27rac}}{Servings of cheese (hard, semi-hard, ball, blue...)}
    \item{\code{P28Kcal}}{Kcal attributable to consumption of eggs}
    \item{\code{P28rac}}{Servings of eggs}
    \item{\code{P29grCom}}{Edible portion (in grams) of red meat (cattle, lamb, pork)}
    \item{\code{P29grGrasa}}{Fat intake attributable to consumption of red meat (cattle, lamb, pork)}
    \item{\code{P29Kcal}}{Kcal attributable to consumption of red meat (cattle, lamb, pork)}
    \item{\code{P29rac}}{Servings of red meat (cattle, lamb, pork)}
    \item{\code{P30grCom}}{Edible portion (in grams) of white meat (poultry, rabbit)}
    \item{\code{P30grGrasa}}{Fat intake attributable to consumption of white meat (poultry, rabbit)}
    \item{\code{P30Kcal}}{Kcal attributable to consumption of white meat (poultry, rabbit)}
    \item{\code{P30rac}}{Servings of white meat (poultry, rabbit)}
    \item{\code{P31grCom}}{Edible portion (in grams) of cold cuts ("embutido")}
    \item{\code{P31grGrasa}}{Fat intake attributable to consumption of cold cuts ("embutido")}
    \item{\code{P31Kcal}}{Kcal attributable to consumption of cold cuts ("embutido")}
    \item{\code{P31rac}}{Servings of cold cuts ("embutido")}
    \item{\code{P32grCom}}{Edible portion (in grams) of serrano ham}
    \item{\code{P32grGrasa}}{Fat intake attributable to consumption of serrano ham}
    \item{\code{P32Kcal}}{Kcal attributable to consumption of serrano ham}
    \item{\code{P32rac}}{Servings of serrano ham}
    \item{\code{P33grGrasa}}{Fat intake attributable to consumption of York ham}
    \item{\code{P33rac}}{Servings of York ham}
    \item{\code{P33Kcal}}{Kcal attributable to consumption of York ham}
    \item{\code{P34grGrasa}}{Fat intake attributable to consumption of offal (guts, pluck or organ meats)}
    \item{\code{P35grCom}}{Edible portion (in grams) of white fish}
    \item{\code{P35Kcal}}{Kcal attributable to consumption of white fish}
    \item{\code{P35rac}}{Servings of white fish}
    \item{\code{P36grCom}}{Edible portion (in grams) of blue fish}
    \item{\code{P36Kcal}}{Kcal attributable to consumption of blue fish}
    \item{\code{P36rac}}{Servings of blue fish}
    \item{\code{P37grCom}}{Edible portion (in grams) of shellfish}
    \item{\code{P37Kcal}}{Kcal attributable to consumption of shellfish}
    \item{\code{P37rac}}{Servings of shellfish}
    \item{\code{P38grCom}}{Edible portion (in grams) of tinned fish}
    \item{\code{P38Kcal}}{Kcal attributable to consumption of tinned fish}
    \item{\code{P38rac}}{Servings of tinned fish}
    \item{\code{P41grCom}}{Edible portion (in grams) of salads}
    \item{\code{P41Kcal}}{Kcal attributable to consumption of salads}
    \item{\code{P41rac}}{Servings of salads}
    \item{\code{P42grCom}}{Edible portion (in grams) of boiled or grilled vegetables}
    \item{\code{P42Kcal}}{Kcal attributable to consumption of boiled or grilled vegetables}
    \item{\code{P42rac}}{Servings of boiled or grilled vegetables}
    \item{\code{P43grCom}}{Edible portion (in grams) of boiled or roasted potatoes}
    \item{\code{P43Kcal}}{Kcal attributable to consumption of boiled or roasted potatoes}
    \item{\code{P43rac}}{Servings of boiled or roasted potatoes}
    \item{\code{P44grCom}}{Edible portion (in grams) of fried home cooked potatoes (not frozen)}
    \item{\code{P44Kcal}}{Kcal attributable to consumption of fried home cooked potatoes (not frozen)}
    \item{\code{P44rac}}{Servings of fried home cooked potatoes (not frozen)}
    \item{\code{P45grCom}}{Edible portion (in grams) of fried frozen potatoes or eaten in restaurants or fast food}
    \item{\code{P45rac}}{Servings of fried frozen potatoes or eaten in restaurants or fast food}
    \item{\code{P45Kcal}}{Kcal attributable to consumption of fried frozen potatoes or eaten in restaurants or fast food}
    \item{\code{P46grCom}}{Edible portion (in grams) of legumes}
    \item{\code{P46Kcal}}{Kcal attributable to consumption of legumes}
    \item{\code{P46rac}}{Servings of legumes}
    \item{\code{P50grCom}}{Edible portion (in grams) of fresh fruit}
    \item{\code{P50Kcal}}{Kcal attributable to consumption of fresh fruit}
    \item{\code{P50rac}}{Servings of fresh fruit}
    \item{\code{P52grCom}}{Edible portion (in grams) of dried figs, dried grapes, dried plums or dates}
    \item{\code{P52rac}}{Servings of dried figs, dried grapes, dried plums or dates}
    \item{\code{P52Kcal}}{Kcal attributable to consumption of dried figs, dried grapes, dried plums or dates}
    \item{\code{P53grCom}}{Edible portion (in grams) of nuts (almonds, pistachios, walnuts, hazelnuts or peanuts)}
    \item{\code{P53Kcal}}{Kcal attributable to consumption of nuts (almonds, pistachios, walnuts, hazelnuts or peanuts)}
    \item{\code{P53rac}}{Servings of nuts (almonds, pistachios, walnuts, hazelnuts or peanuts)}
    \item{\code{P54Kcal}}{Kcal attributable to consumption of olives}
    \item{\code{P54rac}}{Servings of olives}
    \item{\code{P55grCom}}{Edible portion (in grams) of white bread}
    \item{\code{P55Kcal}}{Kcal attributable to consumption of white bread}
    \item{\code{P55rac}}{Servings of white bread}
    \item{\code{P56grCom}}{Edible portion (in grams) of whole grain bread}
    \item{\code{P56Kcal}}{Kcal attributable to consumption of whole grain bread}
    \item{\code{P56rac}}{Servings of whole grain bread}
    \item{\code{P57grCom}}{Edible portion (in grams) of toast bread}
    \item{\code{P57Kcal}}{Kcal attributable to consumption of toast bread}
    \item{\code{P57rac}}{Servings of toast bread}
    \item{\code{P59grCom}}{Edible portion (in grams) of breakfast cereals}
    \item{\code{P59Kcal}}{Kcal attributable to consumption of breakfast cereals}
    \item{\code{P59rac}}{Servings of breakfast cereals}
    \item{\code{P60grCom}}{Edible portion (in grams) of fiber enriched breakfast cereals}
    \item{\code{P60Kcal}}{Kcal attributable to consumption of fiber enriched breakfast cereals}
    \item{\code{P60rac}}{Servings of fiber enriched breakfast cereals}
    \item{\code{P61grCom}}{Edible portion (in grams) of white rice}
    \item{\code{P61Kcal}}{Kcal attributable to consumption of white rice}
    \item{\code{P61rac}}{Servings of white rice}
    \item{\code{P62grCom}}{Edible portion (in grams) of paella (a traditionl Spanish dish based on rice with yellow colorant)}
    \item{\code{P62Kcal}}{Kcal attributable to consumption of paella (a traditionl Spanish dish based on rice with yellow colorant)}
    \item{\code{P62rac}}{Servings of paella (a traditionl Spanish dish based on rice with yellow colorant)}
    \item{\code{P63_2}}{A question about consumption of whole bread (1) or white bread (0)}
    \item{\code{P69Kcal}}{Kcal attributable to consumption of pastries}
    \item{\code{P69rac}}{Servings of pastries}
    \item{\code{P70Kcal}}{Kcal attributable to consumption of churros and fritters}
    \item{\code{P70rac}}{Servings of churros and fritters}
    \item{\code{P71Kcal}}{Kcal attributable to consumption of cakes}
    \item{\code{P71rac}}{Servings of cakes}
    \item{\code{P72Kcal}}{Kcal attributable to consumption of chocolate or bonbons}
    \item{\code{P72rac}}{Servings of chocolate or bonbons}
    \item{\code{P73Kcal}}{Kcal attributable to consumption of ice cream}
    \item{\code{P73rac}}{Servings of ice cream}
    \item{\code{P75rac}}{Servings of sunflower seeds}
    \item{\code{P79rac}}{Servings of butter}
    \item{\code{P84Kcal}}{Kcal attributable to consumption of sugar}
    \item{\code{P89Kcal}}{Kcal attributable to consumption of soft drinks}
    \item{\code{P89rac}}{Servings of soft drinks}
    \item{\code{P90Kcal}}{Kcal attributable to consumption of diet soft drinks}
    \item{\code{P90rac}}{Servings of diet soft drinks}
    \item{\code{P94rac}}{Servings of beer}
    \item{\code{P96Kcal}}{Kcal attributable to consumption of wine}
    \item{\code{P96rac}}{Servings of wine}
    \item{\code{P97rac}}{Servings of vermouth, fine wine or sweet wine}
    \item{\code{P98rac}}{Servings of liquor or anisette}
    \item{\code{P99rac}}{Servings of spirits (whiskey, cognac, gin)}
    \item{\code{Aceitegr}}{olive oil consumption in grams}
    \item{\code{AceiteKcal}}{Kcal attributable to olive oil consumption}
    \item{\code{AceiteTipo}}{kind of oil preferred by the surveyed person (1 = olive oil, 2 = seeds oil, 3 = both)}
    \item{\code{totalgr}}{Total Food consumption, included edible and not edible, in grams}
    \item{\code{totalgrCom}}{Total Edible food consumption, in grams, including liquid foods like milk}
    \item{\code{grBebidas}}{total beberage intake in ml not coming directly from drinken water}
    \item{\code{grSinBebidas}}{total food consumption in grams, without liquid components}
    \item{\code{totalCH}}{total CarboHydrates consumption (grams per day)}
    \item{\code{totalProt}}{total Protein consumption (grams per day)}
    \item{\code{totalGrasa}}{total Fat consumption (grams per day)}
    \item{\code{totalGrasaSat}}{total Saturated Fat consumption (grams per day)}
    \item{\code{totalGrasaMonoins}}{total Monounsaturated Fat consumption (grams per day)}
    \item{\code{totalGrasaPoliins}}{total Polyunsaturated Fat consumption (grams per day)}
    \item{\code{totalCol}}{total Cholesterol consumption (in mg per day)}
    \item{\code{totalFibra}}{total Fiber consumption (grams per day)}
    \item{\code{totalKcal}}{total kcal eaten per day}
    }
}


\references{
Gutiérrez Fuentes JA, Gómez Gerique JA, Rubio Herrera MA, Gómez de la Cámara A, Grupo DRECE. Capítulo 1. DRECE: introducción. Med Clin (Barc). 2011;12(4):1–2.

Gómez Gerique JA, Herrera R, Gómez de la Cámara A, Grupo DRECE. Capítulo 2. El proyecto DRECE. Med Clin (Barc). 2011;12(4):3–5.

Gómez Gerique JA, Herrera R, Gómez de la Cámara A, Gutiérrez Fuentes JA, Grupo DRECE. Capítulo 3. DRECE I (1991). Med Clin (Barc). 2011;12(4):6–15.

Gómez de la Cámara A, Gutiérrez Fuentes JA, Gómez Gerique JA, Herrera R, Grupo DRECE. Capítulo 4. DRECE II (1996). Evolución del perfil cardiovascular y morbilidad en poblaciones de riesgo. Med Clin (Barc). 2011;12(4):16–21.

Gómez de la Cámara A, Herrera R, Gutiérrez Fuentes JA, Jurado Valenzuela C, Cancelas Navia P, Gómez Gerique JA, et al. Capítulo 5. DRECE III (2004). Mortalidad y factores de riesgo cardiovascular. Med Clin (Barc). 2011;12(4):22–30.

Gutiérrez Fuentes JA, Gómez Gerique JA, Gómez de la Cámara A, Cancelas Navia P, Jurado Valenzuela C, Herrera R, et al. Capítulo 6. DRECE IV (2008). Hábitos alimentarios actuales y evolución de la dieta en la población española. Med Clin (Barc). 2011;12(4):31–34.

Gómez-de la Cámara A, Pinilla-Domínguez P, Vázquez-Fernández Del Pozo S, García-Pérez L, Rubio-Herrera MA, Gómez-Gerique JA, et al. Costs resulting from premature mortality due to cardiovascular causes: A 20-year follow-up of the DRECE study. Rev Clin Esp. 2014 Oct;214(7):365–70.
}

\examples{
data(nutriSample)
summary(nutriSample$totalKcal)
}
\keyword{datasets}
