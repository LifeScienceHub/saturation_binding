# Saturation Binding Assay

## Assay principle
With the saturation binding assay the binding affinity can be determined. The binding affinity of a compound is expressed as a konstand Kd in mol/L. In principal increasing amount of a compound (here a radiotracer) are added in ascending concentrations, while the target (here receptor) concentration remains constant throughout the experiment. Saturation occurs as soon as all receptors are occupied by the tested concentration. The Kd is defined as the half-max concentration of the saturation. The saturation is based on a non-linear regression and the Bmax is the maximal calculated value of saturation. 

The non-linear regression is based on the Lanmuir isotherm, which is used for example to describe enzyme kinetics (Michaelis-Menten) as well as saturation binding.
 
Formula of the saturation binding assay:

Specific binding = (Bmax x [L]) / (Kd - [L])

## Packages
- in R you will need the drc package:  install.packages("drc")
- modificaiton: nonlinear regression with Bmax / Kd determination


#Saturation binding - plot and Kd determination
## loading library including drc for nonlinear regression

´library("drc")
library("dplyr")
library("readxl")
library("ggplot2")´


## Loading data:

´df <- read_excel("Saturation-binding_example_data.xlsx", sheet=1, col_names=TRUE, col_types = NULL, na = "", skip = 0)
print(df)´

## Linear regression model and fit

- Loading Variables, Specific binding proportional to Cell concentration according to equation,
- fct=MM.2() function based on of Michealis-Menten model going through 0
- formula from MM.2: $$f(x, (c, d, e)) = c + \frac{d-c}{1+(e/x)}$$  where c=0

´MM.model <- drm(CPM~Concentration, data=df, fct=MM.2())
print(MM.model)´


# creating table for data prediction

- take values of concentration and create a sequence 0 -> max concentration
- length.out=100  -> iteratively 100 steps 

´mmdf <- data.frame(Concentration=seq(0,max(df$Concentration),length.out=100))
print(mmdf)´


## predict variabels using the MM.model for each of the 100 data points

´mmdf$CPM <- predict(MM.model, newdata=mmdf)
print(mmdf)´



## plotting the fitted courve from data frame "mmdr"

´p<-ggplot(df, aes(x = Concentration, y = CPM)) +
  theme_bw() +
  xlab("[64Cu]Cu-NOTA-anti-mCXCR3 [nM]") +
  ylab("Specific Binding (CPM)") +
  ggtitle("") +
  geom_point(alpha = 1, color="black", size=2) +
  geom_line(data = mmdf, 
            aes(x = Concentration, y = CPM), 
            colour = "grey", size = 1)
print(p)
p<- p + theme(axis.text = element_text(size=12))

print(p)
# summary of all calculation of the MM.model:
summary(MM.model)´

## applying formular with nls - function,
## Error message: !Calculation already all done, helps for the elucidation of the variables
 !!!!! not working with counts as Y instead of %bidning

´mm.model.nls <- nls(CPM~r*Concentration/(Kd+Concentration), data=df, 
                    start = list(Kd=max(df$CPM)/2, 
                                r=max(df$CPM)))´

´summary(mm.model.nls)´

#Error in nls(CPM ~ r * Concentration/(Kd + Concentration), data = df,  : 
 #              step factor 0.000488281 reduced below 'minFactor' of 0.000976562
