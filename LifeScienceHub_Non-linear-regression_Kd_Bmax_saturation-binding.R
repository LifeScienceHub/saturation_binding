library("drc")
library("dplyr")
library("readxl")
library("ggplot2")

# Skript taken from: https://techvidvan.com/tutorials/nonlinear-regression-in-r/
# skript EC50s: https://rpubs.com/Kat_T/788707
# a) install.packages("drc")
# b) modificaiton: nonlinear regression with Bmax / Kd determination

df <- read_excel("Saturation-binding_example_data.xlsx", sheet=1, col_names=TRUE, col_types = NULL, na = "", skip = 0)

# LINEAR FIT:
print(df)

# ----------- 1
### Loading Variables, Specific binding proportional to Cell concentration according to equation,
#### fct=MM.2() function based on of Michealis Menten Model going through 0 
#####  formula: $$f(x, (c, d, e)) = c + \frac{d-c}{1+(e/x)}$$  where c=0

MM.model <- drm(CPM~Concentration, data=df, fct=MM.2())

print(MM.model)

# ---------- 2
### creating table (data.frame)
### take values of cell_conc and create a sequence from 0 to 8 mio cells 
#### length.out=100  -> iteratively 100 steps 0 -> 8

mmdf <- data.frame(Concentration=seq(0,max(df$Concentration),length.out=100))

print(mmdf)


# ----------- 3
### creating new column in data.frame called "Sepecific_binding"
#### predict variabels using the MM.model on top
#### taking values from mmdf sequence from Cell_conc 

mmdf$CPM <- predict(MM.model, newdata=mmdf)

print(mmdf)



# ------------- 4 
###  vizualization of the data:
#### taking original data from "df"
##### plotting them as geom_point
##### plotting the fitted courve from data frame "mmdr"

p<-ggplot(df, aes(x = Concentration, y = CPM)) +
  theme_bw() +
  xlab("Binding molecule [nM]") +
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
summary(MM.model)

# applying formular with nls - function,
### !Calculation already all done, helps for the elucidation of the variables
###!!!!! not working with counts as Y instead of %bidning
mm.model.nls <- nls(CPM~r*Concentration/(Kd+Concentration), data=df, 
                    start = list(Kd=max(df$CPM)/2, 
                                Bmax=max(df$CPM)))

summary(mm.model.nls)

#Error in nls(CPM ~ r * Concentration/(Kd + Concentration), data = df,  : 
 #              step factor 0.000488281 reduced below 'minFactor' of 0.000976562