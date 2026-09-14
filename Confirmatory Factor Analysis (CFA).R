# Import Data
library(readxl)

data <- read_excel("Dataset.xlsx")
head(data)
summary(data)

# libraries
library(foreign) # for importing SPSS data
library(psych) # for psychometrics

# descriptive
describe(data)
response.frequencies(data[,-1])
mardia(data[,-1])

# preliminaries atau uji asumsi (normalitas dan homogenitas)
KMO(data[,-1]) # Uji normalitas
cortest.bartlett(data[,-1]) # Uji homogenitas
scree = scree(data[,-1]); print(scree) # Uji komponen penting/ principal component

# uji independent sample t-test.
library(stats)
grouped_data <- read_excel('Dataset.xlsx',sheet = "Sheet2")
grouped_data$Kelas <- as.factor(grouped_data$Kelas)
experiment.class <- subset(grouped_data,grouped_data$Kelas==1)
control.class <- subset(grouped_data,grouped_data$Kelas==2)
# Perform the independent sample t-test
t_test_result <- t.test(experiment.class$`Skor total`, control.class$`Skor total`)
t_test_result

# CFA
library(foreign)
library(lavaan)

m1a  <- ' f  =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 +
Item9 + Item10 + Item11 + Item12 + Item13 + Item14 + Item15 + Item16 + Item17 + Item18+
Item19 + Item20'
onefacall_itemsa <- cfa(m1a, data=data)
summary(onefacall_itemsa)

summary(onefacall_itemsa, fit.measures=TRUE, standardized=TRUE)

## Data 2 ##
library(readxl)
data2 <- read_excel("Dataset.xlsx",sheet = "Sheet3")
head(data2)
summary(data2)

# libraries
library(foreign) # for importing SPSS data
library(psych) # for psychometrics

# descriptive
describe(data2)
response.frequencies(data2[,-1])
mardia(data2[,-1])

# preliminaries atau uji asumsi (normalitas dan homogenitas)
KMO(data2[,-1]) # Uji normalitas
cortest.bartlett(data2[,-1]) # Uji homogenitas
scree2 = scree(data2[,-1]); print(scree2) # Uji komponen penting/ principal component

# CFA
library(foreign)
library(lavaan)

m1a2  <- ' f  =~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8 +
Item9 + Item10 + Item11 + Item12 + Item13 + Item14 + Item15 + Item16 + Item17 + Item18+
Item19 + Item20'
onefacall_itemsa2 <- cfa(m1a2, data=data2)
summary(onefacall_itemsa2)

summary(onefacall_itemsa2, fit.measures=TRUE, standardized=TRUE)
