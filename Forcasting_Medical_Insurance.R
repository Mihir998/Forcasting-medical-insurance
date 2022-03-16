#Used for data manipulation
library(dplyr)

#Used for data visualization
library(ggplot2)

#Used for beautify the data table
library(DT)

#Used for make a pie chart
library(plotly)

#Used for find error value
library(Metrics)

#Read the data from csv file
insurance <- read.csv("C://Users//DELL//Desktop//Mtech DS//Finance//Project//insurance.csv",stringsAsFactors = FALSE)

#View the imported file
view(insurance)

# Age of Insurance Member
age_group <- insurance %>%
  group_by(age) %>%
  summarise(total = n())

Agecut <- cut(age_group$age, c(seq(15, 65, by = 5), Inf), include.lowest = TRUE)

agegroup <- aggregate(total ~ Agecut, age_group, sum)

#Medical Insurance's member aggregated by cost
ggplot(insurance, aes(age)) +
  geom_freqpoly(binwidth = 1, color = 'blue') + 
  geom_histogram(binwidth = 1, fill = 'red', alpha = .5) +
  theme_linedraw() + #make a theme
  theme(panel.background = element_rect(fill = "gainsboro", colour = "white", size = 0.5, linetype = "solid"), #theme panel settings
        plot.background = element_rect(fill = "gainsboro"), #theme panel settings
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), #theme panel settings
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"), #theme panel settings
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), #title settings
        plot.subtitle = element_text(face = "italic"), #subtitle settings
        plot.caption = element_text(size = 6, vjust = -1, face = "italic")) + #caption/credit settings
  labs(x = 'Age', y = 'Frequency', title = "Member of Medical Cost Insurance", #name title and axis
       subtitle = "Medical insurance's member aggregated by age") + 
  guides(fill=FALSE) + #remove color legend
  scale_y_continuous(limits = c(0,80), breaks = c(0,20,40,60,80)) #set axis limits and break

#Sex of Insurance Member
sex <- insurance %>%
  group_by(sex) %>%
  summarise(total = n()) %>%
  mutate(percentage = paste0(round(100*total/sum(total),1), "%"))

#Plotted the Gender of Medical insurance Member
plot_ly(sex, labels = ~sex, values = ~total, type = 'pie', #plotly package
        textposition = 'inside', 
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste(total, 'people'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
  layout(title = 'Gender of Medical Insurance Member', titlefont = list(size = 18, color = 'black'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Region of Insurance Member
# Summarized the medical Insurance by region and displayed it into percentage
reg <- insurance %>%
  group_by(region) %>%
  summarise(total = n()) %>%
  mutate(percentage = paste0(round(100*total/sum(total),1), "%"))

# Plotted the Region of medical Insurance Member
plot_ly(reg, labels = ~region, values = ~total, type = 'pie', #plotly package
        textposition = 'inside', 
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste(total, 'people'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
  layout(title = 'Region of Medical Insurance Member', titlefont = list(size = 18, color = 'black'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Insurance Member who Smoke
# Medical Insurance Of Smokers VS Non Smokers
insurance %>%
  group_by(smoker) %>%
  summarise(total = n()) %>%
  mutate(percentage = paste0(round(100*total/sum(total),1), "%"),
         annot = c("Non-Smoker","Smoker")) %>%
  ggplot(aes(x=annot, y=total, label = percentage, fill = annot)) + 
  geom_bar(stat="identity") +
  geom_text(hjust = 0.5, vjust = -1, color = "black", fontface = "italic", size = 5) + #label type 
  theme_linedraw() + #make a theme
  theme(panel.background = element_rect(fill = "gainsboro", colour = "white", size = 0.5, linetype = "solid"), #theme panel settings
        plot.background = element_rect(fill = "gainsboro"), #theme panel settings
        legend.position = "none", #legend position
        legend.title = element_blank(), #remove legend title
        legend.background = element_rect(fill = "gainsboro", colour = "gainsboro", size = 0.5, linetype = "solid"), #change legend box color
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), #theme panel settings
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"), #theme panel settings
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), #title settings
        plot.subtitle = element_text(face = "italic")) + #subtitle settings
  labs(x = '', y = '', title = "Member of Medical Insurance", #name title and axis
       subtitle = 'How many smokers registered as insurance member?') + #name subtitle
  scale_y_continuous(limits = c(0,1500), breaks = c(0,300,600,900,1200,1500)) #set axis limits and break

# Number of Dependents
#Percenatge Of Children Present in the Region
child <- insurance %>%
  group_by(children) %>%
  summarise(total = n()) %>%
  mutate(percentage = paste0(round(100*total/sum(total),1), "%"),
         annot = c("Zero", "1 Child", "2 Children", "3 Children", "4 Children", "5 Children"))

# Number of Dependents From Medical Insurance Member
plot_ly(child, labels = ~annot, values = ~total, type = 'pie', #plotly package
        textposition = 'outside', 
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste(total, 'member'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
  layout(title = 'Number of Dependents From Medical Insurance Member', titlefont = list(size = 18, color = 'black'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Body Mass Index of Insurance Member
ggplot(insurance, aes(bmi)) +
  geom_histogram(binwidth = 1, fill = 'red', alpha = .5) +
  theme_linedraw() + #make a theme
  theme(panel.background = element_rect(fill = "gainsboro", colour = "white", size = 0.5, linetype = "solid"), #theme panel settings
        plot.background = element_rect(fill = "gainsboro"), #theme panel settings
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), #theme panel settings
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"), #theme panel settings
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), #title settings
        plot.subtitle = element_text(face = "italic"), #subtitle settings
        plot.caption = element_text(size = 6, vjust = -1, face = "italic")) + #caption/credit settings
  labs(x = 'Body Mass Index', y = 'Frequency', title = "Member of Medical Cost Insurance", #name title and axis
       subtitle = "Body mass index of medical insurance's member") + 
  guides(fill=FALSE) + #remove color legend
  scale_y_continuous(limits = c(0,120), breaks = c(0,20,40,60,80,100,120)) #set axis limits and break

# Charges of Insurance Member
#Individual Medical Costs Billed by health Insurance
ggplot(insurance, aes(charges)) +
  geom_histogram(binwidth = 2000, fill = 'red', alpha = .5) +
  theme_linedraw() + #make a theme
  theme(panel.background = element_rect(fill = "gainsboro", colour = "white", size = 0.5, linetype = "solid"), #theme panel settings
        plot.background = element_rect(fill = "gainsboro"), #theme panel settings
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), #theme panel settings
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"), #theme panel settings
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), #title settings
        plot.subtitle = element_text(face = "italic"), #subtitle settings
        plot.caption = element_text(size = 6, vjust = -1, face = "italic")) + #caption/credit settings
  labs(x = 'Charges', y = 'Frequency', title = "Medical Cost Insurance", #name title and axis
       subtitle = "Individual medical costs billed by health insurance") + 
  guides(fill=FALSE) + #remove color legend
  scale_y_continuous(limits = c(0,250), breaks = c(0,50,100,150,200,250)) #set axis limits and break

# Charges Density Vs Sex
ggplot(insurance, aes(x = charges, fill = sex)) + geom_density(alpha = 0.5)

# Charges Density Vs Region
ggplot(insurance, aes(x = charges, fill = region)) + geom_density(alpha = 0.5)

# Charges Density Vs Smoker
ggplot(insurance, aes(x = charges, fill = smoker)) + geom_density(alpha = 0.5)

# Charges Density Vs Children
ggplot(insurance, aes(x = charges, fill = as.factor(children))) + geom_density(alpha = 0.5)

# Smoker, have no dependent, BMI under 30.
ins_smoker_nochild_under30 <- insurance %>%
  filter(smoker == "yes" & children == 0 & bmi < 30)

ggplot(ins_smoker_nochild_under30, aes(x = age, y = charges)) + geom_point()

#Age vs Charges chart looks can be approached by using linear regression.  
ggplot(ins_smoker_nochild_under30, aes(x = bmi, y = charges)) + geom_point()

# BMI vs Charges looks have a disordered correlation.

summary(lm(charges ~ age, data = ins_smoker_nochild_under30))

summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_under30))

# Smoker, have no dependent, BMI over 30.

ins_smoker_nochild_over30 <- insurance %>%
  filter(smoker == "yes" & children == 0 & bmi >= 30)

ggplot(ins_smoker_nochild_over30, aes(x = age, y = charges)) + geom_point()

# Age vs Charges chart looks can be approached by using linear regression.  

ggplot(ins_smoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()

# BMI vs Charges looks have a disordered correlation.

summary(lm(charges ~ age, data = ins_smoker_nochild_over30))

summary(lm(charges ~ age + bmi, data = ins_smoker_nochild_over30))


# Smoker, have dependents, BMI under 30.**

ins_smoker_child_under30 <- insurance %>%
  filter(smoker == "yes" & children > 0 & bmi < 30)

ggplot(ins_smoker_child_under30, aes(x = age, y = charges)) + geom_point()

# Age vs Charges chart looks can be approached by using linear regression.  

ggplot(ins_smoker_child_under30, aes(x = bmi, y = charges)) + geom_point()

# BMI vs Charges looks have a disordered correlation.

summary(lm(charges ~ age, data = ins_smoker_child_under30))

summary(lm(charges ~ age + bmi, data = ins_smoker_child_under30))

# Smoker, have dependents, BMI over 30.**

ins_smoker_child_over30 <- insurance %>%
  filter(smoker == "yes" & children > 0 & bmi >= 30)

ggplot(ins_smoker_child_over30, aes(x = age, y = charges)) + geom_point()

# Age vs Charges chart looks can be approached by using linear regression.  

ggplot(ins_smoker_child_over30, aes(x = bmi, y = charges)) + geom_point()

#BMI vs Charges looks have a disordered correlation.

summary(lm(charges ~ age, data = ins_smoker_child_over30))

summary(lm(charges ~ age + bmi, data = ins_smoker_child_over30))

# Non-smoker, have no dependent, BMI under 30.**
ins_nonsmoker_nochild_under30 <- insurance %>%
  filter(smoker == "no" & children == 0 & bmi < 30)
 
ggplot(ins_nonsmoker_nochild_under30, aes(x = age, y = charges)) + geom_point()

# Age vs Charges chart looks can be approached by using linear regression.  

ggplot(ins_nonsmoker_nochild_under30, aes(x = bmi, y = charges)) + geom_point()

# BMI vs Charges looks have a disordered correlation.

summary(lm(charges ~ age, data = ins_nonsmoker_nochild_under30))

summary(lm(charges ~ age + bmi, data = ins_nonsmoker_nochild_under30))

# Non-smoker, have no dependent, BMI over 30.**

ins_nonsmoker_nochild_over30 <- insurance %>%
  filter(smoker == "no" & children == 0 & bmi >= 30)

ggplot(ins_nonsmoker_nochild_over30, aes(x = age, y = charges)) + geom_point()

#Age vs Charges chart looks can be approached by using linear regression.  

ggplot(ins_nonsmoker_nochild_over30, aes(x = bmi, y = charges)) + geom_point()

#BMI vs Charges looks have a disordered correlation.

summary(lm(charges ~ age, data = ins_nonsmoker_nochild_over30))

summary(lm(charges ~ age + bmi, data = ins_nonsmoker_nochild_over30))

#Non-smoker, have dependents, BMI under 30.**

ins_nonsmoker_child_under30 <- insurance %>%
  filter(smoker == "no" & children > 0 & bmi < 30)

ggplot(ins_nonsmoker_child_under30, aes(x = age, y = charges)) + geom_point()

#Age vs Charges chart looks can be approached by using linear regression.  

ggplot(ins_nonsmoker_child_under30, aes(x = bmi, y = charges)) + geom_point()

#BMI vs Charges looks have a disordered correlation.

summary(lm(charges ~ age, data = ins_nonsmoker_child_under30))

summary(lm(charges ~ age + bmi, data = ins_nonsmoker_child_under30))

# Non-smoker, have dependents, BMI over 30.**

ins_nonsmoker_child_over30 <- insurance %>%
  filter(smoker == "no" & children > 0 & bmi >= 30)

ggplot(ins_nonsmoker_child_over30, aes(x = age, y = charges)) + geom_point()

#Age vs Charges chart looks can be approached by using linear regression.  

ggplot(ins_nonsmoker_child_over30, aes(x = bmi, y = charges)) + geom_point()

#BMI vs Charges looks have a disordered correlation.

summary(lm(charges ~ age, data = ins_nonsmoker_child_over30))

summary(lm(charges ~ age + bmi, data = ins_nonsmoker_child_over30))

# Charges Prediction
# From grouping the equation, I made a function to predict the charges. The function is look like:

predict <- function(x){
  for(i in 1:nrow(x)){
    if(x[i,"smoker"] == "yes" && x[i,"children"] == 0 && x[i,"bmi"] < 30){
      x[i,"result"] = -956.74 + (251.20*x[i,"age"]) + (505.18*x[i,"bmi"])
    } else if(x[i,"smoker"] == "yes" && x[i,"children"] == 0 && x[i,"bmi"] >= 30) {
      x[i,"result"] = 8120.10 + (292.16*x[i,"age"]) + (614.01*x[i,"bmi"])
    } else if(x[i,"smoker"] == "yes" && x[i,"children"] > 0 && x[i,"bmi"] < 30){
      x[i,"result"] = 2428.48 + (259.48*x[i,"age"]) + (359.27*x[i,"bmi"])
    } else if(x[i,"smoker"] == "yes" && x[i,"children"] > 0 && x[i,"bmi"] >= 30){
      x[i,"result"] = 16021.03 + (253.72*x[i,"age"]) + (447.91*x[i,"bmi"])
    } else if(x[i,"smoker"] == "no" && x[i,"children"] == 0 && x[i,"bmi"] < 30){
      x[i,"result"] = -3239.15 + (277.00*x[i,"age"])
    } else if(x[i,"smoker"] == "no" && x[i,"children"] == 0 && x[i,"bmi"] >= 30){
      x[i,"result"] = -2155.79 + (254.01*x[i,"age"])
    } else if(x[i,"smoker"] == "no" && x[i,"children"] > 0 && x[i,"bmi"] < 30){
      x[i,"result"] = -884.08 + (247.85*x[i,"age"])
    } else {
      x[i,"result"] = -2161.36 + (282.54*x[i,"age"])
    }    
  }
  return(x)
}

#And the results of my prediction is shown on table below
predcharges <- predict(insurance)

datatable(predcharges, colnames = c('Age', 'Sex', 'BMI', 'Children', 'Smoker', 'Region', 'Charges', 'Charges Prediction'))

# The Root Mean Square Error of my prediction:**
  
rmse(predcharges$charges, predcharges$result)

# The Mean Absolute Percent Error of my prediction:**

mape(predcharges$charges, predcharges$result)
