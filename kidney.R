# data: kidney- datacamp
#date: may 28, 2020
lapply(c("tidyverse", "ggplot2", "dplyr", "readr"), require, character.only=T)

kidney<- read.csv("https://raw.githubusercontent.com/lichenyangalexa/DataCamp-R-Project/master/Kidney%20Stones%20and%20Simpson's%20Paradox/Datasets/kidney_stone_data.csv")
write.csv(kidney, "kidney.csv")
#############################  1. A new look at an old research study
# In 1986, a group of urologists in London published a research paper in The British Medical Journal that compared the effectiveness of two different methods to remove kidney stones. Treatment A was open surgery (invasive), and treatment B was percutaneous nephrolithotomy (less invasive). When they looked at the results from 700 patients, treatment B had a higher success rate. However, when they only looked at the subgroup of patients different kidney stone sizes, treatment A had a better success rate. What is going on here? This known statistical phenomenon is called Simpon’s paradox. Simpon's paradox occurs when trends appear in subgroups but disappear or reverse when subgroups are combined.
# In this notebook, we are going to explore Simpon’s paradox using multiple regression and other statistical tools. Let's dive in now!

head(kidney)
table(kidney$treatment, kidney$stone_size, kidney$success)

############################# 2. Recreate the Treatment X Success summary table
# The data contains three columns: treatment (A or B), stone_size (large or small) and success (0 = Failure or 1 = Success). 
# To start, we want to know which treatment had a higher
# success rate regardless of stone size. Let's create a table
#with the number of successes and frequency of success by each treatment 
# using the tidyverse syntax.
freq<- kidney %>% 
  group_by(treatment, success) %>%
  tally()%>%
  mutate(freq= n/sum(n))
freq  

frq_success <- freq %>%  filter(success== 1)
frq_success


##################
# 3. Bringing stone size into the picture
# From the treatment and success rate descriptive table, we saw that treatment B performed better on average compared to treatment A (82% vs. 78% success rate). Now, let's consider stone size and see what happens. We are going to stratify the data into small vs. large stone subcategories and compute the same success count and rate by treatment like we did in the previous task.
# The final table will be treatment X stone size X success.  

freq_all <- kidney %>%
  group_by(treatment, stone_size, success) %>%
  tally() %>%
  mutate(freq= n/sum(n))


freq_all  

########################### 4. When in doubt, rely on a plot
# What is going on here? When stratified by stone size, treatment 
# A had better results for both large and small stones compared to
# treatment B (i.e., 73% and 93% v.s. 69% and 87%). Sometimes a plot
# is a more efficient way to communicate hidden numerical information in
# the data. In this task, we are going to apply a plotting technique to 
# reveal the hidden information.  
freq_all%>% ggplot(aes(treatment, n)) +
  geom_bar(aes(fill=stone_size), stat= "identity")
  
############################ 5. Identify and confirm the lurking variable
# From the bar plot, we noticed an unbalanced distribution of kidney stone 
# sizes in the two treatment options. Large kidney stone cases tended to be in 
# treatment A, while small kidney stone cases tended to be in treatment B.
# Can we confirm this hypothesis with statistical testing?
#   Let's analyze the association between stone size (i.e., case severity) 
# and treatment assignment using a statistical test called Chi-squared.
# The Chi-squared test is appropriate to test associations between two categorical variables. 
#This test result, together with the common knowledge that a more severe case would be more 
#likely to fail regardless of treatment, will shed light on the root cause of the paradox.

chi_kideny <- chisq.test(kidney$treatment
                         , kidney$stone_size)

install.packages("broom")
library("broom")
tidy(chi_kideny)



########################### 6. Remove the confounding effect
# After the above exercises, we are confident that stone size/case severity is indeed the
# lurking variable (aka, confounding variable) in this study of kidney stone treatment and
# success rate. The good news is that there are ways to get rid of the effect of the lurking 
# variable.
# # Let's practice using multiple logistic regression to remove the unwanted effect of 
# stone size, and then tidy the output with a function from the broom package.

lg_model <- glm(data = kidney,  success ~ stone_size + treatment, family = "binomial")

tidy(lg_model)


############################## 7. Visualize model output
# We successfully fit a multiple logistic regression and pulled out the model coefficient estimates! Typically (and arbitrarily), P-values below 0.05 indicate statistical significance. Another way to examine whether a significant relationship exists or not is to look at the 95% confidence interval (CI) of the estimate. In our example, we are testing to see:
#   if the effect of a small stone is the same as a big stone, and
# if treatment A is as effective as treatment B.
# If the 95% CI for the coefficient estimates cover zero, we cannot conclude that one is different from the other. Otherwise, there is a significant effect.  \
tidy(lg_model) %>% ggplot(aes(term, estimate))+
  geom_pointrange(aes(ymin= estimate - 1.96* std.error,
                  ymax= estimate + 1.96*std.error))


########################### 8. Generate insights
# Based on the coefficient estimate plot and the model output table, there is enough 
# information to generate insights about the study. Is treatment A superior to B after 
# taking into account the effect of stone size/severity level?
#   Everything is in the output table from the regression model. 
#Recall, a coefficient represents the effect size of the specific model term. 
#A positive coefficient means that the term is positively related to the outcome.
#For categorical predictors, the coefficient is the effect on the outcome relative to
#the reference category. In our study, stone size large and treatment
#A are the reference categories.


# Is small stone more likely to be a success after controlling for treatment option effect?
  # Options: Yes, No (as string)
small_high_success <- "Yes"

# Is treatment A significantly better than B?
# Options: Yes, No (as string)
A_B_sig <- "No"



