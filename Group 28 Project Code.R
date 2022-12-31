library("dplyr")
library("tidyr")
library("descr")
library(haven)
library("janitor")
library("remotes")
library("tidyverse")
library("psych")
project_data <- read_stata("~/Desktop/Second Term/3. Analytics/Project/Datawork/Cleaned_W5.dta")

names(project_data)
head(project_data)

################################### CREATING VARIABLES #################################################################################


  # Creating binary for Children in Household

project_data= project_data %>% mutate(w5_hhchild=ifelse(w5_m_HHchildn!=0, 1, 0))
prop.table(table(project_data$w5_hhchild,project_data$w5_m_HHchildn))

  # Creating Categorical Variable for Race

project_data= project_data %>% mutate(w5_race_cat = ifelse(w5_best_race == 1, "Black",
                                                           ifelse(w5_best_race == 2, "Coloured",
                                                                  ifelse(w5_best_race == 3, "India/Asian", "White"))))
  
  # Creating Categorical Variable for Household OAP Pension Receipt

project_data= project_data %>% mutate(w5_hhpen_cat = ifelse(w5_m_hhpen == 1, "OAP in Household", "No OAP in Household"))

  # Creating Household income per capita
project_data= project_data %>% mutate(w5_m_hhincome_percap=w5_m_hhincome/w5_m_hhsize_res)


################################### CREATING SUB SAMPLES #################################################################################



# Children
children_sample = project_data %>% subset(w5_m_best_age >= 7)  %>%  subset(w5_m_best_age <= 19)

# Black children
black_children= project_data %>% subset(w5_m_best_age >= 7)  %>%  subset(w5_m_best_age <= 19)  %>%  subset(w5_best_race = 1)

# Households
total_households = project_data %>% subset(hhtag= 1)

# Black Children Households
BlackChildren_Households = black_children %>% subset(hhtag= 1)





#################################### CREATING FIGURES #############################################################################

############## FIGURE 1 #############

  # Dropping all rows with NA values for w5_hhpen_cat or w5_race_cat
project_data=project_data %>% 
  drop_na(c("w5_hhpen_cat", "w5_race_cat"))


  # Finding means for mean_hhchild across groups

(figure1_sum <- group_by(project_data,
                     w5_hhpen_cat,
                     w5_race_cat) %>% summarise(mean_hhchild = mean(w5_hhchild)))

  # Plotting these on a grouped bar chat

ggplot(figure1_sum,                                      
       aes(x = w5_race_cat,
           y = mean_hhchild,
           fill = w5_hhpen_cat)) + ggtitle("Households With At Least One Child: by Household Pension Receipt and Race") + xlab("") + ylab("Proportion of Households") + labs(fill = " ") +
  geom_bar(stat = "identity",
           position = "dodge") + 
  scale_fill_manual(values=c("#1C4E80",
                             "#a30707")) + theme_minimal() +  geom_text(aes(label = round(mean_hhchild, digits=2)), vjust = -0.2, size=2, position=position_dodge(width=0.9)) + theme(plot.title=element_text(size=8))



# Method 2: 

## ggplot(data=project_data, aes(x=w5_race_cat, y=w5_hhchild, fill=w5_hhpen_cat)) +
 ##  labs(title="Proportion of Households with At Least One Child", x="", y="") + 
 ##  stat_summary(fun.data=mean_sdl, geom="bar", position = "dodge") + theme_minimal() 


############## FIGURE 2 #############


# Dropping all rows with NA values for w5_hhpen_cat or w5_race_cat
black_children=black_children %>% 
  drop_na(c("w5_m_educyrs"))

# black_children %>% group_by(w5_m_best_age, w5_hhpen_cat) %>% summarise(mean_educyrs = mean(w5_m_educyrs))

(figure3_sum <- group_by(black_children,
                         w5_m_best_age,
                         w5_hhpen_cat) %>% summarise(mean_educyrs = mean(w5_m_educyrs)))

ggplot(figure3_sum, aes(x = w5_m_best_age, y = mean_educyrs, color = w5_hhpen_cat)) + geom_line() +
  geom_point() + ggtitle("Years of Education Over Age and Across Household Pension Receipt") + xlab("Age") + ylab("Mean Years of Education") + labs(fill =" ") + theme_minimal() +  scale_colour_manual(values = c("#1C4E80","#a30707")) + theme(plot.title=element_text(size=9))


############## FIGURE 4 #############


pensioner_sample = project_data %>% subset(w5_m_best_age >= 35)  %>%  subset(w5_m_best_age <= 95)  %>%  subset(w5_best_race == 1)

(figure2_sum <- group_by(pensioner_sample,
                         w5_m_best_age,
                         w5_best_gen) %>% summarise(mean_penrec = mean(w5_m_penrec)))

ggplot(figure2_sum, aes(x = w5_m_best_age, y = mean_penrec)) + geom_point() + ggtitle("Share of Black South Africans Receiving the OAP by Age") + xlab("Age") + ylab("Share of Pension Receivers") + theme_minimal() + scale_fill_manual(values=c("#1C4E80")) + geom_vline(xintercept = 60,color = "red") + theme(plot.title=element_text(size=12))



########################################## DESCRIPTIVE STATS TABLE ###########################################################################################


############## TABLE 1 #############

library(psych)
describe(BlackChildren_Households, fast=TRUE)
describeBy(BlackChildren_Households, group=BlackChildren_Households$w5_m_hhpen, fast=TRUE)

BlackChildren_Households %>% select(w5_m_hhsize_mem, w5_m_hhsize_res, w5_m_hhrural, w5_m_HHchildn, w5_c_mother_resident, w5_c_father_resident, w5_m_hhincome, w5_m_hhincome_percap, w5_c_motherschoolyears, w5_c_fatherschoolyears) %>% describeBy(group=BlackChildren_Households$w5_m_hhpen, fast=TRUE)



############################################# REGRESSIONS ########################################################################################

############## MODEL 1 #############


mod_1<-lm(w5_m_educyrs~w5_m_hhpen + w5_m_best_age, black_children)
summary(mod_1)


############## MODEL 2 #############


black_children["w5_m_prov"]<- factor(black_children$w5_m_prov)
                                 
mod_2<-lm(w5_m_educyrs~w5_m_hhpen + w5_m_best_age + w5_best_gen + w5_m_hhsize_res + w5_m_hhrural + w5_m_prov + w5_m_HHchildn + w5_c_mother_resident + w5_c_father_resident + w5_m_hhincome_percap + w5_c_motherschoolyears + w5_c_fatherschoolyears, black_children)
summary(mod_2)

############## MODEL 3 #############


library(AER)


modIV_3=ivreg(w5_m_educyrs~w5_m_hhpen + w5_m_best_age + w5_best_gen + w5_m_hhsize_res + w5_m_hhrural + w5_m_prov + w5_m_HHchildn + w5_c_mother_resident + w5_c_father_resident + w5_m_hhincome_percap + w5_c_motherschoolyears + w5_c_fatherschoolyears| w5_m_hhpenage + w5_m_best_age + w5_best_gen + w5_m_hhsize_res + w5_m_hhrural + w5_m_prov + w5_m_HHchildn + w5_c_mother_resident + w5_c_father_resident + w5_m_hhincome_percap + w5_c_motherschoolyears + w5_c_fatherschoolyears, data=black_children)
summary(modIV_3)

#################################### TESTING VALIDITY IV CONDITION ########################################################################################

fs=lm(w5_m_hhpen~w5_m_hhpenage + w5_m_best_age + w5_best_gen + w5_m_hhsize_res + w5_m_hhrural + w5_m_prov + w5_m_HHchildn + w5_c_mother_resident + w5_c_father_resident + w5_m_hhincome_percap + w5_c_motherschoolyears + w5_c_fatherschoolyears, data=black_children)
summary(fs)
linearHypothesis(fs,"w5_m_hhpenage=0")

mod1<- lm(w5_m_penrec~w5_m_penage, data=pensioner_sample)
summary(mod1)

mod2<- lm(w5_m_hhpen~w5_m_hhpenage, data=black_children)
summary(mod2)


