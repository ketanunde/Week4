
# Beavers dataset
?beavers

str(beaver2)
# use the transform function to change the 
# active variable to factor
transformed_beaver_data <- transform(beaver2, 
                                     activ = factor(activ, labels = c("no", "yes")))
transformed_beaver_data

# select the appropriate test
# first step is to check normality of the data

library("lattice")
histogram(~temp | activ, data = transformed_beaver_data)

with(transformed_beaver_data, 
     qqplot(temp[activ == "yes"], 
            temp[activ == "no"], 
            main = "Comparing 2 samples",
            xlab = "Active temp = yes", ylab = "Active temp = no"))

# Using qqplot and qqnorm function 
with(transformed_beaver_data, 
     {qqnorm(temp[activ == "no"],
            main = "Inactive")
            qqline(temp[activ =="no"])
    })


# Change active period to "yes" 
with(transformed_beaver_data, 
     {qqnorm(temp[activ == "yes"],
             main = "Inactive")
       qqline(temp[activ =="yes"])
     })

# Formal test for normality
normalty_test <- shapiro.test(transformed_beaver_data$temp)
normalty_test$p.value
# p-value tell us the chances of the sample comes from noraml distribution 
# p-value is clearly less than 0.05
# So the data is not normally distributed.

# we can apply this test on all variable on tapply() function.
with(transformed_beaver_data, tapply(temp, activ, shapiro.test))


t.test(temp ~ activ, data = transformed_beaver_data)

# use separate vectors for the samples that we want to compare
with(transformed_beaver_data, 
     t.test(temp[activ == "yes"], 
            temp[activ == "no"]))

# for the data which we think is not normal
wilcox.test(temp ~ activ, data = transformed_beaver_data)

# sleep dataset
str(sleep)
head(sleep)
#Extras = extras hours of the sleep after medicine
#Group = which varient eacj participent took
#ID = partitient ID

# each person gets both varients - data is therefor paired
# we want to know if both types of sleeps medicine had an effect
# on the length of sleep

t.test(extra ~ group, data = sleep, paired = TRUE)

# Testing counts on a proportions
survivors <- matrix(c(1781, 1443, 135, 47), ncol = 2)
colnames(survivors) <- c("survived", "died")
rownames(survivors) <- c("no seat belt", "seat belt")
survivors

result_prop_test <- prop.test(survivors)
result_prop_test
# p-value tell us how likely that both proportions are equal
# p-value -> null is not true
# Therefore dying in hospital in car crash is lower
# if you're wearing seat belt


chisq.test(survivors)


# Theses tests can be applied with more than 2 cols
str(HairEyeColor)
head(HairEyeColor, 20)
?dimnames
dimnames(HairEyeColor)

# check whether hair color and eye color are related
?margin.table
hair_eye_margin <- margin.table(HairEyeColor, 
                                margin = c(1,2))
hair_eye_margin

#Once the table wa built we can apply the chi-squed test
chisq.test(hair_eye_margin)
