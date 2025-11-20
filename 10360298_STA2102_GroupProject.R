dat = read.csv("renewable_energy_workers-FV.csv", stringsAsFactors = FALSE)
d = subset(dat, select = c(Annual_Earnings, Age, Hours_Worked_Per_Day, Experience_Years))
d = na.omit(d)

# Model 1
m1 = lm(Annual_Earnings ~ Age + Hours_Worked_Per_Day + Experience_Years, data = d)
summary(m1)
coef(m1)

# Global F-test
s1 = summary(m1)
finfo = s1$fstatistic
f_val = finfo[1]; df1 = finfo[2]; df2 = finfo[3]
p_global = pf(f_val, df1, df2, lower.tail = FALSE)
f_val; df1; df2; p_global

# R² and Adjusted R²
s1$r.squared
s1$adj.r.squared

# Residual standard error
s1$sigma

# t-test for Experience_Years
coef(summary(m1))["Experience_Years",]

# 95% CI for Hours_Worked_Per_Day
confint(m1, "Hours_Worked_Per_Day", level = 0.95)

# 95% CI and PI for Age=40, Hours=10, Experience=8
newp = data.frame(Age = 40, Hours_Worked_Per_Day = 10, Experience_Years = 8)
predict(m1, newdata = newp, interval = "confidence", level = 0.95)
predict(m1, newdata = newp, interval = "prediction", level = 0.95)

# Model 2 with interaction
m2 = lm(Annual_Earnings ~ Age + Hours_Worked_Per_Day + Experience_Years + Age:Experience_Years, data = d)
summary(m2)
coef(m2)

# Slopes with interaction
b = coef(m2)
b_age = b["Age"]
b_exp = b["Experience_Years"]
b_int = b["Age:Experience_Years"]
slope_age_exp10 = b_age + 10*b_int
slope_exp_age40 = b_exp + 40*b_int
slope_age_exp10
slope_exp_age40

# Test interaction term
coef(summary(m2))["Age:Experience_Years",]
anova(m1, m2)


# Question 3 – One-way ANOVA


set.seed(2102)

A = c(32.1, 28.7, 34.9, 30.2, runif(1, 30, 32), 33.4, 31.8, 29.9)
B = c(40.5, 39.2, 38.8, 41.7, runif(1, 38, 39), 42.0, 37.9)
C = c(25.4, 27.8, 29.1, 26.0, 28.3, runif(1, 25, 26))

A[5]; B[5]; C[6]

Method = factor(c(rep("A", length(A)), rep("B", length(B)), rep("C", length(C))))
PM25_Reduction = c(A, B, C)
air = data.frame(Method = Method, PM25_Reduction = PM25_Reduction)

# Boxplot
boxplot(PM25_Reduction ~ Method, data = air,
        main = "PM2.5 Reduction by Method",
        xlab = "Method", ylab = "Reduction (µg/m³)")

# ANOVA
fit = aov(PM25_Reduction ~ Method, data = air)
summary(fit)

# Tukey test
TukeyHSD(fit)

# Diagnostic plots
par(mfrow = c(1, 2))
plot(fit, which = 1)
plot(fit, which = 2)
par(mfrow = c(1, 1))
