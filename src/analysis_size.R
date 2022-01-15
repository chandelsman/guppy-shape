advanced.procD.lm(log(CS) ~ population + treatment,
                          ~ population * treatment,
                          groups = ~population*treatment,
                          iter = 9999)

lm1 <- lm(log(CS) ~ population + treatment)
lm2 <- lm(log(CS) ~ population * treatment)

advanced.procD.lm(log(CS) ~ population + treatment,
                          ~ population * treatment,
                  groups = ~population*treatment, 
                  iter=9999, verbose=T)
library(lsmeans) #least squares means from GLM
rg1 <- ref.grid(lm1)
pairs(rg1)
rg2 <- ref.grid(lm2)
pairs(rg2, adjust = c("bon"))

summary(lm2, infer = c(T, T), level = .95, adjust = "bon", by = "treatment")
plot(lm2, by = "treatment")
lsmeans(rg2, pairwise ~ population*treatment)
print(lsmeans (lm2,  list(pairwise ~ population | treatment,  poly ~ treatment | population)))
lsmeans(lm2, list(pairwise ~ population*treatment), 
        adjust = c("bon"))
