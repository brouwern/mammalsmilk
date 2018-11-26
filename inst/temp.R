
library(mammalsmilk)
library(ggpubr)
library(lme4)
library(arm)
library(stringr)
library(bbmle)
data("milk_fat")


milk_fat$fat.logit <- arm::logit(milk_fat$fat/100)
milk_fat$fat.log10 <- log10(milk_fat$fat)
milk_fat$mass.fem.log10 <-  log10(milk_fat$mass.fem)


library(dplyr)


orders <- milk_fat %>% group_by(ord) %>% summarise(order.N = n())

orders$ok <- ifelse(orders$order.N >2,"ok","drop")


milk_fat2 <- merge(milk_fat,orders)
dim(milk_fat)
dim(milk_fat2)

i.ok <- which(milk_fat2$ok == "ok")
ggscatter(data = milk_fat,
       y = "fat.log10",
       x = "mass.fem.log10",
       add = "reg.line") +
  facet_grid(.~fam)


ggplot(data = milk_fat,
       aes(y = fat.log10,
           x = mass.fem.log10,
           color = ord)) +
  geom_point()

milk_fat$genus.spp <- milk_fat$spp


genus_spp_subspp_mat <- milk_fat$genus.spp %>% str_split_fixed(" ", n = 3)

milk_fat$genus <- genus_spp_subspp_mat[,1]

#raw data
summary( lm(fat ~ diet,data = milk_fat))

summary(milk_fat[,c("fat.log10","diet","biome",
                    "ord","fam","genus")])

m.null.noslope <- lmer(fat.log10 ~ 1 +
                 (1|ord) +
                 (1|fam) +
                 (1|genus),
               REML = FALSE,
               data = milk_fat[i.ok,])
m.diet.noslope <- lmer(fat.log10 ~ diet +
                         (1|ord) +
                         (1|fam) +
                         (1|genus),
                       REML = FALSE,
                       data = milk_fat[i.ok,])
m.null <- lmer(fat.log10 ~ 1 +
                 (mass.fem.log10|ord) +
                 (1|fam) +
                 (1|genus),
               REML = FALSE,
               data = milk_fat[i.ok,])
m.diet <- lmer(fat.log10 ~ diet +
             (mass.fem.log10|ord) +
             (1|fam) +
             (1|genus),
             REML = FALSE,
           data = milk_fat[i.ok,])

m.mass <- lmer(fat.log10 ~ mass.fem.log10 +
                 (mass.fem.log10|ord) +
                 (1|fam) +
                 (1|genus),
               REML = FALSE,
               data = milk_fat[i.ok,])

m.biome <- lmer(fat.log10 ~ biome +
                 (mass.fem.log10|ord) +
                 (1|fam) +
                 (1|genus),
                REML = FALSE,
               data = milk_fat[i.ok,])


m.mass.biome <- lmer(fat.log10 ~ mass.fem.log10 + biome +
                  (mass.fem.log10|ord) +
                  (1|fam) +
                  (1|genus),
                  REML = FALSE,
                data = milk_fat[i.ok,])

m.mass.diet <- lmer(fat.log10 ~ mass.fem.log10 + diet +
                       (mass.fem.log10|ord) +
                       (1|fam) +
                       (1|genus),
                    REML = FALSE,
                     data = milk_fat[i.ok,])

m.mass.x.biome <- lmer(fat.log10 ~ mass.fem.log10*biome +
                      (mass.fem.log10|ord) +
                      (1|fam) +
                      (1|genus),
                      REML = FALSE,
                    data = milk_fat[i.ok,])


m.mass.x.biome.diet <- lmer(fat.log10 ~ mass.fem.log10*biome + diet +
                         (mass.fem.log10|ord) +
                         (1|fam) +
                         (1|genus),
                       REML = FALSE,
                       data = milk_fat[i.ok,])

m.mass.x.diet <- lmer(fat.log10 ~ mass.fem.log10*diet +
                         (mass.fem.log10|ord) +
                         (1|fam) +
                         (1|genus),
                      REML = FALSE,
                       data = milk_fat[i.ok,])


m.biome.diet <- lmer(fat.log10 ~ biome + diet +
                        (mass.fem.log10|ord) +
                        (1|fam) +
                        (1|genus),
                      REML = FALSE,
                      data = milk_fat[i.ok,])

m.mass.x.biome.mass.x.diet <- lmer(fat.log10 ~ mass.fem.log10*biome + mass.fem.log10*diet +
                       (mass.fem.log10|ord) +
                       (1|fam) +
                       (1|genus),
                     REML = FALSE,
                     data = milk_fat[i.ok,])

ICtab(type = "AICc",
      weights = "TRUE",
      base = TRUE,
      logLik = TRUE,
      m.null.noslope,
      m.null,
      m.diet.noslope,
       m.diet,
       m.mass,
       m.biome,
       m.biome.diet,
       m.mass.biome,
       m.mass.diet,
       m.mass.x.biome,
       m.mass.x.diet,
      m.mass.x.biome.diet,
       m.mass.x.biome.mass.x.diet)

install.packages("r2glmm")
library(r2glmm)

r2beta(m.mass.x.biome)
r2beta(m.diet)

r2beta(m.mass.x.biome, method = "nsj")
r2beta(m.diet, method = "nsj")

m.diet.means <- lmer(fat.log10 ~ -1 + diet +
                 (mass.fem.log10|ord) +
                 (1|fam) +
                 (1|genus),
               data = milk_fat)



m0 <- lmer(fat.log10 ~ mass.fem.log10 + biome +
             (mass.fem.log10|ord) +
             (1|fam) +
             (1|genus),
           data = milk_fat)
m1 <- lmer(fat.log10 ~ mass.fem.log10*biome +
               (mass.fem.log10|ord) +
               (1|fam) +
             (1|genus),
             data = milk_fat)
m2 <- lmer(fat.log10 ~ mass.fem.log10*biome +
             mass.fem.log10*diet +
             (mass.fem.log10|ord) +
             (1|fam) +
             (1|genus),
           data = milk_fat)
summary(m1)
anova(m0,m1)
