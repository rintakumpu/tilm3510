# Asetetaan työhakemisto

wd <- "C:/Users/Lasse Rintakumpu/Documents/GitHub/tilm3510"
wd <- "D:/Dropbox/Edu/Statistics/Peruskurssi B/Harjoitustyö/"
setwd(wd)

# TILM3510 -harjoitustyö, R-koodi
# Lasse Rintakumpu, 63555
# 19.1.2013

# Funktio kirjastojen asentamiselle / lataamiselle

lataa_kirjasto <- function(kirjasto) {
if(kirjasto %in% rownames(installed.packages()) == FALSE)
  {install.packages(kirjasto)} 
library(kirjasto, character.only = TRUE)
}

# Ladataan/asennetaan käytetyt kirjastot

lapply(c("moments","car", "alr3"), lataa_kirjasto)

# Ladataan havaintoaineisto

talous <- read.csv("https://raw.github.com/rintakumpu/tilm3510/master/talous.csv", sep=";", dec=",");

# Ollaan kiinnostuttu nettotuloista (1.) sekä taloudellisesta tilanteesta (2.)

miehet <- subset(talous, supu==1)
naiset <- subset(talous, supu==2)

# Erotetaan havaintoaineistosta sukupuolittain nettotulot

nettotulot_miehet <- miehet$oma_tulo
nettotulot_naiset <- naiset$oma_tulo

# sekä tyytyväisyys taloudelliseen tilanteeseen
# Ryhmitellään tyytyvaisyys niin, 
# että 1--2 => Tyytyväinen
# 3--4 => Tyytymätön

tyytyvaisyys <- matrix(nrow=2,ncol=2)
rownames(tyytyvaisyys) <- c("Mies", "Nainen")
colnames(tyytyvaisyys) <- c("Tyytyväinen", "Tyytymätön")
tyytyvaisyys[1,1] <- sum(miehet$taltyyt==1 | miehet$taltyyt==2)
tyytyvaisyys[1,2] <- sum(miehet$taltyyt==3 | miehet$taltyyt==4)
tyytyvaisyys[2,1] <- sum(naiset$taltyyt==1 | naiset$taltyyt==2)
tyytyvaisyys[2,2] <- sum(naiset$taltyyt==3 | naiset$taltyyt==4)

#################
# 1. Nettotulot #
#################

# Käytetään normaalikvantiilikuvaajaa sekä laatikko-janakuviota

boxplot(nettotulot_miehet, nettotulot_naiset, col = c("skyblue", "pink"), xaxl="n", ylab = "€/kk")
axis(1, at = c(1,2), labels = c("Miehet", "Naiset"))

# Maksimoidaan kuvaajien luotettavuus tallentamalla pdf-muodossa
# http://xkcd.com/1301/

pdf('boxplot_tulot.pdf')
dev.off()

skewness(nettotulot_miehet)
kurtosis(nettotulot_miehet)
# Miehet: g1, 0.3483119, g2: 2.343662
skewness(nettotulot_naiset)
kurtosis(nettotulot_naiset)
# Naiset: g2, 0.4554618, g2: 2.343662

# Haetaan potenssimuunnosta paremman normaalijakautuneisuuden löytämiseksi

ptm <- powerTransform(nettotulot_miehet)
# Estimated transformation parameters 
# nettotulot_miehet 
# 0.4683207 

ptn <- powerTransform(nettotulot_naiset)
# Estimated transformation parameters 
# nettotulot_naiset 
# 0.4279722 

# Sovelletaan molempiin aineistoihin neliöjuurimuunnosta

nettotulot_m_muunnettu <- nettotulot_miehet^0.5
nettotulot_n_muunnettu <- nettotulot_naiset^0.5

skewness(nettotulot_m_muunnettu)
kurtosis(nettotulot_m_muunnettu)
# Miehet: g1: -0.01, g2: 2.35
skewness(nettotulot_n_muunnettu)
kurtosis(nettotulot_n_muunnettu)
# Naiset: g1: 0.005, g2: 2.58

qqnorm(nettotulot_m_muunnettu, main = "Miehet, muunnettu ^0.5")
pdf('qqnorm_m_muunnettu.pdf')
dev.off()
qqnorm(nettotulot_n_muunnettu, main = "Naiset, muunnettu ^0.5")
pdf('qqnorm_n_muunnettu.pdf')
dev.off()

# Otoskeskiarvot viittaavat tuloeroihin sukupuolten välillä.

nettotulot_m_viiva <- mean(nettotulot_miehet) # 5118.921
nettotulot_n_viiva <- mean(nettotulot_naiset) # 4391.956

# Testataan onko miesten ja naisten nettotuloissa tilastollista eroa.

# Valitaan nollahypoteesiksi H0: mu_m - mu_n == 0 (populaatioiden odotusarvot samat)
# ja vastahypoteesiksi       Hv: mu_m - mu_n != 0

t.test(nettotulot_m_muunnettu, nettotulot_n_muunnettu, alternative = "two.sided", conf.level = 0.95)

# data:  nettotulot_m_muunnettu and nettotulot_n_muunnettu
# t = 2.7782, df = 174.866, p-value = 0.006064
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  1.480589 8.744629
# sample estimates:
#   mean of x mean of y 
# 70.36786  65.25525 

# => Hylätään nollahypoteesi riskitasolla 0.05. Testin p-arvosta näemme,
# että pienen riskitaso, jolla testi voitaisiin hylätä on 0.006064.

###############################################
# 2. Tyytyväisyys taloudelliseen tilanteeseen #
###############################################

# Käytetään aineiston kuvailuun histogrammia sekä ryhmäpylväskuviota.

tyytyvaisyys_m <- hist(miehet$taltyyt, 0:4, breaks = c(0.5,1.5,2.5,3.5,4.5))
plot(tyytyvaisyys_m,
     xlab="Tyytyväisyys\n(1: Erittäin tyytyväinen - 4: Erittäin tyytymätön)",
     ylab="f",
     main="Miesten tyytyväisyys taloudelliseen tilanteeseen",
     col="skyblue")

pdf('tyytyvaisyys_mies.pdf')
dev.off()

tyytyvaisyys_n <- hist(naiset$taltyyt, 0:4, breaks = c(0.5,1.5,2.5,3.5,4.5))
plot(tyytyvaisyys_n,
     xlab="Tyytyväisyys\n(1: Erittäin tyytyväinen - 4: Erittäin tyytymätön)",
     ylab="f",
     main="Naisten tyytyväisyys taloudelliseen tilanteeseen",
     col="pink", ylim = c(0,40))

pdf('tyytyvaisyys_nainen.pdf')
dev.off()

tyytyvaisyys_bp <- barplot(tyytyvaisyys, beside = TRUE, 
        main=c("Tyytyväisyys taloudelliseen tilanteeseen"),
        col=c("skyblue","pink"), 
        ylab="f", legend.text=c("Miehet","Naiset"), xlim = c(0,10))
text(tyytyvaisyys_bp, 0, round(tyytyvaisyys, 1),cex=1,pos=3) 

pdf('tyytyvaisyys_yhteinen.pdf')
dev.off()

# Lasketaan estimaatit

pm <- tyytyvaisyys[1,1] / sum(tyytyvaisyys[1,]) # 0.5842697
pn <- tyytyvaisyys[2,1] / sum(tyytyvaisyys[2,]) # 0.4395604

# Testataan suhteellisten osuuksien erotusta tasolla 0.05
# Valitaan vastahypoteesiksi tehtävänannon mukaan
# oletus, että miehet ovat naisia tyytyväisempiä.
# H0: pm-pn = 0 / Hv: pm-pn > 0 

prop.test(tyytyvaisyys, alternative = "greater", conf.level=0.95)

# data:  tyytyvaisyys
# X-squared = 3.2138, df = 1, p-value = 0.03651
# alternative hypothesis: greater
# 95 percent confidence interval:
#  0.0123195 1.0000000
# sample estimates:
#  prop 1    prop 2 
# 0.5842697 0.4395604 

# Pienin arvo, jolla nollahypoteesi voitaisiin
# hylätä on 0.03651, hylätään nollahypoteesi tasolla 0.05.