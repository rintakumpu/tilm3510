# TILM3510 -harjoitustyö, R-koodi
# Lasse Rintakumpu, 63555
# 18.12.2013

# Asetetaan työhakemisto, käytetään graafien tallentamiseen,
# havaintoaineisto haetaan verkon yli

wd <- "C:\Users\rintakumpu\Documents\GitHub\tilm3510"
# Local:
wd <- "D:/Dropbox/Edu/Statistics/Peruskurssi B/Harjoitustyö/"
setwd(wd)

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
# Local
talous <- read.csv("talous.csv", sep=";", dec=",");

# Ollaan kiinnostuttu nettotuloista (1.) sekä taloudellisesta tilanteesta (2.)
# sukupuolen funktiona

miehet <- subset(talous, supu==1)
naiset <- subset(talous, supu==2)

# Erotetaan havaintoaineistosta sukupuolittain nettotulot (relatiiviasteikko)

nettotulot_miehet <- miehet$oma_tulo
nettotulot_naiset <- naiset$oma_tulo

# sekä tyytyväisyys taloudelliseen tilanteeseen (ordinaaliasteikko, 1--4)
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

# Tarkastellaan havaintoaineiston jakaumia graafisesti,
# jonka jälkeen testataan havaintoaineistoissa mahdollisesti esiintyvien
# erojen tilastollista merkitsevyyttä. (Huom n:t mainittava.)

#################
# 1. Nettotulot #
#################

# Käytetään normaalikvantiilikuvaajaa sekä Tukeyn laatikko-jana-diagrammia.

boxplot(nettotulot_miehet)
boxplot(nettotulot_naiset)
qqnorm(nettotulot_miehet)
qqnorm(nettotulot_naiset)

# Yleisesti graafit tukevat oletusta populaation tulojen normaalijakautuneisuudesta,
# kuten myös vinous- ja huipukkuuskertoimet:
# Miehet: g1, 0.3483119, g2: 2.343662
# Naiset: g2, 0.4554618, g2: 2.343662

# Kuitenkin aineistot ovat oikealle vinoja ja niissä havaitaan epäsymmetrisyyttä
# Erityisesti naisten nettotulojen havaintoaineistojen normaalikvantiilikuviossa
# havaitaan "hyppy" välillä 6000-7000€.

# Laatikko-jana-kuvioiden mukaan aineistossa ei kuitenkaan ole selkeästi poikkeavia havaintoja.
# Haetaan potenssimuunnosta paremman normaalijakautuneisuuden löytämiseksi

powerTransform(nettotulot_miehet)
# Estimated transformation parameters 
# nettotulot_miehet 
# 0.4683207 

powerTransform(nettotulot_naiset)
# Estimated transformation parameters 
# nettotulot_naiset 
# 0.4279722 

# => ~ Neliöjuurimuunnos molemmille aineistoille 
# tuottaa paremman normaalijakautuneisuuden.

# Sovelletaan molempiin aineistoihin neliöjuurimuunnosta (mahdollinen, 
# kaikki havainnot positiivisia kokonaislukuja)

nettotulot_m_muunnettu <- nettotulot_miehet^0.5 # g1: -0.01, g2: 2.35
nettotulot_n_muunnettu <- nettotulot_naiset^0.5 # g1: 0.005, g2: 2.58

# Huomataan, että muunnos tasoittaa normaalikvantiilikuvioita.

qqnorm(nettotulot_m_muunnettu)
png('qqnorm_nettotulot_m_muunnettu.png')
dev.off()
qqnorm(nettotulot_n_muunnettu)
png('qqnorm_nettotulot_n_muunnettu.png')
dev.off()

# Jatketaan siis muunnetuilla arvoilla
# Otoskeskiarvot viittaavat tuloeroihin sukupuolten välillä.

nettotulot_m_viiva <- mean(nettotulot_m_muunnettu) # 70.36786
nettotulot_n_viiva <- mean(nettotulot_n_muunnettu) # 65.25525

# Testataan onko miesten ja naisten nettotuloissa tilastollista eroa.

# Valitaan nollahypoteesiksi H0: mu_m - mu_n == 0 (populaatioiden odotusarvot samat)
# ja vastahypoteesiksi       Hv: mu_m - mu_n != 0
#
# Tehdään testi tasolla 0.05, populaation odotusarvo ei ole tunnettu,
# joten käytetään t-testiä.

t.test(nettotulot_m_muunnettu, nettotulot_n_muunnettu, alternative = "two.sided", conf.level = 0.95)

# Saadaan tulokseksi:

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

# => Miesten ja naisten nettopalkoissa on aineiston perusteella tilastollisesti
# merkittävä ero miesten hyväksi, olettaen, että sekä miesten että naisten
# nettotulot ovat jakautuneet normaalisti ja että havainnot ovat toisistaan
# riippumattomia.

###############################################
# 2. Tyytyväisyys taloudelliseen tilanteeseen #
###############################################

# Käytetään aineiston kuvailuun histogrammia sekä ryhmäpylväskuviota.
# Kuvioiden y-akselit prosenttimuodossa, jolloin kuvaajat vastaavat
# piste-estimaatteja.

tyytyvaisyys_m <- hist(miehet$taltyyt, 0:4, breaks = c(0.5,1.5,2.5,3.5,4.5))
tyytyvaisyys_m$density <- (tyytyvaisyys_m$counts / sum(tyytyvaisyys_m$counts)) * 100
plot(tyytyvaisyys_m,
     xlab="Tyytyväisyys\n(1: Erittäin tyytyväinen - 4: Erittäin tyytymätön)",
     ylab="%",
     main="Miesten tyytyväisyys taloudelliseen tilanteeseen",
     col="skyblue", freq = F)

png('tyytyvaisyys_mies.png')
dev.off()

tyytyvaisyys_n <- hist(naiset$taltyyt, 0:4, breaks = c(0.5,1.5,2.5,3.5,4.5))
tyytyvaisyys_n$density <- (tyytyvaisyys_n$counts / sum(tyytyvaisyys_n$counts)) * 100
plot(tyytyvaisyys_n,
     xlab="Tyytyväisyys\n(1: Erittäin tyytyväinen - 4: Erittäin tyytymätön)",
     ylab="%",
     main="Naisten tyytyväisyys taloudelliseen tilanteeseen",
     col="pink", freq = F)

png('tyytyvaisyys_nainen.png')
dev.off()

tyytyvaisyys_percent <- matrix(tyytyvaisyys, 2, 2, dimnames = list(c("Mies","Nainen"), c("Tyytyväinen","Tyytymätön")))
tyytyvaisyys_percent[1,1] <- (tyytyvaisyys[1,1]/sum(tyytyvaisyys[1,]))*100
tyytyvaisyys_percent[1,2] <- (tyytyvaisyys[1,2]/sum(tyytyvaisyys[1,]))*100
tyytyvaisyys_percent[2,1] <- (tyytyvaisyys[2,1]/sum(tyytyvaisyys[2,]))*100
tyytyvaisyys_percent[2,2] <- (tyytyvaisyys[2,2]/sum(tyytyvaisyys[2,]))*100

tyytyvaisyys_bp <- barplot(tyytyvaisyys_percent, beside = TRUE, 
        main=c("Tyytyväisyys taloudelliseen tilanteeseen"),
        col=c("skyblue","pink"), 
        ylab="%", legend.text=c("Miehet","Naiset"), xlim = c(0,10))
text(tyytyvaisyys_bp, 0, round(tyytyvaisyys_percent, 1),cex=1,pos=3) 

# Maksimoidaan kuvaajien luotettavuus tallentamalla se png-muodossa
# rf. http://xkcd.com/1301/

png('tyytyvaisyys_yhteinen.png')
dev.off()

# Kuvailun perusteella miehet vaikuttaisivat naisia tyytyväisemmiltä
# taloudelliseen tilanteeseensa.

# Testataan onko miesten ja naisten tyytyväisyydessä taloudelliseen tilanteeseen
# aineiston pohjalta tilastollista eroa. Käytetään kahden suhteellisen osuuden testausta.

# Testataan suhteellisten osuuksien erotusta tasolla 0.05
# H0: pm-pn = 0
# Hv: pm-pn > 0 # Valitaan vastahypoteesiksi tehtävänannon mukaan
# oletus, että miehet ovat naisia tyytyväisempiä.

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
# hylätä on 0.03651, hylätään nollahypoteesi tasolla 0.05. => Aineiston pohjalta
# miehet vaikuttavat naisia tyytyväisemmilta taloudelliseen tilanteeseensa.

# Oletukset: TODO