# TILM3510 -harjoitustyö, R-koodi
# Lasse Rintakumpu, 63555
# 18.12.2013

# Asetetaan työhakemisto, käytetään graafien tallentamiseen,
# havaintoaineisto haetaan verkon yli

wd <- "C:\Users\rintakumpu\Documents\GitHub\tilm3510"

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
# sukupuolen funktiona

miehet <- subset(talous, supu==1)
naiset <- subset(talous, supu==2)

# Erotetaan havaintoaineistosta sukupuolittain nettotulot (relatiiviasteikko)

nettotulot_miehet <- miehet$oma_tulo
nettotulot_naiset <- naiset$oma_tulo

# sekä tyytyväisyys taloudelliseen tilanteeseen (ordinaaliasteikko, 1--4)

tyytyvaisyys_miehet <- miehet$taltyyt
tyytyvaisyys_naiset <- naiset$taltyyt

# Tarkastellaan havaintoaineiston jakaumia graafisesti,
# jonka jälkeen testataan havaintoaineistoissa mahdollisesti esiintyvien
# erojen tilastollista merkitsevyyttä.

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

# Sovelletaan molempiin aineistoihin neliöjuurimuunnosta

nettotulot_m_muunnettu <- nettotulot_miehet^0.5 # g1: -0.01, g2: 2.35
nettotulot_n_muunnettu <- nettotulot_naiset^0.5 # g1: 0.005, g2: 2.58

# Huomataan, että muunnos tasoittaa normaalikvantiilikuvioita.

boxplot(nettotulot_m_muunnettu)
boxplot(nettotulot_n_muunnettu)
qqnorm(nettotulot_m_muunnettu)
qqnorm(nettotulot_n_muunnettu)

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


# Tallennetaan kuvaajat png-muodossa *** lisäämiseksi. 