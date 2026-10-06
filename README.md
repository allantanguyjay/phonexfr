
<!-- README.md est genere a partir de README.Rmd : modifiez README.Rmd -->

# phonexfr

**phonexfr** calcule des codes phonétiques pour les noms de famille
français. Il se fonde sur l’algorithme Phonex de Frédéric Brouard
(1999). Deux noms qui se prononcent de la même façon reçoivent le même
code, malgré des orthographes différentes.

Le package a été conçu pour le **linkage de bases de données
historiques**, où un même nom peut être écrit de plusieurs manières
d’une source à l’autre, par erreur de saisie ou du fait de bruit
d’océrisation.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("allantanguyjay/phonexfr")
```

## Exemple

``` r
library(phonexfr)

phonex(c("PHILIPPE", "FILIPE", "DUPONT", "DUPOND", "LEVEQUE", "LEVEC"))
#> [1] "filit" "filit" "tuton" "tuton" "lefek" "lefek"
```

La fonction accepte les noms qu’ils soient en majuscules ou en
minuscules, avec ou sans accents. Elle conserve les valeurs manquantes :

``` r
phonex(c("Le Brun", "LEBRUN", "Hélène", NA))
#> [1] "lefrun" "lefrun" "ylyn"   NA
```

Dans le code, les sons composés sont notés par des chiffres : 1 pour «
an », 2 pour « oi », 3 pour « ou », 4 pour « in », 5 pour « ch » ; la
lettre y note le son « é ».

## Utilisation pour le linkage

Le code phonétique sert typiquement de **clé de blocage** : on ne
compare en détail que les paires de noms qui partagent le même code.

``` r
noms <- data.frame(nom = c("MARTIN", "MARTAIN", "ROUSSEAU", "ROUSSOT", "GUERIN"))
noms$code <- phonex(noms$nom)
noms
#>        nom  code
#> 1   MARTIN nort4
#> 2  MARTAIN nort4
#> 3 ROUSSEAU  r3so
#> 4  ROUSSOT  r3so
#> 5   GUERIN  kyr4
```

## Origine

Les règles suivent l’implémentation de Phonex publiée par Frédéric
Brouard (1999), utilisée avec son accord. Elles sont adaptées aux noms
de famille français, notamment grâce à certaines corrections reprises
d’implémentations antérieures. Le détail de chaque règle et de sa source
est documenté dans l’aide de la fonction : `?phonex`.

## Apports

- Écarts au code de Brouard justifiés et documentés (`?phonex`)
- Nouvelles règles : GU lu K devant e et i, lettres accentuées hors de
  la table de Brouard, chiffres
- Traitement rapide des grandes bases
- Règles couvertes par des tests
- Licence libre (MIT)

## Packages proches

- [phonics](https://cran.r-project.org/package=phonics) propose une
  fonction `phonex()`, mais elle implémente le Phonex **anglais** de
  Lait et Randell (1996), un algorithme différent.
- [phonicsFR](https://github.com/equipe22/phonicsFR) implémente le
  Phonex français à partir de la version Python de Pennaforte. phonexfr
  en reprend certaines corrections.

## Licence

MIT. L’algorithme Phonex est l’œuvre de Frédéric Brouard.

## Références

- Brouard, F. (1999). *Soundex, Soundex2, Phonex*.
  <https://sqlpro.developpez.com/cours/soundex/>
- Pennaforte, C. (2005). *Phonex*, version Python, poursuivie par F.
  Carlier. <http://info.univ-lemans.fr/~carlier/recherche/soundex.html>
- Neuraz, A. *phonicsFR*. <https://github.com/equipe22/phonicsFR>
