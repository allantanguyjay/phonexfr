#' Algorithme Phonex
#'
#' Calcule un code phonetique pour des noms de famille francais, d'apres
#' l'algorithme Phonex de Frederic Brouard (1999). Deux noms qui se
#' prononcent de la meme facon recoivent le meme code.
#'
#' @param noms Un vecteur de chaines de caracteres, en majuscules ou
#'   minuscules, avec ou sans accents.
#'
#' @return Un vecteur de codes (lettres minuscules et chiffres de 1 a 5),
#'   de meme longueur que `noms`. Les valeurs `NA` restent `NA`.
#'
#' @details
#' Phonex appartient a la famille des algorithmes Soundex, nee aux
#' Etats-Unis au debut du XXe siecle pour retrouver des personnes dans les
#' recensements malgre les variations d'orthographe. Contrairement a
#' Soundex, Phonex est concu pour le francais.
#'
#' Le code note les sons composes par des chiffres : 1 pour "an", 2 pour
#' "oi" et "oua", 3 pour "ou", 4 pour "in", 5 pour "ch" ; la lettre y note
#' le son "e" ferme ou ouvert.
#'
#' Les noms peuvent etre fournis en majuscules ou en minuscules, avec ou
#' sans accents : comme chez Frederic Brouard, le calcul se fait en minuscules.
#' Les espaces, tirets et apostrophes sont conserves pendant l'application des
#' regles, puis supprimes.
#'
#' @section Point de depart:
#' Les regles et leur ordre suivent l'implementation Delphi publiee par
#' Frederic Brouard, qui differe sur quelques points de sa description en
#' 18 etapes. Le resultat est une chaine de caracteres (par exemple "nort4"
#' pour MARTIN), sans la conversion finale en nombre en base 22 (etapes 17
#' et 18 non appliquees).
#'
#' @section Alignements sur la description de Brouard:
#' \itemize{
#'   \item Le H qui suit un P est conserve, pour que la regle PH -> F
#'     s'applique.
#'   \item GAM devient KAM (le code indique KAM4).
#'   \item La lettre e accent grave devient Y.
#'   \item Seul le S entoure de voyelles devient Z.
#' }
#'
#' @section Modifications:
#' Certaines reprennent des corrections d'implementations anterieures (voir
#' References).
#' \itemize{
#'   \item EIM et AIM deviennent 4 (son "in"), comme EIN et AIN
#'     \[description de Brouard\].
#'   \item AN, AM, EN, EM et IN ne deviennent pas des sons nasaux devant un
#'     second N ou M : ANNE, ETIENNE, FLAMMANT.
#'   \item La regle SC -> S n'est pas appliquee : SC se prononce "sk"
#'     devant A, O, U (PASCAL) ; devant E et I, la regle du C doux le
#'     traite deja.
#'   \item QU -> K est applique avant Q -> K, afin que la regle QU -> K
#'     prevue par Brouard puisse s'appliquer.
#'   \item GU devient K devant un son e ou i (GUERIN, GUINCHARD), ou le U
#'     est muet, et KU ailleurs, ou il se prononce (GUSTAVE). Le code et la
#'     description de Brouard proposent respectivement KU et K.
#'   \item Un E final apres K est supprime, pour rapprocher les finales en
#'     -QUE et en -C : LEVEQUE, LEVEC.
#'   \item Les lettres accentuees absentes de la table de Brouard (n tilde,
#'     a aigu...) sont ramenees a la lettre simple.
#'   \item Les chiffres sont retires, car 1 a 5 servent de codes.
#' }
#'
#' @references
#' Brouard, F. (1999). Soundex, Soundex2, Phonex.
#'   \url{https://sqlpro.developpez.com/cours/soundex/}
#'
#' Pennaforte, C. (2005). Phonex, version Python, poursuivie par F. Carlier.
#'   \url{http://info.univ-lemans.fr/~carlier/recherche/soundex.html}
#'
#' Neuraz, A. phonicsFR. \url{https://github.com/equipe22/phonicsFR}
#'
#' @examples
#' phonex(c("PHILIPPE", "FILIPE"))
#' phonex(c("LEVEQUE", "LEVEC", "Le Brun", "LEBRUN", NA))
#'
#' @export
phonex <- function(noms) {
  uniques <- unique(stats::na.omit(noms))

  # 1. Mise en forme : minuscules, chiffres retires, Y lu comme I, accents
  # ramenes a leur son (e aigu, grave et circonflexe se lisent "y"),
  # autres lettres accentuees ramenees a la lettre simple
  # (stringi n'est applique qu'aux noms non ASCII, pour la vitesse)
  code <- tolower(uniques)
  non_ascii <- !stringi::stri_enc_isascii(code)
  code[non_ascii] <- stringi::stri_trans_nfc(code[non_ascii])
  code <- gsub("[0-9]", "", code)
  code <- remplacer(code, "y", "i")
  code <- chartr(
    "\u00e2\u00e4\u00e0\u00e7\u00eb\u0153\u00ef\u00ee\u00f4\u00f6\u00f9\u00fb\u00fc\u00e9\u00ea\u00e8\u00ff\u00fd",
    "aaaseeiioouuuyyyii",
    code
  )
  code[non_ascii] <- stringi::stri_trans_general(code[non_ascii], "Latin-ASCII")

  # 2. H muet, sauf dans CH, SH et PH [description de Brouard pour PH]
  code <- gsub("^h", "", code)
  code <- gsub("([^csp])h", "\\1", code)
  code <- remplacer(code, "ph", "f")

  # 3. G dur devant les sons "an" et "ain"
  code <- remplacer(code, "gan", "kan")
  code <- remplacer(code, "gain", "kain")
  code <- remplacer(code, "gam", "kam")
  code <- remplacer(code, "gaim", "kaim")

  # 4. AIN, EIN, AIM, EIM devant une voyelle ne sont pas nasaux : "yn"
  code <- gsub("[ae]i[nm]([aeiou])", "yn\\1", code)

  # 5. Sons de trois lettres : "o", "oua" (code 2), "in" (code 4)
  code <- remplacer(code, "eau", "o")
  code <- remplacer(code, "oua", "2")
  code <- remplacer(code, "ein", "4")
  code <- remplacer(code, "ain", "4")
  code <- remplacer(code, "eim", "4")
  code <- remplacer(code, "aim", "4")

  # 6. Son "e" ferme ou ouvert, note "y"
  code <- remplacer(code, "ai", "y")
  code <- remplacer(code, "ei", "y")
  code <- remplacer(code, "er", "yr")
  code <- remplacer(code, "ess", "yss")
  code <- remplacer(code, "et", "yt")
  code <- remplacer(code, "ez", "yz")

  # 7. Sons nasaux "an" (code 1) et "in" (code 4), sauf devant une voyelle,
  # un son code de 1 a 4, ou un second N ou M
  code <- remplacer_sauf_devant(code, "an", "1", "n")
  code <- remplacer_sauf_devant(code, "am", "1", "m")
  code <- remplacer_sauf_devant(code, "en", "1", "n")
  code <- remplacer_sauf_devant(code, "em", "1", "m")
  code <- remplacer_sauf_devant(code, "in", "4", "n")

  # 8. SCH (code 5), puis S entre deux voyelles lu Z
  code <- remplacer(code, "sch", "5")
  code <- gsub("(?<=[aeiouy1234])s(?=[aeiouy1234])", "z", code, perl = TRUE)

  # 9. Voyelles composees et son "ch" (code 5)
  # SC -> S non applique
  code <- remplacer(code, "oe", "e")
  code <- remplacer(code, "eu", "e")
  code <- remplacer(code, "au", "o")
  code <- remplacer(code, "oi", "2")
  code <- remplacer(code, "oy", "2")
  code <- remplacer(code, "ou", "3")
  code <- remplacer(code, "ch", "5")
  code <- remplacer(code, "sh", "5")
  code <- remplacer(code, "ss", "s")

  # 10. C doux devant E et I
  code <- remplacer(code, "ce", "se")
  code <- remplacer(code, "ci", "si")

  # 11. Son "k" : C, Q et G durs
  # QU avant Q ; GU lu K devant un son e ou i (e, i, y, 4), KU ailleurs
  code <- remplacer(code, "c", "k")
  code <- remplacer(code, "qu", "k")
  code <- remplacer(code, "q", "k")
  code <- remplacer(code, "ga", "ka")
  code <- remplacer(code, "go", "ko")
  code <- gsub("gu(?=[eiy4])", "k", code, perl = TRUE)
  code <- remplacer(code, "gu", "ku")
  code <- remplacer(code, "gy", "ky")
  code <- remplacer(code, "g2", "k2")
  code <- remplacer(code, "g1", "k1")
  code <- remplacer(code, "g3", "k3")

  # 12. Lettres proches ramenees a une seule
  code <- remplacer(code, "a", "o")
  code <- remplacer(code, "d", "t")
  code <- remplacer(code, "p", "t")
  code <- remplacer(code, "j", "g")
  code <- remplacer(code, "b", "f")
  code <- remplacer(code, "v", "f")
  code <- remplacer(code, "m", "n")

  # 13. Lettres doublees, finale muette (T, X, S, Z), KE final lu K,
  # puis caracteres hors du code (espaces, tirets, apostrophes...)
  code <- gsub("(.)\\1+", "\\1", code)
  code <- gsub("[txsz]$", "", code)
  code <- gsub("ke$", "k", code)
  code <- gsub("[^12345efghiklnorstuwxyz]", "", code)

  code[match(noms, uniques)]
}

# Remplace motif par remplacement tant que motif est present
# gsub("ss", "s", "sss") donne "ss", alors que remplacer("sss", "ss", "s") donne "s"
remplacer <- function(x, motif, remplacement) {
  repeat {
    reste <- grepl(motif, x, fixed = TRUE)
    if (!any(reste)) return(x)
    x[reste] <- gsub(motif, remplacement, x[reste], fixed = TRUE)
  }
}

# Remplace motif par remplacement, sauf quand motif est suivi d'une voyelle
# d'un son deja code (1 a 4) ou de la lettre d'exception
# "an" et exception "n" : JEAN -> "je1"
# ANATOLE (voyelle apres AN) et ANNE (N apres AN) restent inchanges
remplacer_sauf_devant <- function(x, motif, remplacement, exception = "") {
  regle <- paste0(motif, "(?![aeiouy1234", exception, "])")
  gsub(regle, remplacement, x, perl = TRUE)
}
