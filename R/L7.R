# Podstawowe komponenty funkcji
# kazda funkcja ma 3 komponenty
# cialo body()
# formals / formals()
# enclosing env / environment

# Przyklad

f <- function( x = "John"){
  print(x)
}

body(f)
typeof(f)
typeof(body(f))
formals(f)
typeof(formals(f))
environment(f)
attributes(f)

# Przyklad wykorzystanie do wstecznej modyfikacji funkcji

f <- function(){}

typeof(f)

formals(f) <- alist(x="Mary")
body(f) <- quote( print(x))

body(f)
formals(f)

attributes(f)
f()
f("John")


c
formals(c)
body(c)

# przyklad: liczba wszystkich funkcji primitive

obs <- mget( ls("package:base"), inherit = TRUE)
typeof(obs)
obs
fs <- Filter( f = is.function, x =obs)
length(fs) # liczba wszystkich funkcji

fsPrimitive <- Filter( f= function(x) is.primitive(x) & is.function(x), x = obs)
length(fsPrimitive)

fsNotPrimitive <- Filter( f= function(x) !is.primitive(x) & is.function(x), x = obs)
length(fsNotPrimitive)

# liczb aargumentow ktore przyjmuja funkcje (... - jeden argument)
w <- unlist(lapply(X = fsNotPrimitive, FUN = function(x) length( formals( x))))
names(w) <- NULL
wCounts <- table(w)
x <- as.integer(names(wCounts))
plot(x, wCounts, type='h',lwd=10, col='magenta',
     main = 'Number of arguments')
     
# Przyklad - ast / everything is a call, wszystko jest funkcja
# ast - abstract syntax t
require(pryr)
ast(2+2)
ast(mean( rnorm(100)))
ast((2+2))

# przedefiniowanie funkcji `(`
`(` <- function(x) x+1
(2+1)
rm(`(`)
(2+1)

sapply(1:10, `+`, 3)

# wywolanie funkcji na argumentach
# do.call - trzeba podac nazwe funkcji i liste argumentow.
# call - sluzy to tworzenia wywolan

do.call(mean, list(1:10, na.rm=T))

# lazy computataions - zostaje oblicone dopierojak zostaje wykorzystane

f1<- function(a, b){
  print(a)
  print(nchar(b))
}
f1("ala", print("John")) # john jest drukowany bo jest wywolywany w nchar(b)

f2<- function(a, b){
  print(a)
}
f2("ala", print("John")) # john w ogole nie jest obliczany

# dziwne rzeczy
# argumenty definiowane przez inne argumenty
f1 <- function(a = 1, b = 2*a){
  c(a, b)
}

f1(2)
f1(2,5)

# argumenty definiowane przez zmienne lokalne
f2 <- function( a=1, b=2*x){
  x <- 2*a
  c(a,x,b)
}
f2(3)
f2(3, 6)
# arugmenty wewnatrz funkcji sa wywolywane w srodowisku execution,nie w srodowisku calling


# wymuszanie obliczen - force
f <- function( x=stop("Evaluated")){
  123
}

f() # argumenty nie zostal obliczeony

# wymuszanie obliczen
f <- function( x=stop("Evaluated")){
  force(x)
  123
}

# function factory
f <- function(x){
  function(y) x + y
}

g2 <- f(2)
g4 <- f(4)

g2(10)
g4(10)

# stworzenie listy funkcji
q1 <- lapply(1:10, f)
q1[[10]](10)
q1[[1]](10)
for(k in 1:length(q1)) print( q1[[k]](10))

# gdzie sa obliczane argumenty
# argumetny domslne sa oblcizene wewnatrz srodowiska exceution dla funckcji
# dlatego jestli podawana jest wartosc explicite to moze ona by cinna niz domyslan

f <- function( x = ls()){
  a <- 123
  x
}
rm(x)
x
f() # wartosc domyslna jest wyliczana w execution environment
f( ls()) # wartosc podana jest wyliczana w calling environment
# to rozroznienie jest kluczowe w przypadku obliczen na jedyzku i przy przeczhwytywaniu calli (cel w tym zeby wiedziec gdzie
# obliczane sa formalsy, ktore tez zostana przechwycane)

ne <- new.env(parent = globalenv())
x <- 10
ne$x <- 1

f <- function(q){
  print(q)
}
environment(f) <- ne # zmieniam enclosing na ne

f(x)
eval(quote(f(x)), envir = ne)

#binding - tam gdzie szukasz funkcji
#enclosing -tam gdzie funkcja szuka zmiennych
#calling - tam gdzie oblicznae sa jawnie podane arguemnty

# !argumenty podane jawnie sa obliczane w srodowisku calling (a nie enclosing)

# promises

# promise to jest obiekt typu language - jest to nie obliczone wyrazenie. kazda obietynica zawiera dwa elementy:
# - wyrazenie ktore bedzie obliczone jest dojdzie do olibczenia / dostaniesz sie przez funkcje substitute()
# - srodowisko,gdzie wyrazenie zostalo stworzone i gdzie powinno byc wyliczone jezeli do niego dojdzie
# info o obietnicach mozna uzyskac za pomoca pryr::promise_info()

# przyklady z ksiazki wickhama
x <- NULL
x > 0
if (!is.null(x) && x>0){
  print("Works?")
}
# to wyrazenie nie zwraca bledu dlatego ze drugie wyraznie nie jest obliczone bo pierwsze zwraca FALSE
if (is.null(x) && x>0){
  print("Works?")
}
# tu jest blad
# zatem wyraznie x >0 jest przykladem promise- nie jest obliczany dopoki nie jest potrzebny

f <- function(a,b){
  c( missing(a),missing(b))
}

f( a= 1, b= 2)
f( a= 1)
f()

#to sie przydaje jezeli chcemy jako wartosc defaultowa wykorzystac cos co wymaga wielu linii kodu do wyliczenia
# wtedy chcemy miec pewnosc ze zostanie policzona tylko jesli arugment jest missing

#argument ...




# testowanie funkcji last
x <- 1:5
address(x) #hash gdzie to siedzi w pamieci
x[0] <- 100
address(x) # adres sie zmienil - tak narpawde stworzono kopie



# wartosci zwracane przez funkcje
# funkcja moze zwrocic jedynie jeden obiekt
# funkcja moze zwracac wartosci niewidoczenpoprzez wykorzystanie invisible(). to jest wygodne bo pozwala laczyc wywolania metod
# invisible(self), przykladem takiej funkcji jest <-. to pozwala na laczenie wywolan

a <-b <- c<- d<- 10
a
b
c
d

# on exit - wygodne sprzatanie
# mozna ustawic wyrazenie, ktore bedzie zawsze wykonywane tuz przed zakonczeniem dzialania funkcji
# ladne wykorzystanie to zmiana katalogu roboczego

doSth <- function(dir, code){
  wd <- setwd(dir) # zwraca stary katalog roboczy oraz ustawia na nowy katalog roboczy
  on.exit(setwd(wd))
  force(code)
}

getwd()
doSth('/home/bpol0190/Documents', quote(print(1)))
getwd()



# obliczenia na jezyku
# wprowadzenie / przechwytywanie wyrazen
# podstawowa funkcja ktora pozwala na wykonywanie obliczen an ajezyku ro fnkcaj subtitute. zamiast zbierac wartosci
# argumentow, zbiera kod, ktory je oblicza

f <- function(x){
  substitute( x)
}

f(1:10)
typeof(f(1:10)) # language, a wiec to samo co zwraca funkcja quote

x <- 10
f(x)
eval(f(x))

g <- function(x){
  print( typeof( substitute( x)))
}

g(2) # double (wektor atomowy), substitute na stalych zwraca stale
g(x<-2) # language, substitue na wyrazeniu zwraca obiekt typu language

# druga przydatna funkcja to deparse() - bierze obiekt typu language i zamienia na string
q <- quote( x <- 1:5)
q
deparse(q)
f <- function(s) deparse( substitute( s))
f(x <- 1:10)
f( function(s) s^2)

# polaczenie quote i eval
# obie funkcje znamy, ale pamietac ze drugi argument eval to srodowisko ALBO lista czy ramka danych

# przyklad - proste zastosowanie quote i eval

# obliczenia na liscie
eval(quote(x), list(x = 100))
x

# obliczenia na ramce danych
eval(quote(x), data.frame(x=1:10))

# obliczenia na srodowisku
q <-new.env(parent = globalenv())
q$x<- rnorm(10)
eval(quote(mean(x)), q)
rm(q)

#implementacja subset
subset2 <- function(data, cond){
  condCall <- substitute( cond)
  ind <- eval(condCall, data)
  data[ind,]
}

data <- data.frame(pos = 1:10, val = rnorm(10))
data
subset2(data, cond = val>0)
subset2(data, cond = val>0 & val<1)

# przyklad ze nalezy zawsze patrzec na to co i gdzie sie oblicza. typowy problem z przeciekaniem danych od gory

d <-data.frame(a= 1:5, b=5:1, c=c(5,3,1,4,1))
d

subset2 <- function(data, cond){
  condCall <- substitute( cond)
  print(condCall)
  print( ls())
  ind <- eval(condCall, data)
  print(ind)
  data[ind,]
}

y<- 4
data<- 4
cond <- 4
condCall <- 4

subset2( d, a==4)#ok
subset2( d, a==y)#ok, y zbierane ze sciezki

subset2(d, a==data)# fails, wyrazenie a== data jest obliczane w ramce danych gdzie nie ma zmiennej data
#Po przejsciu o krokw gore wchodzimy do srodowiska executin, gdzie jest taka zmienna i jest to ramka danych
#porownujemy wektor a do calej ramki danych data, dostajemy spaghetti

subset2(d, a==cond)
# zmienna cond nie istnieje w ramce danych, idziem w gore, w execution env jest zmienna cond

subset2(d, a==condCall)
# nie ma w ramce danych, jest w execution, ale jest to obiekt typu language wiec to nie moze sotac porownane

# aby tego uniknac nalezy ustawic ostatni argument eval() na calling env dla tej funkcji

subset3 <- function(data, cond){
  condCall <- substitute( cond)
  ind <- eval(condCall, data, parent.frame())
  data[ind,]
}

subset3( d, a==4)
subset3( d, a==y)
subset3(d, a==data)
subset3(d, a==cond)
subset3(d, a==condCall)

# wolanie z innej funkcji
rm(list=ls())

d <-data.frame(a= 1:5, b=5:1, c=c(5,3,1,4,1))

scramble <- function(x)x[sample(nrow(x)), ] 

scramble(d)

subscramble <- function(x, cond) {
  scramble(subset3(x,cond)) 
}

subscramble(d, cond = a>3)
traceback() # wywraca sie na evalu
# w zmiennej condCall znajduje sie boiekt language
# zaweirajacy cond. w momencie, gdy chcemy olbiczyc evala na condCall
# otrzymujemy cond, ktorego nie ma w data, wtedy przechodzimy od
# callign env, ktorym jest execution funkcji wolajace
# tam stramy sie obliczcyc cond ale tam juz nie ma a i otrzymujemy blad
# jezeli dodamy a do globalenv() to technicznei wszystko sie policzy ale otrzymamy blad logiczny

# w subset jest quote na quote i daltego(?) sie wywala
# przyklad
x
q <- quote(quote(x <-2))
eval(q)
x #zdjelismy tylko jednego quote
eval( eval(q))
x # teraz dopiero dobralismy sie do x

# escape catch

# cos ponizej jest nie tak - sprawdzic w ksiazce wickhama
subset3Q <- function(data, cond){
  ind <- eval(cond, data, parent.frame())
  data[ind,]
}

subset3 <- function(data, cond){
  subset3Q(data, substitute(cond))
}

subscramble <- function(x, cond) {
  condCall <- substitute( cond)
  scramble(subset3Q(x,cond)) 
}

subscramble(d, cond = a>3)
