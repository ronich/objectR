# 4 srodowiska zwizane z funkcjami:
# enclosing - tutaj funkcja jest tworzona- zawsze jest jedno. tutaj funkcja przeszukuje zmienne
# binding - tutaj funkcja jest wiazana. tutaj funkcja przypisana jest do swojej nazwy
# execution - tutaj funkcja jest wykonywana, tworzone za kazdym razem unikalne (podpiete pod enclosing)
# calling - z tego jest wywolywana funkcja

install.packages("pryr")
require(pryr)

where("where") # pokazuje srodowisko binding ("package:pryr")
environment(where) # pokazuje inne srodowisko (enclosing), namespace:pryr

# w R funkcje nie maja nazw. Nazwa funkcji to de facto nazwa zmiennej do ktorej przypisana jest funkcja

# Przyklad
rm(list=ls())
ne <- new.env()
attr(ne, "name") <- "ne"
environmentName(ne)
ne$ff <- function(x){x^2}
ff(2) # nie dziala bo zdefiniowalismy funkcje w srodowisku pod global, a wywolujemy z global (on nie szuka w dol)
ne$ff(2) # tutaj dziala bo wywolujemy w srodiwsku ne

ls.str(envir = ne)

eval(quote(environment(ff)), envir = ne) # funkcja jest tworzona w srodowisku global(?)
eval(quote(where("ff")), envir = ne) # funkcja jest zwiazana w srodowisku ne

# Przyklad 2
rm(list=ls())
ne <- new.env()
attr(ne, "name") <- "ne"

x <- 0
ne$x <- 10

eval(quote(f <- function(){print(x)}), envir = ne) # definiuje i wiaze(!) funkcje w srodowisku ne
f()
ne$f() # wywolane w globalu, ale bierze zmiennax ktora jest w srodowisku ne. dlatego, ze funkcja przeszukiwac w gore zaczyna od
       # srodowiska enclosing, a nie od srodowiska w ktorym jest wywolywana!
rm(x, envir = ne)
ne$f()

# Przylklad
rm(list=ls())

ne <- new.env()
attr(ne, "name") <- "ne"

ne$x <- 10
x <- 5

f <- function(){
  print(x)
}

where(f)
environment("f")

f() # zwroci 5, bo jest enclosed w global

environment(f) <- ne # zmieniamy srodowisko (enclosing), w ktorym jest funkcja. ale onajest dalej zwiazana w srodowisku globalnym
environment(f)
where("f")

f()

rm(list=ls())

# Przyklad


g <- function(x) {
  if ( !exists("a", inherits = F)) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}

g(4523)
g(45231)

a <- 5
g(3)

g <- function(x) {
  if ( !exists("a", inherits = T)) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
g(3)
# ten eksperyment pokazuje ze srodowisko execution jest za kazdym razem tworzone na nowo

rm(list=ls())

# Przyklad

ne <- new.env()
attr(ne, "name") <- "ne"

# funkcja g ma zczytywac swoje srodowisko i do ktorego jest podpiete
g <- function(){
  exe <- environment()
  exeParent <- parent.env(exe)
  list(exe, exeParent)
}

g() # otrzymujemy srodowisko execution, ktore jest podpiete pod global env

environment(g) <- ne

g() # otrzymujemy srodowisko execution, ktore jest podpiete pod ne

# jak zmienic binding environment
ne$g2 <- g
ne$g2()
g()
# enclosing environment jest to samo
where("g")
eval(quote(where("g2")), envir= ne)
rm(g)
# w tym momencie jest juz tylko jedno binding environment. zbindowana z nazwa g2 w srodowisku ne

rm(list=ls())

# Przyklad

ne <- new.env()
attr(ne, "name") <- "ne"

ne$x <- 123
g <- function(){
  print(x)
}
g()

environment(g) <- ne

g()

# co jest wywalimy srodowisko enclosing dla funkcji g?

rm(ne)

g() # zwraca wartosc ze srodowiska enclosing, mimo tego ze wyrzucilismy ne
environment(g)

mmm <- environment(g)

mmm$x <- 555
g()

typeof(g) # funkcja to jest tak naprawde closure (closure = obiekt + srodowisko enclosure)

rm(list=ls())

# Przyklad

createMultiply <- function(x) {
  y <- 100
  function(y){ x * y} 
}

f1 <- createMultiply(2)
f1(5)
f2 <- createMultiply(5)
f2(5)

q1 <- environment(f1)
q1
q1$y <- 500

q2 <- environment(f2)
q2
q1$y
q2$y

# przy definiowaniu funkcji, ktora tworzy funkcji nalezy miec na uwadze to, ze dla wszystkich tak utworzonych funkcji enclosing
# environemt jest rozne, bo nowe funkcje sa tworzone w jednorazowych, volatile execution environments funkcji createMultiply

rm(list=ls())

# Przyklad

createFunction <- function(){
  x <- 2
  function(y){ x* y}
}

f1 <- createFunction()
f1(4)

q1 <- environment(f1)
q1$x <- 5
f1(4)

rm(list=ls())

# Przyklad (srodowisko calling - srodowisko, z ktorego sie wywoluje funkcje)
# funkcja parentFrame zwraca srodowisko calling

g <- function(){
  print(parent.frame())
}

environment(g) # enclosing
where("g") # binding
g() # calling

ne1 <- new.env()
attr(ne1, "name") <- "ne1"
ne2 <- new.env(parent = ne1)
attr(ne2, "name") <- "ne2"

ne1$f <- function(){
  e <- environment()
  list(
    `execution` = e,
    `enclosing` = parent.env(e),
    `calling` = parent.frame(),
    `binding`= where("f", env=ne2)
  )
}

environment(ne1$f) <- ne2

# binding env. 

ne1$f()
# execution: losowy hash
# enclosing: "stworzone" w ne2
# calling: wywolane z global
# binding: zwiazane w ne1, tam jest zdefiniowana zmienna f




# Zadanie: Sprobowac napisac funkcje, ktora pamieta ile razy sie wywolala

# Srodowiska maja reference semantics, 
# Zadanie: napisac funkcje, ktora bierze jako argument a, a w srodku zmienia a$x <- costam
# Sprawdzic jaka jest roznica miedzy daniem listy jako argument a daniem srodowiska (w liscie nie zmieni sie zawartosc
# tak naprawde, a w srodowisku mozna zmienic in place)

# pakiet datatable implementuje to samo co dataframe ale inplace bo jest zrobiony za pomoca srodowisk













