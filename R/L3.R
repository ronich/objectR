# Programowanie obiektowe w S3

#Przyklad motywujacy

x <- cumsum(rnorm( 100))

y <- ts(x, start=c(2000, 1), freq= 12)

data(cars)

z<- lm(dist ~ +1 + speed, data= cars)

par(mfrow=c(2,2))
plot(x)
plot(y)
plot(cars)
plot(z)


# generic (function)
# |
# plot

# generic sprawdza jaki obiekt jest przekazywany do funkcji i w zależności od tego wywołuje odpowiednią metodę
# dispatching: wywolanie odpowiedniej metody do odpowiedniego obiektu. dzieje sie na to na podstawie:
# 1) typow bazowych
# 2) klas S3

# chcemy nauczyć się dwóch rzeczy żeby programować obiektowo w S3:
# jak podpinać pod generic'i nasze metody(?)
# jak dodawać obiekty do metod(?)

# np dla plot:
methods(plot)
# ogwiazdkowane rzeczy sa ukryte - to znaczy, że nie da się ich explicite wywołać, np:
plot.lm
# dlatego, ze powyzej globala nie ma bindingu do tego

# Typy bazowe

# Wszystkie typy bazowe w R opisane sa na podstawie struktury w jezyku C
typeof()

x <- 1:5
typeof(x)

x <- list(x=x)
typeof(x)

f <- function(x){print(x)}
typeof(f)

x <- quote( x <- 66)
x
typeof(x)

rm(x)

makeActiveBinding("x", function(){rnorm(1)}, env = parent.frame())
x
typeof(x)

typeof(sum) 


# dispatchować w S3 mozna tylko na podstawie jednego argumentu

# Rozpoznawanie, szukanie i jak to dziala
# jak sprawdzic czy obiekt jest obiektem klasy S3? Nie ma prostej metody sprawdzenia

# Przyklad

data <- data.frame(x=1:10, y=letters[1:10])
data

# pewne przublizenie:
is.object(data) & isS4(data)

require(pryr)

otype(data)
otype(data$x)
otype(data$y)

typeof(letters) # to jest obiekt bazowy character
data$y # to jest factor (factor jest obiektem klasy S3)

data1 <- list(x=1:10, y=letters[1:10])
data1

otype(data1) # lista jest typem bazowym, a data.frame jest obiektem klasy S3

# Przyklad

# jezeli w kodzie funkcji wystepuje UseMethod, to to jest funkcja generic, np. plot uzywa, ale data.frame juz nie
# UseMethod jest de facto funkcja ktora robi dispatching. dwa jego rodzaje:
# 1) jezeli mamy obiekt klasy S3
# 2) na podstawie basetypes - na poziomie jezyka C

sum # czy funkcja sum jest generic?
ftype(sum) # funkcja z pakietu pryr, ktora odpowiada na to pytanie dla funkcji
ftype(plot)
ftype(data.frame)

# w s3 metody nazywaja sie w generic.class
# plot.lm generic= plot, class= lm
# plot.ts
# ale juz np. jak jest data.frame to nie wiadomo. dlatego sugeruje sie nie uzywac kropek w nazwach funkcji

# Przyklad

q = data.frame(time=1:100, values= rnorm(100))
str(q)
head(q)
par(mfrow=c(1,1))
plot(q)
plot.data.frame(q)
graphics::plot.data.frame(q)
# fail, bo data.frame nie jest wyeksportowane do namespace
graphics:::plot.data.frame(q) # rozwaizanie
dev.off()

where("plot.data.frame", env = asNamespace("graphics")) # tutaj plot.data.frame jest zbindowane

eval(quote(plot.data.frame(q)), envir= asNamespace("graphics"))
# wywala sie, bo w tym namespace jest zdefiniowana zmienna q, ktora nie jest data frame
q2323 <- data.frame(time=1:100, values= rnorm(100))
eval(quote(plot.data.frame(q2323)), envir= asNamespace("graphics"))
# to juz pojdzie

# Przyklad

# jak zobaczyc jak metody sa implementowane
methods(plot)
methods(mean)

# definiowanie klasy i tworzenie obiektu

# Przyklad/ tworzenie obiektu klasy matrix
rm(list=ls())
x <- structure(1:4, dim=c(2,2))
class(x)
attributes(x)

y <- list(x=1:4)
attr(y, "class") <- "myClass"

y
inherits(y, "myClass")

# dla obiektow ktore dziedzicza z wielu klas najpierw jest sprawdzane czy jest metoda dla najbardziej specific

# Przyklad
utils::data(anorexia, package =  "MASS")
model <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
             family = gaussian,
             data = anorexia)
class(model) # kazda funkcja wywolana dla tego modelu bedzie najpierw sprawdzala dla glm, a potem dla lm

x <- data.frame(x = 1:4, y = rnorm(4))
attributes(x)
typeof(x)
attr(x, "class") <- NULL
attr(x, "row.names") <- NULL
typeof(x)
class(x)
x
# po znullowaniu tych atrybutow zmienia sie typ obiektu z data.frame na liste
attr(x, "class") <- "data.frame"
attr(x, "row.names") <- 1:5
typeof(x)
class(x)
x
# konstruktor zapewnia, ze obiekt pewnej klasy zostanie w poprawny sposob zainicjalizowany

rm(list=ls())

# Przyklad

# konstruktor
graph <- function(nodes, edges){
  # sprawdzanie poprawnosci argumentow:
  a <- TRUE
  a <- a & is.numeric(nodes)
  a <- a & (class(edges) == "matrix")
  
  # testowanie spojnosci
  temp <- sort(unique(as.vector(edges)))
  for(n in temp){
    a <-a & (n %in% nodes)
  }
  
  if(a){
    ret <- structure(list(nodes=nodes, edges=edges), class="graph")
  } else {
    warning("Edges not consistent with nodes")
  }
  
  ret
}


nodes <- 1:4
edges <- rbind(c(1,2), c(1,3), c(2,4))

g <- graph(nodes, edges)

class(g)
typeof(g)
otype(g)

# Przyklad/ Tworzenie generica
showGraph <- function(g) UseMethod("showGraph")

# definiowanie defaulta
showGraph.default <- function(g){
  print(" --- nie wiem co to jest --- ")
  print(g)
}

# definiowanie metody dla klasy graph
showGraph.graph <- function(g){
  print(" --- obiekt klasy graph --- ")
  print(g$edges) # tutaj wiem, ze g ma slot edges, bo ta metoda zostanie zdispatchowana tylko do obiektu graph, ktory musi miec taki slot
}

methods(showGraph)

showGraph(edges)

showGraph(g)


# pisanie metody i podpiecie jej pod istniejacy generic

plot.graph <- function(g){
  # prymitywnie: kazdemu wierzcholkowi przypiszemy losowy punkt na plaszczyznie i wykreslimy krawedzie
  ell <- length(g$nodes)
  px <- rnorm(ell)
  py <- rnorm(ell)
  
  xrange <- range(px, py)
  yrange <- xrange
  
  plot(0, 0, col="white", xlim=xrange, ylim=yrange, main="Graph")
  points(px, py, pch=20, cex=2)
  
  ell <- dim(g$edges)[1]
  for(k in 1:ell){
    t1 <- match((g$edges[k,1]), g$nodes)
    t2 <- match((g$edges[k,2]), g$nodes)
    lines( px[c(t1, t2)], py[c(t1, t2)])
  }
}

plot(g)

nodes <- 1:5
edges <- rbind(c(1,2), c(1,3), c(2,4), c(3,5), c(3,2), c(4,5))

g <- graph(nodes, edges)
showGraph(g)
plot(g)

# warto zapamietac
otype
ftype
UseMethod
generic.class

# Problemy:
# 1) W S3 nie mozna dispatchowac na wiecej niz jednym argumencie - wybiera metode tylko na podstawie pierwszego argumentu
# 2) Sa tzw. plural generics jak np. operatory matematyczne. Gdybysmy chcieli podpiac metode pod wszystkie operatory matemat.
# to to mozna zrobic w jednym kroku (sa groupy genericsow: math, ops, summary oraz complex)
?groupGeneric























