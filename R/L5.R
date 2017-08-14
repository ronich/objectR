# Dokonczenie L4.R

## Tworzenie prostej klasy S4
DataStruct<- setClass( "DataStruct", slots = list ( values  = "data.frame",info= "character"))

## Jak wygladakonstruktor

require(pryr)
ftype(DataStruct)
typeof(DataStruct)

# niedokonczone
temp <- function(object){
  cat("Object of calss: ", class(object), "\n")
  cat("number of ebservations", as.character(dim(object@values)[1]), "\n")
  

  
  e <- environment()
  print(list("Execution" = e,
             "enclosing" = parent.env(e)
  ))

}

setMethod("show", "DataStruct", temp)
  
## Tworzenie generica i gettera
setGeneric( "values", function( object,...){ standardGeneric("values")})
temp <-function( object,...){
  e <- environment()
  eParent <- parent.env(e)
  eCalling <- parent.frame()
  list( "execution" = e, "eclosing" = eParent, "calling"= eCalling)
}

## Tworzenie obiektu
x <- DataStruct( values = data.frame( pos = 1:3, val=rnorm(3)), info = "Exemplary data.")

ftype("values")
isGeneric("values")
typeof(values)
values

## Gdzie jest generic w sensie srodowiska
where("show")
temp <- list( environment(show))
while (!identical(emptyenv(), temp[[length(temp)]])){
  temp <- c( temp, list( parent.env( temp[[length( temp)]])))
}
temp[1:5]

## Standardowy generic show ma ecnclosing env, ktory ejst losowym rodiwskiem
## Jest to srodowisko execution dla funkcji setGeneric ktora tworzy generica
## Jej enclosing env jest srodowisko namespace:mehods
environment(setGeneric)
identical( environment(setGeneric), temp[[2]])

# # Gdzie jest metoda show podpieta pod tego generica
x

# niedokonczone
temp <- function(object){
  cat("Object of calss: ", class(object), "\n")
  cat("number of ebservations", as.character(dim(object@values)[1]), "\n")
  cat("number of features", as.character(dim(object@values)[2]), "\n")
  cat("Info: ", object@info, "\n")
  
  e <- environment()
  print(list("Execution" = e,
             "enclosing" = parent.env(e),
             "calling" = ""))
               
}



##################################

# Model R5/RC
## Klasy RC (R5) sa oparte o klay s4 oraz dodatkowe srodowisko. Scislej, obiekt RC to klasa S4 oraz dopiete do tego srodowisko
## ktore jest przechoywane w liscie atrybutow

## Wszystkie klasy R5 dziedzicza z envRefClass, w szczegolnosci dziedziczone sa ewne pola standardowe dla wszystkch klas)
## oraz metody wspolne dla wszystkich kla

## Przyklad / definiowanie podtawowej klasy

require(pryr)

a <- setRefClass( "student")
otype(a)
ftype("a")
typeof(a)
str(a)

## Definicja zawierajaca pola/ bez metod/ zwraca konstruktor

student <- setRefClass( "student",
                        fields = list(name = "character", age= "numeric", id= "numeric")
                        )

## Jak wyglada generator
student

## Przykladowe wykorzystanie predfefiniowanejmetody getreflclass()

student$getRefClass()

## Tworzenie przykladowegoobiektu
s1 <- student( name = "Mike", age = 20, id = 52034)
otype(s1)
ftype("s1")
typeof(s1) # Typ bazowy to s4
attributes(s1)

# widc atrybut .xData, jego zawratoscia jest srodowisko, w ktorym jest wszystko co jest w tej klasie
ls(envir = attributes(s1)$.xData)

e <- attributes(s1)$.xData
e$name
e$age
e$id

# ostatnia rzecz to funkcja, ktora w zaleznosci od tego czy podane sa arguemntu czy nie wola albo funkcje getClass bezposrednio
# z pakietu methods albo zwraca definicje klasy
e$getClass()

### co tak naprawde zwaiera to srodowisko?
ls(envir = e, all.names = T)

### jak widac zqpisane sa gettery / settery a poza tym zmienna .refClassDef

e$.refClassDef
typeof(e$.refClassDef)
attributes(e$.refClassDef)
class(e$.refClassDef)

### metoda gtClass ma enclosing env ustawione na srodowisko zapisane w atrybutach klsay i daltego widzi zmienna .refClassDef
identical( environment(e$getClass), e)

# jaki jest parnet env dla srodowiska w ktorym trzymane sa dane
parent.env(e)

# przyklad / r5 sa mutowalne
## w przypadku klasu r5 istienieje jedynie jedenobiekt ktory jest mutowalny. wynik to z tego ze wszystkie elementy przechowywane
## sa w srodowisku

a <- list("x" = 1, "y" = 2)
b <- a # tutaj jest tworzona kopia

b$x <- 100

identical(a, b)

str(a)
str(b)
#b zmienione, a nie

str(s1)
s2 <- s1 #tutaj nie tworzy sie kopia
identical(s2, s1)

s2$name <- "Mary"
s2$name
s1$name
identical(s2, s1)

s3 <-s2$copy()

s2$name
s2$name <- "Anne"
s2$name
s3$name

# Metody (internal)

## przyklad / jak wygladaja standardwoe dziedziczone metody

student

#jak wyglada kod
student$copy
student$field

student$help()
student$help("field")
student$help("usingMethods")

#tworzenie metod

student <- setRefClass( "student",
                        fields = list(name = "character", age= "numeric", id= "numeric")
)


student$methods(
  ageInc = function(x=1){ age <<- age +x},
  ageDec = function(x=1){ age <<- age -x}
  )

s1 <- student( name = "John", age = 20, id = 45982)

s1$ageInc()
s1

# aby dokladnie sprawdzic gdzie jest enclosing env dla metod w modelu r5 zbudujemy 

temp <- function(){
  envs <









