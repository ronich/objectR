# Podstawowa definicja klasy, konstruktor
## Podstawowe informacje


### Przykład
### Klasa graph, dwa sloty: vertices (wektor), edges (macierz). Stworzymy obiekt tej klasy (najpierw poprawny, potem nie)

Graph <- setClass("Graph", slots=c(nodes = "numeric", edges = "matrix"))
str(Graph) # formalnie klasa generator, ma pole data, clasName itd.
typeof(Graph)
require(pryr)
ftype(Graph)
Graph

g1 <- Graph()
g1
attributes(g1)
typeof(g1) #dalczego typeof zwraca S4? odp: bo S4 to jest jeden z basetype'ow
otype(g1)

# proba stworzenia poprawnego obiektu
nodes <- 1:10
edges <- cbind( sample(nodes, 10), sample(nodes, 10))
g2 <- Graph( nodes = nodes, edges = edges)
g2
typeof(g2)
otype(g2)
g2

# proba stworzenia niepoprawnego
g3 <- Graph( nodes = 1:4, edges = list( a=3, b=4)) # blad w drugim slocie


# Dostep do slotow w klasie S4

## Odpowiednikiem operatora $ dla slotow jest operator @. Alternatywnie mozna sie dsotac do slotow za pomoca funkcji slot()
## - odpowiednik [[ dla list

## Przyklad

g2
g2@nodes
g2@edges
typeof(g2@nodes)
typeof(g2@edges)
class(g2@edges)

slot(g2, "nodes")
slot(g2, "edges")


## Typ S4 nie jest indeksowalny, nie ma mozliwosci odwolania sie do pierwszego, drugiego slotu. Nie da sie zrobic klas
## ktora ma nienazwane pola

## Unikamy zmuszania uzytkowanika do odwolywania sie do slotow za pomoca '@', to oznacza ze musi znac konstrukcje klasy
## Zamiast tego zdefiniujemy gettery i settery, aby zapewnic calkowita enkapsulacje

# Metody, podstawy

## Informacje ogolne

## Podobnie jak dla S3 metody sa dispatchowane przez genericsy. Dobieraja one metody w zaleznosci od argumentu
## Metoda jest dopinana przez funkcje setMethod()
## nowy generic jest tworzony przez funkcje setGeneric()

## Przyklad / jak sprawdzic czy dany obiekt jest generics

show #standardGeneric dla S4
ftype(show)
isGeneric("show")

## Porownanie z generic dla modelu S3
plot
ftype(plot)
isGeneric("plot") # to nie jest generic dla modelu S4

## Sprawdzenie czy dany generic ma dopiete metody a jak tak to jakie

hasMethod("show")
methods("show")

## Przyklad / dopinanie metod do istniejacego generica

##Definicjametody

showTemp <- function(object){
  cat( "Obiekt klasy ", class(object), "\n", sep = "")
  cat( "Graph with ", length(object@nodes), " nodes.\n", sep = "")
  
  ell <- dim( object@edges)[1]
  if(ell>0){
    cat("Edges ---- \n", sep="")
    ell <- dim( object@edges)[1]
    for(k in 1:(dim(object@edges)[1])){
      cat(as.character(object@edges[k,1]), " -> ", as.character(object@edges[k,2]), "\n", setp="")
    }
    cat("----------\n")
  }
  invisible()
              
}

### Dopiecie do generica

setMethod("show",
          signature = "Graph",
          definition = showTemp
          )

### Wykorzystanie nowej metody
methods(show)
show(g1)
show(g2)
g2

## Przyklad / tworzenie nowegogenerica i dopinanie nowej metody

### Tworzenie generica

setGeneric( "nodes", function( object, ...){standardGeneric("nodes")})

## Dopinanie metody / getter
setMethod("nodes", "Graph", function(object){ object@nodes})

## Wykorzystanie gettera
nodes(g1)
nodes(g2)

setGeneric( "edges", function( object, ...){standardGeneric("edges")})
setMethod("edges", "Graph", function(object){ object@edges})
edges(g1)
edges(g2)

## Przyklad/ subsetting / promocja do generica

### Typowe wywolanie indeksowania

letters[1:3]

#### Pod spodem wywolywana jest funkcja bracket - `[`
`[`(letters, 1:3)


## Dodawanie do funkcji `[` metody dla klasy Graph

temp <- function( x, i, drop = "missing"){
  .nodes <- x@nodes[i] #wyciagam nodey, na ktorych stosuje funkcje bracket
  ell <- dim(x@edges)[1]
  ind <- rep(T, ell) # tworzymy pusty indykator
  for(k in 1:ell){
    # dla wszystkich edges wybieramy tylko te, ktore sie zawieraja w zubsettowanych nodeach
    ind[k] <- prod( as.vector( x@edges[k,]) %in% .nodes) 
  }
  as.logical(ind) -> ind
  .edges <- x@edges[ind,]
  Graph( nodes = .nodes, edges = .edges) # zwracam zsubsettowany obiekt
}

### Dopinanie metody
setMethod("[", "Graph", temp)

g2
nodes(g2)
### Wywolanie subsettera / automatycznie wolana metoda show
g2[1:8]
### tutaj show nie bedzie juz wolana
g5 <- g2[1:8]
nodes(g5)
edges(g5)

## Poprawnosc / walidacja

### Dlaczego chcemy miec dokladniejsza kontrole poprawnosci obiektu

#### powyzszy subsetter nie bierze uwagi ze graf moze nie miec w ogole krawedzi.
#### dlatego chcemy ograniczyc tylko do grafow, ktore maja min. 1 krawedz
#### do tego celu wykorzystujemy funkcje setValidity() oraz validObject() - sprawdza czy object jest valid na podst. setValidity()

#### definicja metody

temp <- function(object){
  val <- T
  warn <- NULL
  if ( !nrow( edges( object)) > 0){
    val <- F
    warn <- "Brak zdefiniowanych krawedzi"
  }
  if( nrow( edges( object)) > 0){
    if( prod( as.vector( edges(object)) %in% nodes(object)) != 1){
      val <-FALSE
      warn <- "Krawedzie pomiedzy nieistniejacymi wierzcholkami"
    }
  }
  if(val){
    TRUE
  } else {
    warn
  }
}

#### Przypiecie metody
setValidity( "Graph", temp)
  
### spradzenie czy istniejace grafy sa dobre
validObject(g2)

### Dlaczego nie chcemy bezposrednio uzywac @
#### Tworzenie poprawnego obiektu
tnodes <- 1:10
tedges <- cbind(sample(tnodes, 10), sample(tnodes, 10))

g7 <- Graph(nodes = tnodes, edges = tedges)

g7

#### Proba zmiany wartosci slotow bezposrednio
g7@nodes <- "ala"
g7@edges <- 1:6

#### delikatna zmiana
g7@edges <- matrix(20:23, 2, 2) # blednie stworzony obiekt
validObject(g7)
  
#### Wazne! @ nie odpala pod spodem validatora!
#### Po to tworzymy settery
#### settery podlegaja pewnym regulom:
##### musza wygladac tak slot(object) <-, np dla slotu nodes wyglada tak nodes(object)<-. Czyli setter = getter + <- w pewnym sensie
##### kazdy setter zawsze przyjmuje min. 2 argumenty, object oraz value

#### tworzenie genrica
setGeneric( "nodes<-", function( object, value){standardGeneric( "nodes<-")})

#### definicja metody
temp <- function( object, value){
  object@nodes <- value
  if( validObject(object)){
    return(object)
  }
}

setMethod( "nodes<-", "Graph", temp)
g3 <- Graph( nodes = 1:10, edges = cbind(sample(tnodes, 10), sample(tnodes, 10)))

nodes(g3)
edges(g3)

nodes(g3) <- 1:20
nodes(g3)
edges(g3)

#### Proba blednej zmiany zbioru wezlow
nodes(g3) <- 1:5 # niezgodnosc logiczna
nodes(g3) <- letters[1:5] # niezgodnosc typow


### Co mozna zrobic z S4
slotNames(g3)

class(g3) #jaka klasa

getClass("Graph") #definicja klasy

showMethods(class = "Graph") # jakie mamy zdefiniowane metody dla klasy

findMethod(f = "nodes", signature = "Graph") # kod dla konkretnej metody
showMethods("nodes")
getMethod("nodes","Graph")


isS4(g3)
otype(g3)
is(g3) # z czego dziedziczy obiekt
ftype(edges)
ftype(Graph)

getGenerics() # wszystkie generics dla s4
getClasses(where = globalenv())
getClasses(where = globalenv(), inherits = T)

## Dziedziczenie

### W modelu S4 klasa moze dziedziczyc z S4, S3 lub base type. W dwoch ostatnich przypadkach klasa zawiera slot .Data, ktory
### zmienia wartosc w base type dla klasy S3 albo ten, z ktorego bezposrednio dziedzczy klasa
### z czego dziedziczy klasa wskazuje sieprzez argument contains, ktory jest wekotrem stringow okreslajacych z czego dziedziczy
### klasa. Wektor ten ma mi. 1 element


#### Tworzneie dwoch klasa


DataStructure <- setClass("DataStructure",
                          slot = list(data = "data.frame")
                          )

getClass("DataStructure")

DataStructureWithDesc <-setClass("DataStructureWithdesc",
                                 contains = "DataStructure",
                                 slots = list(desc = "character"))


a <- DataStructure( data = data.frame(a = 1:10, b = rnorm( 10)))
b <- DataStructureWithDesc( data = data.frame(a = 1:10, b = rnorm( 10)), desc = "Data from some source...")

#### Dodawanie metody dla klasy datastructure

#temp <- function(object){
#  cat("DataStructrure object\n")
#  cat(" number of variables: ", dim(object@data)[2], "\n", sep="")
  





























































