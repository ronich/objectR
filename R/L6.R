Person <- R6Class("Person",
                public = list(
                  name = NULL,
                  hair = NULL,
                  initialize = function( name = NA, hair = NA){
                    self$name <- name
                    self$hair <- hair
                    self$greet()
                },
                set_hair = function(val){
                  self$hair <- val
                },
                greet = function(){
                  cat( paste("Hi my name is", self$name))
                }
              )
)

ann <- Person$new(name = "Ann", hair = "red")

ann$hair
ann$name
ann$greet()

ann$set_hair("purple")
ann$hair

Person <- R6Class("Person",
                  public = list(
                    name = NULL,
                    hair = NULL,
                    initialize = function( name = NA, hair = NA){
                      self$name <- name
                      self$hair <- hair
                      self$greet()
                    },
                    set_hair = function(val){
                      self$hair <- val
                      envs <- list( environment())
                      while( !identical(emptyenv(), envs[[length( envs)]])){
                        envs <- c(envs, parent.env(envs[[length(envs)]]))
                      }
                      envs
                    },
                    greet = function(){
                      cat( paste("Hi my name is", self$name))
                      envs <- list( environment())
                      while( !identical(emptyenv(), envs[[length( envs)]])){
                        envs <- c(envs, parent.env(envs[[length(envs)]]))
                      }
                      envs
                    }
                  )
)

john <- Person$new( name= "John", hair = "blue")
q1 <- john$set_hair("red")
q2 <- john$greet()
q1[1:3] 
q2[1:3]
# rozne metody maja losowe srodowiska execution, ale sa podpiete pod to samo
# enclosing execution
# model obiektowy R6 opiera sie tylko na srodowiskach w przeciwienstwie do poprzednich

# Czy srodowisko public ejst enclosing dla metody publiczncych? Mozna to sprawdzic
# korzsytajac ze zwroconych lsit srodowisk
ls( envir = q1[[2]]) # tylko self

# Zatem jak widac nie jest to srodowisko enclosing dla metody. W srodowisku enclosing
# jest jedynie obiekt self

eval(quote(typeof(self)), envir=q1[[2]]) # to jest srodowisko

eval(quote(ls(envir = self)), envir=q1[[2]]) # to srodowisko self i srodowisko public
# to jest to samo srodowisko
# czyli mozna powiedziec ze self (enclosing envir) to jest wskaznik na public
# John jest w globalu
# ale John to jest to srodowisko public, dlatego mzona sie odwolac John$zmienna


# Elementy prywatne
# niewidoczne z zewnatrz klasy

Queue <- R6Class("Queue",
                  public = list(
                    initialize = function(...){
                      for (item in list(...)){
                        self$add(item)
                      }
                    },
                    
                    add = function(x){
                      private$queue <- c( private$queue, list(x))
                      invisible(self)
                    },
                    remove = function(){
                      if(private$length() == 0) return(NULL)
                      head <- private$queue[[1]]
                      private$queue <- private$queue[-1]
                      head # zwraca usuniety arg
                    },
                    show = function(){
                      private$queue
                    }
                  ),
                 private = list(
                   queue = list(),
                   length = function() base::length(private$queue)
                  )
)

q <- Queue$new(3,2,TRUE,"Jiohn")

q$show()
q$remove()
q$add(1+0i)
q$show()

#prywatne
q$length()
q$queue

#chaining
q$add(1)$add("Last")$add("day")
q$show()

# Elemety active (active binding)

CrazyString <- R6Class("CrazyString",
                 public = list(
                    x="CrazyString"
                   ),
                 active = list(
                   xActive = function(val){
                     if( missing(val)){
                       return( self$x)
                     } else {
                       self$x <- val
                     }
                   },
                   randomCharacter = function(){
                     ell <- nchar(self$x)
                     ell <- sort( sample(1:ell, 2))
                     substr(self$x, ell[1], ell[2])
                   }
                 )
                 )

# Wykorzystanie eleentow aktyqwnych

q <- CrazyString$new()
q$x
q$xActive
q$xActive <- "Here comes Johnny!"
q$xActive
q$randomCharacter # aktywnych bez argumentu nie mozna wykorzystac do ustawienia


# Dziedziczenie

# klasy R6 moga dziedziczyc z klas R6
# klasa potomna moze dziedziczyc z klasy rodzica - moze miec dodatkowe metody
# jak rowniez nadpisywac istniejace metody

HistoryQueue <- R6Class("HistoryQueue",
                        inherit = Queue,
                         public = list(
                           show = function(){
                             cat("Next item is at index: ")
                             private$queue
                           }
                         ),
                         private = list(
                           queue = list(),
                           length = function() base::length(private$queue)
                         )
        )

q <- Queue$new(3,2,TRUE,"Jiohn")
