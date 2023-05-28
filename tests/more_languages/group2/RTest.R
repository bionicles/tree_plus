# RTest.R
# S3 object
person <- list(name = "John Doe", age = 50)
class(person) <- "Person"

# S3 Method
greet.Person <- function(p) {
  paste("Hello,", p$name)
}
