# Test R file for language server
x <- 1:10
y <- x^2
plot(x, y, main="Test Plot")

# Test function
test_function <- function(a, b) {
    return(a + b)
}

result <- test_function(5, 3)
print(result)