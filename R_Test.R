
fun.test <- function(a, b, method ){
  if(method == "add") { ## 如果if或者for/while；
    res <- a + b       ## 等后面的语句只有一行，则无需使用花括号。
  }
  if(method == "subtract"){
    res <- a - b
  }
  return(res)           ## 返回值
}

aa <- fun.test(b=1,a=2,"subtract")
