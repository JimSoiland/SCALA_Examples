object session {
  def pascal(c: Int, r: Int): Int = {
    if (c<0 || r<1) 0
    else if (c==0) 1
    else pascal(c-1,r-1)+pascal(c,r-1)
  }
  pascal(5,3)

  //*This is an example of using a higher-order, tail-recursive
  //*function. The first parameter is a function that takes one
  //* integer and returns an integer.
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1,acc+f(a))
    }
    loop(a, 0)
  }

  //These functions then use the above by embedding anonymous functions
  // (the first parameter) into the sum function above
  def sumInts(a: Int, b: Int) = sum(x => x, a, b)
  def sumCubes(a: Int, b: Int) = sum( x=>x*x*x , a, b)
  sum(x => 1,2,3)
  sumInts(1,4)
  sumCubes(1,3)

  //define a function that will produce the product of
  //a function over a range
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if(a>b) 1
    else f(a)* product(f)(a+1,b)
  }
  product(x => x)(1,4)
  /*compute factorial using the above*/
  def factorial(a: Int): Int =  product(x => x) (1,a)
  factorial(5)

  //generalize sum and product. These differ by their
  // unit value (0 and 1) and the operations performed in them
  // this uses "currying" - defining higher order
  // function parameters piecewise
  def mapReduce(f: Int => Int, combine: (Int,Int) => Int,
                zero: Int)(a: Int, b: Int): Int = {
    if (a>b) zero
    else combine(f(a),mapReduce(f,combine,zero)(a+1,b))
  }
  mapReduce(x=>x, (x,y)=>x+y,0)(1,3)
  mapReduce(x=>x, (x,y) => x*y, 1)(1,3)


}
