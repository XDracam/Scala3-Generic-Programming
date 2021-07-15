


    /*************************************
    *   Generic Programming in Scala 3   *
    *        - Cameron Reuschel -        *
    *************************************/



object Shapes:

/*
 *    Goal: Easy support for JSON serialization for all case classes
 *          (ignoring field names for now...)
 */



/*
 *    Generic Programming:
 *
 *          A style of computer programming in which algorithms are written
 *          in terms of types to-be-specified-later that are then instantiated
 *          when needed for specific types provided as parameters.
 *
 *      - https://en.wikipedia.org/wiki/Generic_programming
 */



// What do the following types have in common?

  case class Foo(a: Int, b: String)
  case class Person(age: Int, name: String)

// => both have fields of same types in same order!

  val foo = (1337, "hello world")
  val person = (24, "Cameron Reuschel")




// Take these two types:

  trait A
  trait B

// What do the following types have in common?

  sealed trait Bar
  case object B1 extends Bar with A
  case object B2 extends Bar with B

  enum Category:
    case Thriller extends Category with A
    case Fantasy extends Category with B

// => both are either an A or a B

  val bar: A | B = B1
  val category: A | B = Category.Thriller





// Every type has a >>SHAPE<<:
//
//   > It is either a PRODUCT TYPE (tuple, and)
//
//   > Or a (SUM / COPRODUCT) TYPE (enum, or)
//
// We will mainly deal with product types today

  enum Shape:
    case Product
    case Coproduct



/*
 *    Product Type:
 *
 *          A product of types is another, compounded, type in a structure.
 *          The "operands" of the product are types,
 *            and the structure of a product type is determined
 *            by the fixed order of the operands in the product.
 *          [...] The expression of an instance of a product type will be a tuple [...]
 *
 *      - https://en.wikipedia.org/wiki/Generic_programming
 */

// Scala provides the `Product` type as well as generic `ProductN` types

  case class Foo1(a: Int, b: String) // all case classes are automatically a product
  val foo1: Product = Foo1(1, "2")

  case class Foo2(a: Int, b: String) extends Product2[Int, String]

// case classes automatically provide all values needed
//   to implement the appropriate ProductN trait

  var foo2: Product2[Int, String] = Foo2(1337, "leet")

// Tuples are automatically a ProductN

  foo2 = (1337, "leet")



// => How can we use this to abstract over case class serialization?

  // Scala has builtin methods to convert between case classes and tuples
  val source   : Foo1          = Foo1(3, "4")
  val asTuple  : (Int, String) = Tuple.fromProductTyped(source)
  val fromTuple: Foo1          = summon[deriving.Mirror.Of[Foo1]].fromProduct(asTuple)

// => We only need to serialize and deserialize tuples!

end Shapes