


    /*************************************
    *   Generic Programming in Scala 3   *
    *        - Cameron Reuschel -        *
    *************************************/



object Implementing_Tuples:

// Tuples in Scala 3 aren't magic, they can easily be implemented yourself
//   (https://github.com/lampepfl/dotty/blob/master/library/src/scala/Tuple.scala)

  sealed trait Tup
  case class ConsTup[H, T <: Tup](head: H, tail: T) extends Tup
  case object EmptyTup extends Tup





// Some Scala tricks for ease of use

  val *: = ConsTup                     // for unapply
  type *:[H, T <: Tup] = ConsTup[H, T] // for type matching
  type EmptyTup = EmptyTup.type        // for type matching

  extension [H](head: H)
    def *:[T <: Tup](tail: T) = ConsTup(head, tail)





// Let's try this out

  val t = 1 *: 2 *: EmptyTup
  val one *: two *: EmptyTup = t





// We can implement typesafe methods on our custom tuples easily

  type Concat[A <: Tup, B <: Tup] <: Tup = A match
    case EmptyTup => B
    case h *: t => h *: Concat[t, B]

  extension [A <: Tup, B <: Tup](a: A)
    def ++(b: B): Concat[A, B] =
      (a match
        case EmptyTup => b
        case h *: t => h *: (t ++ b)
      ).asInstanceOf[Concat[A,B]] // cast necessary, sadly

  val concated: Int *: Char *: String *: Int *: EmptyTup =
    (1337 *: 'x' *: EmptyTup) ++ ("yes" *: 42 *: EmptyTup)





// We can even type-safely index tuples

  // Every literal has a type, e.g.
  val constOne: 1 = 1
  val constStr: "foo" = "foo"

  // These types inherit from their literal's type and from Singleton
  val constOne2: Int & Singleton = constOne
  val constStr2: String & Singleton = constStr






// The scala 3 compiler has builtin type operations on literal types

  import compiletime.ops.int._

  type Length[T <: Tup] <: Int = T match
    case EmptyTup => 0
    case h *: t => 1 + Length[t]

  // Literal types can be converted to values during compiletime in inline methods
  extension [T <: Tup](v: T) inline def length = compiletime.constValue[Length[T]]






// With arithmetic, we can do indexing

// But first, we use a trick to get in-bounds checking

  import compiletime.ops.boolean._

  @annotation.implicitNotFound("Requirement failed: expected ${T} to be true")
  trait Require[T <: Boolean]

  inline given Require[true] with {}






// Now to compiletime-safe bound-checked indexing

  type ElemAt[T <: Tup, I <: Int] = (I, T) match
    case (0, h *: t) => h
    case (n, h *: t) => ElemAt[t, n-1]

  extension [T <: Tup](v: T)
    inline def get[I <: Int & Singleton](i: I)(
      using isPositive: Require[I >= 0],
            isInBounds: Require[I < Length[T]]
    ): ElemAt[T, I] = inline v match
      case v @ (_ *: _) => __getRec(v, i).asInstanceOf[ElemAt[T, I]]

  private inline def __getRec[T <: _ *: _, I <: Int](v: T, i: I): ElemAt[T, I] = (
    inline if i == 0 then v.head
    else inline v.tail match
      case tail @ (_ *: _) => __getRec(tail, i-1)
  ).asInstanceOf[ElemAt[T, I]]







// Let's try it out

  @main def testTypesafeGet =
    val tup = 1 *: "two" *: '3' *: EmptyTup
    val one: Int = tup.get(0)
    val two: String = tup.get(1)
    val three: Char = tup.get(2)
    println(s"$one - $two - $three")

    // will not compile
//    val negative = tup.get(-1)
//    val outOfBounds = tup.get(3)









// Time for something more complex: mapping

  type Map[T <: Tup, F[_]] <: Tup = T match
    case EmptyTup => EmptyTup
    case h *: t => F[h] *: Map[t, F]

  extension [T <: Tup](v: T) def map[F[_]](fn: [A] => A => F[A]): Map[T, F] =
    (v match
      case EmptyTup => EmptyTup
      case h *: t => fn(h) *: t.map(fn)
    ).asInstanceOf[Map[T, F]] // cast necessary, sadly






  val toOption: Option[Int] *: Option[String] *: EmptyTup =
    (1337 *: "leet" *: EmptyTup).map([T] => (x: T) => Option(x))





// [T] => (x: T) => Option(x) is a POLYMORPHIC lambda

// if you can have
  object Foo { def bar[T](v: T) = Option(v) }
// then why not
  val bar = [T] => (v: T) => Option(v)
// ?







// There are also anonymous type lambdas:

  type Explicit[T] = Option[T]
  // is equal to
  type Implicit = [T] =>> Option[T]

  val explicitMapParam = (1337 *: "leet" *: EmptyTup)
    .map[[T] =>> Option[T]]([T] => (x: T) => Option(x))








// What about folding?

  extension [T <: Tup](v: T)
    def foldSimple[R](seed: R)(fn: [A] => (A, R) => R): R =
      v match
        case EmptyTup => seed
        case h *: t => t.foldSimple(fn(h, seed))(fn)

  @main def testSimpleFolding =
    val tup = (1337 *: "leet" *: List(1,2,3) *: EmptyTup)
    println(tup.foldSimple("")([A] => (curr: A, acc: String) => s"$acc\n$curr"))








// Good and well, but we cannot even reverse a tuple with this...

  type Fold[T <: Tup, Seed, F[_,_]] = T match
    case EmptyTup => Seed
    case h *: t => Fold[t, F[Seed, h], F]

  extension [T <: Tup](v: T)
    def fold[Seed, F[_,_]](seed: Seed)(
      fn: [C, Acc] => (C, Acc) => F[C, Acc]
    ): Fold[T, Seed, F] =
      (v match
        case EmptyTup => seed
        case h *: t => t.fold(fn(h, seed))(fn)
      ).asInstanceOf[Fold[T, Seed, F]]






// With this, we could implement reversal, but...

//  extension [T <: Tup](v: T) def reversed =
//    v.fold[EmptyTup, [C, Acc] =>> Acc match
//      case EmptyTup => C *: EmptyTup
//      case h *: t => C *: h *: t
//    ](EmptyTup)(
//      [C, Acc] => (c: C, acc: Acc) =>
//        acc match
//          case tup @ (_ *: _) => c *: tup
//    )

// ... it crashes the compiler with an internal assertion error :c
// https://github.com/lampepfl/dotty/issues/13075
      
// .. which odersky himself fixed ...
// https://github.com/dotty-staging/dotty/commit/75b6143fff5a761fffe05003596dbb7a7306c1b1
      
// .. and this code is now a test case in dotty






  @main def testFold =
    val asStr = (1 *: '2' *: "hello" *: EmptyTup)
      .fold[String, [_,_] =>> String]("")(
        [C, Acc] => (c: C, acc: Acc) => s"$acc\n$c")
    println(asStr)


end Implementing_Tuples

