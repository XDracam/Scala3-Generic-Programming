import Implementing_Tuples.{*:, EmptyTup, Map, Tup}


/*************************************
    *   Generic Programming in Scala 3   *
    *        - Cameron Reuschel -        *
    *************************************/



object Implementing_Tuples:

// Tuples in Scala 3 aren't magic, they can easily be implemented yourself
//   (https://github.com/lampepfl/dotty/blob/master/library/src/scala/Tuple.scala)

  sealed trait Tup
  case class ConsTup[T, H <: Tup](head: T, tail: H) extends Tup
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

  extension [T <: Tup](v: T) def foldSimple[R](seed: R)(fn: [A] => (A, R) => R): R =
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





  @main def testFold =
    val asStr = (1 *: '2' *: "hello" *: EmptyTup)
      .fold[String, [_,_] =>> String]("")(
        [C, Acc] => (c: C, acc: Acc) => s"$acc\n$c")
    println(asStr)


end Implementing_Tuples


    /***************************
    *   Thanks for watching!   *
    ***************************/

