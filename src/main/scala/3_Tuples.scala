


    /*************************************
    *   Generic Programming in Scala 3   *
    *        - Cameron Reuschel -        *
    *************************************/



object Tuples:

// We should all know tuples...

  val t1: (Int, String, Boolean) = (3, "foo", false)






// But did you know that they can be written differently in Scala 3?

  val t2: Int *: String *: Boolean *: EmptyTuple = t1

  val t3: (Int, String, Boolean) = 3 *: "foo" *: false *: EmptyTuple

// Similar to lists, the *: operator type-safely "prepends" to a tuple

  val t4 = ("hello", 'w')
  val t5: (Int, String, Char) = 1337 *: t4





// Also like lists, *: can be used to deconstruct

  val t5i *: t5s *: t5c *: EmptyTuple = t5

  val i: Int = t5i
  val s: String = t5s
  val c: Char = t5c









// With this knowledge, let's build a ToJson for tuples!


  import Intro.{ToJson, int2json, str2json, list2json}








  trait TupToJson[T <: Tuple]:
    extension (tup: T) def tupToJson: Option[String]

  given TupToJson[EmptyTuple] with
    extension (tup: EmptyTuple) def tupToJson = None





  given [H : ToJson, T <: Tuple : TupToJson]: TupToJson[H *: T] with
    extension (tup: H *: T) def tupToJson =
      val h *: t = tup
      val tJson = t.tupToJson
      Some(s"${h.toJson}${
        t.tupToJson match
          case Some(v) => s", $v"
          case None => ""
      }")




  given [T <: Tuple : TupToJson]: ToJson[T] with
    extension (obj: T) def toJson =
      s"[${obj.tupToJson.getOrElse("")}]"





// Let's test this:

  @main def testTupleToJson =
    println((List("hello", "World"), 1, 3, 3, 7).toJson)











// Now, time to serialize our Student

  import Intro.{Name, Gender, Student}
  import Intro.gender2json // doesn't change






  given ToJson[Name] with
    extension (obj: Name) def toJson = Tuple.fromProductTyped(obj).toJson



  given ToJson[Student] with
    extension (obj: Student) def toJson = Tuple.fromProductTyped(obj).toJson



  @main def serializeMe2 =
    val me = Student(
      Name("Cameron", "Reuschel"),
      id = 1234567,
      Gender.Male,
      courses = List("FP_Seminar"))
    println(me.toJson)
    
    
    

// => Less boilerplate per type
//    But: We lose the field names (will still be fine for deserialization)

end Tuples








