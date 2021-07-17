


    /*************************************
    *   Generic Programming in Scala 3   *
    *        - Cameron Reuschel -        *
    *************************************/



/*
 *    Generic Programming:
 *
 *          A style of computer programming in which algorithms are written
 *          in terms of types to-be-specified-later that are then instantiated
 *          when needed for specific types provided as parameters.
 *
 *      - https://en.wikipedia.org/wiki/Generic_programming
 */

object Intro:

// Consider following types:

  case class Student(name: Name, id: Int, gender: Gender, courses: List[String])
  case class Name(firstName: String, lastName: String)
  enum Gender { case Male; case Female; case Diverse }





// We want to save these to JSON (and load at a later point)

  /** Type class for all T that can be converted to JSON */
  trait ToJson[T]:
    extension (obj: T) def toJson: String



// Enable conversion for basic types

  given str2json: ToJson[String] with
    extension (obj: String) def toJson = s"\"$obj\""

  given int2json: ToJson[Int] with
    extension (obj: Int) def toJson = obj.toString

  given list2json[T](using item2j: ToJson[T]): ToJson[List[T]] with
    extension (obj: List[T]) def toJson = obj.map(_.toJson).mkString("[", ", ", "]")





// Serialize enum values as their name as string
//   (doesn't work for more complex enums!)

  given gender2json: ToJson[Gender] with
    extension (obj: Gender) def toJson = s"\"${obj.toString}\""




// Serialize Name by hard-coding the structure,
//   but we use our String serializer from before

  given (using str2j: ToJson[String]): ToJson[Name] with
    extension (obj: Name) def toJson =
      val Name(first, last) = obj
      s"""{"firstName":${first.toJson},"lastName":${last.toJson}"""




// Serialize Student by hard-coding the structure as well...

  given (using
         str2j: ToJson[String],
         int2j: ToJson[Int],
         name2j: ToJson[Name],
         c2j: ToJson[List[String]]
        ): ToJson[Student] with
    extension (obj: Student) def toJson =
      val Student(name, id, gender, courses) = obj
      val nameJson = name.toJson
      val idJson = id.toJson
      val genderJson = gender.toJson
      val coursesJson = courses.toJson
      s"""{"name":$nameJson,"id":$idJson,"gender":$genderJson,"courses":$coursesJson}"""



// Let's see whether this works

  @main def serializeMe =
    val me = Student(
      Name("Cameron", "Reuschel"),
      id = 1234567,
      Gender.Male,
      courses = List("FP_Seminar"))
    println(me.toJson)









// > We don't want to write so much boilerplate for all supported types

// > But we also want:
//   - type safety
//   - composition
//   - no reflection overhead
//     ...

end Intro
