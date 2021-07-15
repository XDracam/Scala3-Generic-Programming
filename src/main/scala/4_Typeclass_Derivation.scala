


    /*************************************
    *   Generic Programming in Scala 3   *
    *        - Cameron Reuschel -        *
    *************************************/



object Typeclass_Derivation:

// We can use the `inline` keyword to
// let the compiler derive typeclass instances

  /** Type class for all T that can be converted to JSON */
  trait ToJson[T]:
    extension (obj: T) def toJson: String



// Basic ToJson givens exactly as before...

  given str2json: ToJson[String] with
    extension (obj: String) def toJson = s"\"$obj\""

  given int2json: ToJson[Int] with
    extension (obj: Int) def toJson = obj.toString

  given list2json[T](using item2j: ToJson[T]): ToJson[List[T]] with
    extension (obj: List[T]) def toJson = obj.map(_.toJson).mkString("[", ", ", "]")

  
  

// `deriving` keyword works by calling `derived` on the object it derives
// The returned value will be given in the same scope

  object ToJson:

    inline def derived[T <: Product](using m: deriving.Mirror.Of[T]): ToJson[T] =
      inline m match
        case s: deriving.Mirror.SumOf[T] => ??? // not today
        case p: deriving.Mirror.ProductOf[T] => new ToJson[T]:

          extension (obj: T) def toJson =
            s"{${caseClassToJsonRec(
              Tuple.fromProductTyped(obj)(using p),
              compiletime.summonAll[Wrapped[ToJson, p.MirroredElemTypes]],
              compiletime.constValueTuple[p.MirroredElemLabels]
                .productIterator.map(_.asInstanceOf[String]).toList
            )}}"



          type Wrapped[F[_], T <: Tuple] <: Tuple = T match
            case EmptyTuple => EmptyTuple
            case h *: t => F[h] *: Wrapped[F, t]

          type THead[T <: Tuple] = T match
            case a *: b => a

          type TTail[T <: Tuple] <: Tuple = T match
            case a *: b => b



          private def caseClassToJsonRec
            [Values <: Tuple, Converters <: Tuple]
            (values: Values, converters: Converters, labels: List[String]): String =
              (values, converters) match
                case (EmptyTuple, EmptyTuple) => ""
                case ((vh *: vt), (ch *: ct)) : (
                  (THead[Values] *: TTail[Values]),
                  (ToJson[THead[Values]] *: Wrapped[ToJson, TTail[Values]])
                ) =>
                  val lh :: lt = labels
                  given ToJson[THead[Values]] = ch
                  s"\"$lh\": ${vh.toJson}${
                    caseClassToJsonRec(vt, ct, lt) match
                      case "" => ""
                      case otherwise => s", $otherwise"
                  }"
                case _ => sys.error("unreachable")
          end caseClassToJsonRec

    end derived

  end ToJson


// Back to our original problem:

  case class Student(name: Name, id: Int, gender: Gender, courses: List[String]) derives ToJson
  case class Name(firstName: String, lastName: String) derives ToJson
  enum Gender { case Male; case Female; case Diverse }
      
  
// => Time to test out our derivation!
      
  // We do not implement derivation for coproduct types for now
  given gender2json: ToJson[Gender] with
    extension (obj: Gender) def toJson = s"\"${obj.toString}\""


  @main def serializeMe3 =
    val me = Student(Name("Cameron", "Reuschel"), id = 2084009, Gender.Male, courses = List("FP_Seminar"))
    println(me.toJson)
      
    

// => we can serialize any case class to json now!
//    (deserialization and coproduct types for another day...)

end Typeclass_Derivation