package ch1


sealed trait Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
final case class JsObject(get: Map[String, Json]) extends Json
case object JsNull extends Json

trait JsonWriter[A] { //type class
  def write(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriterInstances { //type class instances
  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json =
        JsString(value)
    }

  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(value: Person): Json =
        JsObject(Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email)
        ))

    }

  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      def write(option: Option[A]) =
        option match {
          case Some(a) => writer.write(a)
          case None => JsNull
      }
    }
}

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

object Main extends App {
  import JsonWriterInstances._

  def test() = {
    val person = Json.toJson(Person("Dave", "dave@example.com"))/*(personWriter)*/
    //Compiler searches for type class instances that would work since we do not provide implicit parameters

    val optionPerson = Json.toJson(Option(Person("DaveOpt", "dave@example.com")))/*(optionWriter[Person])..(optionWriter(personWriter)*/
    //Recursively searches for a JsonWriter[Person] to use as the parameter to optionWriter
    println(person)
    println(optionPerson)
  }

  def extensionMethodTest() = {
    import JsonSyntax._
    import JsonWriterInstances._

    println(Person("DaveExt", "dave@example.com").toJson/*(personWriter)*/)
  }

  def implicitlyTest() = {
    import JsonWriterInstances._

    val implicitlyJW = implicitly[JsonWriter[String]]
    println(implicitlyJW)
  }

  test()
  extensionMethodTest()
  implicitlyTest()
}