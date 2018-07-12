package ch1

trait Printable[A] { //type class
  def format(value: A): String
}


object PrintableInstances { //type class instances
  implicit val printableString: Printable[String] =
    new Printable[String] {
      def format(value: String): String = value
    }

  implicit val prinatableInt: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String = value.toString
    }
}


object Printable {
  def format[A](a: A)(implicit p: Printable[A]) = p.format(a)
  def print[A](a: A)(implicit p: Printable[A]) = println(p.format(a))
}