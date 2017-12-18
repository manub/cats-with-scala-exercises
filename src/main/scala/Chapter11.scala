object Chapter11 {

  object Exercise_11_2 {

    final case class GCounter(counters: Map[String, Int]) {
      def increment(machine: String, amount: Int): GCounter = {
        val incrementedCounters = counters +
          (machine -> (counters.getOrElse(machine, 0) + amount))
        GCounter(incrementedCounters)
      }

      def merge(that: GCounter): GCounter = {
        val keys = counters.keySet ++ that.counters.keySet
        val mergedCounters = keys
          .foldLeft(Map.empty[String, Int]) { (counters, key) =>
            val counterValue =
              counters.getOrElse(key, 0).max(that.counters.getOrElse(key, 0))
            counters + (key -> counterValue)
          }
        GCounter(mergedCounters)
      }

      def total: Int = counters.values.sum
    }

  }

  object Exercise_11_3 {

    import cats.Monoid

    trait BoundedSemiLattice[A] extends Monoid[A] {
      def combine(a1: A, a2: A): A
      def empty: A
    }

    implicit val intInstance: BoundedSemiLattice[Int] =
      new BoundedSemiLattice[Int] {
        def combine(a1: Int, a2: Int): Int = a1 max a2
        val empty: Int = 0
      }

    implicit def setInstance[A]: BoundedSemiLattice[Set[A]] =
      new BoundedSemiLattice[Set[A]] {
        def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2
        val empty: Set[A] = Set.empty
      }

    final case class GCounter[A](counters: Map[String, A])
                                (implicit bsl: BoundedSemiLattice[A]) {

      import cats.syntax._

      def increment(machine: String, amount: A): GCounter[A] = ???

      def merge(that: GCounter[A]): GCounter[A] = ???

      def total: A = bsl.combineAll(counters.values)
    }
  }
}
