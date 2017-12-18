import Chapter11.Exercise_11_3.BoundedSemiLattice
import cats.kernel.Monoid

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

    final case class GCounter[A](counters: Map[String, A]) {

      import cats.implicits._

      def increment(machine: String, amount: A)(
          implicit m: Monoid[A]): GCounter[A] = {
        val value = amount |+| counters.getOrElse(machine, m.empty)
        GCounter(counters + (machine -> value))
      }

      def merge(that: GCounter[A])(
          implicit b: BoundedSemiLattice[A]): GCounter[A] =
        GCounter(counters |+| that.counters)

      def total(implicit m: Monoid[A]): A = m.combineAll(counters.values)
    }
  }

  object Exercise_11_4 {

    import cats.implicits._

    trait GCounter[F[_, _], K, V] {
      def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

      def merge(f1: F[K, V], f2: F[K, V])(
          implicit b: BoundedSemiLattice[V]): F[K, V]

      def total(f: F[K, V])(implicit m: Monoid[V]): V
    }

    object GCounter {
      def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) = counter
    }

    implicit def mapGCounter[K, V] = new GCounter[Map, K, V] {
      override def increment(map: Map[K, V])(k: K, v: V)(
          implicit m: Monoid[V]): Map[K, V] = {
        val value = v |+| map.getOrElse(k, m.empty)
        map + (k -> value)
      }

      override def merge(map1: Map[K, V], map2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
        map1 |+| map2

      override def total(map: Map[K, V])(implicit m: Monoid[V]): V =
        m.combineAll(map.values)
    }
  }
  
  object Exercise_11_5 {
    
    trait KeyValueStore[F[_, _]] {
      def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
      
      def get[K, V](f: F[K, V])(k: K): Option[V]
      
      def getOrElse[K, V](f: F[K, V])(k: K, default: => V): V =
        get(f)(k).getOrElse(default)
      
      def values[K, V](f: F[K, V]): List[V]
    }
    
    object KeyValueStore {
      def apply[F[_, _]](implicit keyValueStore: KeyValueStore[F]) = keyValueStore
    }
    
    implicit val mapKeyValueStore = new KeyValueStore[Map] {
      def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f.updated(k, v)

      def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)
    
      def values[K, V](f: Map[K, V]): List[V] = f.values.toList
    }
  }
}
