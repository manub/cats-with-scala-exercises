object Chapter11 {

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
