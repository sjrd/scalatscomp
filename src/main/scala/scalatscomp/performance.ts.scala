package scalatscomp
object Performance {
  val performance: ({ def now(): Int } | undefined) = zeroOfMyType
  val timestamp =
    (if (((typeof(performance) !== "undefined") && performance.now))
       (() => performance.now())
     else (if (Date.now) Date.now
           else (() => (+(new Date())))))
}
object ts {
  object performance {
    val onProfilerEvent: {
      def apply(markName: String): Unit
      var profiler: Boolean
    } = zeroOfMyType
    val profilerEvent =
      (if (((typeof(onProfilerEvent) === "function") && (onProfilerEvent.profiler === true)))
         onProfilerEvent
       else ((_markName: String) => {}))
    var enabled = false
    var profilerStart = 0
    var counts: Map[Int] = zeroOfMyType
    var marks: Map[Int] = zeroOfMyType
    var measures: Map[Int] = zeroOfMyType
    def mark(markName: String) = {
      if (enabled) {
        (marks(markName) = timestamp())
        (counts(markName) = (((counts(markName) || 0)) + 1))
        profilerEvent(markName)

      }

    }
    def measure(measureName: String,
                startMarkName: String,
                endMarkName: String) = {
      if (enabled) {
        val end = ((endMarkName && marks(endMarkName)) || timestamp())
        val start = ((startMarkName && marks(startMarkName)) || profilerStart)
        (measures(measureName) = (((measures(measureName) || 0)) + ((end - start))))

      }

    }
    def getCount(markName: String) = {
      return ((counts && counts(markName)) || 0)

    }
    def getDuration(measureName: String) = {
      return ((measures && measures(measureName)) || 0)

    }
    def forEachMeasure(cb: ((String, Int) => Unit)) = {
      (measures).keys.foreach { fresh1 =>
        val key = zeroOfMyType = fresh1 {
          cb(key, measures(key))

        }
      }

    }
    def enable() = {
      (counts = createMap[Int]())
      (marks = createMap[Int]())
      (measures = createMap[Int]())
      (enabled = true)
      (profilerStart = timestamp())

    }
    def disable() = {
      (enabled = false)

    }
  }
}
