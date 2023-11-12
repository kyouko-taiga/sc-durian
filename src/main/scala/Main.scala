import scala.io.Source

/// A helper class to measure time.
final class Stopwatch {

  /// A measure of the time elapsed between two events.
  final case class TimeInteral(ns: Long) {

    /// This interval in microseconds.
    def μs = ns.toDouble / 1_000.0

    /// This interval in milliseconds.
    def ms = ns.toDouble / 1_000_000.0

    /// This interval in seconds.
    def s = ns.toDouble / 1_000_000_000.0

    /// Returns a human-readable representation of `this`.
    def humanFormat: String = {
      ns match {
        case d if d < 1_000L => s"${ns}ns"
        case d if d < 1_000_000L => s"${(μs * 100.0).round / 100.0}μs"
        case d if d < 1_000_000_000L => s"${(ms * 100.0).round / 100.0}ms"
        case d if d < 1_000_000_000_000L => s"${(s * 100.0).round / 100.0}s"
        case _ =>
          val minutes = ns / 60_000_000_000_000L
          val seconds = ns % 60_000_000_000_000L
          if (minutes < 60) {
            s"${minutes}m ${seconds}s"
          } else {
            val hours = minutes / 60L
            s"${hours}h ${minutes % 60L}m ${seconds}s"
          }
      }
    }

  }

  /// The time from which intervals are computed.
  var startTime = System.nanoTime()

  /// The time elapsed since `startTime`.
  def elapsed = TimeInteral(System.nanoTime() - startTime)

}

var r: Option[JSONTree] = None

@main def parse(times: Int, input: String) = {
  val source = Source.fromFile(input).getLines.mkString
  val w = Stopwatch()
  for (_ <- 0 until times) {
    val stream = CharacterStream(source)
    r = Grammar.element.parse(stream)
  }
  println(w.elapsed.humanFormat)
}
