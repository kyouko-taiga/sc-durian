/// A parsing failure preventing automatic backtracking.
///
/// @param context The context in which the error occured.
/// @param partialResult The partial result computed by the combinator that failed.
final case class HardFailure[C, +E](context: C, partialResult: E) extends Exception