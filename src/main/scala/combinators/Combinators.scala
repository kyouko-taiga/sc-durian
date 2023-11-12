import scala.collection.mutable

/// A combinator parsing instances of `E` from a context `C`.
trait Combinator[C, E] {

  /// Parses an element from `context` and returns it.
  def parse(context: C): Option[E]

  /// Returns a combinator applying `this` and transforming its result with `transform`.
  def map[T](transform: (E) => T) = Transform(this, transform)

  /// Returns a combinator applying `this` and `that` and returning their results.
  def ++[T](that: Combinator[C, T]) = Combine(this, that)

  /// Returns a combinator applying `this` and `that` and returning the result of `that`.
  def >+[T](that: Combinator[C, T]) = Combine(this, that).map((a, b) => b)

  /// Returns a combinator applying `this` and `that` and returning the result of `this`.
  def +<[T](that: Combinator[C, T]) = Combine(this, that).map((a, b) => a)

}

/// A combinator that applies a closure.
///
/// @param action A closure parsing and returning an `E` from a `C`.
final case class Apply[C, E](val action: C => Option[E]) extends Combinator[C, E] {

  def parse(context: C) = action(context)

}

/// A combinator that applies `first` and then `second`.
///
/// @param first The first combinator to apply.
/// @param second The second combinator to apply.
final case class Combine[C, A, B](
    val first: Combinator[C, A],
    val second: Combinator[C, B]
) extends Combinator[C, (A, B)] {

  def parse(context: C): Option[(A, B)] = {
    first.parse(context) match {
      case Some(a) => {
        second.parse(context) match {
          case Some(b) => Some((a, b))
          case None => throw HardFailure(context, a)
        }
      }
      case None => None
    }
  }

}

/// A combinator that applies `base` and backtracks if it produces a hard failure.
///
/// @param base The combinator to apply.
final case class Maybe[C: Restorable, E](
    val base: Combinator[C, E]
) extends Combinator[C, Option[E]] {

  def parse(context: C): Option[Option[E]] = {
    val backup = context.backup()
    try {
      Some(base.parse(context))
    } catch {
      case e: HardFailure[C, Any] => { context.restore(backup); Some(None) }
    }
  }

}

/// A combinator that applies `base` until it produces a soft failure.
///
/// @param base The combinator to apply.
final case class Many[C, E](val base: Combinator[C, E]) extends Combinator[C, List[E]] {

  def parse(context: C): Option[List[E]] = {
    def impl(elements: mutable.ListBuffer[E]): Option[List[E]] = {
      base.parse(context) match {
        case Some(x) => elements.append(x); impl(elements)
        case None => Some(elements.toList)
      }
    }
    return impl(mutable.ListBuffer())
  }

}

/// A combinator that applies `base` and transforms its result with `transform`.
///
/// @param base The combinator to apply.
/// @param transform A closure transforming results of `A`.
final case class Transform[C, A, B](
    val base: Combinator[C, A],
    val transform: (A) => B
) extends Combinator[C, B] {

  def parse(context: C): Option[B] = {
    base.parse(context) match {
      case Some(a) => Some(transform(a))
      case None => None
    }
  }

}