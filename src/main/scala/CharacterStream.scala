import scala.collection._

/// A stream of characters from which token can be parsed.
final class CharacterStream(val base: String, offset: Int = 0) extends Iterable[Char] {

  /// The current position in base.
  private var cursor = firstPositionFrom(offset)

  /// Returns the position in `base` corresponding to the head of the stream.
  def startPosition: Int = cursor

  /// Returns the "past-the-end" position in `base`.
  def endPosition: Int = base.length

  /// Advances or rewinds the stream so that its start position is `p`.
  ///
  /// - Precondition: `p` is a valid position the stream.
  def setStartPosition(p: Int): Unit = {
    cursor = p
  }

  /// Returns the position immediately after `p`.
  ///
  /// - Requires: `p` is a valid position in `this` different from `endPosition`.
  /// - Complexity: O(n) where n is the length of `base`.
  def positionAfter(p: Int): Int = firstPositionFrom(p + 1)

  /// Returns `true` iff `this` is empty.
  override def isEmpty: Boolean = cursor == endPosition

  /// Returns the first position in which a character satisfies `predicate`, or `None` if no
  /// character exists.
  ///
  /// - Complexity: O(n) where n is the length of `base[startPosition...]`.
  def firstPosition(predicate: (Char) => Boolean): Option[Int] = {
    def impl(p: Int): Option[Int] = {
      if (p == endPosition) {
        None
      } else if (predicate(base.charAt(p))) {
        Some(p)
      } else {
        impl(firstPositionFrom(p + 1))
      }
    }
    return impl(startPosition)
  }

  /// Returns `this` sans the first `n` elements, or `nil` if `this`'s lenght is shorted than `n`.
  ///
  /// - Precondition: `n >= 0`
  /// - Complexity: O(n) where n is the length of `base[startPosition...]`.
  def sans(n: Int): Option[CharacterStream] = {
    if (n == 0) {
      Some(this)
    } else if (!isEmpty) {
      CharacterStream(base, cursor + 1).sans(n - 1)
    } else {
      None
    }
  }

  /// Returns `this` sans `prefix` or `nil` if `this` does not start with `prefix`.
  ///
  /// - Complexity: O(n) where n is the length of `base[startPosition...]`.
  def sans(prefix: String): Option[CharacterStream] = {
    if (prefix.isEmpty) {
      Some(this)
    } else if (Some(prefix.head) == headOption) {
      CharacterStream(base, cursor + 1).sans(prefix.drop(1))
    } else {
      None
    }
  }

  def iterator: Iterator[Char] = new AbstractIterator[Char] {
    
    private var s = CharacterStream(base, cursor)

    def hasNext: Boolean = s.cursor != s.endPosition

    def next(): Char = {
      val r = s.base.charAt(s.cursor)
      s.setStartPosition(s.firstPositionFrom(s.cursor + 1))
      return r
    }

  }

  /// Returns the position of the first non-whitespace character in `base[p...]` or `endPosition`
  /// if no such character exists.
  private def firstPositionFrom(p: Int): Int = {
    base.indexWhere((c) => !c.isWhitespace, p) match {
      case -1 => endPosition
      case p => p
    }
  }

}

given Restorable[CharacterStream] with {

  type Backup = Int

  extension (self: CharacterStream) {

    def backup(): Backup = self.startPosition

    def restore(backup: Backup): Unit = {
      self.setStartPosition(backup)
    }

  }

}

// Note: Sadly it seems we can't define `sans(prefix:)` as a generic algorithm because of the erase
// caused by its return type. In our specific case, we need to preserve the fact that `self` is a
// `CharacterStream` because we want to apply a functional update.
//
// extension (self: Iterable[Char]) {
//
//   /// Returns `self` sans `prefix` or `nil` if `self` does not start with `prefix`.
//   ///
//   /// - Complexity: O(n) where n is the length of `prefix`.
//   def sans(prefix: String): Option[Iterable[Char]] = {
//     if (prefix.isEmpty) {
//       Some(self)
//     } else if (Some(prefix.head) == self.headOption) {
//       self.drop(1).sans(prefix.drop(1))
//     } else {
//       None
//     }
//   }
//
// }