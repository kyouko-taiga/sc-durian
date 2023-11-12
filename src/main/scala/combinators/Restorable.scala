/// A type class describing types whose notional values can be saved and restored.
trait Restorable[T] {

  /// A type whose instances can store the notional value of a `T`.
  type Backup

  extension(self: T) {

    /// Returns a backup of `this`.
    def backup(): Backup

    /// Restores the value of `this` from `backup`.
    def restore(backup: Backup): Unit

  }

}