/// The start and end positions of a token in a string.
type Site = Range

/// A terminal symbol of the JSON grammar.
final case class Token(kind: Token.Kind, site: Site)

object Token {

  /// The kind of a JSON token.
  sealed trait Kind {

    /// The raw value of this kind.
    def rawValue: String = this.getClass.toString

  }

  object Kind {

    /// A left brace "{".
    case object LBrace extends Kind {
      override def rawValue = "{"
    }

    /// A right brace "}".
    case object RBrace extends Kind  {
      override def rawValue = "}"
    }

    /// A left bracket "[".
    case object LBracket extends Kind {
      override def rawValue = "["
    }

    /// A right bracket "]".
    case object RBracket extends Kind {
      override def rawValue = "]"
    }

    /// A colon ":".
    case object Colon extends Kind {
      override def rawValue = ":"
    }

    /// A comma ",".
    case object Comma extends Kind {
      override def rawValue = ","
    }

    /// A true literal.
    case object True extends Kind {
      override def rawValue = "true"
    }

    /// A false literal.
    case object False extends Kind {
      override def rawValue = "false"
    }

    /// A null literal.
    case object Null extends Kind {
      override def rawValue = "null"
    }

    /// A string literal.
    case object String extends Kind

    /// A number literal.
    case object Number extends Kind

    /// An error denoting invalid input.
    case object Error extends Kind

  }

}