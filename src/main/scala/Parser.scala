import scala.collection._

/// A parser combinator recognizing tokens.
final case class TokenParser(kind: Token.Kind) extends Combinator[CharacterStream, Token] {

  /// Parses a token of `kind` from `context`.
  def parse(context: CharacterStream): Option[Token] = {
    kind match {
      case Token.Kind.Number => parseNumber(context)
      case Token.Kind.String => parseString(context)
      case _ => parseKeywordOrPunctuation(context)
    }
  }

  /// Parses and returns a number literal from `context`.
  private def parseNumber(context: CharacterStream): Option[Token] = {
    if (context.isEmpty) { return None }
    val s = context.startPosition

    // Parse the leading '-', if any.
    if (context.head == '-') { context.setStartPosition(context.positionAfter(s)) }

    // Parse the decimal part.
    var e = context.firstPosition((c) => !c.isDecDigit).getOrElse(context.endPosition)
    if (e == s) { return None }
    context.setStartPosition(e)

    // Parse the fractional part, if any.
    if (context.head == '.') {
      context.setStartPosition(context.positionAfter(e))
      e = context.firstPosition((c) => !c.isDecDigit).getOrElse(context.endPosition)
      context.setStartPosition(e)
    }

    val t = Token(Token.Kind.Number, s until e)
    return Some(t)
  }

  /// Parses and returns a string literal from `context`.
  private def parseString(context: CharacterStream): Option[Token] = {
    context.headOption match {
      case Some(h) if h == '"' =>
        context.sans(1).get.firstPosition((c) => (c == '"')) match {
          case Some(e) =>
            val f = context.positionAfter(e)
            val t = Token(Token.Kind.String, context.startPosition until context.endPosition)
            context.setStartPosition(f)
            Some(t)

          case None => {
            val t = Token(Token.Kind.Error, context.startPosition until context.endPosition)
            context.setStartPosition(context.endPosition)
            Some(t)
          }
        }

      case _ => None
    }
  }

  /// Parses and returns a keyword punctuation symbol of `kind` from `context`.
  private def parseKeywordOrPunctuation(context: CharacterStream): Option[Token] = {
    context.sans(kind.rawValue) match {
      case Some(s) =>
        val t = Token(kind, context.startPosition until s.startPosition)
        context.setStartPosition(s.startPosition)
        Some(t)
      case None => None
    }
  }

}

extension (self: Token.Kind) {

  def p = TokenParser(self)

}

/// A the result of parsing a JSON input.
sealed trait JSONTree

object JSONTree {

  /// A literal value.
  final case class Literal(token: Token) extends JSONTree

  /// An array.
  final case class Array(elements: Seq[JSONTree]) extends JSONTree

  /// An object.
  final case class Object(members: Seq[(Token, JSONTree)]) extends JSONTree

}

object Grammar {

  import Token.Kind._

  lazy val literal = anyOf(Seq(Null.p, True.p, False.p, Number.p, String.p))
    .map((r) => JSONTree.Literal(r))

  lazy val array = (LBracket.p >+ Maybe(element ++ Many(Comma.p >+ element)) +< RBracket.p)
    .map((r) => JSONTree.Array(r.map((h, t) => List(h) ++ t).getOrElse(List())))

  lazy val objekt = (LBrace.p >+ Maybe(member ++ Many(Comma.p >+ member)) +< RBrace.p)
    .map((r) => JSONTree.Object(r.map((h, t) => List(h) ++ t).getOrElse(List())))

  lazy val member = (String.p +< Colon.p ++ element)

  lazy val element = Apply(parseElement)

  private def parseElement(context: CharacterStream): Option[JSONTree] = {
    literal.parse(context)
      .orElse(array.parse(context))
      .orElse(objekt.parse(context))
  }

}

/// Returns a combinator applying `choices` and returning the first successful result.
def anyOf[C, E](choices: Seq[Combinator[C, E]]): Apply[C, E] = {
  def parse(context: C, choices: Seq[Combinator[C, E]]): Option[E] = {
    if (choices.isEmpty) { return None }
    val a = choices.head.parse(context)
    return if (a.isDefined) { a } else { parse(context, choices.drop(1)) }
  }
  return Apply((context) => parse(context, choices))
}

extension (self: Char) {

  /// `true` iff `self` is a decimal digit.
  def isDecDigit: Boolean = {
    0x30 to 0x39 contains self.toInt
  }

}