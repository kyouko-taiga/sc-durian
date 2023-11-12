val source = "[1, \"a\", [], {\"a\": 1}]"

@main def hello: Unit = {
  val stream = CharacterStream(source)
  println(Grammar.element.parse(stream))
}