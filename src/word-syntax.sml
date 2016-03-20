structure WordSyntax =
  struct
    val op & = Word.andb
    val op << = Word.<<
    val op >> = Word.>>
    val op orb = Word.orb
  end
