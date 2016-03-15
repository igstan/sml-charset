structure UTF16 :> CODEC =
  struct
    exception Malformed

    fun surrogatePair endianness word =
      let
        open Word infix andb orb >>
        val code = word - 0wx10000
        val fst16 = 0wxD800 orb (code >> 0w10)
        val snd16 = 0wxDC00 orb (code andb 0wx3FF)
        val a = fst16 >> 0w8
        val b = fst16 andb 0wxFFFF
        val c = snd16 >> 0w8
        val d = snd16 andb 0wxFFFF
      in
        case endianness of
          Endian.Big => [a, b, c, d]
        | Endian.Lit => [b, a, d, c]
      end

    fun basicMultilingualPlane endianness word =
      let
        open Word infix andb >>
        val a = word andb 0wxFF
        val b = word >> 0w8
      in
        case endianness of
          Endian.Big => [a, b]
        | Endian.Lit => [b, a]
      end

    fun encode endianness reader stream =
      case reader stream of
        NONE => NONE
      | SOME (word, stream) =>
        let
          fun toVector words =
            Word8Vector.fromList (List.map (Word8.fromInt o Word.toInt) words)
          val words =
            if word < 0wx10000
            then basicMultilingualPlane endianness word
            else surrogatePair endianness word
        in
          SOME (toVector words, stream)
        end

    fun decode endianness reader stream = raise Fail "not implemented"
  end
