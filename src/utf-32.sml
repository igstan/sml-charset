structure UTF32 :> CODEC =
  struct
    exception Malformed

    fun encode endianness reader stream = raise Fail "not implemented"

    fun decode endianness reader stream =
      let
        fun combine a b c d =
          let
            open Word infix orb <<
          in
            case endianness of
              Endian.Big => (a << 0w24) orb (b << 0w16) orb (c << 0w8) orb d
            | Endian.Lit => (d << 0w24) orb (c << 0w16) orb (b << 0w8) orb a
          end

        val word = Reader.map (Word.fromInt o Word8.toInt) reader
      in
        case Reader.group 4 word stream of
          NONE => NONE
        | SOME (words, stream) =>
            case Vector.toList words of
              [a, b, c, d] => SOME (combine a b c d, stream)
            | _ => NONE
      end
  end
