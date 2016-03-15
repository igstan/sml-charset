structure UTF32 :> CODEC =
  struct
    exception Malformed

    fun encode endianness reader stream =
      let
        fun split word =
          let
            open Word infix andb >>

            val toByte = Word8.fromInt o Word.toInt
            val a = toByte (word andb 0wx7F000000 >> 0w24)
            val b = toByte (word andb 0wx00FF0000 >> 0w16)
            val c = toByte (word andb 0wx0000FF00 >> 0w08)
            val d = toByte (word andb 0wx000000FF)
            val words =
              case endianness of
                Endian.Big => [a, b, c, d]
              | Endian.Lit => [d, c, b, a]
          in
            Word8Vector.fromList words
          end
      in
        Reader.map split reader stream
      end

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
