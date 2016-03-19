functor UTF32(val endianness : Endian.t) :> CHARSET =
  struct
    exception Malformed

    fun encode reader stream =
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

    fun decode reader stream =
      let
        fun combine a b c d =
          let
            open Word infix orb <<

            fun toWord (a, b, c, d) =
              (a << 0w24) orb (b << 0w16) orb (c << 0w8) orb d

            val bytes =
              case endianness of
                Endian.Big => (a, b, c, d)
              | Endian.Lit => (d, c, b, a)
          in
            toWord bytes
          end

        val word = Reader.map (Word.fromInt o Word8.toInt) reader
      in
        case Reader.grouped 4 word stream of
          NONE => NONE
        | SOME ([a, b, c, d], stream) => SOME (combine a b c d, stream)
        | _ => NONE
      end
  end
