structure UTF16 :> CODEC =
  struct
    exception Malformed


    fun encode endianness reader stream =
      let
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
      in
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
      end

    structure Surrogate =
      struct
        datatype t =
          Low of word
        | High of word
        | None of word

        fun match word =
          if word >= 0wxDC00 andalso word <= 0wxDFFF
          then Low word
          else
            if word >= 0wxD800 andalso word <= 0wxDBFF
            then High word
            else None word
      end

    fun decode endianness reader stream =
      let
        open Word infix andb orb <<

        fun codeUnit a b =
          case endianness of
              Endian.Big => (a << 0w8) orb b
            | Endian.Lit => (b << 0w8) orb a

        fun combine high low =
          0wx10000 + ((high andb 0wx03FF) << 0w10) + (low andb 0wx03FF)

        val word = Reader.map (Word.fromInt o Word8.toInt) reader
      in
        case Reader.grouped 2 word stream of
          NONE => NONE
        | SOME ([a, b], stream) =>
          let in
            case Surrogate.match (codeUnit a b) of
              Surrogate.Low _ => raise Malformed
            | Surrogate.None word => SOME (word, stream)
            | Surrogate.High high =>
                case Reader.grouped 2 word stream of
                  NONE => raise Malformed
                | SOME ([a, b], stream) =>
                  let in
                    case Surrogate.match (codeUnit a b) of
                      Surrogate.None _ => raise Malformed
                    | Surrogate.High _ => raise Malformed
                    | Surrogate.Low low => SOME (combine high low, stream)
                  end
                | _ => raise Malformed
          end
        | _ => NONE
      end
  end
