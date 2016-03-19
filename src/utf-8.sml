structure UTF8 :> CODEC =
  struct
    exception Malformed

    fun encode endianness reader stream = raise Fail "not implemented"

    fun decode endianness reader stream =
      let
        infix & <<

        val op & = Word.andb
        val op << = Word.<<

        fun combine bytes =
          case bytes of
            [a, b, c, d] =>
              ((a & 0wx07) << 0w18) +
              ((b & 0wx3F) << 0w12) + ((c & 0wx3F) << 0w06) + (d & 0wx3F)
          | [a, b, c] =>
              ((a & 0wx0F) << 0w12) + ((b & 0wx3F) << 0w06) + (c & 0wx3F)
          | [a, b] =>
              ((a & 0wx1F) << 0w06) + (b & 0wx3F)
          | [a] => a
          | _ => raise Malformed

        val word = Reader.map (Word.fromInt o Word8.toInt) reader

        fun continuationByte min max stream =
          case word stream of
            NONE => raise Malformed
          | SOME (byte, stream) =>
              if min <= byte andalso byte <= max
              then SOME (byte, stream)
              else raise Malformed

        fun continuationBytes byte stream =
          let
            val cont80BF = continuationByte 0wx80 0wxBF
            val contRest =
              (* See Table 3-7, Well-Formed UTF-8 Byte Sequences. *)
              case byte of
                0wxE0 => [continuationByte 0wxA0 0wxBF, cont80BF]
              | 0wxED => [continuationByte 0wx80 0wx9F, cont80BF]
              | 0wxF0 => [continuationByte 0wx90 0wxBF, cont80BF, cont80BF]
              | 0wxF4 => [continuationByte 0wx80 0wx8F, cont80BF, cont80BF]
              | byte =>
                  if 0wxC2 <= byte andalso byte <= 0wxDF
                  then [cont80BF]
                  else
                    if 0wxE0 <= byte andalso byte <= 0wxEF
                    then [cont80BF, cont80BF]
                    else
                      if 0wxF0 <= byte andalso byte <= 0wxF4
                      then [cont80BF, cont80BF, cont80BF]
                      else raise Malformed
          in
            Reader.seq contRest stream
          end
      in
        case word stream of
          NONE => NONE
        | SOME (byte, stream) =>
            if byte <= 0wx7F
            then SOME (byte, stream)
            else
              case continuationBytes byte stream of
                NONE => raise Malformed
              | SOME (bytes, stream) => SOME (combine (byte :: bytes), stream)
      end
  end
