signature CHARSET =
  sig
    exception Malformed
    val encode : (Word.word, 's) Reader.t -> (Word8Vector.vector, 's) Reader.t
    val decode : (Word8.word, 's) Reader.t -> (Word.word, 's) Reader.t
  end
