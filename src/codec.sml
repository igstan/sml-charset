structure Endian =
  struct
    datatype t =  Big | Lit
  end

signature CODEC =
  sig
    exception Malformed
    val encode : Endian.t -> (Word.word, 's) Reader.t -> (Word8Vector.vector, 's) Reader.t
    val decode : Endian.t -> (Word8.word, 's) Reader.t -> (Word.word, 's) Reader.t
  end
