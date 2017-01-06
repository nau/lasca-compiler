Torrent b-encoding
======
package Bencoding

data Bencoding = 
    Num(n: Int)
  | Bytes(bytes: [Byte])
  | BList(list: [Bencoding])
  | BDict(map: Map(Bytes, Bencoding)) 
  

private val l = "l".getBytes StandardCharsets.US_ASCII
private val d = "d".getBytes StandardCharsets.US_ASCII
private val e = "e".getBytes StandardCharsets.US_ASCII
private val colon = ":".getBytes StandardCharsets.US_ASCII

def putIn(bytes: [Byte], start: String, end: String): [Bytes] = 
  start.getBytes StandardCharsets.US_ASCII ++ bytes ++ (end.getBytes StandardCharsets.US_ASCII)
  
  
def encode(be: Bencoding): [Byte] = {
  Num n =>
    val strint = n.toString
    "i${strint}e".getBytes StandardCharsets.US_ASCII
  Bytes xs => 
    val len = xs.length.toString.getBytes StandardCharsets.US_ASCII
    len ++ colon ++ xs
  BList xs =>  
    val bytes = xs.flatMap encode
    l ++ bytes ++ e
  BDict xs => 
    val bytes = xs.flatMap((k, v) => k.encode ++ v.encode)
    d ++ bytes ++ e
}

type Result = Result(String, Bencoding)    

private readInt(xs: [Byte]): Result(String, (Int, [Byte]) = {
  val len = xs.length
  var i = 0
  while i < len and xs(i) >= '0'.getByte and xs(i) <= 9.getByte {
    i :+= 1
  }
  val bytes = xs.take(i).toString
  if bytes.isNumeric then Ok (Int.read bytes, xs.drop i) 
  else Err "Can't read bytes len: $bytes"
}
    
def decode(bytes: [Byte]): Result = {     
  [] => Err "Fuck you"
  [b] => Err "Fuck you too"
  [`i`, xs*] => readInt(xs).flatMap((n, xs) =>
  	  xs match {
  	    [`e`, _] => Ok (Num n, xs.drop 1)
  	    _ => Err "Expected end of Int $xs"
  	  }
  	}
  [`l`, xs*] => 
  	val res = decode(xs) 
  [`d`, xs*] => 	
  [xs*] if xs.length > 0 and xs.head.isDigit => do {
    (len, xs) <- readInt xs
    ret Ok(Bytes(xs.take len), xs.drop len)
  }
  
  xs => xs.decodeInt.orElse xs.decodeBytes
    decodeInt(xs).orElse(decodeBytes(xs)).orElse(dec)   
  x.toString match
     "i" => decodeInt xs
}