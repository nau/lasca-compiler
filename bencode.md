Torrent b-encoding
======
package Bencoding

data Bencoding (n : Nat) = Num Integer
  | Bytes Vec n Byte
  | BList Vec n Bencoding
  | BDict Vec n (Tuple Bytes Bencoding) 
  

@private 
def putIn(bytes: [Byte], start: String, end: String): [Bytes] = 
  start.getBytes StandardCharsets.US_ASCII ++ bytes ++ (end.getBytes StandardCharsets.US_ASCII)
  
def encode(be: Bencoding): [Byte] = match
  Num int =>
    val strint = int.toString
    "i${strint}e".getBytes StandardCharsets.US_ASCII
  Bytes xs => 
    val len = xs.length.toString.getBytes StandardCharsets.US_ASCII
    len ++ [':'.toByte] ++ xs
  BList xs =>  
    val bytes = xs.flatMap encode
    bytes.putIn("l", "e")    
  BDict xs => 
    val bytes = xs.flatMap \(k, v) => k.encode ++ v.encode
    bytes.putIn("d", "e")

type Result = Result String Bencoding

def decodeInt    
    
def decode(bytes: [Byte]): Result = match     
  [] => Err "Fuck you"
  [b] => Err "Fuck you too"
  xs => xs.decodeInt.orElse xs.decodeBytes
    decodeInt(xs).orElse(decodeBytes(xs)).orElse(dec)   
  x.toString match
     "i" => decodeInt xs