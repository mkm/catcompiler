signature Compiler =
sig

  exception Error of string*(int*int)

  val compile : Cat.Prog -> Mips.mips list

end
