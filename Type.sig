signature Type =
sig

  exception Error of string*(int*int)

  val checkProgram : Cat.Prog -> unit

end
