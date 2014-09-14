exception SandboxedOperation;

local
  structure Dummy = struct end

  fun dummy _ = raise SandboxedOperation
in

  structure BasicIO  = Dummy
  structure BinIO    = Dummy
  structure Callback = Dummy (* sounds risky; let's block it. *)
  structure Dynlib   = Dummy
  structure FileSys  = Dummy
  structure Gdbm     = Dummy
  (* structure Gdimage = Dummy *) (* only can write to stdout *)
  structure Help     = Dummy (* not dangerous, but no need in sandbox *)
  structure Lexing   = Dummy
  structure Meta     = Dummy
  structure Mosml    = Dummy
  structure MySQL    = Dummy
  structure NJ93     = Dummy
  structure Nonstdio = Dummy
  structure Obj      = Dummy
  structure Option   = Dummy
  structure OS       = Dummy
  structure Parsing  = Dummy
  structure Path     = Dummy
  structure Polygdbm = Dummy
  structure PP       = Dummy
  structure Process  = Dummy
  structure Socket   = Dummy
  structure Unix     = Dummy 

  structure TextIO =
  struct
    open TextIO
    val openIn   = dummy
    val closeIn  = dummy

    val openOut    = dummy
    val openAppend = dummy
    val closeOut   = dummy
  end

  val help    = dummy

  (* functions exported from Misc *)
  val printVal         = dummy
  val printDepth       = dummy
  val printLength      = dummy
  val installPP        = dummy
  val liberal          = dummy
  val conservative     = dummy
  val orthodox         = dummy
  val use              = dummy
  val compile          = dummy
  val compileToplevel  = dummy
  val compileStructure = dummy
  val load             = dummy
  val loadOne          = dummy
  val loaded           = dummy
  val loadPath         = dummy
  val quietdec         = dummy
  val verbose          = dummy
  val quotation        = dummy
  val valuepoly        = dummy
  val quit             = dummy

end;

