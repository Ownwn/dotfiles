let () =
  Aspire.Main_cmd.default_main
    {
      lex = None;
      parse = Some Parse.parse;
      desugar = None;
      typecheck = None;
      eval = Some Eval.eval;
    }
    ()
