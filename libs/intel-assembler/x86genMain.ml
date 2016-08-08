open X86genLexer


let pattern_of_token i = function
  | OPCODE _ -> Printf.sprintf "OPCODE opcode%d" i
  | REG_INT _ -> "REG_INT reg"
  | REG_REG -> "REG_REG"
  | REX_W -> "REX_W"
  | PLUS -> "PLUS"
  | RB -> "RB"
  | RW -> "RW"
  | RD -> "RD"
  | IB -> "IB"
  | IW -> "IW"
  | ID -> "ID"
  | IO -> "IO"
  | I -> "I"
  | REX -> "REX"

  | INS string -> "INS ins"

  | OP_IMM8 -> "OP_IMM8"
  | OP_IMM16 -> "OP_IMM16"
  | OP_IMM32 -> "OP_IMM32"
  | OP_IMM64 -> "OP_IMM64"
  | OP_R8 -> "OP_R8"
  | OP_R16 -> "OP_R16"
  | OP_R32 -> "OP_R32"
  | OP_R64 -> "OP_R64"

  | OP_RM8 -> "OP_RM8"
  | OP_RM16 -> "OP_RM16"
  | OP_RM32 -> "OP_RM32"
  | OP_RM64 -> "OP_RM64"

  | OP_AL -> "OP_AL"
  | OP_AX -> "OP_AX"
  | OP_EAX -> "OP_EAX"
  | OP_RAX -> "OP_RAX"
  | OP_STAR -> "OP_STAR"
  | OP_M16INT -> "OP_M16INT"
  | OP_M32INT -> "OP_M32INT"
  | OP_M64INT -> "OP_M64INT"

  | OP_M32FP  -> "OP_M32FP"
  | OP_M64FP  -> "OP_M64FP"
  | OP_M80FP  -> "OP_M80FP"
  | OP_ST0  -> "OP_ST0"
  | OP_STi  -> "OP_STi"
  | OP_XMM -> "OP_XMM"
  | OP_XMM1 -> "OP_XMM1"
  | OP_XMM2_M64 -> "OP_XMM2_M64"
  | OP_XMM2_M128 -> "OP_XMM2_M128"

  | COMMA -> "COMMA"

  | VV -> "VV"
  | VNE -> "VNE"
  | SSE2 -> "SSE2"

  | OPS_M -> "OPS_M"
  | OPS_O -> "OPS_O"
  | OPS_I -> "OPS_I"
  | OPS_R -> "OPS_R"
  | OPS_NP -> "OPS_NP"

  | VALID -> "VALID"
  | N_E   -> "N_E"

  | EOL -> "EOL"
  | EOF -> "EOF"

type pat =
  | S of string
  | V

let _ =
  let files = Sys.readdir "." in

  let all_files = ref [] in
  Array.iter (fun file ->
    if Filename.check_suffix file ".x86" then
      let ic = open_in file in
      X86genLexer.init ();
      try
        let lexbuf = Lexing.from_channel ic in
        let rec iter line lines =
          match token lexbuf with
          | EOF ->
            let lines = match line with
                [] -> lines
              | _ -> (List.rev line) :: lines
            in
            List.rev lines
          | EOL -> begin
              match line with
                [] -> iter [] lines
              | _ -> iter [] ( (List.rev line) :: lines)
            end
          | tok -> iter (tok :: line) lines
        in
        all_files := (file, iter [] []) :: !all_files;
        close_in ic
      with SyntaxError (lexeme, line_num) ->
        Printf.eprintf "Error: Lexing error at line %d, file %S, lexeme %S\n%!"
          line_num file lexeme;
        exit 2
  ) files;

  let h = Hashtbl.create 111 in

  let add_ins comment ins pats vs =
    try
      let (counter, patterns, lines) = Hashtbl.find h ins in
      incr counter;
      patterns := (!counter, pats) :: !patterns;
      lines := (!counter, vs, comment) :: !lines
    with Not_found ->
      let counter = ref 0 in
      let patterns = ref [ 0, pats ] in
      let lines = ref [ 0, vs, comment] in
      Hashtbl.add h ins (counter, patterns, lines)
  in

  List.iter (fun (file, lines) ->
    List.iter (fun line ->
      let ins =
        let instr = ref None in
        List.iter (function
            INS ins -> instr := Some ins
          | _ -> ()) line;
        match !instr with
        | None -> assert false
        | Some ins -> ins
      in
      let comment =
        let b = Buffer.create 100 in
        Printf.bprintf b "(*";
        List.iter (fun tok ->
          Printf.bprintf b " %s" (string_of_token tok)
        ) line;
        Printf.bprintf b " *)";
        Buffer.contents b
      in

      match line with
      | [ OPCODE opcode0; OPCODE opcode1; PLUS; I; INS ins; OP_STi; VALID; VALID; ] ->
        add_ins comment ins
          [S "  | [Regf (ST i)] -> buf_opcodes b [ "; V;S "; "; V;S " + i ]" ]
          [opcode0; opcode1]

      | [ OPCODE opcode0; REG_INT reg; INS ins; OP_M32FP; VALID; VALID; ] ->
        add_ins comment ins
          [ S "  | [Mem ( (REAL4|DWORD), _) as rm] -> emit_mod_rm_reg b 0 ["; V; S "] rm "; V; S "" ]
          [opcode0; string_of_int reg];

      | [ OPCODE opcode0; REG_INT reg; INS ins; OP_M64FP; VALID; VALID; ] ->
        add_ins comment ins
          [ S "  | [Mem ( (REAL8|QWORD), _) as rm] -> emit_mod_rm_reg b 0 ["; V; S "] rm "; V; S "" ]
          [opcode0; string_of_int reg]

      | [ OPCODE opcode0; REG_INT reg; INS ins; OP_M80FP; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [Mem ( (REAL10|WORD), _) as rm] -> emit_mod_rm_reg b 0 ["; V; S "] rm "; V; S "" ]
          [opcode0; string_of_int reg]

      | [ OPCODE opcode0; OPCODE opcode1; INS ins; VALID; VALID; ] ->
        add_ins comment ins
          [ S "  | [] -> buf_opcodes b [ "; V; S "; "; V; S " ]" ]
          [ opcode0; opcode1]

      | [ OPCODE opcode0; OPCODE opcode1; PLUS; I; INS ins; OP_STi; COMMA; OP_ST0; VALID; VALID; ] ->
        add_ins comment ins
          [ S "  | [ Regf (ST i); Regf (ST 0) ] -> buf_opcodes b [ "; V; S "; "; V; S "+i ]" ]
          [ opcode0; opcode1]

      | [ OPCODE opcode0; OPCODE opcode1; PLUS; I; INS ins; OP_ST0; COMMA; OP_STi; VALID; VALID; ] ->
        add_ins comment ins
          [ S "  | [ Regf (ST 0); Regf (ST i) ] -> buf_opcodes b [ "; V; S "; "; V; S "+i ]" ]
          [ opcode0; opcode1]

      | [ OPCODE opcode0; REG_REG; INS ins; OP_R32; COMMA; OP_RM32; OPS_R; OPS_M; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [Reg32 reg; (Reg32 _ | Mem _ as rm)] ->  emit_mod_rm_reg b no_rex ["; V; S "] rm (rd_of_reg32 reg)" ]
          [ opcode0 ]

      | [ REX_W; PLUS; OPCODE opcode2; REG_REG; INS ins; OP_R64; COMMA; OP_RM64; OPS_R; OPS_M; VALID; N_E; ] ->
        (* REX.W + 03 /r ADD r64 , r/m64 R M Valid N.E. *)
        add_ins comment ins
     [ S "  | [ Reg64 reg; (Mem _  as rm) ] -> emit_mod_rm_reg b rexw [ "; V; S "] rm (rd_of_reg64 reg)" ]
          [ opcode2 ]

      | [ OPCODE opcode0; REG_INT reg; INS ins; OP_RM32; OPS_M; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [Reg32 _ | Mem (DWORD, _)  as rm] -> emit_mod_rm_reg b no_rex ["; V; S "] rm "; V; S "" ]
          [opcode0; string_of_int reg]

      | [ REX_W; PLUS; OPCODE opcode2; REG_INT reg; INS ins; OP_RM64; OPS_M; VALID; N_E; ] ->
        add_ins comment ins
     [ S "  | [Reg64 _ | Mem (QWORD, _)  as rm] -> emit_mod_rm_reg b rexw ["; V; S "] rm "; V; S "" ]
          [opcode2; string_of_int reg]

      | [ OPCODE opcode0; REG_INT reg; INS ins; OP_RM8; OPS_M; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg8 _ | Mem (BYTE, _) as rm ] -> emit_mod_rm_reg b no_rex ["; V; S "] rm "; V; S "" ]
          [opcode0; string_of_int reg]

      | [ OPCODE opcode0; IB; INS ins; OP_AL; COMMA; OP_IMM8; OPS_I; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg8 AL; Imm (B8, (None,imm)) ] -> buf_opcodes b ["; V; S "]; buf_int8L b imm " ]
          [ opcode0 ]

      | [ OPCODE opcode0; IW; INS ins; OP_AX; COMMA; OP_IMM16; OPS_I; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg16 AX; Imm ((B8|B16), (None,n)) ] -> buf_opcodes b [0x66;"; V; S "]; buf_int16L b n " ]
          [ opcode0 ]

      | [ OPCODE opcode0; ID; INS ins; OP_EAX; COMMA; OP_IMM32; OPS_I; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg32 (R32 RAX); Imm ((B8|B16|B32), n) ] -> buf_opcodes b ["; V; S "]; buf_int32_imm b n " ]
          [ opcode0 ]

      | [ OPCODE opcode0; REG_INT reg; INS ins; OP_RM16; OPS_M; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg16 _ | Mem (WORD, _ ) as rm ] -> emit_mod_rm_reg b no_rex [0x66; "; V; S "] rm "; V; S "" ]
          [opcode0; string_of_int reg]

      | [ OPCODE opcode0; REG_INT reg; INS ins; OP_M32INT; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Mem(DWORD, _ ) as rm ] -> emit_mod_rm_reg b no_rex ["; V; S "] rm "; V; S "" ]
          [opcode0; string_of_int reg]

      (* TODO: Does this really work ? shall we use an 0x66 prefix in 64 bit mode *)
      | [ OPCODE opcode0; REG_INT reg; INS ins; OP_M16INT; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Mem(WORD, _ ) as rm ] -> emit_mod_rm_reg b no_rex ["; V; S "] rm "; V; S "" ]
          [opcode0; string_of_int reg]

      | [ OPCODE opcode0; OPCODE opcode1; OPCODE opcode2; REG_REG; INS ins; OP_XMM1; COMMA; OP_XMM2_M64; OPS_R; OPS_M; VV; SSE2; ] ->
        add_ins comment ins
     [ S "  | [Regf reg, (Regf _ | Mem _  as rm)] -> buf_int8 b "; V; S "; emit_mod_rm_reg b 0 [ "; V; S "; "; V; S " ] rm (rd_of_regf reg)" ]
          [ opcode0; opcode1; opcode2]

      | [ OPCODE opcode0; REG_INT reg; IB; INS ins; OP_RM8; COMMA; OP_IMM8; OPS_M; OPS_I; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  |  [ Reg8 _ | Mem(BYTE, _) as rm; Imm(B8, (None,n)) ] -> emit_mod_rm_reg b no_rex ["; V; S "] rm "; V; S "" ]
          [opcode0; string_of_int reg]

      | [ REX_W; PLUS; OPCODE opcode2; ID; INS ins; OP_RAX; COMMA; OP_IMM32; OPS_I; VALID; N_E; ] ->
        add_ins comment ins
     [ S "  |  [ Reg64 RAX; Imm((B8|B16|B32), imm32) ] -> emit_rex b rexw; buf_opcodes b ["; V; S "]; buf_int32_imm b imm32" ]
          [ opcode2 ]

      | [ OPCODE opcode0; REG_INT reg; IW; INS ins; OP_RM16; COMMA; OP_IMM16; OPS_M; OPS_I; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg16 _ | Mem(WORD,_) as rm; Imm ((B8|B16), (None,imm16))] -> emit_mod_rm_reg b no_rex [0x66;"; V; S "] rm "; V; S "; buf_int16L b imm16" ]
          [opcode0; string_of_int reg]

      | [ OPCODE opcode0; REG_INT reg; ID; INS ins; OP_RM32; COMMA; OP_IMM32; OPS_M; OPS_I; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg32 _ | Mem(DWORD,_) as rm; Imm ((B8|B16|B32), imm32)] -> emit_mod_rm_reg b no_rex ["; V; S "] rm "; V; S "; buf_int32_imm b imm32" ]
          [opcode0; string_of_int reg]

      | [ REX_W; PLUS; OPCODE opcode2; REG_INT reg; ID; INS ins; OP_RM64; COMMA; OP_IMM32; OPS_M; OPS_I; VALID; N_E; ] ->
        add_ins comment ins
     [ S "  | [ Reg64 _ | Mem(QWORD,_) as rm; Imm ((B8|B16|B32), imm32)] -> emit_mod_rm_reg b rexw ["; V; S "] rm "; V; S "; buf_int32_imm b imm32" ]
          [opcode2; string_of_int reg]

      | [ OPCODE opcode0; REG_INT reg; IB; INS ins; OP_RM16; COMMA; OP_IMM8; OPS_M; OPS_I; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg16 _ | Mem(WORD,_) as rm; Imm (B8, (None,imm8))] -> emit_mod_rm_reg b no_rex [0x66;"; V; S "] rm "; V; S "; buf_int8L b imm8" ]
          [opcode0; string_of_int reg]

      | [ OPCODE opcode0; REG_INT reg; IB; INS ins; OP_RM32; COMMA; OP_IMM8; OPS_M; OPS_I; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg32 _ | Mem(DWORD,_) as rm; Imm (B8, (None,imm8))] -> emit_mod_rm_reg b no_rex ["; V; S "] rm "; V; S "; buf_int8L b imm8" ]
          [opcode0; string_of_int reg]

      | [ REX_W; PLUS; OPCODE opcode2; REG_INT reg; IB; INS ins; OP_RM64; COMMA; OP_IMM8; OPS_M; OPS_I; VALID; N_E; ] ->
        add_ins comment ins
     [ S "  | [ Reg64 _ | Mem(QWORD,_) as rm; Imm (B8, (None,imm8))] -> emit_mod_rm_reg b rexw ["; V; S "] rm "; V; S "; buf_int8L b imm8" ]
          [opcode2; string_of_int reg]

      | [ OPCODE opcode0; REG_REG; INS ins; OP_RM8; COMMA; OP_R8; OPS_M; OPS_R; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg8 _ | Mem(BYTE,_) as rm; Reg8 r8] -> emit_mod_rm_reg b no_rex ["; V; S "] rm (rd_of_reg8 r8)" ]
          [ opcode0 ]

      | [ OPCODE opcode0; REG_REG; INS ins; OP_RM16; COMMA; OP_R16; OPS_M; OPS_R; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg16 _ | Mem(WORD,_) as rm; Reg16 r16] -> emit_mod_rm_reg b no_rex [0x66; "; V; S "] rm (rd_of_reg16 r16)" ]
          [ opcode0 ]

      | [ OPCODE opcode0; REG_REG; INS ins; OP_RM32; COMMA; OP_R32; OPS_M; OPS_R; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg32 _ | Mem(DWORD,_) as rm; Reg32 r32] -> emit_mod_rm_reg b no_rex ["; V; S "] rm (rd_of_reg32 r32)" ]
          [ opcode0 ]

      | [ REX_W; PLUS; OPCODE opcode2; REG_REG; INS ins; OP_RM64; COMMA; OP_R64; OPS_M; OPS_R; VALID; N_E; ] ->
        add_ins comment ins
     [ S "  | [ Reg64 _ | Mem(QWORD,_) as rm; Reg64 r64] -> emit_mod_rm_reg b rexw ["; V; S "] rm (rd_of_reg64 r64)" ]
          [ opcode2 ]

      | [ OPCODE opcode0; REG_REG; INS ins; OP_R8; COMMA; OP_RM8; OPS_R; OPS_M; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg8 r8; Reg8 _ | Mem(BYTE,_) as rm ] -> emit_mod_rm_reg b no_rex ["; V; S "] rm (rd_of_reg8 r8)" ]
          [ opcode0 ]

      | [ OPCODE opcode0; REG_REG; INS ins; OP_R16; COMMA; OP_RM16; OPS_R; OPS_M; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [ Reg16 r16; Reg16 _ | Mem(WORD,_) as rm ] -> emit_mod_rm_reg b no_rex [0x66; "; V; S "] rm (rd_of_reg16 r16)" ]
          [ opcode0 ]

      | [ REX_W; PLUS; OPCODE opcode2; OPCODE opcode3; PLUS; RD; INS ins; OP_R64; OPS_O; VALID; N_E; ] ->
        add_ins comment ins
     [ S "  | [Reg64 reg] -> let reg = rd_of_reg64 reg in\n    if reg > 7 then emit_rex b (rexw lor (rex_opcode reg));\n    buf_opcodes b [ "; V; S "; "; V; S " + reg7 reg ]" ]
          [opcode2; opcode3]

      | [ OPCODE opcode0; OPCODE opcode1; PLUS; RD; INS ins; OP_R32; OPS_O; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [Reg32 reg] -> buf_opcodes b [ "; V; S "; "; V; S " + reg7 (rd_of_reg32 reg) ]" ]
          [ opcode0; opcode1]

      | [ OPCODE opcode0; OPCODE opcode1; OPCODE opcode2; REG_REG; INS ins; OP_XMM1; COMMA; OP_XMM2_M128; OPS_R; OPS_M; VV; SSE2; ] ->
        add_ins comment ins
     [ S " | Regf reg, (Regf _ | Mem _  as rm) -> emit_mod_rm_reg b no_rex ["; V; S ";"; V; S ";"; V; S "] rm (rd_of_regf reg)" ]
          [ opcode0; opcode1; opcode2 ]

      | [ OPCODE opcode0; PLUS; RB; IB; INS ins; OP_R8; COMMA; OP_IMM8; OPS_O; OPS_I; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [Reg8 r8; Imm(B8,(None,imm))] -> buf_opcodes b ["; V; S " + (reg7 (rd_of_reg8 r8))]; buf_int8L b imm" ]
          [ opcode0 ]

      | [ OPCODE opcode0; PLUS; RW; IW; INS ins; OP_R16; COMMA; OP_IMM16; OPS_O; OPS_I; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [Reg16 r16; Imm((B8|B16),(None,n))] -> buf_opcodes b [0x66; "; V; S " + (reg7 (rd_of_reg16 r16))]; buf_int16L b n" ]
          [ opcode0 ]

      | [ OPCODE opcode0; PLUS; RD; ID; INS ins; OP_R32; COMMA; OP_IMM32; OPS_O; OPS_I; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  | [Reg32 r32; Imm( (B8|B16|B32), iL)] -> buf_opcodes b ["; V; S " + (reg7 (rd_of_reg32 r32))]; buf_int32_imm b iL" ]
          [ opcode0 ]

      | [ REX_W; PLUS; OPCODE opcode2; PLUS; RD; IO; INS ins; OP_R64; COMMA; OP_IMM64; OPS_O; OPS_I; VALID; N_E; ] ->
        add_ins comment ins
     [ S "  | [Reg64 r64; Imm( (B8|B16|B32|B64),imm64)] -> let reg = rd_of_reg64 r64 in emit_rex b (rexw lor (rex_opcode reg) ); buf_opcodes b ["; V; S " + (reg7 reg)]; buf_int64_imm b imm64" ]
          [ opcode2 ]

      | [ OPCODE opcode0; INS ins; OPS_NP; VALID; VALID; ] ->
        add_ins comment ins
     [ S "  |  [] -> buf_int8 b "; V; S "" ]
          [ opcode0 ]

      | _ ->

        let b = Buffer.create 100 in
        Printf.bprintf b "(*\n  | [";
        List.iteri (fun i tok ->
          Printf.bprintf b " %s;" (pattern_of_token i tok)
        ) line;
        Printf.bprintf b " ] -> \n*)\n";

        add_ins comment ins [ S (Buffer.contents b) ] []
    ) lines
  ) !all_files;

  let oc = open_out "intel_assembler.ml" in
  output_string oc "open Intel_proc\n";
  output_string oc "open Intel_more\n";

  let all_ins = ref [] in
  Hashtbl.iter (fun ins (counter, patterns, lines) ->
    all_ins := (ins, List.sort compare !patterns, List.sort compare !lines) :: !all_ins
  ) h;
  let all_ins = List.sort compare !all_ins in

  let rec bprint b pats vs =
    match pats, vs with
    | S s :: pats, _ -> Buffer.add_string b s; bprint b pats vs
    | V :: pats, v :: vs ->
      Buffer.add_string b v; bprint b pats vs
    | V :: _, [] -> assert false
    | [], [] -> ()
    | [], _ :: _ -> assert false
  in

  let common_patterns = Hashtbl.create 13 in

  List.iter (fun (ins, patterns, lines) ->
    begin
      try
        let (ins2, used) = Hashtbl.find common_patterns patterns in
        Printf.eprintf "%S and %S share the same assembling sequence\n%!"
          ins ins2;
        used := ins :: !used
      with Not_found ->
        Hashtbl.add common_patterns patterns (ins, ref [ins])
    end;
  ) all_ins;

  List.iter (fun (ins, patterns, lines) ->
    let b = Buffer.create 100 in
    let (ins2, used) = Hashtbl.find common_patterns patterns in
    begin match List.rev !used with
      | [] -> assert false
      | [ _ ] ->
        Printf.bprintf b "\nlet emit_%s b = function\n" ins;
        List.iter2 (fun (count1, pats) (count2, vs, comment) ->
          assert (count1 = count2);
          Buffer.add_string b comment;
          Buffer.add_string b "\n";
          bprint b pats (List.map (Printf.sprintf "0x%s") vs);
          Buffer.add_string b "\n";
        ) patterns lines;
        Printf.bprintf b "  | _ -> assert false\n";
      | ins2 :: others ->
        if ins = ins2 then begin
          Printf.bprintf b
            "\n(* Generic implementation of %s, shared with %s *)"
            ins (String.concat "," others);
          Printf.bprintf b "\nlet emit_%s_gen (" ins;
          let c = ref 0 in
          let first = ref true in
          List.iter2 (fun (count1, pats) (count2, vs, comment) ->
            assert (count1 = count2);
            List.iter (function _n ->
              incr c;
              if !first then first := false else Buffer.add_string b ",";
              Printf.bprintf b "arg%d" !c;
            ) vs;
          ) patterns lines;
          Buffer.add_string b ") b = function\n";
          let c = ref 0 in
          List.iter2 (fun (count1, pats) (count2, vs, comment) ->
            assert (count1 = count2);
            let vs = List.map (function _n ->
                incr c;
                Printf.sprintf "arg%d" !c;
              ) vs in
            Buffer.add_string b comment;
            Buffer.add_string b "\n";
            bprint b pats vs;
            Buffer.add_string b "\n";
          ) patterns lines;
          Printf.bprintf b "  | _ -> assert false\n";
        end;
        Printf.bprintf b "\nlet emit_%s = emit_%s_gen (\n" ins ins2;
        let first = ref true in
        List.iter2 (fun (count1, pats) (count2, vs, comment) ->
          assert (count1 = count2);
          Buffer.add_string b comment;
          Buffer.add_string b "\n  ";
          List.iter (function n ->
            if !first then first := false else Buffer.add_string b ",";
            Buffer.add_string b "0x";
            Buffer.add_string b n;
          ) vs;
          Buffer.add_string b "\n";
        ) patterns lines;

        Printf.bprintf b ")\n"
    end;
    output_string oc (Buffer.contents b);
  ) all_ins;
  close_out oc

