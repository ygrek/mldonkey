open Ocamlbuild_plugin
open Command

let headers = [
  "config/config.h";
  "src/utils/lib/os_stubs.h";
  "src/utils/lib/tiger.h";
  "src/utils/lib/md4.h";
  "src/utils/lib/md5.h";
  "src/utils/lib/sha1_c.h";
  "src/networks/direct_connect/che3_c.h"
  ]

let pp name dep prod cmd =
    rule name ~dep ~prod (fun env _build ->
      let src = env dep in
      let dst = env prod in
      Cmd (S (cmd @ [P src; T (tags_of_pathname src); Sh ">"; Px dst])))

;;

dispatch begin function
(*
| Before_options ->

      Options.ocamlmklib := S[A"ocamlmklib";A"-v"]
*)

| Before_rules ->

      (* preprocessing *)
      pp "cpp: mlcpp -> ml" "%.mlcpp" "%.ml" [A"cpp";A"-P"];
      pp "camlp4o: ml4 -> ml" "%.ml4" "%.ml" [A"camlp4o";A"pa_o.cmo";A"pa_op.cmo";A"pr_o.cmo";A"-impl"];
      pp "md4.c -> md4_cc.c" "src/utils/lib/md4.c" "src/utils/lib/md4_cc.c" [A"cat"];

| After_rules ->

      (* C stubs *)
      let libos_stubs = "src/utils/lib" / "libos_stubs." ^ !Options.ext_lib in
      let libs = [libos_stubs;"-lz";"-pthread";"-lbz2";"-lgd"] in
      let cclibs = List.fold_left (fun acc x -> A"-cclib"::A x::acc) [] libs in
      flag ["link"; "ocaml"; "use_os"] (S cclibs);
      dep  ["link"; "ocaml"; "use_os"] [libos_stubs];

      (* ensure C headers are copied to _build *)
      dep ["compile"] headers;

      (* static *)
      flag ["link"; "ocaml"; "byte"] (A"-custom");

      (* plugins get linked in as cma with linkall *)
      flag ["link";"ocaml";"program"] (A"-linkall");
      ocaml_lib "bittorrent";
      ocaml_lib "direct_connect";

| _ -> ()
end

