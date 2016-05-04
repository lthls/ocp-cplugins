(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

open MonitorProtocol

(* wait for (1) the program to terminate + (2) each connection to close *)
let wait = ref 1

let only_commands = ref true
let log = ref stderr
let close_log_on_exit = ref true

let should_exit () =
  decr wait;
  if !wait = 0 then begin
    if !close_log_on_exit then close_out !log;
    Concur.exit ()
  end

let start_watcher_command dir =
  Printf.sprintf "ocp-watcher-client -start-daemon -dbdir %s" dir

let get_port_from_dir watcher_dir =
  let port_file =
    Filename.concat watcher_dir WatcherProtocol.db_socket_unix
  in
  if Sys.file_exists port_file
  then
    (* Server is running *)
    let ic = open_in port_file in
    let res = WatcherServerUtils.load_port ic in
    close_in ic;
    res
  else
    begin
      (* No server is running *)
      ignore (Sys.command (start_watcher_command watcher_dir));
      (* The server may take some time loading the database, but it
         should allow connections to be made much earlier *)
      let res = ref None in
      let count = ref 0 in
      while (!count < 1000 && !res = None)
      do
        if Sys.file_exists port_file
        then
          begin
            let ic = open_in port_file in
            res := WatcherServerUtils.load_port ic;
            close_in ic;
            (* In the unlikely case where there was an error loading port_file,
               prevent from iterating anyway *)
            count := 1000
          end
        else
          incr count
      done;
      !res
    end

let get_watcher_port exe_name =
  (* Assume that exe_name is an absolute path *)
  let rec search dir =
    let watcher_dir = Filename.concat dir WatcherProtocol.db_dir_name in
    if Sys.file_exists watcher_dir && Sys.is_directory watcher_dir
    then get_port_from_dir watcher_dir
    else
      let parent_dir = Filename.basename dir in
      (* Avoid looping on filesystem root *)
      if String.length parent_dir < String.length dir
      then search parent_dir
      else None
  in
  search (Filename.basename exe_name)

let init_watcher exe_name =
  let prog = Filename.basename exe_name in
  (* TODO: support .exe suffix *)
  match prog with
  | "ocamlc" | "ocamlc.opt" | "ocamlopt" | "ocamlopt.opt" ->
    get_watcher_port exe_name
  | _ ->
    (* ocp-watcher only logs compilation commands *)
    None

type comp_kind = Impl | Intf
type link_kind = Exe | Lib | Pack
type backend = Byte | Asm
type compile_phase = Read | Write (* Read is typing/code generation,
                                     Write is file outputting *)

type compile_state =
  { c_source: string;
    c_output: string;
    c_kind: comp_kind;
    c_index: int;
    mutable c_cmis_read: string list;
    mutable c_phase: compile_phase;
    mutable c_written: string list;
  }

type link_data =
  { l_kind: link_kind;
    l_output: string;
    mutable l_read_objs: string list;
    mutable l_written: string list;
  }

let get_backend exe_name =
  match Filename.basename exe_name with
  | "ocamlc" | "ocamlc.opt" -> Byte
  | "ocamlopt" | "ocamlopt.opt" -> Asm
  | _ -> Byte (* Not tracked anyway *)

let get_output output src cflag =
  match output with
  | Some o -> if cflag then o else src
  | None -> src

let get_compile_list args =
  let mk_cs c_source c_output c_kind c_index =
    { c_source;
      c_output;
      c_kind;
      c_index;
      c_cmis_read = [];
      c_phase = Read;
      c_written = [];
    }
  in
  let rec aux acc out intf_suff cflag i = function
    | [] -> acc
    | "-intf-suffix" :: suff :: tail ->
      aux acc out suff cflag (i+2) tail
    | "-impl" :: src :: tail ->
      let cs = mk_cs src (get_output out src cflag) Impl (i+2) in
      aux (cs::acc) (if cflag then None else out) intf_suff cflag (i+2) tail
    | "-intf" :: src :: tail ->
      let cs = mk_cs src (get_output out src cflag) Intf (i+2) in
      aux (cs::acc) out intf_suff cflag (i+2) tail
    | "-c" :: tail ->
      aux acc out intf_suff true (i+1) tail
    | "-o" :: o :: tail ->
      aux acc (Some o) intf_suff cflag (i+2) tail
    | arg :: tail ->
      if Filename.check_suffix arg ".ml" then
        let cs = mk_cs arg (get_output out arg cflag) Impl (i+1) in
        aux (cs::acc) out intf_suff cflag (i+1) tail
      else if Filename.check_suffix arg intf_suff then
        let cs = mk_cs arg (get_output out arg cflag) Intf (i+1) in
        aux (cs::acc) out intf_suff cflag (i+1) tail
      else aux acc out intf_suff cflag (i+1) tail
  in
  aux [] None ".mli" false 0 args

let get_link args =
  let rec aux kind output = function
    | [] -> (kind, output)
    | "-c" :: _ ->
      (* Either compilation only, or incompatible flags *)
      (None, output)
    | "-a" :: tail ->
      begin
        match kind with
        | Some Exe | Some Lib -> aux (Some Lib) output tail
        | _ -> (None, output) (* Incompatible flags *)
      end
    | "-pack" :: tail ->
      begin
        match kind with
        | Some Exe | Some Pack -> aux (Some Pack) output tail
        | _ -> (None, output) (* Incompatible flags *)
      end
    | "-o" :: out :: tail ->
      aux kind (Some out) tail
    | _ :: tail -> aux kind output tail
  in
  let mk_ls l_kind l_output =
    { l_kind; l_output; l_read_objs = []; l_written = [] }
  in
  match aux (Some Exe) None args with
  | Some Exe, None -> Some (mk_ls Exe "a.out")
  | Some _, None -> (* Error *) None
  | Some l_kind, Some l_output -> Some (mk_ls l_kind l_output)
  | None, _ -> None

let send_request req port =
  let open Unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  try
    connect sock (ADDR_INET (inet_addr_loopback, port));
    let oc = out_channel_of_descr sock in
    let ic = in_channel_of_descr sock in
    output_value oc (WatcherProtocol.Req_compiler req);
    flush oc;
    shutdown sock SHUTDOWN_SEND;
    let err = input_binary_int ic in
    let r = (input_value ic : WatcherProtocol.result) in
    close sock;
    (err, r)
  with
  | Unix_error (ENOENT, "connect", "")
  | Unix_error (ECONNREFUSED, "connect", "")
    ->
    begin
      close sock;
      (-1, WatcherProtocol.Server_error (WatcherProtocol.No_server))
    end
  | e -> close sock; raise e

let absolute dir file =
  if Filename.is_relative file then
    Filename.concat dir file
  else file

module Server = Concur.MakeServer(struct

  type server_info = unit
  type info = { mutable pid : int;
                mutable synchronous_mode: bool;
                mutable watcher_port: int option;
                mutable args: string list;
                mutable curdir: string;
                mutable backend: backend;
                mutable compile_list: compile_state list;
                mutable link: link_data option;
              }

  let finalize_compile info =
    match info.compile_list with
    | [] -> assert false
    | cs :: tail ->
      begin
        info.compile_list <- tail;
        let mk_abs_obj obj = (absolute info.curdir obj, None) in
        let watcher_cs =
          {
            WatcherProtocol.cs_source = absolute info.curdir cs.c_source;
            cs_prod = List.map mk_abs_obj cs.c_written;
            cs_dep = List.map mk_abs_obj cs.c_cmis_read;
            cs_cmd = (Array.of_list info.args, info.curdir, cs.c_index);
          }
        in
        let req =
          match cs.c_kind with
          | Impl ->
            WatcherProtocol.Reg_ml watcher_cs
          | Intf ->
            WatcherProtocol.Reg_mli watcher_cs
        in
        let port =
          match info.watcher_port with
          | None -> assert false
          | Some p -> p
        in
        ignore (send_request req port) (* It might be better to check for errors *)
      end

  let connection_info server_info sockaddr =
    { pid = 0;
      synchronous_mode = false;
      watcher_port = None;
      args = [];
      curdir = "";
      backend = Byte;
      compile_list = [];
      link = None;
    }

  let connection_handler con =
    incr wait;
  (* Printf.eprintf "\tConnected\n%!"; *)
    ()

  let message_handler con msg =
    let info = Concur.info con in
    try
      (*      Printf.eprintf "\tMessage received %d %d\n%!"
              msg_id (String.length msg); *)
      let msg = MonitorProtocol.C2S.parse_msg msg in

      begin
        match msg with
        | C2S.MSG_INIT t ->
          info.pid <- t.C2S.MSG_INIT.pid;
          info.synchronous_mode <- t.C2S.MSG_INIT.synchronous_mode <> 0;
          info.watcher_port <- init_watcher t.C2S.MSG_INIT.exe_name;
          info.args <- t.C2S.MSG_INIT.args;
          info.curdir <- t.C2S.MSG_INIT.curdir;
          info.backend <- get_backend t.C2S.MSG_INIT.exe_name;
          info.compile_list <- get_compile_list t.C2S.MSG_INIT.args;
          info.link <- get_link t.C2S.MSG_INIT.args;
        | _ -> ()
      end;
      if info.watcher_port <> None
      then begin
        match msg with
        | C2S.MSG_OPEN t ->
          begin
            let file = t.C2S.MSG_OPEN.filename in
            if info.watcher_port = None then ()
            else
              (* Finding the opening mode:
                 - For most flags, it is simply a matter of checking the
                   right bit
                 - For the access mode, it is encoded in the least significant
                   two digits (in particular, O_RDWR is not O_RDONLY | O_WRONLY
                 Finally, the caml runtime doesn't seem to generate O_RDWR calls,
                 so the corresponding case can be ignored *)
              let accmode = t.C2S.MSG_OPEN.flags mod 4 in
              match accmode with
              | 0 -> (* O_RDONLY *)
                begin
                  begin
                    match info.compile_list with
                    | { c_phase = Write } :: tail -> finalize_compile info
                    | _ -> ()
                  end;
                  match info.compile_list with
                  | [] -> ()
                  | cs :: _ ->
                    if Filename.check_suffix file ".cmi"
                    then
                      cs.c_cmis_read <- file :: cs.c_cmis_read
                end
              | 1 -> (* O_WRONLY *)
                begin
                  let match_output expected actual =
                    let expected =
                      try Filename.chop_extension expected
                      with _ -> expected
                    in
                    let actual =
                      try Filename.chop_extension actual
                      with _ -> ""
                    in
                    expected = actual
                  in
                  begin
                    match info.compile_list with
                    | { c_phase = Write; c_output } :: tail ->
                      if match_output c_output file then ()
                      else finalize_compile info
                    | _ -> ()
                  end;
                  match info.compile_list with
                  | [] -> (* Linking phase *)
                    begin
                      match info.link with
                      | None -> (* No idea how this could happen *) ()
                      | Some l ->
                        if file = l.l_output
                        then l.l_written <- file :: l.l_written
                        (* else (* Maybe an error ? *) () *)
                    end
                  | cs :: tail ->
                    if match_output cs.c_output file
                    then cs.c_written <- file :: cs.c_written
                    (* else (* Maybe an error ? *) () *)
                end
              | 2 -> (* O_RDWR *) assert false
              | _ -> assert false
          end
        | C2S.MSG_EXIT t ->
          begin
            match info.link with
            | None -> ()
            | Some l ->
              let written =
                if List.mem l.l_output l.l_written
                then l.l_written
                else l.l_output :: l.l_written
              in
              let mk_abs_obj obj = (absolute info.curdir obj, None) in
              let watcher_ls =
                { WatcherProtocol.ls_outputs = List.map mk_abs_obj written;
                  ls_objs = List.map mk_abs_obj l.l_read_objs;
                  ls_cmd = (Array.of_list info.args,
                            info.curdir,
                            List.length info.args);
                }
              in
              let req =
                match l.l_kind with
                | Exe -> WatcherProtocol.Reg_exe watcher_ls
                | Pack -> WatcherProtocol.Reg_pack watcher_ls
                | Lib -> WatcherProtocol.Reg_lib watcher_ls
              in
              let port =
                match info.watcher_port with
                | None -> assert false
                | Some p -> p
              in
              ignore (send_request req port)
          end
        | _ -> ()
      end;

      if info.synchronous_mode then
        Concur.send_message con (
          MonitorProtocol.S2C.marshal S2C.MSG_ACK);

    with exn ->
      Printf.eprintf "message_handler: exception %s\n%!"
        (Printexc.to_string exn)

  let disconnection_handler conn_id =
    (*    Printf.eprintf "\tDisconnected\n%!"; *)
    should_exit ()

end)

let main args =
  let loopback = true in
  let port = Server.create ~loopback () in

  Unix.putenv "OCP_WATCHER_PORT" ((string_of_int port) ^ "s");

  Concur.exec args.(0)  args
    (function
    | Unix.WEXITED n->
      should_exit ()
    | _ ->
      should_exit ()

  );
  Concur.main ()

let args = ref []

let arg_anon s = args := s :: !args
let arg_list = Arg.align [
  "-o", Arg.String (fun filename ->
    let oc = open_out filename in
    log := oc;
    close_log_on_exit := true
  ), "FILE Store results in FILE";
  "--all", Arg.Clear only_commands, " Print all messages";
  "--", Arg.Rest arg_anon, "COMMAND Command to call";
]
let arg_usage = String.concat "\n"
  [ "ocp-show-build [OPTIONS] COMMAND";
      "";
    "ocp-show-build can be used to display the OCaml commands called during";
    "a build. The OCaml runtime must support use of CAML_CPLUGINS, and";
    "especially the central_monitor.so plugin must be in use.";
    "";
    "Available options:";
  ]
let  () =
  begin
    try ignore (Sys.getenv "CAML_CPLUGINS") with
      Not_found ->
        let dirname = Filename.dirname Sys.executable_name in
        let plugin = Filename.concat dirname "central_monitor.so" in
        if Sys.file_exists plugin then
          Unix.putenv "CAML_CPLUGINS" plugin
        else begin

          Printf.eprintf "Error: CAML_CPLUGINS should be set with the absolute path to central_monitor.so\n%!";
          exit 2
        end
  end;
  Arg.parse arg_list arg_anon arg_usage;
  match !args with
  | [] ->
    Printf.eprintf "Error: you must specify a command to call\n\n%!";
    Arg.usage arg_list arg_usage;
    exit 2
  | args ->
    main (Array.of_list(List.rev args))
