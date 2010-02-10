
open Format
open Unix


type worker = { 
  sockaddr : sockaddr;
  mutable connected : bool;
  mutable fdin : file_descr;
  mutable fdout : file_descr;
}

type logical_worker = {
  worker : worker;
  wid : int;
}

let workers = ref []
let logical_workers = ref []

let declare_workers a n = 
  let w = { sockaddr = a; connected = false; fdin = stdin; fdout = stdout } in
  workers := w :: !workers;
  for i = 1 to n do 
    logical_workers := { worker = w; wid = i }  :: !logical_workers 
  done


let connect_worker w =
  if not w.connected then begin
    let ic,oc = open_connection w.sockaddr in
    let fdin = descr_of_in_channel ic in
    let fdout = descr_of_out_channel oc in
    w.connected <- true;
    w.fdin <- fdin;
    w.fdout <- fdout
  end

let next_id = let r = ref 0 in fun () -> incr r; !r

let running_tasks = Hashtbl.create 17

let create_job lw ((_,f,a) as task) =
  connect_worker lw.worker;
  let id = next_id () in
  Protocol.Master.send lw.worker.fdout (Protocol.Master.Assign (id, f, a));
  Hashtbl.add running_tasks id (lw, task)

let print_sockaddr fmt = function
  | ADDR_UNIX s -> fprintf fmt "%s" s
  | ADDR_INET (ia, port) -> fprintf fmt "%s:%d" (string_of_inet_addr ia) port

let wait continuation =
  let listen_for_worker w =
    let l,_,_ = select [w.fdin] [] [] 0.1 in
    if l = [] then raise Exit;
    let m = Protocol.Worker.receive w.fdin in
    eprintf "received from %a: %a@." print_sockaddr w.sockaddr
      Protocol.Worker.print m;
    match m with
      | Protocol.Worker.Started _ ->
	  raise Exit
      | Protocol.Worker.Completed (id, r) ->
	  let lw, task = Hashtbl.find running_tasks id in
	  Hashtbl.remove running_tasks id;
	  lw, continuation task r
      | Protocol.Worker.Aborted id ->
	  let lw, task = Hashtbl.find running_tasks id in
	  Hashtbl.remove running_tasks id;
	  lw, [task]
  in
  let rec loop = function
    | [] -> loop !workers
    | w :: wl -> try listen_for_worker w with Exit -> loop wl
  in
  loop !workers

let master ~(f : 'a -> 'b) ~(handle : 'a -> 'b -> 'a list) tasks =
  Master.run
    ~create_job
    ~wait:(fun () -> wait handle)
    !logical_workers tasks;
  List.iter
    (fun w -> Protocol.Master.send w.fdout Protocol.Master.Stop)
    !workers


(* and its instances *)

let map f l =
  let tasks = let i = ref 0 in List.map (fun x -> incr i; !i,"f",x) l in
  let results = Hashtbl.create 17 in (* index -> 'b *)
  master 
    ~f:(fun (_,_,x) -> f x)
    ~handle:(fun (i,_,_) r -> Hashtbl.add results i r; [])
    tasks;
  List.map (fun (i,_,_) -> Hashtbl.find results i) tasks



module Worker = struct

  let computations : (string, (string -> string)) Hashtbl.t = Hashtbl.create 17

  let register_computation = Hashtbl.add computations
    
  type running_task = {
    pid : int;
    file : file_descr;
  }

  open Protocol

  exception ExitOnStop

  let server_fun cin cout =
    printf "new connection@.";
    let fdin = descr_of_in_channel cin in
    let fdout = descr_of_out_channel cout in
    let pids = Hashtbl.create 17 in (* ID -> running_task *)
    let handle_message_from_master _ = 
      let m = Master.receive fdin in
      printf "received: %a@." Master.print m;
      match m with
	| Master.Assign (id, f, a) ->
	    if Hashtbl.mem computations f then begin
	      let f = Hashtbl.find computations f in
	      Worker.send fdout (Worker.Started id);
	      let fin, fout = pipe () in
	      begin match fork () with
		| 0 -> 
		    close fin;
		    (* perform computation *)
		    eprintf "  id %d: computation is running...@." id;
		    let r = f a in
		    let c = out_channel_of_descr fout in
		    output_value c r;
		    eprintf "  id %d: computation done@." id;
		    exit 0
		| pid -> 
		    close fout;
		    let t = { pid = pid; file = fin } in
		    Hashtbl.add pids id t
	      end
	    end else
	      Worker.send fdout (Worker.Aborted id)
	| Master.Kill id ->
	    begin 
	      try
		let t = Hashtbl.find pids id in
		kill t.pid Sys.sigkill;
		Hashtbl.remove pids id
	      with Not_found ->
		() (* ignored Kill *)
	    end
	| Master.Stop ->
	    raise ExitOnStop
    in
    let wait_for_completed_task id t =
      match waitpid [WNOHANG] t.pid with
	| 0, _ -> (* not yet completed *)
	    ()
	| _, WEXITED _ -> (* success FIXME: check return code *)
	    Hashtbl.remove pids id;
	    let c = in_channel_of_descr t.file in
	    let r : string = input_value c in
	    close_in c;
	    Worker.send fdout (Worker.Completed (id, r))
	| _, (WSIGNALED _ | WSTOPPED _) -> (* failure *)
	    Hashtbl.remove pids id;
	    Worker.send fdout (Worker.Aborted id)
    in
    try 
      while true do    
	let l,_,_ = select [fdin] [] [] 1. in
	List.iter handle_message_from_master l;
	Hashtbl.iter wait_for_completed_task pids
      done
    with 
      | End_of_file -> 
	  printf "master disconnected@."; 
	  Hashtbl.iter (fun _ t -> kill t.pid Sys.sigkill) pids;
	  exit 0 
      | ExitOnStop ->
	  ()
      | e -> 
	  printf "anomaly: %s@." (Printexc.to_string e); exit 1

  let compute ?(stop=false) ?(port=51000) () = 
    let sock = socket PF_INET SOCK_STREAM 0 in
    let sockaddr = Unix.ADDR_INET (inet_addr_any, port) in
    setsockopt sock SO_REUSEADDR true;
    bind sock sockaddr;
    listen sock 3;
    if stop then
      let s, _ = Unix.accept sock in 
      let inchan = Unix.in_channel_of_descr s 
      and outchan = Unix.out_channel_of_descr s in 
      server_fun inchan outchan;
      begin try 
	shutdown s SHUTDOWN_ALL
      with e -> 
	eprintf "cannot shutdown socket: %s@." (Printexc.to_string e) 
      end
    else
      while true do
	let (s, _) = Unix.accept sock in 
	match Unix.fork() with
	  | 0 -> 
	      if Unix.fork() <> 0 then exit 0; 
              let inchan = Unix.in_channel_of_descr s 
              and outchan = Unix.out_channel_of_descr s in 
	      server_fun inchan outchan;
              close_in inchan;
              close_out outchan;
              exit 0
	  | id -> 
	      Unix.close s; 
	      ignore(Unix.waitpid [] id)
      done


end

(**** test **************************************************)

(****
let master_test () =
  let ic,oc = open_connection sockaddr in
  at_exit (fun () -> shutdown_connection ic);
  let fdin = descr_of_in_channel ic in
  let fdout = descr_of_out_channel oc in
  let id = ref 0 in
  while true do
    incr id;
    let msg = "hello " ^ string_of_int (Random.int 1000) in
    Protocol.Master.send fdout (Protocol.Master.Assign (!id, msg));
    let l,_,_ = select [fdin] [] [] 1. in
    List.iter
      (fun _ -> 
	 let m = Protocol.Worker.receive fdin in
	 eprintf "received: %a@." Protocol.Worker.print m) l;
  done;
(*   Master.send fdout (Master.Kill 3); *)
(*    *)
  ()



****)
