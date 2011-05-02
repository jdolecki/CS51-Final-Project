open BDD
open String

(* Adapted from http://stackoverflow.com/questions/1733311/pretty-print-a-tree 
 * for Ocaml instead of F#
 * Thank you to Brian for the original post.
 *)
let nSpaces (i : int) : string  = 
  let n = String.create i in 
  let _ =  String.fill n 0 i ' ' in n;;

let nBars (i : int) : string  = 
  let n = String.create i in 
  let _ =  String.fill n 0 i '_' in n;;

let rec init (i : int): string list = 
  if i = 0 then []
  else "" :: init (i - 1)


let rec prettyandwidthinfo (t: BDD.bdd) : string list * int * int * int = 
  match t with
    | BDD.Zero -> (["Zero"], length "Zero", 0, (length "Zero") -1)
    | BDD.One -> (["One"], length "One", 0, (length "One") -1)
    | BDD.Node (i, low, high) -> 
        let s = string_of_int i in
        let sw = length s in
        let swl = sw / 2 in
        let swr = (sw - 1) / 2 in
        let lp, lw, _, lc = prettyandwidthinfo low in
        let rp, rw, rc, _ = prettyandwidthinfo high in
        let lw, lb = if lw = 0 then 1, " " else lw, "/" in
        let rw, rb = if rw = 0 then 1, " " else rw, "\\" in
        let totalLeftWidth = (max (max lw swl) 1) in
        let totalRightWidth = (max (max rw swr) 1) in
        let w = totalLeftWidth + 1 + totalRightWidth in
        let rc2 = totalLeftWidth + 1 + rc in
        let lp = if List.length lp < List.length rp 
        then let diff = (List.length rp) - (List.length lp) in
          lp @ (init diff) else lp in
        let rp = if List.length rp < List.length lp 
        then let diff = (List.length lp) - (List.length rp) in 
          rp @ (init diff) else rp in
        let lp = List.map 
          (fun s -> if length s < totalLeftWidth then 
             let diff = totalLeftWidth - (length s) in 
             let n = String.create diff in 
             let _ = String.fill n 0 diff ' ' in n ^ s else s) lp in
        let rp = List.map 
          (fun s -> if length s < totalRightWidth 
           then let diff = totalRightWidth - (length s) in
           let n = String.create diff in
           let _ = String.fill n 0 diff ' ' in s ^ n else s) rp in 
        let line1 = if swl < lw - lc - 1 then
          (nSpaces (lc + 1)) ^ (nBars (lw - lc - swl)) ^ s
        else (nSpaces (totalLeftWidth - swl)) ^ s in
        let line1 = if rc2 > length line1 then
          line1 ^ (nBars (rc2 - (length line1)))
        else line1 in
        let line1 = line1 ^ (nSpaces (w - (length line1))) in
        let line2 = (nSpaces (totalLeftWidth - lw + lc)) ^ lb in
        let line2 = line2 ^ (nSpaces (totalLeftWidth - (length line2))) in 
        let line2 = line2 ^ " " ^ (nSpaces rc) ^ rb in
        let line2 = line2 ^ (nSpaces (w - (length line2))) in 
        let resultLines = (line1) :: (line2) :: 
          (List.map2 (fun l r -> l ^ " " ^ r) lp rp) in
          (resultLines, w, lw - swl, totalLeftWidth + 1 + swr);;

let prettyPrint (b : BDD.bdd) = 
  let (s1, _, _, _) = prettyandwidthinfo b in
  let rec loop m = 
    match m with
      | [] -> let _ = Printf.printf "%s" "\n" in ()
      | hd::tl -> let _ = Printf.printf "%s" (hd ^ "\n") in loop tl
  in loop s1;;
      

