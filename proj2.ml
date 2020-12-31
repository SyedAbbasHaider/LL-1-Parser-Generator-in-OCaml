open Proj2_types;;

let remeps x =
  SymbolSet.remove "eps" x;;

let getStartSymbol (g : grammar) : string =
  match g with
  (x,y) -> x;;

let getNonterminals (g : grammar) : string list =
  match g with 
    (a,b) -> let rec joker acc = function
      | [] -> acc
      |(x,y)::t -> joker (x::acc) t in
      joker [] b;;

let getInitFirstSets (g : grammar) : symbolMap =
    let v = getNonterminals g in
    List.fold_left (fun m t -> SMap.add t SymbolSet.empty m) SMap.empty v;;
  
  
let getInitFollowSets (g : grammar) : symbolMap = 
  let m = (getNonterminals g) in
  let n =   List.fold_left (fun m t -> SMap.add t SymbolSet.empty m) SMap.empty m in 
  SMap.add (getStartSymbol g) (SymbolSet.singleton "eof") (n);; 
        

let rec computeFirstSet (first : symbolMap) (symbolSeq : string list) : SymbolSet.t =
  match symbolSeq with
    [] -> SymbolSet.singleton "eps"
  |h::t -> if (SMap.mem h first)
      then let firrst = SMap.find h first in
        (if SymbolSet.mem "eps" firrst
         then SymbolSet.union(remeps firrst)(computeFirstSet first t)
         else SMap.find h first) 
      else SymbolSet.singleton h;; 

let rec recurseFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
  match g with
    (s, r) -> match r with 
      [] -> first
    | h :: t -> let (lhs, rhs) = h in 
        let nfmap = SMap.add lhs (SymbolSet.union (firstFunc first rhs) (SMap.find lhs first)) first in
        recurseFirstSets (s,t) nfmap firstFunc ;; 


let rec getFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
  let m = recurseFirstSets g first firstFunc in
  if SMap.equal SymbolSet.equal first m
  then (m) else (getFirstSets g m firstFunc);;


let rec updateFollowSet (first : symbolMap) (follow : symbolMap) (nt : string) (symbolSeq : string list) : symbolMap =
  match symbolSeq with
    [] -> follow
  |h::t -> if SMap.mem h first then
        let cfs = computeFirstSet first t in 
        let wing = if (SymbolSet.mem "eps" (cfs))
          then 
            (let un = SymbolSet.union (remeps cfs)(SMap.find nt follow) in
             let un1 = SymbolSet.union (un)(SMap.find h follow) in
             SMap.add h un1 follow)
          else
            SMap.add h (SymbolSet.union(SMap.find h follow)(cfs)) follow in
        updateFollowSet first wing nt t 
      else
        updateFollowSet first follow nt t;;
  

let rec recurseFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
  match g with
    (s,r) -> match r with
      [] -> follow
    |h::t -> let (lhs,rhs) = h in
        let ctprice = followFunc first follow lhs rhs in
        recurseFollowSets (s,t) first ctprice followFunc;;


let rec getFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
  let m = recurseFollowSets g first follow followFunc in 
  if SMap.equal SymbolSet.equal m follow
  then (m) else (getFollowSets g first m followFunc);; 


let rec getPredictSets (g : grammar) (first : symbolMap) (follow : symbolMap) firstFunc : ((string * string list) * SymbolSet.t) list =
  match g with
    (_,[]) -> []
  |(st, (nt, rhs) :: rest) ->
      let rhs_first = firstFunc first rhs in
      let predict = (
        if SymbolSet.mem "eps" rhs_first then
          SymbolSet.union (SymbolSet.remove "eps" rhs_first) (SMap.find nt follow)
        else
          rhs_first
      ) in ((nt, rhs), predict) :: getPredictSets (st, rest) first follow firstFunc;;



let tryDerive (g : grammar) (inputStr : string list) : bool =
  (*
(*helper func to pop head of stack*)
let pop stack = 
  match stack with
    [] -> [] 
  |h::t -> t;; 

(*helper func to get next token*) 
let get_token lst =
  match lst with
    [] -> []
  |h::t -> h;;
    
(*HELPER TO GET TOP OF STACK*)
let top_of_stack lst = 
  match lst with
    [] -> []
  |h::t -> h;;

let inputStr_tail inputStr = 
  match inputStr with
    []->[]
  |h::t -> t;;

let get_lhs predict =
  match predict with
    []->[]
  |(lhs, rhs)::t -> lhs;;

let get_rhs predict =
  match predict with
    []->[]
  |(lhs, rhs)::t -> rhs;;


let rec check X token = 
  let case = if(X <> "S") (*if X is terminal*)
    then(if X = token
         then let x = pop stack in (*pop X from stack*)
           get_token (inputStr_tail inputStr)           
         else false)
    else (*if X is Non_terminal*)
      (if lhs = X
       then let x = pop stack in 
         let new_stack = rhs::x in 
         check x new_stack
       else false);;





let tryDerive g inputStr =
  let predict = getPredictSets g(getInitFirstSets g)(getInitFollowSets g)(computeFirstSet) in 
  let lhs = get_lhs predict in
  let rhs = get_rhs predict in
  let top = getStartSymbol g in
  let stack = [top ; "eof"] in (*top is S*)
  let token = get_token inputStr in (*get first elem in inputStr*)
  let X = top_of_stack stack in (*get top of the stack*) 
  check X token;;
*)


  false;;

let tryDeriveTree (g : grammar) (inputStr : string list) : parseTree =
  (* YOUR CODE GOES HERE *)
Terminal "empty";;

let genParser g = tryDerive g;;
let genTreeParser g = tryDeriveTree g;;
