module Fovel.CoreLib
open Fovel
open Fovel.Gen
open Fovel.Gen.Expr
open Microsoft.FSharp.Compiler

let private (|CoreFunction|_|) name fn = Expr.Intrinsics.(|Fn|_|) <| sprintf "Microsoft.FSharp.Core.%s" name <| fn
let private (|SeqFn|_|) name = Expr.Intrinsics.(|Fn|_|) <| sprintf "Microsoft.FSharp.Collections.Seq.%s" name
let private (|ArrFn|_|) name = Expr.Intrinsics.(|Fn|_|) <| sprintf "Microsoft.FSharp.Collections.Array.%s" name

let getAttribute<'typ> attrs = 
  attrs |> Seq.tryFind (fun (a: System.Reflection.CustomAttributeData) -> a.AttributeType = typeof<'typ>) 

let getFirstArg<'argType> (attr: System.Reflection.CustomAttributeData) : 'argType option = 
  attr.ConstructorArguments |> Seq.choose (fun a -> if a.ArgumentType = typeof<'argType> then Some (unbox a.Value) else None) |> Seq.tryHead

let moduleName (moduleType: System.Type) =
  let moduleSuffix =
    getAttribute<CompilationRepresentationAttribute> moduleType.CustomAttributes
    |> Option.bind getFirstArg<CompilationRepresentationFlags>
    |> Option.map (fun a -> (a &&& CompilationRepresentationFlags.ModuleSuffix) <> CompilationRepresentationFlags.None)
    |> Option.orElse false
  let name = moduleType.Name
  if moduleSuffix && name.EndsWith("Module") then name.Substring(0, name.Length-6) else name

let rec fullModuleName (moduleType: System.Type) =
  if moduleType.IsNested 
    then [ fullModuleName moduleType.DeclaringType; moduleName moduleType ]
    else [ moduleType.Namespace; moduleName moduleType ]
  |> Seq.filter (fun s -> s <> "" && s <> null)
  |> String.concat "." 

let quotedFnFullName = function
  | FSharp.Quotations.Patterns.Call (_,f,_) -> 
    let name = 
      f.CustomAttributes |> getAttribute<CompilationSourceNameAttribute> |> Option.bind getFirstArg<string>
      |> Option.orElse f.Name
    let name = 
      if PrettyNaming.IsMangledOpName name
        then sprintf "( %s )" <| PrettyNaming.DecompileOpName name 
        else name
    Some <| sprintf "%s.%s" (fullModuleName f.DeclaringType) name

  | _ -> None

let sameFullName a b = 
  #if INTERACTIVE
    let sanitize (s: string) = if s.StartsWith "FSI_" then s.Substring( s.IndexOf('.')+1 ) else s
  #else
    let sanitize = id
  #endif
    (sanitize a) = (sanitize b)

let (|Fn|_|) quote fn = 
  match quotedFnFullName quote with 
  | Some name when sameFullName name (FSharp.fnFullName fn) -> Some() 
  | _ -> None

type CoreLibIntrinsic<'CustomIntrinsic> =
  | ArrayLength
  | ArrayCreate
  | ArraySet
  | SeqCreate
  | FailWith
  | Custom of 'CustomIntrinsic

let parseIntrinsic (|ParseCustomIntrinsic|_|) = function
  | ParseCustomIntrinsic custom -> Some (Custom custom)
  | Fn <@ Array.length [||] @> -> Some ArrayLength
  | Fn <@ LanguagePrimitives.IntrinsicFunctions.SetArray [|0|] 0 0 @> -> Some ArraySet
  | Fn <@ FovelCore.Array.create 0 @> -> Some ArrayCreate
  | Fn <@ failwith "" @> -> Some FailWith
  | Fn <@ Operators.seq [] @> -> Some SeqCreate
  | _ -> None

let intrinsicCode customIntrinsicCode intr args =
  match intr, args with
  | Custom c, _ -> customIntrinsicCode c args
  | ArrayLength, [a] -> sprintf "length( %s )" a
  | ArrayCreate, [size] -> sprintf "arrayN( %s )" size
  | ArraySet, [arr; idx; value] -> sprintf "{%s}[%s] = {%s}" arr idx value
  | SeqCreate, [s] -> s
  | FailWith, args -> sprintf "panic( %s )" (defaultArg (List.tryHead args) "")
  | intr, args -> sprintf "panic( 'Intrinsic %A applied to %d args' )" intr (List.length args)

let wrapInstrinsics (parseCustomIntrinsic: 'a -> 'b option, customIntrinsicCode: 'b -> string list -> string) = 
  (parseIntrinsic parseCustomIntrinsic, intrinsicCode customIntrinsicCode)

let replaceSymbols allFns fn = 
  let findFnByFullName fullName = Seq.tryFind (FSharp.fnFullName >> sameFullName fullName) allFns
  match fn with
    | Fn <@ ( .. ) 0 1 @>                       -> Some <@@ FovelCore.Seq.range 0 1 @@>
    | Fn <@ ( .. .. ) 0 1 2 @>                  -> Some <@@ FovelCore.Seq.rangeStep 0 1 2 @@>
    | Fn <@ Seq.toArray [] @>                   -> Some <@@ FovelCore.Seq.toArray FovelCore.Seq.empty @@>
    | Fn <@ Seq.toList [] @>                    -> Some <@@ FovelCore.Seq.toList FovelCore.Seq.empty @@>
    | Fn <@ List.fold (fun _ _ -> ()) () [] @>  -> Some <@@ FovelCore.List.fold (fun _ _ -> ()) () [] @@>
    | Fn <@ List.map (fun _ -> ()) [] @>        -> Some <@@ FovelCore.List.map (fun _ -> ()) @@>
    | Fn <@ box () @>                           -> printfn "A"; Some <@@ FovelCore.id () @@>
    | _ -> None
  |> Option.bind quotedFnFullName
  |> Option.bind findFnByFullName
  |> Option.orElse fn

let prelude() = "[<AutoOpen>]\n" + Resources.CoreLib()