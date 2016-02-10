namespace Fovel

type UnionCase = { CaseId: Identifier; Fields: Identifier list }

type Type =
  | Union of name: Identifier * cases: UnionCase list
  | Tuple of size: int
  | Record of name: Identifier * fields: Identifier list
  | Unit
  | String | Int | Float | Bool
  | Array
  | Function
  | GenericParameter of Identifier

[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module Type =
  let name = function
  | Union (name, _) | Record(name, _) -> sanitizeId name
  | Tuple size -> sprintf "tuple%d" size
  | Unit -> "unit"
  | String -> "string"
  | Int -> "int"
  | Float -> "float" 
  | Bool -> "bool"
  | Array -> "array"
  | Function -> "function"
  | GenericParameter p -> p