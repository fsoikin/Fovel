namespace Fovel

type UnionCase = { CaseId: Identifier; Fields: Identifier list }

type Type =
  | NotImportant
  | Union of name: Identifier * cases: UnionCase list
  | Record of name: Identifier * fields: Identifier list

[<CompilationRepresentation( CompilationRepresentationFlags.ModuleSuffix )>]
module Type =
  let name = function
  | Union (name, _) | Record(name, _) -> sanitizeId name
  | NotImportant -> ""