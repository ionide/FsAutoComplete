module SemanticSignature

let semanticFunctionDeclaration (value: int) = value

[<AbstractClass>]
type SemanticTypeDeclaration() =
  abstract SemanticMethodDeclaration: int -> int

type SemanticDelegate = delegate of int -> int
