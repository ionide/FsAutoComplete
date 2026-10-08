module SemanticSignature

val semanticFunctionDeclaration: int -> int

[<AbstractClass>]
type SemanticTypeDeclaration =
  abstract SemanticMethodDeclaration: int -> int

type SemanticDelegate = delegate of int -> int
