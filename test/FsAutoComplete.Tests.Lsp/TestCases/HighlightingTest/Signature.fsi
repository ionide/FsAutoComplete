module SemanticSignature

val semanticFunctionDeclaration: int -> int

[<AbstractClass>]
type SemanticTypeDeclaration =
  abstract SemanticMethodDeclaration: int -> int
