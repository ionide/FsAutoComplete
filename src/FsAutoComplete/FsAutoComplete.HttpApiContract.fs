module FsAutoComplete.HttpApiContract

type PositionDto = { Line : int; Column : int}

type ParseRequest = { FileName : string; IsAsync : bool; Lines : string[]; Version : int }
type ProjectRequest = { FileName : string;}
type DeclarationsRequest = {FileName : string; Lines : string[]; Version : int}
type HelptextRequest = {Symbol : string}
type CompletionRequest = {FileName : string; SourceLine : string; Line : int; Column : int; Filter : string; IncludeKeywords : bool; IncludeExternal : bool; Version: int}
type PositionRequest = {FileName : string; Line : int; Column : int; Filter : string}
type RangesAtPositionRequest = {FileName: string; Positions:PositionDto[]}
type FileRequest = {FileName : string}
type WorkspacePeekRequest = {Directory : string; Deep: int; ExcludedDirs: string array}
type WorkspaceLoadRequest = {Files : string array; DisableInMemoryProjectReferences: bool}
type DocumentationForSymbolReuqest = {XmlSig: string; Assembly: string}
type QuitRequest() = class end
