module FsAutoComplete.HttpApiContract

type ParseRequest = { FileName : string; IsAsync : bool; Lines : string[]; Version : int }
type ProjectRequest = { FileName : string;}
type DeclarationsRequest = {FileName : string; Lines : string[]; Version : int}
type HelptextRequest = {Symbol : string}
type CompletionRequest = {FileName : string; SourceLine : string; Line : int; Column : int; Filter : string; IncludeKeywords : bool; IncludeExternal : bool}
type PositionRequest = {FileName : string; Line : int; Column : int; Filter : string}
type FileRequest = {FileName : string}
type WorkspacePeekRequest = {Directory : string; Deep: int; ExcludedDirs: string array}
type WorkspaceLoadRequest = {Files : string array}
type QuitRequest() = class end
