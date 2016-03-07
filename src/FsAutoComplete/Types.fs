namespace FsAutoComplete

type Result<'a> =
  | Success of 'a
  | Failure of string

