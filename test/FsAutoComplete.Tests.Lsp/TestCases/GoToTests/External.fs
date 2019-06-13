module External

open System.Net

let getHttpMethod (r: HttpWebRequest) = r.Method