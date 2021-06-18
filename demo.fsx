open System
let first = 0
let last = 0
let interval = 0
let count = 0
let sum = 0
let per = 0
let av = 0
let attention = true
let logName = $"""CGGC_{DateTime.Now:yyyy_MM_dd_HH_mm_ss_fff}"""
let msg = $"""{first,10:F}ms-{last,10:F}ms:: GCCGPause_in_{interval}ms -> count:{count, 8:D} sum:{sum, 8:F}ms per:{per,8:P1} av:{av, 8:F}ms min:{min, 8:F}ms max:{max, 8:F}ms {(if attention then "******" else "")}"""
