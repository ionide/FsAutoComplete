``` ini

BenchmarkDotNet=v0.13.5, OS=Windows 11 (10.0.22621.1413/22H2/2022Update/SunValley2)
12th Gen Intel Core i9-12900F, 1 CPU, 24 logical and 16 physical cores
.NET SDK=7.0.202
  [Host]     : .NET 7.0.4 (7.0.423.11508), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 7.0.4 (7.0.423.11508), X64 RyuJIT AVX2


```
|                  Method |        Mean |     Error |    StdDev |      Gen0 |     Gen1 |    Gen2 |   Allocated |
|------------------------ |------------:|----------:|----------:|----------:|---------:|--------:|------------:|
|        Lines1_forceOnce |    931.1 μs |  16.89 μs |  24.75 μs |   56.6406 |  27.3438 |       - |   883.59 KB |
|        Lines2_forceOnce |    925.7 μs |  18.35 μs |  25.12 μs |   56.6406 |  27.3438 |  0.9766 |   878.67 KB |
|        Lines3_forceOnce |    912.4 μs |  17.78 μs |  24.34 μs |   55.6641 |  27.3438 |  0.9766 |   864.68 KB |
|        Lines4_forceOnce |    942.8 μs |  18.44 μs |  23.33 μs |   57.6172 |  28.3203 |  0.9766 |      883 KB |
|     Lines1_forceEvery10 |  5,034.1 μs |  73.30 μs |  68.57 μs |  343.7500 | 117.1875 |  7.8125 |  5369.61 KB |
|     Lines2_forceEvery10 |  5,002.6 μs |  54.70 μs |  48.49 μs |  343.7500 | 125.0000 |  7.8125 |  5366.51 KB |
|     Lines3_forceEvery10 |  4,806.7 μs |  68.06 μs |  63.67 μs |  335.9375 | 109.3750 |  7.8125 |  5181.22 KB |
|     Lines4_forceEvery10 |  4,937.2 μs |  64.86 μs |  60.67 μs |  343.7500 | 117.1875 |  7.8125 |  5369.61 KB |
| Lines1_forceEveryUpdate |  6,889.1 μs | 137.58 μs | 255.02 μs |  671.8750 | 367.1875 | 23.4375 | 10307.67 KB |
| Lines2_forceEveryUpdate | 10,635.9 μs | 209.56 μs | 307.17 μs |  750.0000 | 406.2500 | 15.6250 | 11627.83 KB |
| Lines3_forceEveryUpdate | 38,512.4 μs | 511.57 μs | 478.52 μs | 2785.7143 | 571.4286 | 71.4286 | 43363.85 KB |
| Lines4_forceEveryUpdate |  6,996.9 μs | 134.30 μs | 159.88 μs |  671.8750 | 382.8125 | 23.4375 | 10291.06 KB |
