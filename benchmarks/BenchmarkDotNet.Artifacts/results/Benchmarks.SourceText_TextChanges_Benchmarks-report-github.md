``` ini

BenchmarkDotNet=v0.13.5, OS=Windows 11 (10.0.22621.1413/22H2/2022Update/SunValley2)
12th Gen Intel Core i9-12900F, 1 CPU, 24 logical and 16 physical cores
.NET SDK=7.0.202
  [Host]     : .NET 7.0.4 (7.0.423.11508), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 7.0.4 (7.0.423.11508), X64 RyuJIT AVX2


```
|                        Method |       Mean |    Error |   StdDev | Ratio | RatioSD |    Gen0 |    Gen1 |   Gen2 |  Allocated | Alloc Ratio |
|------------------------------ |-----------:|---------:|---------:|------:|--------:|--------:|--------:|-------:|-----------:|------------:|
|          File1_Text_forceOnce |   731.9 μs | 13.50 μs | 11.97 μs |  1.48 |    0.04 | 43.9453 | 21.4844 |      - |  684.39 KB |        2.76 |
|          File2_Text_forceOnce |   504.3 μs |  9.41 μs |  8.81 μs |  1.02 |    0.03 | 31.2500 | 26.3672 | 0.4883 |  483.14 KB |        1.95 |
|        Roslyn_Text_changeText |   494.2 μs |  9.69 μs |  9.52 μs |  1.00 |    0.00 | 15.6250 |  0.9766 |      - |  248.38 KB |        1.00 |
|     File1_Lines1_forceEvery10 |   854.3 μs | 12.82 μs | 11.99 μs |  1.73 |    0.05 | 52.7344 | 25.3906 |      - |  810.78 KB |        3.26 |
|      File2_Lines_forceEvery10 |   581.2 μs |  7.10 μs |  6.29 μs |  1.18 |    0.03 | 34.1797 | 23.4375 |      - |  532.25 KB |        2.14 |
| File1_Lines1_forceEveryUpdate | 1,088.3 μs | 21.15 μs | 19.79 μs |  2.20 |    0.07 | 64.4531 | 23.4375 |      - | 1005.52 KB |        4.05 |
|  File2_Lines_forceEveryUpdate |   782.8 μs |  8.41 μs |  7.46 μs |  1.58 |    0.03 | 42.9688 | 20.5078 |      - |  666.65 KB |        2.68 |
