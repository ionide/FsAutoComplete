``` ini

BenchmarkDotNet=v0.13.5, OS=Windows 11 (10.0.22621.1413/22H2/2022Update/SunValley2)
12th Gen Intel Core i9-12900F, 1 CPU, 24 logical and 16 physical cores
.NET SDK=7.0.202
  [Host]     : .NET 7.0.4 (7.0.423.11508), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 7.0.4 (7.0.423.11508), X64 RyuJIT AVX2


```
| Method |      Mean |     Error |    StdDev | Allocated |
|------- |----------:|----------:|----------:|----------:|
|    MD5 | 18.153 μs | 0.3382 μs | 0.3322 μs |      80 B |
| Sha256 |  4.345 μs | 0.0784 μs | 0.0695 μs |     112 B |
