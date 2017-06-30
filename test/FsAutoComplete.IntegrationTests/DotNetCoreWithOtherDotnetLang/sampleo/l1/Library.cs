using System;
using Newtonsoft.Json;

namespace l1
{
    public class Movie
    {
        public string Name { get; set; }
        public int Year { get; set; }
    }

    public class Say
    {
        public static void hello(string name)
        {
            Console.WriteLine($"Hello {name}");
        }

        public static void jsonstuff()
        {
            var movies = new[] {
                new Movie { Name = "Bad Boys", Year = 1995 },
                new Movie { Name = "Bad Boys 2", Year = 2003 }
            };

            string json = JsonConvert.SerializeObject(movies);
            Console.WriteLine(json);
        }
    }
}
