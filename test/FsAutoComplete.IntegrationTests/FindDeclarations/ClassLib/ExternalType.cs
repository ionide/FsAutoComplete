using System;
using System.Collections.Generic;

namespace ClassLib
{
    public class ExternalType
    {
        public static string SimpleMethod()
        {
            return null;
        }

        public static string SimpleMethod(string a, string b)
        {
            return null;
        }

        public static string RefParameter(ref IList<string> lst)
        {
            lst = null;
            return null;
        }

        public static object Generic<T,U>(Dictionary<T, Dictionary<T, U>> dict)
        {
            return dict;
        }

        
    }
}
