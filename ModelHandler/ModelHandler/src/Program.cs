using System;
using System.IO;
using System.Collections.Generic;
using System.Data.SQLite;

namespace ModelHandler
{
    class Program
    {
        static void Main(string[] args)
        {
            Directory.SetCurrentDirectory("../../../..");
            ModelHandler.Init(7);
            ModelHandler.Join();
            Console.WriteLine("Done.");
            Console.WriteLine("Press any key to continue...");
            Console.Read();
        }
    }
}
