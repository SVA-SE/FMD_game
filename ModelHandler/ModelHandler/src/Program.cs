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
            //ModelHandler.Init(90);
            //ModelHandler.Join();
            Console.WriteLine("Done.");
            
            //SQLiteConnection conn = new SQLiteConnection("Data Source=model.sqlite");
            //conn.Open();
            //using (var cmd = conn.CreateCommand())
            //{
            //    cmd.CommandText = "SELECT node, S, I, R FROM U WHERE time = 90 AND I > 0";
            //    var reader = cmd.ExecuteReader();
            //    while (reader.Read())
            //    {
            //        uint ID = (uint)reader.GetInt32(0);
            //        uint S = (uint)reader.GetInt32(1);
            //        uint I = (uint)reader.GetInt32(2);
            //        uint R = (uint)reader.GetInt32(3);
            //
            //        Console.WriteLine("Farm[{0}]: S {1}, I {2}, R {3}", ID, S, I, R);
            //    }
            //}
            //conn.Close();
            //Console.WriteLine("Press any key to continue...");
            //Console.Read();
        }
    }
}
