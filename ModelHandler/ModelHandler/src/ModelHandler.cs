using System;
using System.Collections.Generic;
using System.Data.SQLite;
using System.Diagnostics;
using System.Threading;

//Intensity is in range [0.0349315068493151, 0.0527397260273973]

namespace ModelHandler
{
    delegate void ActionRef<T>(ref T arg);

    /**
     * <summary>Struct <c>Exchange</c>, data structure describing an Exchange between farms.</summary>
     * <see cref="Farm"/>
     */
    public struct Exchange
    {
        /// <summary>ID of the farm where the cattle originated from.</summary>
        public uint From;
        /// <summary>ID of the farm where the cattle was send.</summary>
        public uint To;
        /// <summary>Number of cattle that was transfered on this exchange.</summary>
        public uint N;//TODO(Vasilis): Maybe change to propotions
        /// <summary>Day on which the exchange occuried.</summary>
        public uint Day;

        public Exchange(uint from, uint to, uint n, uint day)
        {
            From = from;
            To = to;
            N = n;
            Day = day;
        }
    }

    /**
     * <summary>Struct <c>Farm</c>, data structure containing information about a Farm.</summary>
     */
    public class Farm
    {
        /// <summary>Farm's ID. Use ID-1 to find the index of the Farm on the list.</summary>
        public uint ID;
        /// <summary>Number of cattle that neither <b>Infected</b> nor <b>Recovered</b>.</summary>
        public uint S;
        /// <summary>Number of cattle that are <b>Infected</b>.</summary>
        public uint I;
        /// <summary>Number of cattle that have <b>Recovered</b> from the infection.</summary>
        public uint R;
        /// <summary>The possibility to send animals each day</summary>
        public double Intensity;

        /**
         * <summary>List of exchanges with this farm (aka Farm's log).</summary>
         * <see cref="Exchange"/>
         */
        public List<Exchange> Logs;

        /// <summary> List of friends that this farms has.
        /// Each element is basically a pair that contains a Farm's ID and "weight" value.
        /// If a value with farm's ID 0 (usually the last one) means that the "weight" represents the posibility to send anywhere else.
        /// </summary>
        public List<Tuple<uint, float>> Connections;

        private static Random sRandomEngine = new Random();
        private static double sMinIntensity = 0.0349315068493151;
        private static double sMaxIntensity = 0.0527397260273973;

        public Farm(uint id, bool initialize = true)
        {
            ID = id;
            Logs = new List<Exchange>();

            if (!initialize)
                return;

            double elapsed = sMaxIntensity - sMinIntensity;
            Intensity = sRandomEngine.NextDouble() * elapsed + sMinIntensity;

            Connections = new List<Tuple<uint, float>>();
            int Max = 101;
            for (int aux = 0; aux < 10; aux++)
            {
                int to;
                while ((to = sRandomEngine.Next(1, 151)) == id)
                    to = sRandomEngine.Next(1, 151);
                int rnd = sRandomEngine.Next(1, Max);
                Max -= rnd;

                float weight = (float)rnd / 100;
                Connections.Add(new Tuple<uint, float>((uint)to, weight));
                if (Max <= 1)
                    break;
            }

            if (Max > 1)
            {
                float weight = (float)Max / 100;
                Connections.Add(new Tuple<uint, float>(0, weight));
            }
        }

        public Farm(uint id, uint s, uint i, uint r)
            : this(id, false)
        {
            S = s;
            I = i;
            R = r;
        }
    }

    /**
     * <summary>Class <c>ModelHandler</c>, manages interaction with the model asynchronously so it doesn't block the game due to IO.</summary>
     */
    public static class ModelHandler
    {
        private static Farm[] sFarms = null;
        private static volatile uint sCurrentDay = 1;

        private static ReaderWriterLock sLock = new ReaderWriterLock();
        private static Thread sThread = null;//Temporary
        private static Random sRandomEngine = new Random();

        public static void Join() { sThread.Join(); } //Also Temporary

        /**
         * <summary>Initializes the database and run the script for the specified number of days</summary>
         * <param name="days">The numbers of the days that we should simulate at the start</param>
         */
        public static void Init(uint days)
        {
            sThread = new Thread(() =>
            {
                sFarms = new Farm[150];
                for (int i = 0; i < sFarms.Length; i++)
                    sFarms[i] = new Farm((uint)i + 1);

                RunRScript("-e \"game.FMD::init('model.sqlite')\"");
                
                //Change database so it works only with 150 farms
                ExecuteQuery("DELETE FROM U WHERE node > 150;");
                ExecuteQuery("UPDATE U SET I = 0;");
                ExecuteQuery("DELETE FROM ldata WHERE node > 150;");
                ExecuteQuery("DELETE FROM events;");

                //Create zero patient at random
                int zeroPatient = sRandomEngine.Next(1, 151);
                string query = string.Format("UPDATE U SET I = 1 WHERE node = {0};", zeroPatient);
                ExecuteQuery(query);

                _RunModel(new List<uint>(), days);
            });
            sThread.Start();
        }

        /**
         * <summary>
         * Runs the model using R for the specified numbers of days.
         * </summary>
         * <param name="quarantine">A list containing the ID of the farms that are under quarantine.</param>
         * <param name="days">Number of days that we should run the model.</param>
         */
        public static void RunModel(List<uint> quarantine, uint days = 1)
        {
            sThread = new Thread(() => { _RunModel(quarantine, days); });
            sThread.Start();
        }

        private static void _RunModel(List<uint> quarantine, uint days = 1)
        {
            uint until = sCurrentDay + days;
            for (uint i = sCurrentDay; i < until; i++)
            {
                if (quarantine.Count > 0)
                    UpdateEvents(quarantine);

                CreateEvents();
                RunRScript("-e \"game.FMD::run('model.sqlite')\"");
                UpdateFarms();

                sCurrentDay++;
            }
        }

        private static void UpdateEvents(List<uint> quarantine)
        {
            string query = "DELETE FROM events WHERE time = " + sCurrentDay + " AND (";
            for (int i = 0; i < quarantine.Count - 1; i++)
                query += " node = " + quarantine[i] + " OR dest = " + quarantine[i] + " OR";
            query += " node = " + quarantine[quarantine.Count - 1] + " OR dest = " + quarantine[quarantine.Count - 1] + " );";

            ExecuteQuery(query);
        }

        private static void UpdateFarms()
        {
            List<Farm> farms = new List<Farm>();
            string query = string.Format("SELECT node, S, I, R FROM U WHERE time = {0};", sCurrentDay);
            ExecuteQuery(query, true, (ref SQLiteDataReader reader) =>
            {
                uint ID = (uint)reader.GetInt32(0);
                uint S = (uint)reader.GetInt32(1);
                uint I = (uint)reader.GetInt32(2);
                uint R = (uint)reader.GetInt32(3);

                farms.Add(new Farm(ID, S, I, R));
            });

            //Publish results
            sLock.AcquireWriterLock(-1);
            for(int i = 0; i<farms.Count; i++)
            {
                Farm farm = farms[i];
                sFarms[i].ID = farm.ID;
                sFarms[i].S = farm.S;
                sFarms[i].I = farm.I;
                sFarms[i].R = farm.R;
            }
            sLock.ReleaseWriterLock();
        }

        private static void CreateEvents()
        {
            //Make local copy of farm array
            sLock.AcquireReaderLock(-1);
            List<Farm> farms = new List<Farm>(sFarms);
            sLock.ReleaseReaderLock();

            List<Exchange> events = new List<Exchange>();
            foreach(Farm farm in farms)
            {
                double p = sRandomEngine.NextDouble();
                if (p > farm.Intensity)//Check the farm should make a transfer today
                    continue;

                //Find out to which of it's friends he will send
                p = sRandomEngine.NextDouble();
                uint to = 0;
                float lucklyhood = 0.0f;
                bool found = false;
                List<uint> friends = new List<uint>(farm.Connections.Count);
                foreach(Tuple<uint, float> friend in farm.Connections)
                {
                    lucklyhood += friend.Item2;
                    if(lucklyhood <= p && !found)
                    {
                        to = friend.Item1;
                        found = true;
                    }

                    friends.Add(friend.Item1);
                }

                if(to == 0)//Selected farm means he/she could to anybody so we choose one randomly that is not the same farm on of it's friends
                {
                    to = (uint)sRandomEngine.Next(1, 151);
                    while (to == farm.ID || friends.Contains(to))
                        to = (uint)sRandomEngine.Next(1, 151);
                }

                Exchange exchange = new Exchange(farm.ID, to, 1, sCurrentDay);
                events.Add(exchange);

                Farm src = farms[(int)farm.ID - 1];
                Farm dst = farms[(int)to - 1];
                
                src.Logs.Add(exchange);
                dst.Logs.Add(exchange);
            }

            foreach(Exchange exchange in events)
            {
                string query = string.Format("INSERT INTO events (event, time, node, dest, n, proportion, 'select', shift) VALUES(\"extTrans\", {0}, {1}, {2}, {3}, 0.0, 4, 0);", sCurrentDay, exchange.From, exchange.To, 1);
                ExecuteQuery(query);
            }

            //Publish changes
            sLock.AcquireWriterLock(-1);
            //TODO(Vasilis): Remove old logs 
            for (int i = 0; i < farms.Count; i++)
                sFarms[i].Logs = farms[i].Logs;
            sLock.ReleaseWriterLock();
        }

        //private static void UpdateLogs()
        //{
        //    //Make local copy of farm array
        //    sLock.AcquireReaderLock(-1);
        //    List<Farm> farms = new List<Farm>(sFarms);
        //    uint day = sCurrentDay + 1;
        //    sLock.ReleaseReaderLock();
        //
        //    //TODO(Vasilis): If we change log to propotions change what we are actually looking for
        //    string query = string.Format("SELECT node, dest, n FROM events WHERE event = \"extTrans\" AND time = {0};", day);
        //    ExecuteQuery(query, true, (ref SQLiteDataReader reader) =>
        //    {
        //        int from = reader.GetInt32(0);
        //        int to = reader.GetInt32(1);
        //        uint n = (uint)reader.GetInt32(2);
        //
        //        Farm src = farms[from - 1];
        //        Farm dst = farms[to - 1];
        //
        //        src.Logs.Add(new Exchange((uint)from, (uint)to, n, day));
        //        dst.Logs.Add(new Exchange((uint)from, (uint)to, n, day));
        //    });
        //
        //    //Publish changes
        //    sLock.AcquireWriterLock(-1);
        //    for (int i = 0; i < farms.Count; i++)
        //        sFarms[i].Logs = farms[i].Logs;
        //    sLock.ReleaseWriterLock();
        //}

        private static void RunRScript(string args)
        {
            ProcessStartInfo startInfo = new ProcessStartInfo();
            startInfo.FileName = "Rscript.exe";
            startInfo.RedirectStandardOutput = true;//Although not need it
            startInfo.CreateNoWindow = true;
            startInfo.UseShellExecute = false;
            startInfo.Arguments = args;
            Process Rscript = Process.Start(startInfo);
            Rscript.WaitForExit();
        }

        private static void ExecuteQuery(string query, bool selection = false, ActionRef<SQLiteDataReader> action = null)
        {
            SQLiteConnection conn = new SQLiteConnection("Data Source=model.sqlite");
            conn.Open();
            using (var cmd = conn.CreateCommand())
            {
                cmd.CommandText = query;
                if (!selection)
                    cmd.ExecuteNonQuery();
                else
                {
                    var reader = cmd.ExecuteReader();
                    while (reader.Read())
                        action(ref reader);
                }
            }
            conn.Close();
        }

    }
}
