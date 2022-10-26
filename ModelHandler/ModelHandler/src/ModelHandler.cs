using System;
using System.Collections.Generic;
using System.Data.SQLite;
using System.Diagnostics;
using System.Threading;

namespace ModelHandler
{
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
    public struct Farm
    {
        /// <summary>Farm's ID. Use ID-1 to find the index of the Farm on the list.</summary>
        public uint ID;
        /// <summary>Number of cattle that neither <b>Infected</b> nor <b>Recovered</b>.</summary>
        public uint S;
        /// <summary>Number of cattle that are <b>Infected</b>.</summary>
        public uint I;
        /// <summary>Number of cattle that have <b>Recovered</b> from the infection.</summary>
        public uint R;
        /**
         * <summary>List of exchanges with this farm (aka Farm's log).</summary>
         * <see cref="Exchange"/>
         */
        public List<Exchange> Logs;

        public Farm(uint id, uint s, uint i, uint r)
        {
            ID = id;
            S = s;
            I = i;
            R = r;
            Logs = new List<Exchange>();
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

        public static void Join() { sThread.Join(); } //Also Temporary

        /**
         * <summary>Initializes the database and run the script for the specified number of days</summary>
         * <param name="days">The numbers of the days that we should simulate at the start</param>
         */
        public static void Init(uint days)
        {
            sThread = new Thread(() =>
            {
                RunRScript("-e \"game.FMD::init('model.sqlite')\"");
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

                RunRScript("-e \"game.FMD::run('model.sqlite')\"");
                UpdateFarms();
                UpdateLogs();

                sCurrentDay++;
            }
        }

        private static void UpdateEvents(List<uint> quarantine)
        {
            string query = "DELETE FROM events WHERE time = " + sCurrentDay + " AND (";
            for (int i = 0; i < quarantine.Count - 1; i++)
                query += " node = " + quarantine[i] + " OR dest = " + quarantine[i] + " OR";
            query += " node = " + quarantine[quarantine.Count - 1] + " OR dest = " + quarantine[quarantine.Count - 1] + " );";

            SQLiteConnection conn = new SQLiteConnection("Data Source=model.sqlite");
            conn.Open();
            using (var cmd = conn.CreateCommand())
            {
                cmd.CommandText = query;
                cmd.ExecuteNonQuery();
            }
            conn.Close();
        }

        private static void UpdateFarms()
        {
            List<Farm> farms = new List<Farm>();
            SQLiteConnection conn = new SQLiteConnection("Data Source=model.sqlite");
            conn.Open();
            using (var cmd = conn.CreateCommand())
            {
                cmd.CommandText = "SELECT node, S, I, R FROM U WHERE time = " + sCurrentDay + ';';
                var reader = cmd.ExecuteReader();
                while(reader.Read())
                {
                    uint ID = (uint)reader.GetInt32(0);
                    uint S = (uint)reader.GetInt32(1);
                    uint I = (uint)reader.GetInt32(2);
                    uint R = (uint)reader.GetInt32(3);

                    farms.Add(new Farm(ID, S, I, R));
                }
            }
            conn.Close();

            //Publish results
            sLock.AcquireWriterLock(-1);
            if (sFarms == null)
            {
                sFarms = new Farm[farms.Count];
                for(int i = 0; i<sFarms.Length; i++)
                    sFarms[i].Logs = new List<Exchange>();
            }
            
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

        private static void UpdateLogs()
        {
            //Make local copy of farm array
            sLock.AcquireReaderLock(-1);
            List<Farm> farms = new List<Farm>(sFarms);
            uint day = sCurrentDay + 1;
            sLock.ReleaseReaderLock();

            SQLiteConnection conn = new SQLiteConnection("Data Source=model.sqlite");
            conn.Open();
            using(var cmd = conn.CreateCommand())
            {
                //TODO(Vasilis): If we change log to propotions change what we are actually looking for
                cmd.CommandText = "SELECT node, dest, n FROM events WHERE event = \"extTrans\" AND time = " + day + ';';
                var reader = cmd.ExecuteReader();
                while(reader.Read())
                {
                    int from = reader.GetInt32(0);
                    int to = reader.GetInt32(1);
                    uint n = (uint)reader.GetInt32(2);

                    Farm src = farms[from - 1];//Source farm
                    Farm dst = farms[to - 1];//Destination farm

                    src.Logs.Add(new Exchange((uint)from, (uint)to, n, day));
                    dst.Logs.Add(new Exchange((uint)from, (uint)to, n, day));
                }
            }
            conn.Close();

            //Publish changes
            sLock.AcquireWriterLock(-1);
            for (int i = 0; i < farms.Count; i++)
                sFarms[i].Logs = farms[i].Logs;
            sLock.ReleaseWriterLock();
        }

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


    }
}
