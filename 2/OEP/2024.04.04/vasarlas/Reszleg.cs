using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace vasarlas
{
    public class Reszleg
    {
        public List<Termek> keszlet;

        public Reszleg(String filename)
        {
            keszlet = new List<Termek>();
            StreamReader sr = new StreamReader(filename);
            while(sr.Peek() != -1)
            {
                String[] input = sr.ReadLine().Split();
                keszlet.Add(new Termek(input[0], int.Parse(input[1])));
            }
        }
    }
}
