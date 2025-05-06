using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfRegistration
{
    public class Folder : Registration
    {
        private List<Registration> items;

        public Folder() { items = new List<Registration> (); }

        public override int GetSize()
        {
            int s = 0;
            foreach (var item in items) s += item.GetSize();
            return s;
        }
        public void Add(Registration r) { items.Add(r); }
        public void Remove(Registration r) { items.Remove(r); }
    }

    public class FileSystem : Folder { }
}
