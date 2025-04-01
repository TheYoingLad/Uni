using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HfRegistration
{
    public class File : Registration
    {
        private int size;
        
        public File(int s) { size = s; }

        public override int GetSize() { return size; }
    }
}
