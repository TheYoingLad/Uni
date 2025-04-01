namespace Bag
{
    public class Bag
    {
        public class EmptyBagException : Exception { }
        public class Pair { 
            public Pair(string e, int c) { data = e; count = c; }
            public string data;
            public int count;
            public override string ToString()
            {
                return "(" + data + ":" + count.ToString() + ")";
            }
        };
        private List<Pair> seq =  new();
        private int maxind;
        public Bag() {  }
        public void SetEmpty() { seq.Clear(); /*seq = new List<Pair>();*/ }
        public bool Empty() { return seq.Count == 0; }

        private bool LogSearch(string e, out int ind)
        {
            ind = 0;
            bool l = false;
            int ll = 0;
            int ul = seq.Count - 1; 
            while( ll <= ul ) 
            {
                ind = (ll + ul) / 2;
                if (seq[ind].data == e) { l = true; break; }
                else if (string.Compare(seq[ind].data, e) > 0) { ul = ind - 1; }
                else if (string.Compare(seq[ind].data, e) < 0) { ll = ind + 1; }
            }
	    if(!l) { ind = ll; } 
            return l;
        }
        public int Multipl(string e) 
        {
            bool l = LogSearch(e, out int ind);
            if (l) return seq[ind].count; else return 0; 
        }

        public string Max() 
        { 
            if (Empty()) throw new EmptyBagException();
            return seq[maxind].data; 
        }

        public void Insert(string e)
        {
            bool l = LogSearch(e, out int ind);
            if (l) seq[ind].count++;
            else seq.Insert(ind, new Pair(e, 1));
            if (seq.Count == 1) maxind = 0;
            else if (seq[ind].count > seq[maxind].count) maxind = ind;
        }

        private void MaxSearch()
        {
            maxind = 0;
            int max = seq[0].count;
            for(int i=0; i<seq.Count; ++i)
            {
                if (seq[i].count > max) { max = seq[i].count; maxind = i; }
            }
        }

        public void Remove(string e)
        {
            bool l = LogSearch(e, out int ind);
            if (l)
            {
                if (seq[ind].count > 1) --seq[ind].count;
                else if(seq[ind].count == 1) {seq.RemoveAt(ind); return; }

                if (seq[ind].count > 0) MaxSearch(); 
            }
        }

        public override string ToString()
        {
            string output = "[";
            foreach (Pair pair in seq)
            {
                output += pair.ToString();
            }
            output += "]";
            return output;
        }

    }
}
