using Snake.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Snake.Persistence
{
    public interface IDataAccess
    {
        (int, List<Coordinate>) Load();
        Task<(int, List<Coordinate>)> LoadAsync();
    }
}
