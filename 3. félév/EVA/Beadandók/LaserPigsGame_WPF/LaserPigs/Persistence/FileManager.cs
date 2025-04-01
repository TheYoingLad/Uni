using LaserPigs.Persistence.Instructions;
using System.Text;

namespace LaserPigs.Persistence
{
    public class FileManager : IFileManager
    {
        public async Task<(Player p1, Player p2, int instructionIndex)> LoadAsync(string path)
        {
            Player p1;
            Player p2;
            string[] data;
            int instructionIndex;

            try
            {
                data = (await File.ReadAllTextAsync(path)).Split(Environment.NewLine);
                
                p1 = GetPlayerFromData(data, true);
                p2 = GetPlayerFromData(data, false);

                instructionIndex = int.Parse(data[2]);
            }
            catch (Exception e)
            {
                throw new FileManagerException(e.Message, e);
            }
            return (p1, p2, instructionIndex);
        }
        public async Task SaveAsync(string path, Player p1, Player p2, int instructionIndex)
        {
            try
            {
                List<string> data = new List<string>();

                data.Add(p1.GetSize.ToString());
                data.Add(p1.GetMaxInstructions.ToString());
                data.Add(instructionIndex.ToString());

                GetDataFromPlayer(p1, ref data);
                GetDataFromPlayer(p2, ref data);

                await File.WriteAllLinesAsync(path, data);
            }
            catch (Exception e)
            {
                throw new FileManagerException(e.Message, e);
            }
        }

        private void GetDataFromPlayer(Player p, ref List<string> data)
        {
            StringBuilder sb = new StringBuilder();
            
            data.Add(p.GetCoordinate.ToString());
            data.Add(p.GetMaxHp.ToString() + " " + p.GetHp.ToString());
            data.Add(p.GetDirection.ToString());

            int instructionCount = p.GetInstructionCount;
            if (instructionCount == 0)
            {
                sb.Append("-1");
            }
            else
            {
                Instruction[] instructions = p.GetInstructions;

                for(int i = 0; i < instructionCount; i++)
                {
                    Instruction ins = instructions[i];
                    if (ins is Step step) sb.Append("0 " + step.GetDirection.ToString());
                    else if (ins is Turn turn) sb.Append("1 " + turn.GetRotation.ToString());
                    else if (ins is Punch punch) sb.Append("2");
                    else if (ins is Shoot shoot) sb.Append("3");

                    sb.Append(",");
                }

                sb.Remove(sb.Length - 1, 1);
            }
            
            data.Add(sb.ToString());
            data.Add(p.Locked.ToString());
        }
        private Player GetPlayerFromData(string[] data, bool isP1)
        {
            string[] helper1;
            string[] helper2;

            int offset = isP1 ? 0 : 5;

            int size = int.Parse(data[0]);

            int instructionsLength = int.Parse(data[1]);

            helper1 = data[3 + offset].Split(" ");
            Coordinate coord = new Coordinate(int.Parse(helper1[0]), int.Parse(helper1[1]));

            helper1 = data[4 + offset].Split(" ");
            int maxHp = int.Parse(helper1[0]);
            int hp = int.Parse(helper1[1]);

            Direction direction = (Direction)Enum.Parse(typeof(Direction), data[5 + offset]);

            Player p = new Player(size, coord, maxHp, hp, direction, instructionsLength);

            helper1 = data[6 + offset].Split(",");
            helper2 = helper1[0].Split(" ");
            if (int.Parse(helper2[0]) != -1)
            {
                for (int i = 0; i < helper1.Length; i++)
                {
                    helper2 = helper1[i].Split(" ");
                    int instructionType = int.Parse(helper2[0]);
                    Instruction ins;

                    switch (instructionType)
                    {
                        case 0:
                            ins = new Step((Direction)Enum.Parse(typeof(Direction), helper2[1]));
                            break;
                        case 1:
                            ins = new Turn((Rotation)Enum.Parse(typeof(Rotation), helper2[1]));
                            break;
                        case 2:
                            ins = new Punch();
                            break;
                        case 3:
                            ins = new Shoot();
                            break;
                        default:
                            throw new ArgumentOutOfRangeException(nameof(instructionType), "Invalid instruction type");
                    }

                    p.AddInstruction(ins);
                }
            }

            p.Locked = bool.Parse(data[7 + offset]);

            return p;
        }
    }
}
