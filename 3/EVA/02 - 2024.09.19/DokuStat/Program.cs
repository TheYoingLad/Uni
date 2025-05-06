using DokuStat.Model;

namespace DokuStat
{
    internal class Program
    {
        static int Main(string[] args)
        {
            string? path;
            do
            {
                Console.WriteLine("Please provide a valid file path: ");
                path = Console.ReadLine();
            } while (path == null || !File.Exists(path) || Path.GetExtension(path) != ".txt");

            IDocumentStatistics? model = new DocumentStatistics(path);

            try
            {
                model.Load();
            }
            catch (Exception e)
            {
                Console.WriteLine("An error occured: " + e.Message);
                return -1;
            }
            //Console.WriteLine(model.FileContent);
            Console.WriteLine();

            int minOccurence = ReadPositive("Please provide a minimum occurance threshold: ");
            int minLength = ReadPositive("Please provide a minimum length threshold: ");

            var pairs = model.DistinctWordCount
                             .Where(p => p.Value >= minOccurence)
                             .Where(p => p.Key.Length >= minLength)
                             .OrderByDescending(p => p.Value);

            bool ignore = ReadYN("Do you wish to provide a list of words to ignore?");
            string? ignoreBase;
            List<string> ignoredWords;
            if (ignore)
            {
                do
                {
                    Console.WriteLine("Please provide a list of ignored words separated by commas or spaces:");
                    ignoreBase = Console.ReadLine();
                } while (ignoreBase == null);
                ignoredWords = ignoreBase.Split(' ', ',').ToList();
                pairs = pairs.Where(p => !ignoredWords.Contains(p.Key))
                             .OrderByDescending(p => p.Value);
            }

            foreach (var pair in pairs)
            {
                Console.WriteLine($"{pair.Key}: {pair.Value}");
            }
            Console.WriteLine($"Character count: {model.CharacterCount}");
            Console.WriteLine($"Character count without white spaces: {model.NonWhiteSpaceCharacterCount}");
            Console.WriteLine($"Sentence count: {model.SentenceCount}");
            Console.WriteLine($"Proper noun count: {model.ProperNounCount}");
            Console.WriteLine($"Coleman Liau readability index: {model.ColemanLieuIndex}");
            Console.WriteLine($"Flesch Reading Ease metric: {model.FleschReadingEase}");
            return 0;
        }

        static int ReadPositive(string message)
        {
            int res;
            bool suc;
            do
            {
                Console.Write(message);
                suc = int.TryParse(Console.ReadLine(), out res);
            } while (!suc || res <= 0);
            return res;
        }

        static bool ReadYN(string message)
        {
            char? suc;
            do
            {
                Console.WriteLine(message);
                Console.Write("Y/N: ");
                suc = Console.ReadKey().KeyChar;
            } while (suc == null || (suc != 'Y' && suc != 'N'));
            Console.WriteLine();
            return suc == 'Y' ? true : false;
        }
    }
}
