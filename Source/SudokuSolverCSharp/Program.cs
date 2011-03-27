using System;
using System.Collections.Generic;
using System.Linq;

namespace SudokuSolverCSharp
{
    class Program
    {
        private static readonly List<List<int>> Original = new List<List<int>>
                {
                    new List<int>{1,5,9,13},
                    new List<int>{2,6,10,14},
                    new List<int>{3,7,11,15},
                    new List<int>{4,8,12,16}
                };

        private static readonly List<List<int>> Rsesult = new List<List<int>>
                {
                    new List<int>{1,2,3,4},
                    new List<int>{5,6,7,8},
                    new List<int>{9,10,11,12},
                    new List<int>{13,14,15,16}
                };

        static void Main(string[] args)
        {
            PrintResult(Transpose(Original));
            Console.ReadKey();
        }

        private static List<List<int>> Transpose(List<List<int>> original)
        {
            List<List<int>> result = new List<List<int>>();

            for (int i = 0; i < original[0].Count; i++)
            {
                result.Add(new List<int>());
                foreach (List<int> number in original)
                {
                    result[i].Add(number[i]);
                }
            }

            return result;
        }

        private static void PrintResult(List<List<int>> data)
        {
            data.ForEach(l => Console.WriteLine(string.Join("; ", l.Select(z => z.ToString()))));
        }
    }
}
