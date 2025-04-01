﻿namespace HfRational
{
    public class Rational
    {
        private int n;
        private int d;
        public class ZeroDenominator : ArgumentException { }

        public Rational(int i, int j)
        {
            if (j == 0) throw new ZeroDenominator();
            n = i;
            d = j;
        }
        public static Rational operator +(Rational a, Rational b)
        {
            return new Rational(a.n * b.d + b.n * a.d, a.d * b.d);
        }
        public static Rational operator -(Rational a, Rational b)
        {
            return new Rational(a.n * b.d - b.n * a.d, a.d * b.d);
        }
        public static Rational operator *(Rational a, Rational b)
        {
            return new Rational(a.n * b.n, a.d * b.d);
        }
        public static Rational operator /(Rational a, Rational b)
        {
            if (b.n == 0) throw new DivideByZeroException();
            return new Rational(a.n * b.d, a.d * b.n);
        }
        public override string ToString()
        {
            return n.ToString() + "/" + d.ToString();
        }
    }

    internal class Program
    {
        static void CreateRational()
        {
            /*
             * Creates a new rational number with two integer inputs, the denominator (latter) not being 0. 
            */
            int n, d = 1;
            string[] input;
            bool correct;
            do
            {
                input = Console.ReadLine().Split();
                correct = int.TryParse(input[0], out n);
                correct = correct && int.TryParse(input[1], out d);
            } while (!correct);
            try
            {
                Rational a = new Rational(n, d);
                Console.WriteLine(a);
            }
            catch (Exception e)
            {
                Console.WriteLine("Konstruktor - sikertelen");
            }
        }
        static void IsStatic()
        {
            /*
             * Checks if certain class methods can be called staticly.
             */
            try
            {
                int n, d = 1;
                string[] input;
                Rational a, b, c;
                // First input
                bool correct;
                do
                {
                    input = Console.ReadLine().Split();
                    correct = int.TryParse(input[0], out n);
                    correct = correct && int.TryParse(input[1], out d);
                } while (!correct);
                a = new Rational(n, d);
                // Second input
                do
                {
                    input = Console.ReadLine().Split();
                    correct = int.TryParse(input[0], out n);
                    correct = correct && int.TryParse(input[1], out d);
                } while (!correct);
                b = new Rational(n, d);
                // Calling addition statically
                c = a + b;
                Console.WriteLine(c);
                // Calling substraction statically
                c = a - b;
                Console.WriteLine(c);
                // Calling multiplication statically
                c = a * b;
                Console.WriteLine(c);
                // Calling division statically
                c = a / b;
                Console.WriteLine(c);
            }
            catch (Exception e)
            {
                // Couldn't call one of the methods statically
                Console.WriteLine("Statikus metódusok - sikertelen");
            }
        }
        static void IsOverwritten()
        {
            /*
             * Checks if certain operators are overwritten and can be called on the class.
             */
            try
            {
                int n, d = 1;
                string[] input;
                Rational a, b, c;
                // First input
                bool correct;
                do
                {
                    input = Console.ReadLine().Split();
                    correct = int.TryParse(input[0], out n);
                    correct = correct && int.TryParse(input[1], out d);
                } while (!correct);
                a = new Rational(n, d);
                // Second input
                do
                {
                    input = Console.ReadLine().Split();
                    correct = int.TryParse(input[0], out n);
                    correct = correct && int.TryParse(input[1], out d);
                } while (!correct);
                b = new Rational(n, d);
                // Calling addition operator
                c = a + b;
                Console.WriteLine(c);
                // Calling substraction operator
                c = a - b;
                Console.WriteLine(c);
                // Calling multiplication operator
                c = a * b;
                Console.WriteLine(c);
                // Calling division operator
                c = a / b;
                Console.WriteLine(c);
            }
            catch (Exception e)
            {
                // Couldn't call one the overwritten operators
                Console.WriteLine("Operátor felülírás - sikertelen");
            }
        }
        static void NoZeroDivision()
        {
            int n, d = 1;
            string[] input;
            Rational a, b, c;
            // First input
            bool correct;
            do
            {
                input = Console.ReadLine().Split();
                correct = int.TryParse(input[0], out n);
                correct = correct && int.TryParse(input[1], out d);
            } while (!correct);
            a = new Rational(n, d);
            // Second input
            do
            {
                input = Console.ReadLine().Split();
                correct = int.TryParse(input[0], out n);
                correct = correct && int.TryParse(input[1], out d);
            } while (!correct);
            b = new Rational(n, d);
            try
            {
                c = a / b;
                Console.WriteLine(c);
            }
            catch (Exception e)
            {
                Console.WriteLine("Az osztás sikertelen");
            }
        }
        static void Main(string[] args)
        {
            int choice = int.Parse(Console.ReadLine());
            switch (choice)
            {
                case 0:
                    CreateRational();
                    break;
                case 1:
                    IsStatic();
                    break;
                case 2:
                    IsOverwritten();
                    break;
                case 3:
                    NoZeroDivision();
                    break;
            }
        }
    }
}