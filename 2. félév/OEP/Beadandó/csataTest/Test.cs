using elfOrkCsata;

namespace BeadandoTesztelo
{
    [TestClass]
    public class HarcosTest
    {
        [TestMethod]
        public void TestKonstruktor()
        {
            Ork o = new Verengzo("alma", 15);
            Elf e = new Bolcs("korte");

            Assert.AreEqual(o.getNev(), "alma");
            Assert.AreEqual(o.getHp(), 100);
            Assert.AreEqual(o.getKincs(), 15);

            Assert.AreEqual(e.getNev(), "korte");
            Assert.AreEqual(e.getHp(), 60);
            Assert.AreEqual(e.getKincs(), 0);
            Assert.AreEqual(e.getElixir(), 0);
        }

        [TestMethod]
        public void TestMeghal()
        {
            Ork o = new Verengzo("alma", 15);
            int k = o.Meghal();

            Assert.AreEqual(o.getKincs(), 0);
            Assert.AreEqual(k, 15);
        }

        [TestMethod]
        public void TestTamad()
        {
            Ork o = new Ravasz("alma", 15);
            Elf e = new Bolcs("korte");
            Elf e2 = new Vakmero("cica");

            //saját magát támad
            Assert.ThrowsException<System.ArgumentException>(() => o.Tamad(o));
            Assert.ThrowsException<System.ArgumentException>(() => e.Tamad(e));

            //ork elfet
            bool halott = o.Tamad(e);

            Assert.IsFalse(halott);
            Assert.AreEqual(o.getHp(), 90);
            Assert.AreEqual(e.getHp(), 55);
            Assert.AreEqual(o.getKincs(), 15);
            Assert.AreEqual(e.getKincs(), 0);
            Assert.AreEqual(e.getElixir(), 0);

            //elf orkot
            halott = e2.Tamad(o);

            Assert.IsFalse(halott);
            Assert.AreEqual(o.getHp(), 80);
            Assert.AreEqual(e2.getHp(), 100);
            Assert.AreEqual(o.getKincs(), 15);
            Assert.AreEqual(e2.getKincs(), 0);
            Assert.AreEqual(e2.getElixir(), 0);

            //meghal a védekező fél
            do { halott = e2.Tamad(o); }
            while (!halott);

            Assert.IsTrue(halott);
            Assert.AreEqual(o.getHp(), 0);
            Assert.AreEqual(e2.getHp(), 100);
            Assert.AreEqual(o.getKincs(), 0);
            Assert.AreEqual(e2.getKincs(), 15);
            Assert.AreEqual(e.getElixir(), 0);
        }

        [TestMethod]
        public void TestVedekez()
        {
            Ork o = new Verengzo("alma", 15);
            
            //negatív erő
            Assert.ThrowsException<System.ArgumentOutOfRangeException>(() => o.Vedekez(-10));

            //pajzs értéknél kevesebb erő
            o.Vedekez(1);

            Assert.AreEqual(o.getHp(), 100);
            Assert.AreEqual(o.getKincs(), 15);

            //pajzs értéknél több, de nem halálos erő
            o.Vedekez(10);

            Assert.AreEqual(o.getHp(), 95);
            Assert.AreEqual(o.getKincs(), 15);

            //halálos erő
            o.Vedekez(1000);

            Assert.AreEqual(o.getHp(), 0);
            Assert.AreEqual(o.getKincs(), 0);
        }
    }

    [TestClass]
    public class ElfTest
    {
        [TestMethod]
        public void TestAtvalt()
        {
            Elf e = new Vakmero("a");
            Ork delikvens = new Verengzo("jaj", 100);

            delikvens.Vedekez(100);
            e.Tamad(delikvens);

            Assert.AreEqual(e.getKincs(), 100);
            Assert.AreEqual(e.getElixir(), 0);

            //negatív kincs
            Assert.ThrowsException<System.ArgumentOutOfRangeException>(() => e.Atvalt(-10));

            //megfelelő kincs
            e.Atvalt(10);

            Assert.AreEqual(e.getKincs(), 90);
            Assert.AreEqual(e.getElixir(), 10);

            //túl sok kincs
            Assert.ThrowsException<System.ArgumentOutOfRangeException>(() => e.Atvalt(100));
        }

        [TestMethod]
        public void TestIszik()
        {
            Elf e = new Vakmero("a");
            Ork delikvens = new Verengzo("jaj", 100);

            delikvens.Vedekez(100);
            e.Tamad(delikvens);
            e.Vedekez(60);
            e.Atvalt(100);

            Assert.AreEqual(e.getElixir(), 100);
            Assert.AreEqual(e.getHp(), 50);

            //negatív kincs
            Assert.ThrowsException<System.ArgumentOutOfRangeException>(() => e.Atvalt(-10));

            //megfelelő kincs
            e.Iszik(10);

            Assert.AreEqual(e.getElixir(), 90);
            Assert.AreEqual(e.getHp(), 60);

            //túl sok kincs
            Assert.ThrowsException<System.ArgumentOutOfRangeException>(() => e.Iszik(100));
        }

        [TestMethod]
        public void TestGyogyul()
        {
            Elf e1 = new Vakmero("a");
            Elf e2 = new Ugyes("b");
            Elf e3 = new Bolcs("c");

            Ork delikvens1 = new Verengzo("jaj", 100);
            Ork delikvens2 = new Verengzo("oh", 100);
            Ork delikvens3 = new Verengzo("au", 100);

            delikvens1.Vedekez(100);
            delikvens2.Vedekez(100);
            delikvens3.Vedekez(100);

            e1.Tamad(delikvens1);
            e2.Tamad(delikvens2);
            e3.Tamad(delikvens3);

            e1.Vedekez(100);
            e2.Vedekez(80);
            e3.Vedekez(30);

            Assert.AreEqual(e1.getHp(), 10);
            Assert.AreEqual(e1.getKincs(), 100);
            Assert.AreEqual(e1.getElixir(), 0);
            Assert.AreEqual(e2.getHp(), 20);
            Assert.AreEqual(e2.getKincs(), 100);
            Assert.AreEqual(e2.getElixir(), 0);
            Assert.AreEqual(e3.getHp(), 40);
            Assert.AreEqual(e3.getKincs(), 100);
            Assert.AreEqual(e3.getElixir(), 0);

            //mind a három féle elf gyógyulása
            e1.Gyogyul();
            e2.Gyogyul();
            e3.Gyogyul();

            Assert.AreEqual(e1.getHp(), 10);
            Assert.AreEqual(e1.getKincs(), 100);
            Assert.AreEqual(e1.getElixir(), 0);
            Assert.AreEqual(e2.getHp(), 50);
            Assert.AreEqual(e2.getKincs(), 50);
            Assert.AreEqual(e2.getElixir(), 20);
            Assert.AreEqual(e3.getHp(), 60);
            Assert.AreEqual(e3.getKincs(), 0);
            Assert.AreEqual(e3.getElixir(), 80);
        }
    }

    [TestClass]
    public class CsataTest
    {
        [TestMethod]
        public void TestKonstruktor()
        {
            //nemlétező file
            Assert.ThrowsException<System.IO.FileNotFoundException>(() => new Csata("nemletezofile.txt"));

            //egyik/másik/mindkét sereg üres
            Csata cs = new Csata("input1.txt");

            Assert.AreEqual(cs.getElfek().Count, 2);
            Assert.AreEqual(cs.getOrkok().Count, 2);

            Ork o = new Verengzo("alma", 15);
            Elf e = new Bolcs("korte");

            List<Elf> uresE = new List<Elf>();
            List<Ork> uresO = new List<Ork>();
            List<Elf> vanE = new List<Elf> { e, e, e };
            List<Ork> vanO = new List<Ork> { o, o };

            Assert.ThrowsException<System.Exception>(() => new Csata(uresE, vanO));
            Assert.ThrowsException<System.Exception>(() => new Csata(vanE, uresO));
            Assert.ThrowsException<System.Exception>(() => new Csata(uresE, uresO));
            
            //normális eset
            cs = new Csata(vanE, vanO);

            Assert.AreEqual(cs.getOrkok().Count, 1);
            Assert.AreEqual(cs.getOrkok().Count, 1);
        }

        [TestMethod]
        public void TestKuzd()
        {
            Ork o = new Verengzo("alma", 15);
            Ork o2 = new Verengzo("alma2", 15);
            Elf e = new Bolcs("korte");
            Elf e2 = new Bolcs("korte2");

            //seregen kívüli harcos
            Csata cs = new Csata(new List<Elf> { e }, new List<Ork> { o, o2 });

            Assert.ThrowsException<System.ArgumentOutOfRangeException>(() => cs.Kuzd(e2, o));

            //mindkettő él
            (bool elfEl, bool orkEl) = cs.Kuzd(e, o);

            Assert.IsTrue(elfEl);
            Assert.IsTrue(orkEl);
            Assert.AreEqual(o.getHp(), 95);
            Assert.AreEqual(o.getKincs(), 15);
            Assert.AreEqual(e.getHp(), 40);
            Assert.AreEqual(e.getKincs(), 0);
            Assert.AreEqual(e.getElixir(), 0);

            //elf él
            o.Vedekez(95);
            (elfEl, orkEl) = cs.Kuzd(e, o);

            Assert.IsTrue(elfEl);
            Assert.IsFalse(orkEl);
            Assert.IsFalse(cs.getOrkok().Contains(o));
            Assert.AreEqual(o.getHp(), 0);
            Assert.AreEqual(o.getKincs(), 0);
            Assert.AreEqual(e.getHp(), 40);
            Assert.AreEqual(e.getKincs(), 15);
            Assert.AreEqual(e.getElixir(), 0);

            //ork él
            e.Vedekez(40);
            (elfEl, orkEl) = cs.Kuzd(e, o2);
            Assert.IsFalse(elfEl);
            Assert.IsFalse(cs.getElfek().Contains(e));
            Assert.IsTrue(orkEl);
            Assert.AreEqual(o2.getHp(), 95);
            Assert.AreEqual(o2.getKincs(), 30);
            Assert.AreEqual(e.getHp(), 0);
            Assert.AreEqual(e.getKincs(), 0);
            Assert.AreEqual(e.getElixir(), 0);
        }

        [TestMethod]
        public void TestMenet()
        {
            Ork o = new Verengzo("alma", 15);
            Elf e = new Bolcs("korte");
            
            //üres elf sereg
            Csata cs = new Csata(new List<Elf> { e }, new List<Ork> { o });
            e.Vedekez(60);
            cs.Kuzd(e, o);

            Assert.ThrowsException<System.ArgumentException>(() => cs.Menet());
            
            //egy menet
            cs = new Csata("input1.txt");
            cs.Menet();
            List<Elf> es = cs.getElfek();
            List<Ork> os = cs.getOrkok();

            Assert.AreEqual(es[0].getHp(), 70);
            Assert.AreEqual(es[0].getKincs(), 0);
            Assert.AreEqual(es[0].getElixir(), 0);
            Assert.AreEqual(es[1].getHp(), 80);
            Assert.AreEqual(es[1].getKincs(), 0);
            Assert.AreEqual(es[1].getElixir(), 0);
            Assert.AreEqual(os[0].getHp(), 95);
            Assert.AreEqual(os[0].getKincs(), 7);
            Assert.AreEqual(os[1].getHp(), 75);
            Assert.AreEqual(os[1].getKincs(), 4);

            //több menet, végén ork sereg üres
            cs = new Csata("input2.txt");
            cs.Menet();
            cs.Menet();
            cs.Menet();
            es = cs.getElfek();
            os = cs.getOrkok();

            Assert.AreEqual(os.Count, 0);
            Assert.AreEqual(es[0].getHp(), 60);
            Assert.AreEqual(es[0].getKincs(), 0);
            Assert.AreEqual(es[0].getElixir(), 60);
            Assert.AreEqual(es[1].getHp(), 20);
            Assert.AreEqual(es[1].getKincs(), 0);
            Assert.AreEqual(es[1].getElixir(), 0);
        }

        [TestMethod]
        public void TestSzimulal()
        {
            //ork nyer
            Csata cs = new Csata("input1.txt");
            cs.Szimulal();
            List<Elf> es = cs.getElfek();
            List<Ork> os = cs.getOrkok();

            Assert.AreEqual(es.Count, 0);
            Assert.AreEqual(os[0].getHp(), 10);
            Assert.AreEqual(os[0].getKincs(), 11);

            //elf nyer
            cs = new Csata("input2.txt");
            cs.Szimulal();
            es = cs.getElfek();
            os = cs.getOrkok();

            Assert.AreEqual(os.Count, 0);
            Assert.AreEqual(es[0].getHp(), 60);
            Assert.AreEqual(es[0].getKincs(), 0);
            Assert.AreEqual(es[0].getElixir(), 60);
            Assert.AreEqual(es[1].getHp(), 20);
            Assert.AreEqual(es[1].getKincs(), 0);
            Assert.AreEqual(es[1].getElixir(), 0);
        }
    }
}