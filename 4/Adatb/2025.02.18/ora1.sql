-- 1 soros komment
/*
több
soros
komment
*/

//SZERET
//1
SELECT gyumolcs FROM szeret WHERE nev = 'Micimackó';

//2
SELECT gyumolcs FROM szeret
MINUS
SELECT gyumolcs FROM szeret WHERE nev = 'Micimackó';

//3
SELECT nev FROM szeret WHERE gyumolcs = 'alma';

//4
SELECT nev FROM szeret
MINUS
SELECT nev FROM szeret WHERE gyumolcs = 'körte';

//5
SELECT DISTINCT nev FROM szeret WHERE gyumolcs = 'alma' OR gyumolcs = 'körte';

//6
SELECT nev FROM szeret WHERE gyumolcs = 'alma'
INTERSECT
SELECT nev FROM szeret WHERE gyumolcs = 'körte';

//7
SELECT nev FROM szeret WHERE gyumolcs = 'körte'
MINUS
SELECT nev FROM szeret WHERE gyumolcs = 'alma';

//DOLGOZO
//1
SELECT dnev, fizetes FROM dolgozo WHERE fizetes > 2800;

//2
SELECT dnev, oazon FROM dolgozo WHERE oazon = 10 OR oazon = 20;

//3
SELECT dnev, jutalek FROM dolgozo WHERE jutalek > 600;

//4
SELECT dnev, jutalek FROM dolgozo WHERE jutalek <= 600;

//5
SELECT dnev, jutalek FROM dolgozo WHERE jutalek IS NULL;

//6
SELECT DISTINCT foglalkozas FROM dolgozo;

//7
SELECT dnev, fizetes*2 as dupla, oazon FROM dolgozo WHERE oazon = 10;

//8
SELECT dnev, belepes FROM dolgozo WHERE belepes > TO_DATE('1982.01.01', 'YYYY.MM.DD');

//9
SELECT dnev, fonoke FROM dolgozo WHERE fonoke IS NULL;

//10
SELECT * FROM dolgozo WHERE fonoke = (SELECT dkod FROM dolgozo WHERE dnev = 'KING');

SELECT d1.*, d2.dnev as fonok FROM dolgozo d1, dolgozo d2 WHERE d2.dnev = 'KING' AND d1.fonoke = d2.dkod;