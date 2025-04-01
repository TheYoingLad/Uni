SELECT * FROM szeret sz1, szeret sz2;
SELECT * FROM szeret sz1 CROSS JOIN szeret sz2;

//SZERET
//1
SELECT DISTINCT sz1.nev FROM szeret sz1, szeret sz2 WHERE sz1.nev = sz2.nev AND sz1.gyumolcs != sz2.gyumolcs;

//2
SELECT DISTINCT sz1.nev FROM szeret sz1, szeret sz2, szeret sz3 WHERE sz1.nev = sz2.nev AND sz2.nev = sz3.nev AND
                                                       sz1.gyumolcs != sz2.gyumolcs AND sz1.gyumolcs != sz3.gyumolcs AND sz2.gyumolcs != sz3.gyumolcs;

//3
SELECT nev FROM szeret
MINUS
SELECT DISTINCT sz1.nev FROM szeret sz1, szeret sz2, szeret sz3 WHERE sz1.nev = sz2.nev AND sz2.nev = sz3.nev AND
                                                       sz1.gyumolcs != sz2.gyumolcs AND sz1.gyumolcs != sz3.gyumolcs AND sz2.gyumolcs != sz3.gyumolcs;

//4
SELECT DISTINCT sz1.nev FROM szeret sz1, szeret sz2 WHERE sz1.nev = sz2.nev AND sz1.gyumolcs != sz2.gyumolcs
MINUS
SELECT DISTINCT sz1.nev FROM szeret sz1, szeret sz2, szeret sz3 WHERE sz1.nev = sz2.nev AND sz2.nev = sz3.nev AND
                                                       sz1.gyumolcs != sz2.gyumolcs AND sz1.gyumolcs != sz3.gyumolcs AND sz2.gyumolcs != sz3.gyumolcs;

SELECT DISTINCT sz1.nev FROM szeret sz1, szeret sz2 WHERE sz1.nev = sz2.nev AND sz1.gyumolcs != sz2.gyumolcs
INTERSECT
SELECT nev FROM szeret
MINUS
SELECT DISTINCT sz1.nev FROM szeret sz1, szeret sz2, szeret sz3 WHERE sz1.nev = sz2.nev AND sz2.nev = sz3.nev AND
                                                       sz1.gyumolcs != sz2.gyumolcs AND sz1.gyumolcs != sz3.gyumolcs AND sz2.gyumolcs != sz3.gyumolcs;

//DOLGOZO
//1
SELECT d1.dnev FROM dolgozo d1, dolgozo d2 WHERE d2.dnev = 'KING' AND d1.fonoke = d2.dkod;

//2
SELECT d1.dnev FROM dolgozo d1, dolgozo d2 WHERE d1.dkod = d2.fonoke
MINUS
SELECT dnev FROM dolgozo WHERE foglalkozas = 'MANAGER';

//3
SELECT d1.dnev FROM dolgozo d1, dolgozo d2 WHERE d1.fonoke = d2.dkod AND d1.fizetes > d2.fizetes;

//4
SELECT d1.dnev FROM dolgozo d1, dolgozo d2, dolgozo d3 WHERE d3.dnev = 'KING' AND d2.fonoke = d3.dkod AND d1.fonoke = d2.dkod;

//5
SELECT dnev FROM dolgozo, osztaly WHERE dolgozo.oazon = osztaly.oazon AND (telephely = 'DALLAS' OR telephely = 'CHICAGO');

//6
SELECT dnev FROM dolgozo
MINUS
SELECT dnev FROM dolgozo, osztaly WHERE dolgozo.oazon = osztaly.oazon AND (telephely = 'DALLAS' OR telephely = 'CHICAGO');

//7
SELECT dnev FROM dolgozo, osztaly WHERE dolgozo.oazon = osztaly.oazon AND (fizetes > 2000 OR telephely = 'CHICAGO');

//8
SELECT onev FROM osztaly
MINUS
SELECT onev FROM dolgozo, osztaly WHERE dolgozo.oazon = osztaly.oazon;

//9
SELECT DISTINCT d1.dnev FROM dolgozo d1, dolgozo d2 WHERE d1.dkod = d2.fonoke AND d2.fizetes > 2000;

//10
SELECT dnev FROM dolgozo
MINUS
SELECT DISTINCT d1.dnev FROM dolgozo d1, dolgozo d2 WHERE d1.dkod = d2.fonoke AND d2.fizetes > 2000;

//11
SELECT DISTINCT telephely FROM dolgozo, osztaly WHERE dolgozo.oazon = osztaly.oazon AND foglalkozas = 'ANALYST';

//12
SELECT telephely FROM osztaly
MINUS
SELECT DISTINCT telephely FROM dolgozo, osztaly WHERE dolgozo.oazon = osztaly.oazon AND foglalkozas = 'ANALYST';

//13
SELECT dnev FROM dolgozo
MINUS
SELECT d1.dnev FROM dolgozo d1, dolgozo d2 WHERE d1.fizetes < d2.fizetes;

