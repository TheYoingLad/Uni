//Szert
//1
SELECT nev FROM szeret
MINUS
SELECT nev FROM(
    SELECT * FROM (SELECT nev FROM szeret), (SELECT gyumolcs FROM szeret)
    MINUS
    SELECT * FROM szeret
);

//2
SELECT nev FROM szeret
MINUS
SELECT nev FROM(
    SELECT * FROM (SELECT nev FROM szeret), (SELECT gyumolcs FROM szeret WHERE nev = 'Micimackó')
    MINUS
    SELECT * FROM szeret
);

//3
SELECT nev FROM szeret
MINUS
SELECT nev FROM(
    SELECT * FROM szeret
    MINUS
    SELECT * FROM (SELECT nev FROM szeret), (SELECT gyumolcs FROM szeret WHERE nev = 'Micimackó')
);

//4
SELECT nev FROM szeret
MINUS
SELECT nev FROM(
    SELECT * FROM (SELECT nev FROM szeret), (SELECT gyumolcs FROM szeret WHERE nev = 'Micimackó')
    MINUS
    SELECT * FROM szeret
)
INTERSECT
SELECT nev FROM szeret
MINUS
SELECT nev FROM(
    SELECT * FROM szeret
    MINUS
    SELECT * FROM (SELECT nev FROM szeret), (SELECT gyumolcs FROM szeret WHERE nev = 'Micimackó')
);


//CROSS JOIN    
SELECT * FROM dolgozo, osztaly WHERE dolgozo.oazon = osztaly.oazon;

//NATURAL JOIN
SELECT * FROM dolgozo NATURAL JOIN osztaly;

//(INNER) JOIN
SELECT * FROM dolgozo d1 JOIN dolgozo d2 ON d1.fonoke = d2.dkod WHERE d2.dnev = 'KING';

//LEFT/RIGHT/FULL (OUTER) JOIN
SELECT * FROM dolgozo LEFT JOIN osztaly ON dolgozo.oazon = osztaly.oazon;
SELECT * FROM dolgozo RIGHT JOIN osztaly ON dolgozo.oazon = osztaly.oazon;
SELECT * FROM dolgozo FULL JOIN osztaly ON dolgozo.oazon = osztaly.oazon;