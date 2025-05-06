SELECT oazon, AVG(fizetes) atl FROM dolgozo WHERE fizetes > 1000 GROUP BY oazon HAVING COUNT(*) > 2 ORDER BY oazon; --ASC/DESC

SELECT MAX(fizetes) FROM dolgozo;  --GROUP BY 1;

SELECT COUNT(*), COUNT(jutalek), COUNT(DISTINCT jutalek) FROM dolgozo;

//DOLGOZO
//1
SELECT MAX(fizetes) FROM dolgozo;

//2
SELECT MIN(fizetes) FROM dolgozo;

//3
SELECT AVG(fizetes) FROM dolgozo;

//4
SELECT COUNT(*) FROM dolgozo;

//5
SELECT oazon, COUNT(*) db FROM dolgozo GROUP BY oazon;

//6
SELECT oazon, AVG(fizetes) atl FROM dolgozo GROUP BY oazon HAVING AVG(fizetes) > 2000;

//7
SELECT oazon, AVG(fizetes) atl FROM dolgozo GROUP BY oazon HAVING COUNT(*) >= 4;

//8
SELECT telephely, atl FROM 
(SELECT oazon, AVG(fizetes) atl FROM dolgozo GROUP BY oazon HAVING COUNT(*) >= 4)
NATURAL JOIN osztaly;

//9
SELECT onev, telephely FROM 
(SELECT oazon FROM dolgozo GROUP BY oazon HAVING AVG(fizetes) > 2000)
NATURAL JOIN osztaly;

//10
SELECT kategoria  FROM fiz_kategoria, dolgozo WHERE fizetes BETWEEN also AND felso GROUP BY kategoria HAVING COUNT(*) = 3;

//11
SELECT kategoria FROM 
(SELECT kategoria, oazon FROM fiz_kategoria JOIN dolgozo ON fizetes BETWEEN also AND felso GROUP BY kategoria, oazon)
GROUP BY kategoria HAVING COUNT(*) = 1;

//12
SELECT DISTINCT onev, telephely FROM
(SELECT oazon FROM fiz_kategoria JOIN dolgozo ON fizetes BETWEEN also AND felso WHERE kategoria = 1)
NATURAL JOIN osztaly;

//13
SELECT DISTINCT onev, telephely FROM
(SELECT oazon FROM fiz_kategoria JOIN dolgozo ON fizetes BETWEEN also AND felso WHERE kategoria = 1 GROUP BY oazon HAVING COUNT(*) >= 2)
NATURAL JOIN osztaly;

//14
SELECT dnev, ABS(fizetes - atl) elteres FROM
(SELECT oazon, AVG(fizetes) atl FROM dolgozo GROUP BY oazon)
NATURAL JOIN dolgozo;


//NEM FONTOS
//1
SELECT MOD(dkod, 2) paritas, COUNT(*) FROM dolgozo GROUP BY MOD(dkod, 2);


//KOMPLEX
//1
SELECT dnev, fizetes, atl FROM
(SELECT oazon, AVG(fizetes) atl FROM dolgozo GROUP BY oazon)
NATURAL JOIN dolgozo;

//2
SELECT oazon FROM osztaly
MINUS
(SELECT x1.oazon FROM
(SELECT oazon, AVG(fizetes) atl FROM dolgozo GROUP BY oazon ORDER BY AVG(fizetes)) x1,
(SELECT oazon, AVG(fizetes) atl FROM dolgozo GROUP BY oazon ORDER BY AVG(fizetes)) x2
WHERE x1.atl > x2.atl);

//3
SELECT * FROM dolgozo
NATURAL JOIN
(SELECT dkod, db FROM
(SELECT MAX(db) db FROM (SELECT d1.dkod, COUNT(*) db FROM dolgozo d1 JOIN dolgozo d2 ON d1.dkod = d2.fonoke GROUP BY d1.dkod))
NATURAL JOIN
(SELECT d1.dkod, COUNT(*) db FROM dolgozo d1 JOIN dolgozo d2 ON d1.dkod = d2.fonoke GROUP BY d1.dkod));