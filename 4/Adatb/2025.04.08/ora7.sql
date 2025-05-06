set serveroutput on;
CREATE OR REPLACE PROCEDURE hello(nev VARCHAR2) IS
    --deklaracio    
    uzenet VARCHAR2(20);
    hanyszor INT := 20;

BEGIN
    --program
    uzenet := 'hali';
    IF hanyszor IS NULL THEN
        hanyszor := 10;
    END IF;
    
    FOR i IN REVERSE 1..hanyszor LOOP
        IF MOD(i,3) = 0 THEN
            dbms_output.put_line(uzenet || ' ' || nev);
        ELSIF MOD(i,3) = 1 THEN
            dbms_output.put_line('1 a maradek');
        ELSE
            dbms_output.put_line(hanyszor);
        END IF;
    END LOOP;
    
    WHILE hanyszor > 1 LOOP
        hanyszor := hanyszor - 1;
        dbms_output.put_line(hanyszor);
    END LOOP;
END hello;
/ 
--^ ez kell ide mert sqldev buta
CALL hello('alma');


CREATE OR REPLACE FUNCTION paros(szam INT) RETURN INT IS
BEGIN
    RETURN 1 - MOD(szam, 2);
END;
/
SELECT paros(dkod), dkod FROM dolgozo;

CREATE OR REPLACE FUNCTION prim(szam INT) RETURN INT IS
BEGIN
    FOR i in 2..(szam**(1/2)) LOOP
        IF MOD(szam, i) = 0 THEN
            RETURN 0;
        END IF;
    END LOOP;
    RETURN 1;
END;
/
SELECT prim(2),prim(3),prim(5),prim(6),prim(7) from dual;

create or replace function fib(n INT) return INT IS
BEGIN
    IF n = 0 THEN
        RETURN 0;
    ELSIF n = 1 THEN
        RETURN 1;
    ELSE
        RETURN fib(n-1) + fib(n-2);
    END IF;
END;
/
select fib(2),fib(3),fib(4),fib(5),fib(6),fib(10) from dual;


create or replace FUNCTION lnko(p1 INT, p2 INT) RETURN INT IS
BEGIN
    if p1 < p2 then
        if mod(p2,p1) = 0 then
            return p1;
        end if;
        return lnko(p1, mod(p2,p1));
    else
        if mod(p1,p2) = 0 then
            return p2;
        end if;
        return lnko(p2, mod(p1,p2));
    end if;
END;
/
select lnko(10,30) from dual;

CREATE OR REPLACE FUNCTION faktor(n INT) RETURN INT IS
begin
    IF n <= 1 THEN
        RETURN 1;
    ELSE
        RETURN n*faktor(n-1);
    END IF;
end;
/
select faktor(0),faktor(1),faktor(2),faktor(3),faktor(4),faktor(5) from dual;