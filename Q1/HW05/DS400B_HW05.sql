/*
UW PCE Data Science Autumn 2017 Assignment 5
Leo Salemann 11/03/17


Based on RelationalAlgebraAndSQL.sql
Copyright 2013-2015 by Ernst Henle

*/

USE MatrixAlgebra
GO
BEGIN TRY 
DROP TABLE R;
END TRY BEGIN CATCH END CATCH
BEGIN TRY 
DROP TABLE S;
END TRY BEGIN CATCH END CATCH
BEGIN TRY 
DROP TABLE T;
END TRY BEGIN CATCH END CATCH
GO



-- Create Relations as operands for the operations
CREATE TABLE R (RC1  nchar(1), RC2 nchar(1)); 
CREATE TABLE S (SC1  nchar(1), SC2 nchar(1));
CREATE TABLE T (C1 int);
GO

TRUNCATE TABLE R;
GO
INSERT INTO R VALUES ('a', 'A');
INSERT INTO R VALUES ('b', 'B');
INSERT INTO R VALUES ('X', 'x');
INSERT INTO R VALUES ('y', 'Y');
SELECT * FROM R;

TRUNCATE TABLE S;
GO
INSERT INTO S VALUES ('a', 'A');
INSERT INTO S VALUES ('B', 'b');
INSERT INTO S VALUES ('c', 'C');
-- SELECT * FROM R;
SELECT * FROM S;


-- 5.  πS.C1, R.C2(σφ1(R) ⋈φ2 S)
-- where
-- • φ1 = (R.C2 = ‘A’) 
-- • φ2 = (R.C1 = S.C2)

-- Write out equivalent SQL and test this SQL using relations R and S that you create for this example. 

SELECT * FROM R
SELECT * FROM S

-- Join (theta Join): R ⋈{R.C1=S.C2} S
SELECT * FROM R
JOIN S ON RC1=SC2

-- Select:  σ{C2 = 'A'}(R)
SELECT * FROM R
WHERE RC2 = 'A';

-- Join (theta Join): σ{C2 = 'B'}(R) ⋈{R.C1=S.C2} S
SELECT * FROM (SELECT * FROM R WHERE RC2 = 'A') q1
JOIN S ON q1.RC1=SC2 

SELECT * FROM (SELECT * FROM (SELECT * FROM R WHERE RC2 = 'A') q1
JOIN S ON q1.RC1=SC2) q2


-- Project:  π{S.C1 R.C2}(σ{C2 = 'B'}(R) ⋈{R.C1=S.C2} S)
SELECT SC1, RC2 FROM (SELECT * FROM (SELECT * FROM R WHERE RC2 = 'A') q1
JOIN S ON q1.RC1=SC2) q2

