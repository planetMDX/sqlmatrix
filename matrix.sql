-- create data tables for the gene expression database

-- IMPORTANT: all values are writen sequentially
DROP TABLE IF EXISTS matrix;
DROP TABLE IF EXISTS matrix_rownames;
DROP TABLE IF EXISTS matrix_colnames;
DROP TABLE IF EXISTS matrix_val;
CREATE TABLE matrix (id INTEGER PRIMARY KEY, nrow integer, ncol integer, name character, desc character);
CREATE TABLE matrix_rownames (matrixId integer, rowInd integer, name character, desc character, FOREIGN KEY(matrixId) REFERENCES matrix(id));
CREATE TABLE matrix_colnames (matrixId integer, colInd integer, name character, desc character, FOREIGN KEY(matrixId) REFERENCES matrix(id));
CREATE TABLE matrix_val (matrixId integer, rowInd integer, colInd integer, value numeric, FOREIGN KEY(matrixId) REFERENCES matrix(id));
CREATE INDEX matrix_matrixIdIndex ON matrix(id);
CREATE INDEX matrix_colnames_matrixIdIndex ON matrix_colnames(matrixId);
CREATE INDEX matrix_rownames_matrixIdIndex ON matrix_rownames(matrixId);
CREATE INDEX matrix_val_matrixIdIndex ON matrix_val(matrixId);
