CREATE TABLE questions (
    id                BIGSERIAL,
    question_num      INT  NOT NULL,
    question          TEXT NOT NULL,
    answer            TEXT NOT NULL,
    PRIMARY KEY(id,question_num)
);

CREATE TABLE accounts
  ( email         VARCHAR(50)
  , password      VARCHAR(64) NOT NULL
  , PRIMARY KEY(email)
  );

CREATE TABLE backupinfo
  ( account         VARCHAR(50)
  , secretQuestion  VARCHAR(64) NOT NULL
  , PRIMARY KEY(account)
  , FOREIGN KEY(account) REFERENCES accounts(email)
  );

CREATE TABLE profiles 
  ( account         VARCHAR(50)
  , studentPoints   VARCHAR(50) NOT NULL
  , teacherPoints   VARCHAR(50) NOT NULL
  , PRIMARY KEY(account)
  , FOREIGN KEY(account) REFERENCES accounts(email)
  );

CREATE TABLE articlesMeta 
  ( id              UUID
  , owner           VARCHAR(50)
  , nrOfParts       INT
  , PRIMARY KEY(id)
  , FOREIGN KEY(owner) REFERENCES accounts(email)
  );

CREATE TABLE articleTextParts 
  ( articleID      UUID   
  , id             UUID
  , partnr         INT     
  , path       VARCHAR(50)    
  , PRIMARY  KEY(articleID)
  , FOREIGN  KEY(articleID) REFERENCES articlesMeta(id) 
  );

CREATE TABLE articleImageParts 
  ( articleID    UUID
  , id           UUID
  , partnr       INT       
  , path         VARCHAR(50)
  , PRIMARY KEY(id)
  , FOREIGN  KEY(articleID) REFERENCES articlesMeta(id) 
  );

CREATE TABLE  accountVerificationCountdown
  ( account             VARCHAR(50)
  , timeUntillLocked    VARCHAR(50) NOT NULL
  , verificationLink    VARCHAR(50) NOT NULL
  , PRIMARY KEY(account)
  , FOREIGN KEY(account) REFERENCES accounts(email)
  );


