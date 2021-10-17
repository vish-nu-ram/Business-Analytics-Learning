CREATE DATABASE University_DB;

USE University_DB;


CREATE table Student_Details(
Student_ID varchar(10) NOT NULL PRIMARY KEY, 
Student_FirstName varchar(20), 
Student_LastName varchar(20) NOT NULL, 
Student_DateOfBirth date, 
Student_Gender varchar(6), 
Student_PhNo varchar(22), 
Student_Email varchar(50) NOT NULL, 
Student_Address varchar(200), 
Student_GPA varchar(5)
);

CREATE table Payroll(
Staff_Role varchar(30) NOT NULL PRIMARY KEY,
Role_Salary INT,
Role_Hours INT
);

CREATE table Staff_Details(
Staff_ID varchar(10) NOT NULL PRIMARY KEY, 
Staff_FirstName varchar(20), 
Staff_LastName varchar(20) NOT NULL, 
Staff_DateOfBirth date, 
Staff_Gender varchar(6), 
Staff_PhNo varchar(22), 
Staff_Email varchar(50) NOT NULL, 
Staff_Address varchar(200), 
DOJ date,
Staff_Role varchar(30),
FOREIGN KEY(Staff_Role) references Payroll(Staff_Role)
);

CREATE Table School(
School_ID varchar(10) NOT NULL PRIMARY KEY,
School_Name varchar(50),
School_Building varchar(30)
);

CREATE Table Program_Details(
Program_ID varchar(10) NOT NULL PRIMARY KEY,
Program_Name varchar(40),
Program_Duration INT,
Program_Director varchar(10),
Program_School varchar(10),
FOREIGN KEY(Program_Director) references Staff_Details(Staff_ID),
FOREIGN KEY(Program_School) references School(School_ID)
);

CREATE Table Course_Details(
Course_ID varchar(10) NOT NULL PRIMARY KEY,
Course_Name varchar(40),
Course_MaxStudents INT,
Course_Credit INT,
Course_School varchar(10),
FOREIGN KEY(Course_School) references School(School_ID)
);

CREATE Table Instructs(
Faculty_ID varchar(10) NOT NULL,
Course_ID varchar(10) NOT NULL,
Class_Rep varchar(10),
Primary key(Faculty_ID,Course_ID),
FOREIGN KEY(Class_Rep) references Student_Details(Student_ID),
FOREIGN KEY(Course_ID) references Course_Details(Course_ID),
FOREIGN KEY(Faculty_ID) references Staff_Details(Staff_ID)
);

CREATE Table Enrolment(
Student_ID varchar(10) NOT NULL,
Course_ID varchar(10) NOT NULL,
Grade varchar(10),
PRIMARY KEY ( Student_ID, Course_ID),
foreign key ( Course_ID) references Course_Details(Course_ID),
foreign key ( Student_ID) references Student_Details(Student_ID)
);

CREATE Table Academic_Registry(
Student_ID varchar(10) NOT NULL PRIMARY KEY,
Fees_Paid INT,
Fees_Total INT,
Program varchar(10),
Foreign key (Program) references Program_Details(Program_ID),
Foreign key (Student_ID) references Student_Details(Student_ID)
);

CREATE Table Complaints(
Complaint_ID varchar(10) NOT NULL PRIMARY KEY,
Complaint_Reason varchar(500),
Complaint_Date DATE,
Student_ID varchar(10),
Staff_ID varchar(10),
Course_ID varchar(10),
Resolution varchar(500),
Resolution_Date DATE,
Allocated_To varchar(10),
foreign key(Student_ID) references Student_Details(Student_ID),
foreign key(Staff_ID) references Staff_Details(Staff_ID),
foreign key(Course_ID) references Course_Details(Course_ID),
foreign key(Allocated_To) references Staff_Details(Staff_ID)
);

INSERT INTO Student_Details(
Student_ID, 
Student_FirstName, 
Student_LastName, 
Student_DateOfBirth, 
Student_Gender, 
Student_PhNo, 
Student_Email, 
Student_Address, 
Student_GPA
)
Values
(2122000005, "Merrit", "Ravi",'1998-1-09', "Male", "353 0893599034", "merrit.ravi@gmail.com", 
" A18IS09 783, LLOIK street", "7.9"),
(2122000006, "Sheyl", "Roy",'1996-9-11', "Female", "353 0888907776", "sheryl.roy@gmail.com", 
" A12D890 990, JJMKL", "8.2")
;

ALTER TABLE Student_Details
ADD CONSTRAINT chk_email CHECK(Student_Email LIKE '%_@__%.__%');

INSERT INTO Payroll(
Staff_Role,
Role_Salary,
Role_Hours
)
Values( "Faculty", 50000, 45),
("Professor", 75000, 48),
("Assistant", 45000, 35),
("Intern", 35000, 40),
("Director", 80000, 45);

INSERT INTO Staff_Details(
Staff_ID, 
Staff_FirstName, 
Staff_LastName, 
Staff_DateOfBirth, 
Staff_Gender, 
Staff_PhNo, 
Staff_Email, 
Staff_Address,
DOJ,
Staff_Role
)
Values
(2007000022, "Elaina", "Bonnie",'1971-09-11', "Female", "353 0893090919", "elaina.bonnie@gmail.com", 
" A90BB30 90, CYH street",'2010-01-01', "Director"),
(2011000090, "Bernard", "Enis",'1995-04-01', "Male", "353 0890971014", "benrand.enis@gmail.com", 
" A89GUM1 11, NMY street",'2020-04-01', "Intern")
;

INSERT INTO School(
School_ID,
School_Name,
School_Building
)
Values( "101", "Business", "A Block"),
("102", "Science", "B Block"),
("103", "Arts", "C Block"),
("104", "Sports", "D Block");

INSERT INTO Program_Details(
Program_ID,
Program_Name ,
Program_Duration ,
Program_Director ,
Program_School 
)
Values ( "BU101", "Business Analytics", 12,"2007000022" ,"101"),
("BU102","Management",12,"2007000022", "101");