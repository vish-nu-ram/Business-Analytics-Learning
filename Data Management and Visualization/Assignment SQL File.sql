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
Values(
2122000001, "Sable", "Kristen",'1995-7-04', "Female", "353 0891234234", 
"sable.kristen@gmail.com", " A12AS34 123, QWERT stret", "7.3")
;
