# pdfalg
Library for extracting data from PDF files

## Building

This Scala project has a circular dependency on [RESTful API project](https://github.com/kveskimae/pdfextractor) in Java and Maven.

Following is a description how to avoid adding Java source files to compilation path:
* First make Java project's database module available in local Maven repo:
```
[~/workspaces]/pdfextractor/db $ mvn install
```
* Now you can publish the Scala project to local Maven repo:
````
[~/workspaces]/pdfalg $ sbt publishM2
````
* The last created binary 

*~/.m2/repository/org/pdfextractor/pdf-extractor-algorithm_2.12/0.1.0-SNAPSHOT/pdf-extractor-algorithm_2.12-0.1.0-SNAPSHOT.jar*

* Now the Scala algorithm dependency becomes available to Java project's services module from following co-ordinates:
````
<dependency>
    <groupId>org.pdfextractor</groupId>
    <artifactId>pdf-extractor-core_2.12</artifactId>
    <version>0.1.0-SNAPSHOT</version>
</dependency>
````
* Finally build the whole Java project as well:
````
~/workspaces/pdfextractor $ mvn clean install
````
