# Programmeerproject2
A programming project made for the second bachelor of Computer Science, graded 17/20.
The project was created in 3 phases, this being the last one.

# Info
(Copied from the SpecificatieF3.pdf file)
Voor programmeerproject 2 is er een softwaretoepassing gemaakt die een controlesysteem realiseert om modeltreinen te besturen.
Dit systeem wordt opgedeeld in 2 lagen. De eerste heet  Infrabel, deze staat in voor communicatie tussen de software en de hardware. 
De zogenaamde  command & control, de regeling van doorlopend verkeer, is ook de taak van Infrabel. Dit wordt  bereikt d.m.v. Racket. 
De andere component is NMBS samen met de Grafische User Interface  (GUI). 
Dit is de communicatie naar de gebruiker toe en is geen onderdeel van Infrabel, hiervoor  wordt ook Racket gebruikt. 
Er is een duidelijke splitsing, NMBS bouwt boven op de infrastructuur  van Infrabel en ze kunnen draaien als aparte processen.
De uitgebreide functionele vereisten zijn  gefocust op automatisering. Het is mogelijk om een traject te berekenen naar een gekozen locatie, 
botsingen worden voorkomen aan de hand van een reservatiesysteem en de snelheden van de  treinen worden automatisch aangepast op basis van hun traject. 
De automatische  trajectberekening hoort bij NMBS, eenmaal verwerkt stuurt NMBS de juiste commando’s door  naar Infrabel om het effectieve traject af te leggen.
Het voorkomen van botsingen en het  automatisch aansturen van de snelheden worden bestuurd door Infrabel.
