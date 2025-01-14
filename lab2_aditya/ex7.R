#working with lists and dataframe
Xvec <- c("Aditya","Naman",119,"Ageing",2000,2003,2006,2012,2017,2023)  #create a vector
Xvec
Mylist <- list(name="AN",mixedlist=Xvec,numbers=1:10) #create a list
Mylist
Mylist$numbers #Use $ to subset by structure at each level
Mylist[3] #access the list at index 3
Mylist[[3]] #Mylist[[3]] accesses all the data available in the list 3 (This has class integer) while Mylist[3] at the data contains at third element at list 3 as a list (This has class list).
