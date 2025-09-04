Assumptions - comments
==============================

1st meaningful commit
- assuming that we need to assembly a jar and run it on console
- assuming that no matter the way I read the jar just need to be sure it get's the triangle
- assuming the triangle is composed by integers to create a parser
- It's been ages since last time I was using stdin and a jar but I'll manage it with FS2

Archieved to read from console using fs2 with
cat << EOF | java -jar petrostest-assembly-0.1.0-SNAPSHOT.jar
7  
6 3  
3 8 5  
11 2 10 9  
EOF  

2nd commit
- I decided to use a Vector to move inside the triangle in an easy way
- Creating and testing a parser

3rd commit
- I thought about a basic recursive solution but this has the potential to break the stack
- This is because the same path could be processed several times so the complexity could become exponential
- I'll first evaluate a bottom-up solution with a complexity of O(n²)

4th commit
- I don't like the iterative idea, it's been more or less easy but using mutable objects...
- I also remember our conversation in the interview when I asked you about if you have any 
- eventual consistency issues or race conditions with your clients and I think my code should
- work with that. I'm trying to figure out an idea using FP and recursion and 
- I also would like to go from top to bottom and make the path in just one shot 
- Trying to do it from top without any protections at all

5th commit
- Forgot to change the function to the top-down version
  
6th commit
- Using cats Deferred and Ref to build an atomic system without race conditions
- Testing locally against data_big and small datasets seems to be working but I feel that it could still block
- I see it clearly taking longer than the previous approach. Using the files breaks the memory
- I'm fucked xDDDD

7th commit
- I was stuck so first of all I saw in cats documentation that I should be using MapRef instead of Ref
- Secondly I could see when trying to run the code that the problem was in the parMapN of the compute
- function and could be because an uncontrolled exception so I wrapped the access to the vector in a Delay
- getting referencial transparency in that piece of code
- Also gave the scheduler control again of the threads so I could get the scheduler balance the load
- increasing the responsiveness
