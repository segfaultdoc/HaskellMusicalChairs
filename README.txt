1. The shared resource that represents music is an MVar of type bool. Line 61 the emcee turns on the music. Line 132 is where the music is turned off. I use a list of MVar bools. GenerateChairs function is called at line 82 and the function declaration begins at line 173. True represents and empty chair.

2. Each player thread recursive loops in the sitinchair function, each iteration checking if the chair is empty. If it is it sits. If not it recurses, if it never sits and there are no chairs left, then the player failed to sit. By looping through the list I ensure each chair is attempted to be sat in, and one thread is guaranteed to sit.

3. There was competition for chairs and the strings to be printed. Each thread fought for a the chair resources. The challenging part was getting the emcee to coordinate the music alongside the players. Once I figured this out, it was clear to me how the multithreading worked.

4. Being a concurrency project not much was straightforward, except for the overall design, since we already did this project in Java.

5. Deadlocks were hard to debug, but once I realized that the printing was causing the deadlocks. The way I debugged this was by commenting out certain print statements and figuring out which comments would allow the program to continue execution. I would also place print statements in places to figure out where the program would hang. 
