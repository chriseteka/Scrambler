# Scrambler

## How to use

- Place a valid JSON file you want to scramble in your resources folder
- Start the main class `com.chrisworks.Main`
- Choose the file you just placed in the resources folder
- Look at the logs
- When the operation is done, your file will be scrambled

Hypothetically,

What if each of the key pointing to an object is in fact a node, it follows that:
- Create a list to hold each of the node with a list of nodes composed into it
- Build up a data structure whose to string method will build a digraph that will be consumed to make a graph
