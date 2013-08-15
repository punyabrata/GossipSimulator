import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random

//The Message case class
case class Message(sValue: Double, wValue: Double, msgString:String){}

//The Scala Object
object project2 {
  def main(args: Array[String]): Unit = {
    //No. of nodes should be at least '4'
    if(args(1) == "2D-Grid" || args(1) == "Imperfect-2D-Grid"){
      if(args(0).toInt < 4){
        println("No. of nodes should be at least '4' for 2D-Grid or Imperfect-2D-Grid. Try again!")
        return
      }
    }
    //Checking for any other algorithm than gossip or push-sum
    if(args(2) != "gossip" && args(2) != "push-sum"){
      println("Suppored algorithms are:\n1.gossip\n2.push-sum\nThese names are case sensitive. Try again!")
      return
    }
    //Initializing the main actor
    val theMainActor = new TheMainActor(args(0).toInt,args(1),args(2))
    theMainActor.start
  }
}

//Grid Structure class - used both for 2D Grid and Imperfect 2D Grid
class GridStructure(hasLeft: Boolean,hasTop: Boolean,hasRight: Boolean,hasBottom: Boolean,indexOfRandomNode: Int){
  var gridHasLeft: Boolean = hasLeft
  var gridHasTop: Boolean = hasTop
  var gridHasRight: Boolean = hasRight
  var gridHasBottom: Boolean = hasBottom
  var gridIndexRandom: Int = indexOfRandomNode
}

//The Main Actor class
class TheMainActor(numNodes: Int, topology: String, algorithm: String) extends Actor{
  var responseCount: Int = 0
  //The act method
  def act(){        
//    println("Values for topology: Full-Network, 2D-Grid, Line, Imperfect-2D-Grid")    
//    println("Entered topology: "+topology)
//    println("Entered algorithm: "+algorithm)
//    println("Entered number of nodes: "+numNodes)    
    var noOfNodes: Int = numNodes    
    var computation = new Computation()    
    
    //Line Topology    
    if(topology == "Line"){
      //The node array
      var nodeArray: Array[Node] = new Array[Node](noOfNodes)      
      //Initialization of nodes
      for(i: Int <- 0 until noOfNodes){
        //Only one neighbor for the ending node
        if(i == 0 || i == (noOfNodes-1)){
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](1),this)  
          nodeArray(i) = node
        }else{
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](2),this)
          nodeArray(i) = node
        }        
      }      
      println("------------- NODE INITIALIZATION ENDS ------------")
      println("---------------------------------------------------")      
      //Initialization of neighbors
      for(j: Int <- 0 until noOfNodes){
        if(j == 0){
          //Left ending node
          nodeArray(j).assignNeighbors(1,Array[Node](nodeArray(j+1)))
        }else if(j == (noOfNodes-1)){
          //Right ending node
          nodeArray(j).assignNeighbors(1,Array[Node](nodeArray(j-1)))
        }else{
          //All other nodes
          nodeArray(j).assignNeighbors(2,Array[Node](nodeArray(j-1),nodeArray(j+1)))
        }
        //For the ease of understanding:
        //Current node
        println("current node name: "+nodeArray(j).nodeName)
        //List of neighbors
        if(nodeArray(j).nodeNeighbors.size > 1){
          println("first neighbor: "+nodeArray(j).nodeNeighbors(0).nodeName)
          println("second neighbor: "+nodeArray(j).nodeNeighbors(1).nodeName)
        }else
          println("only neighbor: "+nodeArray(j).nodeNeighbors(0).nodeName)
      }      
      println("-------------- TOPOLOGY IS BUILT -----------------")
      println("--------------------------------------------------")      
      //Choosing a random node to start with
      var randomStartIndex: Int = computation.generateRandom(noOfNodes)      
      println("Random start node: "+randomStartIndex)      
      nodeArray(randomStartIndex).theReceiverActor ! new Message(0.0,0.0,"rumor") 
      //The failure scenario
      randomStartIndex = computation.generateRandom(noOfNodes)
      nodeArray(randomStartIndex).isAlive = false
    }
    
    //Full Network Topology
    else if(topology == "Full-Network"){
      //The node array
      var nodeArray: Array[Node] = new Array[Node](noOfNodes)      
      //Initialization of nodes
      for(i: Int <- 0 until noOfNodes){
        val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](noOfNodes-1),this)
        nodeArray(i) = node
      }      
      println("------------- NODE INITIALIZATION ENDS ------------")
      println("---------------------------------------------------")            
      //Initialization of neighbors      
      for(j: Int <- 0 until noOfNodes){
        var neighborArray: Array[Node] = new Array[Node](noOfNodes-1)
        var m: Int = 0
        for(k: Int <- 0 until (noOfNodes-1)){          
          if(j==k)
            m += 1
          neighborArray(k) = nodeArray(m)
          m += 1
        }	      
        nodeArray(j).assignNeighbors(noOfNodes - 1,neighborArray)           
        //For the ease of understanding:
        //Current node
        println("current node name: "+nodeArray(j).nodeName)
        //List of neighbors
        for(l: Int <- 0 until (noOfNodes-1))
          println("neighbor: "+nodeArray(j).nodeNeighbors(l).nodeName)        
      }      
      println("------------- TOPOLOGY IS BUILT ------------")
      println("--------------------------------------------") 
      //Choosing a random node to start with
      var randomStartIndex: Int = computation.generateRandom(noOfNodes)      
      println("Random start node: "+randomStartIndex)
      nodeArray(randomStartIndex).theReceiverActor ! new Message(0.0,0.0,"rumor")
      //The failure scenario
      randomStartIndex = computation.generateRandom(noOfNodes)
      nodeArray(randomStartIndex).isAlive = false
    }
    
    //2D Grid Topology
    else if(topology == "2D-Grid"){
      //Taking the nearest perfect square smaller than the currently entered number
      while(!computation.isPerfectSquare(noOfNodes)){
        noOfNodes -= 1
      }
      //The node array
      var nodeArray: Array[Node] = new Array[Node](noOfNodes)
      //Instantiating the Grid Structure Array
      var gridStructureArray: Array[GridStructure] = new Array[GridStructure](noOfNodes)           
      //Instantiating Grid Structures for every node of the Array
      for(p: Int <- 0 until noOfNodes){
        var myGS: GridStructure = new GridStructure(false,false,false,false,0)
        gridStructureArray(p) = myGS
      }      
      //Initialization of nodes
      for(i: Int <- 0 until noOfNodes){
        if((i % (Math.sqrt(noOfNodes)) == 0) && (i <= (Math.sqrt(noOfNodes)-1))){
          //Should not have both left and top neighbors
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](2),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true          
        }else if((i <= (Math.sqrt(noOfNodes)-1)) && ((i+1) % (Math.sqrt(noOfNodes)) == 0)){
          //Should not have both top and right neighbors
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](2),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = true             
        }else if(((i+1) % (Math.sqrt(noOfNodes)) == 0) && (i >= (Math.sqrt(noOfNodes)-1)*(Math.sqrt(noOfNodes)))){
          //Should not have both right and bottom neighbors
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](2),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = false           
        }else if((i >= (Math.sqrt(noOfNodes)-1)*(Math.sqrt(noOfNodes))) && (i % (Math.sqrt(noOfNodes)) == 0)){
          //Should not have both bottom and left neighbors
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](2),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = false           
        }else if(i % (Math.sqrt(noOfNodes)) == 0){
          //Should not have left neighbor
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](3),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true             
        }else if((i+1) % (Math.sqrt(noOfNodes)) == 0){
          //Should not have right neighbor
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](3),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = true             
        }else if(i <= (Math.sqrt(noOfNodes)-1)){
          //Should not have top neighbor
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](3),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true             
        }else if(i >= (Math.sqrt(noOfNodes)-1)*(Math.sqrt(noOfNodes))){
          //Should not have bottom neighbor
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](3),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = false             
        }else{
          //Should have all four neighbors
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](4),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true             
        }
      }      
      println("------------- NODE INITIALIZATION ENDS ------------")
      println("---------------------------------------------------")                  
      //Initialization of neighbors
      for(j: Int <- 0 until noOfNodes){
        //List of Neighbors        
        var neighborList = List[Node]()
        //Adding to the list one by one from the nodeArray based on whether or not it has got certain neighbors
        if(gridStructureArray(j).gridHasLeft)
          neighborList ::= nodeArray(j-1)
        if(gridStructureArray(j).gridHasTop)
          neighborList ::= nodeArray(j-(Math.sqrt(noOfNodes)).toInt)
        if(gridStructureArray(j).gridHasRight)
          neighborList ::= nodeArray(j+1)
        if(gridStructureArray(j).gridHasBottom)
          neighborList ::= nodeArray(j+(Math.sqrt(noOfNodes)).toInt)  
        //Array of neighbors
        var neighborArray: Array[Node] = new Array[Node](neighborList.size)
        //Adding from the list to the array
        for(l:Int <- 0 until neighborList.size){
          neighborArray(l) = neighborList(l)
        }
        //Assigning neighbors to the node  
        nodeArray(j).assignNeighbors(neighborList.size,neighborArray)
        //For the ease of understanding:
        //Current node
        println("Current node: "+nodeArray(j).nodeName)
        //List of neighbors
        for(p: Int <- 0 until neighborList.size){
          println(p+"-th neighbor: "+neighborList(p).nodeName)
        }
      }
      //Choosing a random node to start with
      var randomStartIndex: Int = computation.generateRandom(noOfNodes)      
      println("Random start node: "+randomStartIndex)            
      nodeArray(randomStartIndex).theReceiverActor ! new Message(0.0,0.0,"rumor")
      //The failure scenario
      randomStartIndex = computation.generateRandom(noOfNodes)
      nodeArray(randomStartIndex).isAlive = false      
    }
    
    //Imperfect 2D Grid Topology
    else if(topology == "Imperfect-2D-Grid"){
      //Taking the nearest perfect square smaller than the currently entered number
      while(!computation.isPerfectSquare(noOfNodes)){
        noOfNodes -= 1
      }      
      println("noOfNodes: "+noOfNodes)
      //The node array
      var nodeArray: Array[Node] = new Array[Node](noOfNodes)
      //Instantiating the Grid Structure Array
      var gridStructureArray: Array[GridStructure] = new Array[GridStructure](noOfNodes)
      //Instantiating Grid Structures for every node of the Array
      for(p: Int <- 0 until noOfNodes){
        var myGS: GridStructure = new GridStructure(false,false,false,false,0)
        gridStructureArray(p) = myGS
      }      
      //Initialization of nodes
      for(i: Int <- 0 until noOfNodes){
        if((i % (Math.sqrt(noOfNodes)) == 0) && (i <= (Math.sqrt(noOfNodes)-1))){
          //Should not have both left and top neighbors
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](3),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true
          //One random neighbor
          var generatedRandom: Int = computation.generateRandom(noOfNodes)
          while(generatedRandom == i || 
              generatedRandom == i+1 ||                
              generatedRandom == (i+Math.sqrt(noOfNodes))){
            generatedRandom = computation.generateRandom(noOfNodes)
          }          
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th node | "+generatedRandom)          
        }else if((i <= (Math.sqrt(noOfNodes)-1)) && ((i+1) % (Math.sqrt(noOfNodes)) == 0)){
          //Should not have both top and right neighbors
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](3),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = true   
          //One random neighbor
          var generatedRandom: Int = computation.generateRandom(noOfNodes)
          while(generatedRandom == i || 
              generatedRandom == i-1 ||                
              generatedRandom == (i+Math.sqrt(noOfNodes))){
            generatedRandom = computation.generateRandom(noOfNodes)
          }          
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th node | "+generatedRandom)          
        }else if(((i+1) % (Math.sqrt(noOfNodes)) == 0) && (i >= (Math.sqrt(noOfNodes)-1)*(Math.sqrt(noOfNodes)))){
          //Should not have both right and bottom neighbors
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](3),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = false
          //One random neighbor          
          var generatedRandom: Int = computation.generateRandom(noOfNodes)
          while(generatedRandom == i || 
              generatedRandom == i-1 ||                
              generatedRandom == (i-Math.sqrt(noOfNodes))){
            generatedRandom = computation.generateRandom(noOfNodes)
          }          
          gridStructureArray(i).gridIndexRandom = generatedRandom 
          println(i+"-th node | "+generatedRandom)          
        }else if((i >= (Math.sqrt(noOfNodes)-1)*(Math.sqrt(noOfNodes))) && (i % (Math.sqrt(noOfNodes)) == 0)){
          //Should not have both bottom and left neighbors
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](3),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = false            
          //One random neighbor
          var generatedRandom: Int = computation.generateRandom(noOfNodes)
          while(generatedRandom == i || 
              generatedRandom == i+1 ||                
              generatedRandom == (i-Math.sqrt(noOfNodes))){
            generatedRandom = computation.generateRandom(noOfNodes)
          }          
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th node | "+generatedRandom)          
        }else if(i % (Math.sqrt(noOfNodes)) == 0){
          //Should not have left neighbor
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](4),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = false
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true                       
          //One random neighbor
          var generatedRandom: Int = computation.generateRandom(noOfNodes)
          while(generatedRandom == i || 
              generatedRandom == i+1 ||
              generatedRandom == (i+Math.sqrt(noOfNodes)) ||
              generatedRandom == (i-Math.sqrt(noOfNodes))){
            generatedRandom = computation.generateRandom(noOfNodes)
          }          
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th node | "+generatedRandom)          
        }else if((i+1) % (Math.sqrt(noOfNodes)) == 0){
          //Should not have right neighbor
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](4),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = false
          gridStructureArray(i).gridHasBottom = true 
          //One random neighbor
          var generatedRandom: Int = computation.generateRandom(noOfNodes)
          while(generatedRandom == i || 
              generatedRandom == i-1 ||
              generatedRandom == (i+Math.sqrt(noOfNodes)) ||
              generatedRandom == (i-Math.sqrt(noOfNodes))){
            generatedRandom = computation.generateRandom(noOfNodes)
          }          
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th node | "+generatedRandom)          
        }else if(i <= (Math.sqrt(noOfNodes)-1)){
          //Should not have top neighbor
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](4),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = false
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true      
          //One random neighbor          
          var generatedRandom: Int = computation.generateRandom(noOfNodes)
          while(generatedRandom == i || 
              generatedRandom == i+1 ||
              generatedRandom == i-1 ||
              generatedRandom == (i+Math.sqrt(noOfNodes))){
            generatedRandom = computation.generateRandom(noOfNodes)
          }          
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th node | "+generatedRandom)                    
        }else if(i >= (Math.sqrt(noOfNodes)-1)*(Math.sqrt(noOfNodes))){
          //Should not have bottom neighbor
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](4),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = false   
          //One random neighbor          
          var generatedRandom: Int = computation.generateRandom(noOfNodes)
          while(generatedRandom == i || 
              generatedRandom == i+1 ||
              generatedRandom == i-1 ||
              generatedRandom == (i-Math.sqrt(noOfNodes))){
            generatedRandom = computation.generateRandom(noOfNodes)
          }          
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th node | "+generatedRandom)              
        }else{
          //Should have all four neighbors
          val node = new Node("Node"+i,algorithm,i.toDouble,1.0,new Array[Node](5),this)
          nodeArray(i) = node
          gridStructureArray(i).gridHasLeft = true
          gridStructureArray(i).gridHasTop = true
          gridStructureArray(i).gridHasRight = true
          gridStructureArray(i).gridHasBottom = true     
          //One random neighbor          
          var generatedRandom: Int = computation.generateRandom(noOfNodes)
          while(generatedRandom == i || 
              generatedRandom == i+1 ||
              generatedRandom == i-1 ||
              generatedRandom == (i+Math.sqrt(noOfNodes)) ||
              generatedRandom == (i-Math.sqrt(noOfNodes))){
            generatedRandom = computation.generateRandom(noOfNodes)
          }          
          gridStructureArray(i).gridIndexRandom = generatedRandom
          println(i+"-th node | "+generatedRandom)          
        }
      }
      println("------------- NODE INITIALIZATION ENDS ------------")
      println("---------------------------------------------------")            
      //Initialization of neighbors
      for(j: Int <- 0 until noOfNodes){
        //List of Neighbors        
        var neighborList = List[Node]()
        //Adding to the list one by one from the nodeArray based on whether or not it has got the neighbors
        //Also adding the random neighbor
        if(gridStructureArray(j).gridHasLeft)
          neighborList ::= nodeArray(j-1)
        if(gridStructureArray(j).gridHasTop)
          neighborList ::= nodeArray(j-(Math.sqrt(noOfNodes)).toInt)
        if(gridStructureArray(j).gridHasRight)
          neighborList ::= nodeArray(j+1)
        if(gridStructureArray(j).gridHasBottom)
          neighborList ::= nodeArray(j+(Math.sqrt(noOfNodes)).toInt)  
        neighborList ::= nodeArray(gridStructureArray(j).gridIndexRandom)
        //Array of neighbors
        var neighborArray: Array[Node] = new Array[Node](neighborList.size)
        //Adding from the list to the array
        for(l:Int <- 0 until neighborList.size){
          neighborArray(l) = neighborList(l)
        }
        //Assigning neighbors to the node  
        nodeArray(j).assignNeighbors(neighborList.size,neighborArray)
        //For the ease of understanding:
        //Current node
        println("Current node: "+nodeArray(j).nodeName)
        //List of neighbors
        for(p: Int <- 0 until neighborList.size){
          println(p+"-th neighbor: "+neighborList(p).nodeName)
        }                
      }
      //Choosing a random node to start with
      var randomStartIndex: Int = computation.generateRandom(noOfNodes)      
      println("Random start node: "+randomStartIndex)      
      nodeArray(randomStartIndex).theReceiverActor ! new Message(0.0,0.0,"rumor")
      //The failure scenario
      randomStartIndex = computation.generateRandom(noOfNodes)
      nodeArray(randomStartIndex).isAlive = false      
    }
    
    //No topology matches
    else{
      println("This topology is not supported.\nSuppored topologies are:\n1.Line\n2.Full-Network\n3.2D-Grid\n4.Imperfect-2D-Grid\nThese names are case sensitive. Try again!")
      exit
    }      
    
    //Time counting starts
    val timeAfterTopologyBuilt = System.currentTimeMillis;
    
    //Waiting to hear responses from the node actors
    loop{
      react{
        case msg: Message =>
          responseCount += 1
          //Main actor exits only if it gets messages/responses back from all the node actors
          if(responseCount == noOfNodes){
            println("---------------------")
            println("MAIN ACTOR EXITS")
            println("---------------------")
            //Printing the total time
            println("Total time taken: "+(System.currentTimeMillis-timeAfterTopologyBuilt)+" ms")
            exit            
          }
      }
    }    
//    println(timeAfterTopologyBuilt-System.currentTimeMillis)
  }
}

//The computation class - it checks for a perfect square and generates a random number
class Computation{
  //To check whether it's a perfect square or not
  def isPerfectSquare(number: Int): Boolean = {
    val sqaureRoot = Math.sqrt(number)
    if((sqaureRoot%1)>0)
      return false
    else
      return true
  }
  //To generate Random number
  def generateRandom(seed: Int): Int = {    
    return new Random().nextInt(seed)
  }  
}

//The Node Actor class
class Node(name: String, algorithm: String, sValue: Double, wValue: Double, neighbors: Array[Node], mainActor: Actor){
  
  //Property members of the node actor class  
  var nodeName: String = name
  var nodeAlgorithm: String = algorithm
  var nodeNeighbors: Array[Node] = neighbors
  var messageCount: Int = 0	//Required for Gossip
  var theSenderActor: Actor = this.sendMessage
  var theReceiverActor: Actor = this.receiveMessage
  var theMainActor: Actor = mainActor    
  
  //Property members especially required for Gossip
  var isAlive: Boolean = true
  var nodeSValue: Double = sValue
  var nodeWValue: Double = wValue  
  var rCurrent: Double = this.nodeSValue/this.nodeWValue
  var rPrevious: Double = -100.0
  var rSPrevious: Double = -200.0    
  
  //This method assigns neighbors to a node  
  def assignNeighbors(noOfNeighbors: Int,listOfNeighbors: Array[Node]): Unit = {
    for(i:Int <- 0 until noOfNeighbors){
      this.nodeNeighbors(i) = listOfNeighbors(i)
    }
  }
  
  //The Receiver Actor
  def receiveMessage: Actor = {
    println("inside receive message of "+this.nodeName)
    val theReceiver = actor{
      loop{
	      react{
	        case msg:Message =>
	          println("msg.msgString "+msg.msgString)	          
	          if(msg.msgString.equals("rumor")){
	            if(this.algorithm.equals("gossip")){
	              println("this.messageCount: "+this.messageCount+"for: "+this.nodeName)
	              //Converges when the message count reaches 10
		            if(this.messageCount < 10){
		              this.messageCount += 1
		              //Sending to the sender actor while working
		              this.theSenderActor ! msg
		              println("sent to the sender")
		            }else{
		              //Asking the sender actor to stop and informing the main actor that it has converged
		              this.theSenderActor ! new Message(0.0,0.0,"stop")
		              this.theMainActor ! new Message(0.0,0.0,"done")
		              println(this.nodeName+" receiver exits")
		              println("-----------------------------")
		              exit
		            }
	            }else if(this.algorithm.equals("push-sum")){
	              //Computations for Push-Sum
	              println("Before Calculation in Receiver: "+this.nodeSValue+" | "+this.nodeWValue)
	              this.nodeSValue += msg.sValue
	              this.nodeWValue += msg.wValue	              
	              this.rSPrevious = this.rPrevious
	              this.rPrevious = this.rCurrent
	              this.rCurrent = this.nodeSValue/this.nodeWValue
	              println("After Calculation in Receiver: "+this.nodeSValue+" | "+this.nodeWValue)
	              println("this.rCurrent: "+this.rCurrent+" this.rPrevious: "+this.rPrevious+" this.rSPrevious: "+this.rSPrevious+" for: "+this.nodeName)	  
		          //Converges when the s/w value did not change more than 0.0000000001 for 3 consecutive rounds    
	              if(Math.abs(this.rCurrent-this.rPrevious) > 0.0000000001 
	                  || Math.abs(this.rCurrent-this.rSPrevious) > 0.0000000001 
	                  || Math.abs(this.rSPrevious-this.rPrevious) > 0.0000000001){	   
	                //Passing to the sender actor
		              this.theSenderActor ! (new Message(this.nodeSValue,this.nodeWValue,msg.msgString))
		              println("sent to the sender")
	              }else{
	                //Making itself dead
	                this.isAlive = false
	                //Passing to sender actor for forwarding before it dies
	                this.theSenderActor ! (new Message(this.nodeSValue,this.nodeWValue,msg.msgString))
	                //Asking the sender actor to stop
	                this.theSenderActor ! new Message(0.0,0.0,"stop")
	                //Informing the main actor that it has converged
	                this.theMainActor ! new Message(0.0,0.0,"done")
	                println(this.nodeName+" receiver exits ")
	                println("------------------------------")
	                exit	                
	              }
	            }	            
	          }
	      }
      }
    }
    theReceiver
  }
  
  //The Sender Actor
  def sendMessage: Actor = {
    println("inside send message of "+this.nodeName)
    val theSender = actor{
      loop{
	      react{
	        case msg:Message =>
	          if(this.algorithm.equals("gossip")){
	            //Sender actor runs till it converges
		          while(this.messageCount < 10){
		            var randomNeighborIndex: Int = (new Computation()).generateRandom(this.nodeNeighbors.size)
		            //Checking for an alive neighbor
		            while(this.nodeNeighbors(randomNeighborIndex).isAlive == false){
		              randomNeighborIndex = (new Computation()).generateRandom(this.nodeNeighbors.size)
		            }
		            //Forwarding the message to an alive random neighbor
		            this.nodeNeighbors(randomNeighborIndex).theReceiverActor ! msg
		          }
	          }else if(this.algorithm.equals("push-sum") && !msg.msgString.equals("stop")){
		            println("Before Calculation in Sender: "+this.nodeSValue+" | "+this.nodeWValue)
		            this.nodeSValue /= 2
		            this.nodeWValue /= 2
		            println("After Calculation in Sender: "+this.nodeSValue+" | "+this.nodeWValue)
		            //Checking if there is any alive neighbor
		            var isAliveNeighbor: Boolean = false
		            for(i: Int <- 0 until this.nodeNeighbors.size){
		              if(nodeNeighbors(i).isAlive)
		                isAliveNeighbor = true
		            }
		            //Sending to an alive neighbor
		            if(isAliveNeighbor){
			            var randomNeighborIndex: Int = (new Computation()).generateRandom(this.nodeNeighbors.size)
			            while(this.nodeNeighbors(randomNeighborIndex).isAlive == false){
			              randomNeighborIndex = (new Computation()).generateRandom(this.nodeNeighbors.size)
			            }
			            println("randomNeighborIndex: "+randomNeighborIndex+" for "+this.nodeName)
			            this.nodeNeighbors(randomNeighborIndex).theReceiverActor ! (new Message(this.nodeSValue,this.nodeWValue,msg.msgString))
		            }
		            //Informing the main actor about force exit if no alive neighbor
		            else{
		              this.isAlive = false
		              this.theMainActor ! new Message(0.0,0.0,"done")	
		              println(this.nodeName+"'s sender and receiver actor force-exited.")
		              exit
		            }		            
	          }
	          if(msg.msgString.equals("stop")){
	        	  println(this.nodeName+" sender exits ")
	              println("============================")            
	            exit
	          }
	      }
      }
    }
    theSender
  }
}