// #include <emscripten/emscripten.h>
#include <emscripten/bind.h>
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <math.h>
#include <numeric>
#include <unordered_map>

using namespace emscripten;

// ----------------------------------------------------------------------------------------
// STRUCTS
// ----------------------------------------------------------------------------------------

struct Node { 
    int nodeId;
    std::string label;
    float truth;
    float indeterminacy;
    float falsehood;
    std::string state;
    std::string linkState;
    int inDegree;
    int outDegree;
};

struct Edge {
    int edgeId;
    int from;
    int to;
    float truth;
    float indeterminacy;
    float falsehood;
};

class NeutroCalculator {
public:
    NeutroCalculator
        ( std::string y
        // , std::vector<Node> nodes
        // , std::vector<Edge> edges
        // , std::vector<Node> finalResNodes
        // , std::vector<Node> tempResNodes 
        )
        : y(y)
        // , nodes(nodes)
        // , edges(edges)
        // , finalResNodes(finalResNodes)
        // , tempResNodes(tempResNodes)
    {}


// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------




    static std::string getStringFromInstance(const NeutroCalculator& instance) 
    {
        return instance.y;
    }

    Node getSimulationResult(int index) const { return finalResNodes[index]; }

    // ------------------------------------------------------------------------------------
    // SIMULATE FUNCTION
    // ------------------------------------------------------------------------------------    



    void simulate()
    {
        
        // 1- DATA VALIDATION
        // ----------------------------------------------------------------------------------------

            // EDGES VALIDATION 

            // IsEmpty
            if (edges.size() == 0)
            {
                std::cout << "There are no edges to process this simulation." << std::endl;
                exit(0);
            } 
            else
            {
                // (FROM/TO)
                for (int i = 0; i < edges.size(); i++)
                {   
                    if (edges[i].from == edges[i].to)
                    {
                        std::cout << "Edge ID" << edges[i].edgeId << " is invalid. FROM == TO." << std::endl;
                        exit(0);
                    }
                }
            }

            // NODES VALIDATION 

            // IsEmpty
            if (nodes.size() == 0)
            {
                std::cout << "There are no nodes to process this simulation." << std::endl;
                exit(0);
            }
            else
            {
                // (LABELS)
                for (int i = 0; i < nodes.size() - 1; i++)
                {   
                    if (nodes[i].label == nodes[i + 1].label)
                    {
                        std::cout << "Node ID: " << nodes[i].nodeId << " and " << nodes[i+1].nodeId << " have the same label. Invalid nodes." << std::endl;
                        exit(0);
                    }
                }
            }

        // 2- EDGES DATA PREPPING  COMPUTATION
        // ----------------------------------------------------------------------------------------

            // 2.0- Create hash map for nodes
            std::unordered_map<int, Node> hashMapNodes {};
            for(int i = nodes.size()-1; i >= 0; i--)
            {
                int currNodeId = nodes[i].nodeId;
                Node currNode = nodes[i];

                hashMapNodes[currNodeId] = currNode;               
            }

            // 2.1- Create temporary edge category vectors and separate the edges in them
            std::vector<Edge> simNodeEdges, transNodeEdges, ordiNodeEdges, targetNodeEdges;
            for (int i = 0; i < edges.size(); i++)
            {
                Node currTransmitterNode = hashMapNodes[edges[i].from];
                Node currReceiverNode = hashMapNodes[edges[i].to];
                Edge currEdge = edges[i];

                if(currTransmitterNode.state == "Sim")
                {
                    simNodeEdges.push_back(currEdge);
                }
                else if (currTransmitterNode.linkState == "Tra")
                {                
                    transNodeEdges.push_back(currEdge);
                }
                else if (currReceiverNode.state == "Tar")
                {
                    targetNodeEdges.push_back(currEdge);
                }
                else
                {
                    ordiNodeEdges.push_back(currEdge);
                }
            }

            // 2.2- Sort edges in each category
            sortEdges(simNodeEdges, hashMapNodes); // Sorting Simulated Node Edges
            sortEdges(transNodeEdges, hashMapNodes); // Sorting transmitter Node Edges
            sortEdges(ordiNodeEdges, hashMapNodes); // Sorting ordinary Node Edges                
            sortEdges(targetNodeEdges, hashMapNodes); // Sorting target Node Edges

            // 2.3- Insert sorted edges into edgesCopy vector
            edgesCopy = appendToEdgeList(edgesCopy, simNodeEdges); // Inserting Simulated Node Edges to edgesCopy
            edgesCopy = appendToEdgeList(edgesCopy, transNodeEdges); // Inserting Transmitter Node Edges to edgesCopy
            edgesCopy = appendToEdgeList(edgesCopy, ordiNodeEdges); // Inserting ordinary Node Edges to edgesCopy
            edgesCopy = appendToEdgeList(edgesCopy, targetNodeEdges); // Inserting target Node Edges to edgesCopy

        // 3- DATA COMPUTATION
        // ----------------------------------------------------------------------------------------
            // 3.1- Processing all the node relationships (edges)
            for (int i = 0; i < edgesCopy.size(); i++)
            {
                Edge currEdge;
                Node currTransmitter, currReceiver;

                currEdge = edgesCopy[i];
                currTransmitter = hashMapNodes[currEdge.from];
                currReceiver = hashMapNodes[currEdge.to];

                // Check current Transmitter for type ordinary and temporary processed result for not empty
                if(currTransmitter.linkState == "Ord")
                {
                    if(tempResNodes.size() > 0)
                    {

                        std::vector<Node> sameIdProcessedNodes;

                        // Search if current Transmitter has been processed before and store the result separatly 
                        for (int j = 0; j < tempResNodes.size(); j++)
                        {
                            if (tempResNodes[j].nodeId == currTransmitter.nodeId)
                            {
                                sameIdProcessedNodes.push_back(tempResNodes[j]);
                            }
                        }
                        
                        // Current transmitter has more than one processed state (multi-edges processed for current transmitter)
                        if(sameIdProcessedNodes.size() > 1)
                        {
                            Node processedNodeAdjacency = hybridAvgAdj(sameIdProcessedNodes);

                            // Updating current transmitter neutro number for processing current edge                            
                            currTransmitter.truth = processedNodeAdjacency.truth;
                            currTransmitter.indeterminacy = processedNodeAdjacency.indeterminacy;
                            currTransmitter.falsehood = processedNodeAdjacency.falsehood;

                            // Deletes processed results for current Transmitter from tempResNodes 
                            for (int y = 0; y < tempResNodes.size(); y++)
                            {
                                if (tempResNodes[y].nodeId == currTransmitter.nodeId)
                                {
                                    tempResNodes.erase(begin(tempResNodes)+y);
                                }
                            }
                            
                            // Stores processed adjacency for current transmitter in tempResNodes
                            tempResNodes.push_back(processedNodeAdjacency);
                        }

                        // Current transmitter has one processed state (single-edge processed for current transmitter)                        
                        else if(sameIdProcessedNodes.size() == 1)
                        {
                            Node processedNode = sameIdProcessedNodes[sameIdProcessedNodes.size()-1];
                            
                            // Updating current transmitter neutro number for processing current edge                            
                            currTransmitter.truth = processedNode.truth;
                            currTransmitter.indeterminacy = processedNode.indeterminacy;
                            currTransmitter.falsehood = processedNode.falsehood;
                        }

                        // Delete the current processed MultiEdge Nodes result for next iteration check
                        sameIdProcessedNodes.erase(begin(sameIdProcessedNodes),end(sameIdProcessedNodes));
                    }
                }

                Node currReceiverProcessed = {};
                // Processing the edge

                if (currEdge.truth < currEdge.falsehood)
                {
                    // Inverse impact between nodes 
                    currEdge = complement(currEdge);
                    currReceiverProcessed = inverselyPropRelation(currReceiver, currTransmitter, currEdge);
                    tempResNodes.push_back(currReceiverProcessed);
                }
                else
                {
                    // Direct impact between nodes
                    currReceiverProcessed = directlyPropRelation(currReceiver, currTransmitter, currEdge);
                    tempResNodes.push_back(currReceiverProcessed);
                }

            }

            // 3.2- Final adjacency check
            for (int i = 0; i < tempResNodes.size();i++)
            {
                std::vector<Node> finalAdjacencyProcessing {};
                Node currNode = tempResNodes[i];
                for (int j = 0; j < tempResNodes.size();j++)
                {   
                    if (currNode.nodeId == tempResNodes[j].nodeId)
                    {
                        finalAdjacencyProcessing.push_back(tempResNodes[j]);
                    }            
                }

                Node currNodeFinalProcessing = {};
                if(finalAdjacencyProcessing.size() > 1)
                {
                    currNodeFinalProcessing = hybridAvgAdj(finalAdjacencyProcessing);
                }
                else
                {
                    currNodeFinalProcessing = finalAdjacencyProcessing[finalAdjacencyProcessing.size()-1];
                }

                // Delete final adj processing vector elements for next iteration
                finalAdjacencyProcessing.erase(begin(finalAdjacencyProcessing)+finalAdjacencyProcessing.size()-1);
                
                // Store current node final result in finalResNodes (only receiver or ordinary nodes)
                finalResNodes.push_back(currNodeFinalProcessing);            
            }

            // Delete tempResNode
            tempResNodes.erase(begin(tempResNodes), end(tempResNodes));
                     
            // 3.3- Store transmitters and zeroLink nodes in the finalResNodes
            for (int i = 0; i < nodes.size(); i++)
            {
                Node currNode = hashMapNodes[nodes[i].nodeId];
                if(currNode.linkState == "Tra" || (currNode.inDegree == 0 && currNode.outDegree == 0))
                {
                    finalResNodes.push_back(currNode);
                }
            }

            // 3.4- Sort final results vector by nodeId
            finalResNodes = sortNodes(finalResNodes);

            // 3.5- Delete duplicate results in the final results nodes vector (these represent adjacency duplication) <-- this process is a temp solution
            finalResNodes = deleteDuplicateNodes(finalResNodes);

            std::cout << std::endl;
            std::cout << "Original Edges:" << edges.size() << std::endl;

            std::cout << std::endl;
            std::cout << "Edges Copy:" << edgesCopy.size() << std::endl;

            std::cout << std::endl;
            std::cout << "Original Nodes:" << nodes.size() << std::endl;

            std::cout << std::endl;
            std::cout << "FinalResNodes:" << finalResNodes.size() << std::endl;
    }



    // ------------------------------------------------------------------------------------
    // NEUTRO PROCESSING NODE STATE
    // ------------------------------------------------------------------------------------    



    // (T > F) - ADDITION FORK
    Node directlyPropRelation(Node destinyNode, Node originNode, Edge destinyOriginEdge)
    {
        Node tempMultiplicationRes, tempFinalRes, res;

        tempMultiplicationRes = multiply(originNode, destinyOriginEdge);
        tempFinalRes = addition(destinyNode, tempMultiplicationRes);
        
        res.nodeId = destinyNode.nodeId;
        res.label = destinyNode.label;
        res.state = destinyNode.state;
        res.linkState = destinyNode.linkState;
        res.inDegree = destinyNode.inDegree;
        res.outDegree = destinyNode.outDegree;

        res.truth = tempFinalRes.truth;
        res.indeterminacy = tempFinalRes.indeterminacy;
        res.falsehood = tempFinalRes.falsehood;

        return res;
    }

    // (T < F) - SUBTRACTION FORK
    Node inverselyPropRelation(Node destinyNode, Node originNode, Edge destinyOriginEdge)
    {
        // VAR DECLARATION
        Node tempMultiplicationRes, tempFinalRes, res;

        tempMultiplicationRes = multiply(originNode, destinyOriginEdge);
        tempFinalRes = subtraction(destinyNode, tempMultiplicationRes);
        
        res.nodeId = destinyNode.nodeId;
        res.label = destinyNode.label;
        res.state = destinyNode.state;
        res.linkState = destinyNode.linkState;
        res.inDegree = destinyNode.inDegree;
        res.outDegree = destinyNode.outDegree;

        res.truth = tempFinalRes.truth;
        res.indeterminacy = tempFinalRes.indeterminacy;
        res.falsehood = tempFinalRes.falsehood;

        return res;
    }



    // ------------------------------------------------------------------------------------
    // NEUTRO ADJACENCY
    // ------------------------------------------------------------------------------------



    // MIN-MAX - NOT USED
    Node minMaxAdj(std::vector<Node> n)
    {
        Node res;
        std::vector<float> tempTrus, tempInds, tempFals;
        
        res.nodeId = n[0].nodeId;
        res.label = n[0].label;
        res.state = n[0].state;
        res.linkState = n[0].linkState;


        // Storing trus, inds, fals in temp vectors for the method computation
        for(int i = 0; i < n.size();i++)
        {
            tempTrus.push_back(n[i].truth);
            tempInds.push_back(n[i].indeterminacy);
            tempFals.push_back(n[i].falsehood);
        }
        
        res.truth = *max_element(tempTrus.begin(), tempTrus.end());
        res.indeterminacy = *min_element(tempInds.begin(), tempInds.end());
        res.falsehood = *min_element(tempFals.begin(), tempFals.end());
    
        return res;
    }

    // ARITHMETIC AVG - NOT USED
    Node arithAvgAdj(std::vector<Node> n)
    {
        Node res;
        std::vector<float> tempTrus, tempInds, tempFals;
        float weight = 1.00 / n.size();
        float init = 1.00;   

        res.nodeId = n[0].nodeId;
        res.label = n[0].label;
        res.state = n[0].state;
        res.linkState = n[0].linkState;

        for(int i = 0; i < n.size();i++)
        {
            tempTrus.emplace_back(n[i].truth);
            tempTrus[i] = pow(1-tempTrus[i],weight);

            tempInds.emplace_back(n[i].indeterminacy);
            tempInds[i] = pow(tempInds[i],weight);

            tempFals.emplace_back(n[i].falsehood);
            tempFals[i] = pow(tempFals[i],weight);
        }

        res.truth = 1 - (accumulate(begin(tempTrus), end(tempTrus), init, std::multiplies<float>()));
        res.indeterminacy = accumulate(begin(tempInds), end(tempInds), init, std::multiplies<float>());
        res.falsehood = accumulate(begin(tempFals), end(tempFals), init, std::multiplies<float>());

        return res;
    }

    // GEOMETRIC AVG - NOT USED
    Node geoAvgAdj(std::vector<Node> n)
    {
        Node res;
        std::vector<float> tempTrus, tempInds, tempFals;
        float weight = 1.00 / n.size();
        float init = 1.00;   

        res.nodeId = n[0].nodeId;
        res.label = n[0].label;
        res.state = n[0].state;
        res.linkState = n[0].linkState;

        for(int i = 0; i < n.size();i++)
        {
            tempTrus.push_back(n[i].truth);
            tempTrus[i] = pow(tempTrus[i],weight);

            tempInds.push_back(n[i].indeterminacy);
            tempInds[i] = pow(1-tempInds[i],weight);

            tempFals.push_back(n[i].falsehood);
            tempFals[i] = pow(1-tempFals[i],weight);
        }    

        res.truth = accumulate(begin(tempTrus), end(tempTrus), init, std::multiplies<float>());
        res.indeterminacy = 1 - (accumulate(begin(tempInds), end(tempInds), init, std::multiplies<float>()));
        res.falsehood = 1 - (accumulate(begin(tempFals), end(tempFals), init, std::multiplies<float>()));

        return res;
    }

    // ARITHMETIC-GEOMETRIC AVG (HYBRID)
    Node hybridAvgAdj(std::vector<Node> n)
    {
        Node res;
        std::vector<float> tempTrusAa, tempIndsAa,tempFalsAa, tempTrusGa, tempIndsGa, tempFalsGa;

        float weight = 1.00 / n.size();
        float methodWeight = 1.00 / 2;
        float init = 1.00;

        res.nodeId = n[0].nodeId;
        res.label = n[0].label;
        res.state = n[0].state;
        res.linkState = n[0].linkState;
        
        for(int i = 0; i < n.size(); i++)
        {
            tempTrusAa.push_back(n[i].truth);
            tempTrusAa[i] = pow(1 - tempTrusAa[i], weight);
        
            tempIndsAa.push_back(n[i].indeterminacy);
            tempIndsAa[i] = pow(tempIndsAa[i], weight);
        
            tempFalsAa.push_back(n[i].falsehood);
            tempFalsAa[i] = pow(tempFalsAa[i], weight);
        
            tempTrusGa.push_back(n[i].truth);
            tempTrusGa[i] = pow(tempTrusGa[i], weight);
        
            tempIndsGa.push_back(n[i].indeterminacy);
            tempIndsGa[i] = pow(1 - tempIndsGa[i], weight);
        
            tempFalsGa.push_back(n[i].falsehood);
            tempFalsGa[i] = pow(1 - tempFalsGa[i], weight);
        }    

        res.truth = pow((1 - (accumulate(begin(tempTrusAa), end(tempTrusAa), init, std::multiplies<float>()))), methodWeight) * pow((accumulate(begin(tempTrusGa), end(tempTrusGa), init, std::multiplies<float>())), methodWeight);
        res.indeterminacy = pow((accumulate(begin(tempIndsAa), end(tempIndsAa), init, std::multiplies<float>())), methodWeight) * pow((1 - (accumulate(begin(tempIndsGa), end(tempIndsGa), init, std::multiplies<float>()))), methodWeight);
        res.falsehood = pow((accumulate(begin(tempFalsAa), end(tempFalsAa), init, std::multiplies<float>())), methodWeight) * pow((1 - (accumulate(begin(tempFalsGa), end(tempFalsGa), init, std::multiplies<float>()))), methodWeight);

        return res;
    }



    // ------------------------------------------------------------------------------------
    // NEUTRO OPERATIONS
    // ------------------------------------------------------------------------------------



    // COMPLEMENT
    Edge complement(Edge e)
    {
        float temp = e.falsehood;

        e.falsehood = e.truth;
        e.indeterminacy = 1 - e.indeterminacy;
        e.truth = temp;
        
        return e;
    }

    // MULTIPLICATION
    Node multiply(Node m, Edge e)
    {
        Node temp;

        temp.truth = m.truth * e.truth;
        temp.indeterminacy = m.indeterminacy + e.indeterminacy - (m.indeterminacy * e.indeterminacy);
        temp.falsehood = m.falsehood + e.falsehood - (m.falsehood * e.falsehood);

        return temp;
    }

    // ADDITION
    Node addition(Node n, Node t)
    {
        Node temp1;

        temp1.truth = (n.truth + t.truth) - (n.truth * t.truth);
        temp1.indeterminacy = n.indeterminacy * t.indeterminacy;
        temp1.falsehood = n.falsehood * t.falsehood;

        return temp1;
    }

    // SUBTRACTION
    Node subtraction(Node n, Node t)
    {
        Node temp1;

        temp1.truth = (n.truth - t.truth) / (1 - t.truth);
        temp1.indeterminacy = n.indeterminacy / t.indeterminacy;
        temp1.falsehood = n.falsehood / t.falsehood;

        return temp1;
    }



    // ------------------------------------------------------------------------------------
    // HELPER FUNCTIONS
    // ------------------------------------------------------------------------------------



    std::vector<Node> appendToNodeList(std::vector<Node> baseList, std::vector<Node> listToBeAppended)
    {
        for (int i = 0; i < listToBeAppended.size(); i++)
        {
            baseList.push_back(listToBeAppended[i]);  
        }

        return baseList;
    }

    std::vector<Node> sortNodes(std::vector<Node> nodes)
    {
        for (int z = 0; z < nodes.size(); z++)
        {
            for (int y = nodes.size() - 1; y > z; y--)
            {
                if(nodes[y].nodeId < nodes[y-1].nodeId)
                {
                    Node temp = nodes[y];
                    nodes[y] = nodes[y - 1];
                    nodes[y - 1] = temp;
                }
            }
        }

        return nodes;
    }

    std::vector<Node> deleteDuplicateNodes(std::vector<Node> nodes)
    {
        for (int i = 0; i < nodes.size(); i++)
        {
            for (int j = i + 1; j < nodes.size(); j++)
            {
                if (nodes[i].nodeId == nodes[j].nodeId)
                {
                    nodes.erase(nodes.begin() + j);
                }
            }
        }

        return nodes;
    }

    std::vector<Edge> appendToEdgeList(std::vector<Edge> baseList,std::vector<Edge> listToBeAppended)
    {
        for (int i = 0; i < listToBeAppended.size(); i++)
        {
            baseList.push_back(listToBeAppended[i]);  
        }

        return baseList;
    }

    void sortEdges(std::vector<Edge> e, std::unordered_map<int, Node> hashMapNodes)
    {
        for (int i = 1; i < e.size(); i++)
        {
            Edge temp;
            bool compEdgesRes = compareEdges(hashMapNodes, e[i], e[i-1]);
            if(compEdgesRes == true)
            {
                temp = e[i-1];
                e[i-1] = e[i];
                e[i] = temp;                  
            }  
        }
    }

    bool compareEdges(std::unordered_map<int, Node>hashMapNodes, Edge e, Edge f)
    {
        Edge temp;
        Node currTransNode = hashMapNodes[e.from];
        Node currRecNode = hashMapNodes[e.to];

        Node compTransNode = hashMapNodes[f.from];
        Node compRecNode = hashMapNodes[f.to];

        int currDegreeSum, compDegreeSum, currInDegSum, 
            currOutDegSum, compInDegSum, compOutDegSum;

        currInDegSum = currTransNode.inDegree + currRecNode.inDegree;
        currOutDegSum = currTransNode.outDegree + currRecNode.outDegree;

        compInDegSum = compTransNode.inDegree + compRecNode.inDegree;
        compOutDegSum = compTransNode.outDegree + compRecNode.outDegree;

        currDegreeSum = currInDegSum + currOutDegSum;
        compDegreeSum = compInDegSum + compOutDegSum;

        if(currDegreeSum < compDegreeSum || currRecNode.nodeId < compRecNode.nodeId)
        {
            return true;
        }
        return false;
    }

    void addNode(Node n){
        nodes.push_back(n);
    }

    void addEdge(Edge e){
        edges.push_back(e);
    }

    Node printNode(int index){
        return nodes[index];
    }

    Node printFinalResNode(int index){
        return finalResNodes[index];
    }

    Edge printEdge(int index){
        return edges[index];
    } 

    void deleteModel()
    {
        nodes.clear();
        tempResNodes.clear();
        finalResNodes.clear();
        edges.clear();
        edgesCopy.clear();
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

private:
  std::vector<Node> nodes, finalResNodes, tempResNodes;
  std::vector<Edge> edges, edgesCopy;
                  
  std::string y; // test variable
};

// Binding code
EMSCRIPTEN_BINDINGS(neutro_calculator) {

    // NODE OBJECT
    value_object<Node>("Node")
        .field("nodeId", &Node::nodeId)
        .field("label", &Node::label)
        .field("truth", &Node::truth)
        .field("indeterminacy", &Node::indeterminacy)
        .field("falsehood", &Node::falsehood)
        .field("state", &Node::state)
        .field("linkState", &Node::linkState)
        .field("inDegree", &Node::inDegree)
        .field("outDegree", &Node::outDegree)
        ;
    // EDGE OBJECT
    value_object<Edge>("Edge")
        .field("edgeId", &Edge::edgeId)
        .field("from", &Edge::from)
        .field("to", &Edge::to)
        .field("truth", &Edge::truth)
        .field("indeterminacy", &Edge::indeterminacy)
        .field("falsehood", &Edge::falsehood)
        ;

    
    class_<NeutroCalculator>("NeutroCalculator")
        .constructor<std::string>()

        // Neutro Computation functions
        .function("simulate", &NeutroCalculator::simulate)
        .function("directlyPropRelation", &NeutroCalculator::directlyPropRelation)
        .function("inverselyPropRelation", &NeutroCalculator::inverselyPropRelation)
        
        // Neutro Adjacency Operator functions
        .function("minMaxAdj", &NeutroCalculator::minMaxAdj)
        .function("arithAvgAdj", &NeutroCalculator::arithAvgAdj)
        .function("geoAvgAdj", &NeutroCalculator::geoAvgAdj)
        .function("hybridAvgAdj", &NeutroCalculator::hybridAvgAdj)
        
        // Neutro Operators functions
        .function("complement", &NeutroCalculator::complement)
        .function("multiply", &NeutroCalculator::multiply)
        .function("addition", &NeutroCalculator::addition)
        .function("subtraction", &NeutroCalculator::subtraction)
        
        // Helper functions
        .function("addNode", &NeutroCalculator::addNode)
        .function("addEdge", &NeutroCalculator::addEdge)
        .function("appendToNodeList", &NeutroCalculator::appendToNodeList)
        .function("appendToEdgeList", &NeutroCalculator::appendToEdgeList)
        .function("compareEdges", &NeutroCalculator::compareEdges)
        .function("deleteDuplicateNodes", &NeutroCalculator::deleteDuplicateNodes)
        .function("sortNodes", &NeutroCalculator::sortNodes)
        .function("sortEdges", &NeutroCalculator::sortEdges)
        .function("getSimulationResult", &NeutroCalculator::getSimulationResult)
        .function("deleteModel", &NeutroCalculator::deleteModel)


        // test functions 
        .function("printNode", &NeutroCalculator::printNode)
        .function("printFinalResNode", &NeutroCalculator::printFinalResNode)
        .function("printEdge", &NeutroCalculator::printEdge)
        .class_function("getStringFromInstance", &NeutroCalculator::getStringFromInstance)
        ;

}
