def findnth(haystack, needle, n):
    parts= haystack.split(needle, n+1)
    if len(parts)<=n+1:
        return -1
    return len(haystack)-len(parts[-1])-len(needle)


fh = open("emi_2295_sm_S2_Rev","r")
nodes = []
for line in fh:
    line = line.replace("\n","")
    line = line.replace("\t","")
    line = line.replace("_domai", "_domain")
    line = line.strip("|")
    line = line.strip("{")
    line = line.strip("}")
    line = line.strip("0 1")

    if "8607aa" in line:
        break
    nodes.append(line)

edges = []
for line in fh:
    line = line.replace("\n", "")
    line = line[:line.find(" 0 ")]
    if "version" in line:
        break
    edges.append(line)

fh.close()

edge2 = []
for edge in edges:
    node1, node2 = edge.split(" ")
    node1 = node1.replace(node1, nodes[int(node1)+3])
    node2 = node2.replace(node2, nodes[int(node2)+3])
    edge2.append(node1+"\t"+node2+"\t1\n")

"""

fh = open("HGT_edges.txt","w")
for edge in edge2:
    fh.write(edge)

fh.close()

fh = open("HGT_nodes.txt","w")
for node in nodes:
    node = node + "\tunknown\n"
    fh.write(node)

fh.close()

accession = [89009830, 226807693, 226810004, 33757319, 23928457, 956601, 51492604, 5983524, 5983535,
                 70783437, 215528133, 38261100, 56709212, 4881111, 209921963, 45294027, 70783420]
accession = sorted(accession)
node_acc = []
for node in nodes:
    for i in accession:
        if str(i) in str(node) and node not in node_acc:
            node_acc.append(node)

node_acc = sorted(node_acc)
print(node_acc)

node_final = {}
for node in node_acc:
    node1 = node[0:findnth(node,"_",2)]
    node_final[node] = node1

print(node_final)

edge_acc = []
for edge in edge2:
   node1,node2,n = edge.split("\t")
   if node1 in node_acc and node2 in node_acc and edge not in edge_acc:
       edge_acc.append(edge)

print(edge_acc)

edge_join = []
edges_final = []
for edge in edge_acc:
    edge = edge.split("\t")
    edge_join = []
    for element in edge:
        if element in node_final.keys():
            element = node_final[element]
        edge_join.append(element)
    edge_join = "\t".join(edge_join)
    edges_final.append(edge_join)

print(edges_final)

fh = open("HGT1_edges.txt","w")
for edge in edges_final:
    fh.write(edge)

fh.close()
#
# fh = open("HGT1_nodes.txt","w")
# for node in node_acc:
#     node = node + "\tunknown\n"
#     fh.write(node)
#
# fh.close()

"""


accession = [89332439,8875441,56522763,45301309,32470483,89142216,70650787,956206,5534812,61867932,209947535,70780408,62145844,211774472,956142,70780405,90570438,9633061,2084929,8466598,60431832,90576892,52973805,87736853,49176955,33756463,226810115,38347932,226807801,68998556,34044746]
community = ["host","unknown","host","water","soil","unknown","soil","soil","water","host","host","host","host","host","host",
             "water","water","unknown","host","host","soil","soil","host","host","host","host","host","host","host","host"]
accession = sorted(accession)
print(len(accession))
node_acc = []
for node in nodes:
    for i in accession:
        if str(i) in str(node) and node not in node_acc:
            node_acc.append(node)

node_acc = sorted(node_acc)


node_final = {}
for node in node_acc:
    node1 = node[0:findnth(node,"_",2)]
    node_final[node] = node1

print(node_final)

edge_acc = []
for edge in edge2:
   node1,node2,n = edge.split("\t")
   if node1 in node_acc and node2 in node_acc and edge not in edge_acc:
       edge_acc.append(edge)

edge_join = []
edges_final = []
for edge in edge_acc:
    edge = edge.split("\t")
    edge_join = []
    for element in edge:
        if element in node_final.keys():
            element = node_final[element]
        edge_join.append(element)
    edge_join = "\t".join(edge_join)
    edges_final.append(edge_join)

print(len(edge_acc))

fh = open("HGT2_edges.txt","w")
for edge in edges_final:
    fh.write(edge)

fh.close()

fh = open("HGT2_nodes.txt","w")
for value, node in node_final.items():
    node = node + "\tunknown\n"
    fh.write(node)

fh.close()

