def findnth(haystack, needle, n):
    parts= haystack.split(needle, n+1)
    if len(parts)<=n+1:
        return -1
    return len(haystack)-len(parts[-1])-len(needle)

#read the file with nodes info
fh = open("CAT_nodes.txt","r")
nodes = fh.readlines()
print(nodes)

fh.close()

#get the species  information, and the order it belongs to
nodes_dict = {}
gamma_list = ["Aeromonas","Haemophilus","Escherichia","Salmonella","Yersinia","Klebsiella","Pseudomonas","Serratia","Mannheimia","Shigella","Bibersteinia","Vibrio","Pasteurella"]
bacilli_list = ["Staphylococcus","Lactobacillus","Lactococcus","Enterococcus","Streptococcus"]
actino_list = ["Corynebacterium","Rhodococcus","Actinobacillus"]
other_list = ["Ruegeria","Laribacter","Clostridium","Neisseria"]

#make a dictionnary linking species with order
for line in nodes:
    species = line[findnth(line,"_",0)+1:findnth(line,"_",1)]
    if species not in nodes_dict.keys():
        if species in gamma_list:
            nodes_dict[species] = "Gammaproteobacteria"
        elif species in bacilli_list:
            nodes_dict[species] = "Bacilli"
        elif species in actino_list:
            nodes_dict[species] = "Actinobacteria"
        elif species in other_list:
            nodes_dict[species] = "Other"


print(nodes_dict)

#write the result in a file
fh = open("CAT_nodes_final.txt","w")
for line in nodes:
    line = line.replace("\n","")
    species = line[findnth(line, "_", 0) + 1:findnth(line, "_", 1)]
    node_info = line + "\t" + nodes_dict[species] + "\n"
    fh.write(node_info)

fh.close()
