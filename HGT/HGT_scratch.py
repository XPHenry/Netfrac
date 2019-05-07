#some string manipulation to get the same format as in HGT1_nodes.txt

def findnth(haystack, needle, n):
    parts= haystack.split(needle, n+1)
    if len(parts)<=n+1:
        return -1
    return len(haystack)-len(parts[-1])-len(needle)

#extract the tetracycline lines and put them in the right format

fh = open("./mothur/TetA_seq.fasta","r")

nodes = []
i = 0
for line in fh:
    line = line.replace("\n", "")
    if ">pl" in line:
        line = line.replace(">pl", "")
        line = line[0:findnth(line,"|",6)]
        x = findnth(line,"|",2)
        line = line[x:] + line[:x]
        line = line.replace("||","_")
        line = line.replace("|","")
        y = line[findnth(line," ",0):findnth(line,"_",1)]
        line = line.replace(y,"")
        nodes.append(line)
fh.close()
print(nodes)


#only replace the lines that have the header
sequences = []
fh = open("./mothur/TetA_seq.fasta","r")
for line in fh:
    if ">pl" in line:
        line = ">" + nodes[i] + "\n"
        i += 1
    sequences.append(line)

print(sequences)

fh = open("./mothur/TetA_seq_good.fasta","w")
for string in sequences:
    fh.write(string)

fh.close()

"""
nodes = []
sequences = []
i = 0
for line in fh:
    line = line.replace("\n", "")
    print(line)
    if ">" in line:
        nodes.append(line)
        i += 1
        nodes.append(" ")
    else:
        nodes[i] += line

for i in range(0,len(nodes)):
    if i%2 == 1:
        sequences.append(nodes[i])
del nodes[1::2]
print(len(nodes))
print(len(sequences))

print(nodes[0:100])
print(sequences[0:100])
"""