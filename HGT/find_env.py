import pandas as pd

def findnth(haystack, needle, n):
    parts= haystack.split(needle, n+1)
    if len(parts)<=n+1:
        return -1
    return len(haystack)-len(parts[-1])-len(needle)


num_acc = []

#read the nodes, and add the accession in a list
"""
fh = open("CAT_nodes.txt","r")
for line in fh:
    line1 = line[0:findnth(line,"_",0)]
    num_acc.append(line1)

print(num_acc)

fh.close()
"""
fh = open("TetA_nodes.txt","r")
for line in fh:
    line1 = line[0:findnth(line,"_",0)]
    num_acc.append(line1)

print(num_acc)

fh.close()


colors = pd.read_csv("NODE_info_env.txt", sep="\t",
                  engine='python')

rows = []
columns = ["Accession","r","g","b"]
for acces in num_acc:
    for i in range(colors.shape[0]):
        if acces in colors.iloc[i,3]:
            row = [colors.iloc[i,3],colors.iloc[i,0],colors.iloc[i,1],colors.iloc[i,2]]
            rows.append(row)
            break

colors_CAT = pd.DataFrame(rows, columns=columns)
print(colors_CAT)

rows = []
columns = ["Accession", "tax"]
for i in range(colors_CAT.shape[0]):
    acces = colors_CAT.iloc[i,0]
    acces = acces[0:findnth(acces,"_",2)]
    if colors_CAT.iloc[i,1] == 255:
        color = "host"
    elif colors_CAT.iloc[i,3] == 255:
        color = "water"
    elif colors_CAT.iloc[i,2] == 139:
        color = "ubiquitous"
    else:
        color = "unknown"
    row = [acces, color]
    rows.append(row)

#tax_CAT = pd.DataFrame(rows, columns=columns)
tax_TetA = pd.DataFrame(rows, columns=columns)

#tax_CAT.to_csv("CAT_nodes_env.txt", sep='\t', encoding='utf-8', index = False, header=None)
tax_TetA.to_csv("TetA_nodes_env.txt", sep='\t', encoding='utf-8', index = False, header=None)



#there are missing values in the environment file (with rgb) for the TetA nodes