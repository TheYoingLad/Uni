with open("quiz_data.js", "r") as filp:
	data = filp.readlines()

lin = 1
for i,e in enumerate(data):
	if e[0]=='"':
		sp = e.split(": ")
		data[i] = f'"{lin}": {sp[1]}'
		lin += 1

with open("quiz_new.js","w") as filp:
	filp.write(''.join(data))