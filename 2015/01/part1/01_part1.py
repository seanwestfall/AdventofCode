with open("./data/input.txt", "r") as f:
    instructions = f.read().strip()

floor = instructions.count("(") - instructions.count(")")
print(floor)
