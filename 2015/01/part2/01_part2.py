with open("./data/input.txt", "r") as f:
    instructions = f.read().strip()

floor = 0
basement_position = None

for position, char in enumerate(instructions, start=1):
    if char == "(":
        floor += 1
    elif char == ")":
        floor -= 1

    if floor == -1 and basement_position is None:
        basement_position = position

print("Part 1:", floor)
print("Part 2:", basement_position)
