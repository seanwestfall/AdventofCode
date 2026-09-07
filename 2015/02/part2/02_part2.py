total_ribbon = 0

with open("./data/input.txt", "r") as f:
    for line in f:
        l, w, h = map(int, line.strip().split("x"))

        # Sort so the first two dimensions are the smallest
        dimensions = sorted([l, w, h])

        # Ribbon around the smallest face
        wrap = 2 * dimensions[0] + 2 * dimensions[1]

        # Ribbon for the bow
        bow = l * w * h

        total_ribbon += wrap + bow

print(total_ribbon)
