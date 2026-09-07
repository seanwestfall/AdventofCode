total_paper = 0

with open("./data/input.txt", "r") as f:
    for line in f:
        l, w, h = map(int, line.strip().split("x"))

        sides = [
            l * w,
            w * h,
            h * l,
        ]

        surface_area = 2 * sum(sides)
        slack = min(sides)

        total_paper += surface_area + slack

print(total_paper)
