import os
import sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import numpy as np
import worker
data = worker.get_aoc(3, 2016)

def triangle_check(sides):
    sides.sort()
    if sides[0] + sides[1] > sides[2]: return 1 
    else: return 0

def solve(data: str) -> int:       # start on '5'
    count = 0

    for sides in data.splitlines():
        sides = [int(x) for x in sides.split()]
        count += triangle_check(sides)

    return count

def part2(data: str) -> int:       # start on '5'
    count = 0
    vector_2d = np.array([[int(x) for x in line.split()] for line in data.strip().split('\n')])
    arr = np.rot90(vector_2d)
    for rows in arr:           
            for i in range(0, len(rows), 3):
                triangle = rows[i:i+3]
                count += triangle_check(triangle)

    return count



if __name__ == "__main__":
    example = "5 10 25\n3 4 5\n7 10 12"
    print("Example:", solve(example), "(expected 2)")


    data = worker.get_aoc(3, 2016)
    print("Part 1 answer:", solve(data)) #862
    print("Part 2 answer:", part2(data)) #1577