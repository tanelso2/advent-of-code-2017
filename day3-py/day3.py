def get_location(n):
    if n == 1:
        return 0, 0
    for layer in xrange(9999999):
        odd_number = 2 * layer + 1
        if n <= odd_number ** 2:
            upper_n = odd_number
            lower_n = odd_number - 2
            upper_bound = upper_n ** 2
            lower_bound = lower_n ** 2
            break
    difference = upper_bound - lower_bound
    lower_right_corner = upper_bound
    upper_right_corner = lower_bound + (difference / 4)
    upper_left_corner = lower_bound + 2 * (difference / 4)
    lower_left_corner = lower_bound + 3 * (difference / 4)
    corner_distance = layer
    if n <= upper_right_corner:
        diff = upper_right_corner - n
        return corner_distance, corner_distance - diff
    elif n <= upper_left_corner:
        diff = upper_left_corner - n
        return (-corner_distance) + diff, corner_distance
    elif n <= lower_left_corner:
        diff = lower_left_corner - n
        return -corner_distance, (-corner_distance) + diff
    elif n <= lower_right_corner:
        diff = lower_right_corner - n
        return corner_distance - diff, -corner_distance

def manhattan_distance(x, y):
    return abs(x) + abs(y)

def print_distance(n):
    x, y = get_location(n)
    print(manhattan_distance(x, y))

puzzle_input = 368078

print_distance(1)
print_distance(12)
print_distance(23)
print_distance(1024)
print_distance(puzzle_input)

LOC_TO_N = {get_location(1): 1}
LOC_TO_VAL = {get_location(1): 1}

def adjacent_locations(x, y):
    ret = []
    for i in range(-1, 2):
        for j in range(-1, 2):
            if i == 0 and j == 0:
                continue
            ret.append((x + i, y + j))
    return ret

for i in xrange(2, puzzle_input):
    location = get_location(i)
    LOC_TO_N[location] = i
    adj_locs = adjacent_locations(location[0], location[1])
    total = 0
    for adj_loc in adj_locs:
        if adj_loc in LOC_TO_VAL:
            total += LOC_TO_VAL[adj_loc]
    LOC_TO_VAL[location] = total
    if total >= puzzle_input:
        print("i is {}".format(i))
        print("total is {}".format(total))
        break

