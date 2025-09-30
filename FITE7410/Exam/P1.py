# Problem 1
# Write a Python program which takes one user input integer n (0 <= n <=100,000)
# and find all right-angled triangles with integer side lengths whose perimeter is n.
# Input: 
#       - The program reads one integer from user.
# Output:
#       - Each triangle should be denoted by its three side lengths a, b, c, and d, where a <= b <= c.
#       - The program should output all unique triangles, ordered by side lengths a, b, then c.
#       - The program should output "No triangle found" when there is no result.

def right_triangle_finder(n):
    assert 0 <= n <= 100000, "n must be between 0 and 100,000"
    triangles = []
    for x in range(1, n):
        for y in range (x, n):
            z = (x**2 + y**2)**0.5
            if z.is_integer() and z <= n and x + y + z == n:
                triangles.append((x, y, int(z)))
    if len(triangles) == 0:
        return "No triangle found"
    return triangles